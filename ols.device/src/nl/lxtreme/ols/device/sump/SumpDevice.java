/*
 * OpenBench LogicSniffer / SUMP project
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
 *
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump;


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.device.sump.config.*;
import nl.lxtreme.ols.device.sump.profile.*;
import nl.lxtreme.ols.task.execution.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.io.*;


/**
 * A representation of the LogicSniffer device.
 * 
 * @author J.W. Janssen
 */
public class SumpDevice implements Device
{
  // CONSTANTS

  private static final String NAME = "OpenBench LogicSniffer";

  private static final Logger LOG = Logger.getLogger( SumpDevice.class.getName() );

  // VARIABLES

  private SumpConfig config = null;
  private SumpConfigDialog configDialog;

  private volatile DependencyManager dependencyManager;
  private volatile ManagedServiceFactory deviceProfileManagerServiceFactory;
  private volatile ConnectorService connectorService;
  private volatile TaskExecutionService taskExecutionService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Future<AcquisitionData> acquireData( DeviceConfiguration aConfig, AcquisitionProgressListener aProgressListener )
      throws IOException
  {
    if ( aConfig == null || !( aConfig instanceof SumpConfig ) )
    {
      throw new IllegalArgumentException( "Invalid device configuration!" );
    }

    this.config = ( SumpConfig )aConfig;

    return this.taskExecutionService.execute( new SumpAcquisitionTask( this.config, getStreamConnection(),
        aProgressListener ), Collections.singletonMap( "type", "acquisition" ) );
  }

  /**
   * @param uri
   * @return
   * @throws IOException
   */
  public StreamConnection createStreamConnection( final String uri ) throws IOException
  {
    return ( StreamConnection )this.connectorService.open( uri, ConnectorService.READ_WRITE, true /* timeouts */);
  }

  /**
   * @param aMetadata
   * @return
   * @throws InvalidSyntaxException
   */
  public DeviceProfile findDeviceProfile( DeviceMetadata aMetadata ) throws InvalidSyntaxException
  {
    return getDeviceProfileManager().findProfile( aMetadata );
  }

  /**
   * Returns the default device profile.
   * 
   * @return a default profile, never <code>null</code>.
   */
  public DeviceProfile getDefaultProfile()
  {
    return getDeviceProfileManager().getDefaultProfile();
  }

  /**
   * @return
   */
  public List<DeviceProfile> getDeviceProfiles()
  {
    return getDeviceProfileManager().getProfiles();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#getName()
   */
  public String getName()
  {
    return NAME;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#isSetup()
   */
  @Override
  public boolean isSetup()
  {
    return this.config != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceConfiguration setupDevice()
  {
    final Window currentWindow = SwingComponentUtils.getCurrentWindow();

    disposeConfigDialog();

    this.configDialog = new SumpConfigDialog( currentWindow, this );

    if ( this.configDialog.showDialog() )
    {
      this.config = this.configDialog.getConfiguration();
    }

    return null; // XXX this.config != null ? this.config.asMap() : null;
  }

  /**
   * Called when this class is unregistered as OSGi service.
   */
  protected void destroy( final Component aComponent )
  {
    disposeConfigDialog();
  }

  /**
   * Called when this class is registered as OSGi service.
   * 
   * @param aComponent
   *          the bundle context to use, cannot be <code>null</code>.
   */
  protected void init( final Component aComponent )
  {
    final String pmFilter = String.format( "(%s=%s)", org.osgi.framework.Constants.SERVICE_PID,
        DeviceProfileManager.SERVICE_PID );

    aComponent //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( ManagedServiceFactory.class, pmFilter ) //
            .setAutoConfig( "deviceProfileManagerServiceFactory" ) //
            .setInstanceBound( true ) //
            .setRequired( true ) ) //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( ConnectorService.class ) //
            .setAutoConfig( "connectorService" ) //
            .setInstanceBound( true ) //
            .setRequired( true ) //
        );
  }

  /**
   * Disposes the current configuration dialog, if one is still visible on
   * screen. If no configuration dialog is visible, this method does nothing.
   */
  private void disposeConfigDialog()
  {
    SwingComponentUtils.dispose( this.configDialog );
    this.configDialog = null;
  }

  /**
   * Returns the current device profile manager.
   * 
   * @return a device profile manager, never <code>null</code>.
   */
  private DeviceProfileManager getDeviceProfileManager()
  {
    return ( DeviceProfileManager )this.deviceProfileManagerServiceFactory;
  }

  /**
   * Returns the current stream connection that is opened.
   * 
   * @return a stream connection, can be a cached one, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems creating the stream connection.
   */
  private StreamConnection getStreamConnection() throws IOException
  {
    final String uri = this.config.getConnectionURI();

    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Connecting to " + uri );
    }

    return createStreamConnection( uri );
  }
}
