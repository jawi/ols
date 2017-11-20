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
package org.sump.device.logicsniffer;


import java.awt.*;
import java.io.*;
import java.util.logging.*;
import java.util.logging.Logger;

import javax.microedition.io.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.io.*;
import org.sump.device.logicsniffer.profile.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * A representation of the LogicSniffer device.
 *
 * @author J.W. Janssen
 */
public class LogicSnifferDevice implements Device
{
  // CONSTANTS

  private static final String NAME = "OpenBench LogicSniffer";

  private static final Logger LOG = Logger.getLogger( LogicSnifferDevice.class.getName() );

  // VARIABLES

  private LogicSnifferConfig config;

  private volatile DependencyManager dependencyManager;
  private volatile ManagedServiceFactory deviceProfileManagerServiceFactory;
  private volatile ConnectorService connectorService;
  private volatile StreamConnection connection;
  private volatile LogicSnifferConfigDialog configDialog;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    if ( this.connection != null )
    {
      this.connection.close();
      this.connection = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionTask createAcquisitionTask( final AcquisitionProgressListener aProgressListener ) throws IOException
  {
    return new LogicSnifferAcquisitionTask( this.config, getStreamConnection(), getDeviceProfileManager(),
        aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CancelTask createCancelTask() throws IOException
  {
    if ( this.config.isRleEnabled() )
    {
      return new LogicSnifferCancelTask( getStreamConnection() );
    }
    // Simply use the default behaviour...
    return null;
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
   * Displays the device controller dialog with enabled configuration portion
   * and waits for user input.
   *
   * @see nl.lxtreme.ols.api.devices.Device#setupCapture()
   */
  @Override
  public boolean setupCapture( final Window aOwner )
  {
    // Just to be sure...
    disposeConfigDialog();

    this.configDialog = new LogicSnifferConfigDialog( aOwner, this );

    try
    {
      boolean configConfirmed = this.configDialog.showDialog();
      if ( configConfirmed )
      {
        this.config = this.configDialog.getConfiguration();
      }
      return configConfirmed;
    }
    finally
    {
      this.configDialog.dispose();
      this.configDialog = null;
    }
  }

  /**
   * @param uri
   * @return
   * @throws IOException
   */
  final StreamConnection createStreamConnection( final String uri ) throws IOException
  {
    return ( StreamConnection )this.connectorService.open( uri, ConnectorService.READ_WRITE,
        true /* timeouts */ );
  }

  /**
   * Returns the default device profile.
   *
   * @return a default profile, never <code>null</code>.
   */
  final DeviceProfile getDefaultProfile()
  {
    return getDeviceProfileManager().getDefaultProfile();
  }

  /**
   * Returns the current device profile manager.
   *
   * @return a device profile manager, never <code>null</code>.
   */
  final DeviceProfileManager getDeviceProfileManager()
  {
    return ( DeviceProfileManager )this.deviceProfileManagerServiceFactory;
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
    final String pmFilter = String.format( "(%s=%s)", Constants.SERVICE_PID, DeviceProfileManager.SERVICE_PID );

    aComponent //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( ManagedServiceFactory.class, pmFilter ) //
            .setAutoConfig( "deviceProfileManagerServiceFactory" ) //
            .setRequired( true ) ) //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( ConnectorService.class ) //
            .setAutoConfig( "connectorService" ) //
            .setRequired( true ) //
    );
  }

  /**
   * Disposes the current configuration dialog, if one is still visible on
   * screen. If no configuration dialog is visible, this method does nothing.
   */
  private void disposeConfigDialog()
  {
    if ( this.configDialog != null )
    {
      SwingComponentUtils.dispose( this.configDialog );
      this.configDialog = null;
    }
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
    if ( this.connection == null )
    {
      final String uri = this.config.getConnectionURI();

      if ( LOG.isLoggable( Level.INFO ) )
      {
        LOG.info( "Connecting to " + uri );
      }

      this.connection = createStreamConnection( uri );
    }
    return this.connection;
  }
}
