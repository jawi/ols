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

import javax.microedition.io.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.devices.*;

import org.osgi.framework.*;
import org.sump.device.logicsniffer.profile.*;


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

  private final LogicSnifferConfig deviceConfig;

  private DeviceProfileManagerTracker deviceProfileManagerTracker;
  private StreamConnectionFactory streamConnectionFactory;

  private LogicSnifferConfigDialog configDialog;
  private boolean setup;

  private volatile StreamConnection connection;

  // CONSTRUCTORS

  /**
   * Constructs device controller component.
   */
  public LogicSnifferDevice()
  {
    this.deviceConfig = new LogicSnifferConfig();
    this.setup = false;
  }

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
  public AcquisitionTask createAcquisitionTask( final AcquisitionProgressListener aProgressListener )
      throws IOException
  {
    return new LogicSnifferAcquisitionTask( this.deviceConfig, getStreamConnection(), getDeviceProfileManager(),
        aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CancelTask createCancelTask() throws IOException
  {
    if ( this.deviceConfig.isRleEnabled() )
    {
      return new LogicSnifferCancelTask( getStreamConnection() );
    }
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
    return this.setup;
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
    // check if dialog exists with different owner and dispose if so
    if ( ( this.configDialog != null ) && ( this.configDialog.getOwner() != aOwner ) )
    {
      this.configDialog.dispose();
      this.configDialog = null;
    }
    // if no valid dialog exists, create one
    if ( this.configDialog == null )
    {
      this.configDialog = new LogicSnifferConfigDialog( aOwner, this.deviceConfig, this.deviceProfileManagerTracker );
    }

    this.setup = this.configDialog.showDialog();
    return this.setup;
  }

  /**
   * Called when this class is unregistered as OSGi service.
   */
  protected void destroy()
  {
    this.streamConnectionFactory.close();
    this.deviceProfileManagerTracker.close();
  }

  /**
   * Called when this class is registered as OSGi service.
   * 
   * @param aBundleContext
   *          the bundle context to use, cannot be <code>null</code>.
   */
  protected void init( final BundleContext aBundleContext )
  {
    // Keep for later use...
    this.deviceProfileManagerTracker = new DeviceProfileManagerTracker( aBundleContext );
    this.deviceProfileManagerTracker.open();

    this.streamConnectionFactory = new StreamConnectionFactory( aBundleContext );
    this.streamConnectionFactory.open();
  }

  /**
   * @return
   */
  private DeviceProfileManager getDeviceProfileManager()
  {
    return this.deviceProfileManagerTracker.getService();
  }

  /**
   * @return
   * @throws IOException
   */
  private StreamConnection getStreamConnection() throws IOException
  {
    if ( this.connection == null )
    {
      final String portName = this.deviceConfig.getPortName();
      final int baudrate = this.deviceConfig.getBaudrate();
      final boolean dtrValue = this.deviceConfig.isOpenPortDtr();

      // Make sure we release the device if it was still attached...
      LOG.log( Level.INFO, "Attaching to {0} @ {1}bps (DTR = {2}) ...",
          new Object[] { portName, Integer.valueOf( baudrate ), dtrValue ? "high" : "low" } );

      this.connection = this.streamConnectionFactory.getConnection( portName, baudrate, dtrValue );
    }
    return this.connection;
  }
}
