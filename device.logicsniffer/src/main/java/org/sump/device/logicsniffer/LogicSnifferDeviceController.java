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
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package org.sump.device.logicsniffer;


import java.awt.*;
import java.beans.*;
import java.io.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import org.osgi.framework.*;


/**
 * GUI Component that allows the user to control the device and start captures.
 * <p>
 * Its modelled after JFileChooser and should allow for non-dialog
 * implementations making it somewhat reusable.
 * 
 * @version 0.7
 * @author Michael "Mr. Sump" Poppitz
 */
public class LogicSnifferDeviceController implements DeviceController
{
  // CONSTANTS

  private static final String NAME = "OpenBench LogicSniffer";

  private static final Logger LOG = Logger.getLogger( LogicSnifferDeviceController.class.getName() );

  // VARIABLES

  private final LogicSnifferDevice device;
  private LogicSnifferConfigDialog configDialog;
  private boolean setup;

  // CONSTRUCTORS

  /**
   * Constructs device controller component.
   */
  public LogicSnifferDeviceController()
  {
    this.device = new LogicSnifferDevice();

    this.setup = false;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#cancel()
   */
  @Override
  public void cancel() throws IllegalStateException
  {
    if ( isCapturing() )
    {
      this.device.stop();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#captureData(nl.lxtreme.ols.api.devices.CaptureCallback)
   */
  @Override
  public void captureData( final CaptureCallback aCallback ) throws IOException
  {
    final String portName = this.configDialog.getPortName();
    final int baudrate = this.configDialog.getPortBaudrate();

    // Tell the device on what port & what rate to do its job...
    this.device.setPortSettings( portName, baudrate );

    // Listen to various properties for reporting it to our callback...
    this.device.addPropertyChangeListener( new PropertyChangeListener()
    {
      /**
       * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
       */
      @Override
      public void propertyChange( final PropertyChangeEvent aEvent )
      {
        final Object value = aEvent.getNewValue();
        final String propertyName = aEvent.getPropertyName();

        if ( LogicSnifferDevice.PROP_CAPTURE_PROGRESS.equals( propertyName ) )
        {
          final Integer progress = ( Integer )value;

          LOG.log( Level.FINE, "Progress {0}%", progress );

          aCallback.updateProgress( progress );
        }
        else if ( LogicSnifferDevice.PROP_CAPTURE_ABORTED.equals( propertyName ) )
        {
          final String abortReason = ( String )value;

          LOG.log( Level.WARNING, "Capture aborted: {0}.", abortReason );

          aCallback.captureAborted( abortReason );
        }
        else if ( LogicSnifferDevice.PROP_CAPTURE_DONE.equals( propertyName ) )
        {
          final CapturedData data = ( CapturedData )value;

          LOG.log( Level.INFO, "Capture completed {0}.", ( data == null ? "WITHOUT data" : "with data" ) );

          aCallback.captureComplete( data );
        }
      }
    } );

    // Let the capturing take place in a background thread...
    this.device.execute();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#getName()
   */
  public String getName()
  {
    return NAME;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#isCapturing()
   */
  @Override
  public boolean isCapturing()
  {
    return this.device.isRunning();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#isSetup()
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
   * @see nl.lxtreme.ols.api.devices.DeviceController#setupCapture()
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
      this.configDialog = new LogicSnifferConfigDialog( aOwner, this.device );
    }

    this.setup = this.configDialog.showDialog();
    return this.setup;
  }

  /**
   * Called when this class is registered as OSGi service.
   * 
   * @param aBundleContext
   *          the bundle context to use, cannot be <code>null</code>.
   * @throws Exception
   *           in case of errors.
   */
  protected void init( final BundleContext aBundleContext ) throws Exception
  {
    // Keep for later use...
    this.device.setBundleContext( aBundleContext );
  }
}
