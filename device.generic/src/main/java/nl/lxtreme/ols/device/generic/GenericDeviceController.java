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
package nl.lxtreme.ols.device.generic;


import java.awt.*;
import java.beans.*;
import java.io.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.SwingWorker.StateValue;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class GenericDeviceController implements DeviceController
{
  // CONSTANTS

  private static final String NAME = "Generic I/O";

  private static final Logger LOG = Logger.getLogger( GenericDeviceController.class.getName() );

  // VARIABLES

  private GenericDeviceConfigDialog deviceConfig = null;
  private GenericDevice device = null;
  private boolean setup = false;

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#cancel()
   */
  @Override
  public void cancel() throws IllegalStateException
  {
    if ( ( this.device != null ) && isCapturing() )
    {
      this.device.stop();
      this.device.cancel( true /* aMayInterruptIfRunning */);
    }
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#captureData(nl.lxtreme.ols.api.devices.CaptureCallback)
   */
  @Override
  public void captureData( final CaptureCallback aCallback ) throws IOException
  {
    // Listen to various properties for reporting it to our callback...
    final PropertyChangeListener propertyChangeListener = new PropertyChangeListener()
    {
      /**
       * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
       */
      @Override
      public synchronized void propertyChange( final PropertyChangeEvent aEvent )
      {
        final String propertyName = aEvent.getPropertyName();

        if ( GenericDevice.PROP_CAPTURE_PROGRESS.equals( propertyName ) )
        {
          final Integer progress = ( Integer )aEvent.getNewValue();

          LOG.log( Level.FINE, "Progress {0}%", progress );

          aCallback.updateProgress( progress.intValue() );
        }
        else if ( GenericDevice.PROP_CAPTURE_STATE.equals( propertyName ) )
        {
          final StateValue state = ( StateValue )aEvent.getNewValue();
          if ( StateValue.STARTED.equals( state ) )
          {
            final int sampleRate = GenericDeviceController.this.deviceConfig.getSampleRate();
            final int channelCount = GenericDeviceController.this.deviceConfig.getChannelCount();
            final int channelMask = GenericDeviceController.this.deviceConfig.getEnabledChannelsMask();

            // Notify our caller that we're started capturing...
            aCallback.captureStarted( sampleRate, channelCount, channelMask );
          }
        }
      }
    };

    this.device = new GenericDevice( this.deviceConfig )
    {
      /**
       * @see javax.swing.SwingWorker#done()
       */
      @Override
      protected void done()
      {
        try
        {
          final AcquisitionResult data = get();

          aCallback.captureComplete( data );
        }
        catch ( CancellationException exception )
        {
          // simply canceled by user...
          aCallback.captureAborted( "" );
        }
        catch ( ExecutionException exception )
        {
          // Make sure to handle IO-interrupted exceptions properly!
          if ( !HostUtils.handleInterruptedException( exception.getCause() ) )
          {
            aCallback.captureAborted( exception.getCause().getMessage() );
          }
        }
        catch ( InterruptedException exception )
        {
          // Make sure to handle IO-interrupted exceptions properly!
          if ( !HostUtils.handleInterruptedException( exception ) )
          {
            aCallback.captureAborted( exception.getCause().getMessage() );
          }
        }
      }

      /**
       * @see org.sump.device.logicsniffer.GenericDevice#process(java.util.List)
       */
      @Override
      protected void process( final List<Sample> aSamples )
      {
        aCallback.samplesCaptured( aSamples );
      }
    };

    // Tell the device on what port & what rate to do its job...
    this.device.addPropertyChangeListener( propertyChangeListener );

    // Let the capturing take place in a background thread...
    this.device.execute();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#getName()
   */
  @Override
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
    return ( this.device != null ) && this.device.isRunning();
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
   * @see nl.lxtreme.ols.api.devices.DeviceController#setupCapture(java.awt.Window)
   */
  @Override
  public boolean setupCapture( final Window aParent )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.deviceConfig != null ) && ( this.deviceConfig.getOwner() != aParent ) )
    {
      this.deviceConfig.dispose();
      this.deviceConfig = null;
    }
    // if no valid dialog exists, create one
    if ( this.deviceConfig == null )
    {
      this.deviceConfig = new GenericDeviceConfigDialog( aParent );
    }

    return ( this.setup = this.deviceConfig.showDialog() );
  }

}
