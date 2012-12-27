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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui;


import static nl.lxtreme.ols.client.ui.signaldisplay.view.UIManagerKeys.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import nl.lxtreme.ols.client.acquisition.*;
import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.ioutil.*;

import org.osgi.service.log.*;


/**
 * Provides a front-end controller for all acquisition related tasks.
 */
public final class AcquisitionController
{
  // VARIABLES

  // Injected by Felix DM...
  private volatile IDataAcquirer dataAcquirer;
  private volatile StatusListener statusListener;
  private volatile LogService log;

  // METHODS

  /**
   * Cancels any ongoing acquisition for the current selected device.
   */
  public void cancelCapture()
  {
    final DeviceInvoker device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot cancel data acquisition!" );
      return;
    }

    try
    {
      this.log.log( LogService.LOG_INFO, "Cancelling data acquisition for '" + device.getName() + "' ..." );

      this.dataAcquirer.cancelAcquisition( device );
    }
    catch ( IllegalStateException exception )
    {
      this.log.log( LogService.LOG_WARNING, "Failed to cancel data acquisition; no acquisition in progress!" );
      this.statusListener.setStatus( "No acquisition in progress!" );
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to cancel data acquisition; I/O problem!", exception );
        this.statusListener.setStatus( "I/O problem: " + exception.getMessage() );
      }
    }
  }

  /**
   * Starts an acquisition for the current selected device.
   * 
   * @param aParent
   *          the parent window to use for displaying any other dialog, can be
   *          <code>null</code>.
   */
  public boolean captureData( final Window aParent )
  {
    DeviceInvoker device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot start data acquisition!" );
      return false;
    }

    try
    {
      this.log.log( LogService.LOG_INFO, "Starting data acquisition for '" + device.getName() + "' ..." );

      if ( device.configure( aParent ) )
      {
        this.statusListener.setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", //
            device.getName(), new Date() );

        this.dataAcquirer.acquireData( device );

        return true;
      }
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to start data acquisition; I/O problem!", exception );
        this.statusListener.setStatus( "I/O problem: " + exception.getMessage() );
      }
    }

    return false;
  }

  /**
   * @return <code>true</code> if there an acquisition ongoing,
   *         <code>false</code> otherwise.
   */
  public boolean isDeviceCapturing()
  {
    DeviceInvoker device = getCurrentSelectedDevice();
    if ( device == null )
    {
      return false;
    }

    return this.dataAcquirer.isAcquisitionInProgress( device );
  }

  /**
   * Returns whether or not a device is selected.
   * 
   * @return <code>true</code> if a device is selected, <code>false</code>
   *         otherwise.
   */
  public boolean isDeviceSelected()
  {
    return ( getCurrentSelectedDevice() != null );
  }

  /**
   * {@inheritDoc}
   */
  public boolean isDeviceSetup()
  {
    final DeviceInvoker device = getCurrentSelectedDevice();
    return ( device != null ) && device.isSetup();
  }

  /**
   * Restarts a new acquisition with the current device and with its current
   * settings.
   */
  public void repeatCaptureData()
  {
    DeviceInvoker device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot repeat data acquisition!" );
      return;
    }
    if ( !device.isSetup() )
    {
      this.log.log( LogService.LOG_WARNING, "Device is not set up, cannot repeat data acquisition!" );
      return;
    }

    try
    {
      this.log.log( LogService.LOG_INFO, "Repeating data acquisition for '" + device.getName() + "' ..." );

      this.statusListener.setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", //
          device.getName(), new Date() );

      if ( !UIManager.getBoolean( RETAIN_ANNOTATIONS_WITH_RECAPTURE ) )
      {
        getAnnotationData().clearAll();
      }

      this.dataAcquirer.acquireData( device );
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to repeat data acquisition; I/O problem!", exception );
        this.statusListener.setStatus( "I/O problem: " + exception.getMessage() );
      }
    }
  }

  /**
   * @return the current annotation data, never <code>null</code>.
   */
  private AnnotationData getAnnotationData()
  {
    return Client.getInstance().getSession().getAnnotationData();
  }

  /**
   * @return the current selected device, can be <code>null</code> if no device
   *         is selected.
   */
  private DeviceInvoker getCurrentSelectedDevice()
  {
    return getDeviceController().getSelectedDevice();
  }

  /**
   * @return the current device controller, never <code>null</code>.
   */
  private DeviceController getDeviceController()
  {
    return Client.getInstance().getDeviceController();
  }
}
