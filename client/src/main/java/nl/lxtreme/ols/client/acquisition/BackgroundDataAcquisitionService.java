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
package nl.lxtreme.ols.client.acquisition;


import java.io.*;
import java.util.concurrent.*;
import java.util.logging.*;

import org.osgi.framework.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.acquisition.AcquisitionResultStatus.ResultStatus;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a {@link DataAcquisitionService} that executes the acquisition in a
 * background thread.
 */
public class BackgroundDataAcquisitionService implements DataAcquisitionService
{
  // INNER TYPES

  /**
   * Wrapper for {@link Device#call()}.
   */
  static final class DeviceWrapper implements Callable<AcquisitionResult>
  {
    // VARIABLES

    private final Device device;
    private final AcquisitionStatusListener statusListener;

    // CONSTRUCTORS

    /**
     * Creates a new BackgroundDataAcquisitionService.DeviceWrapper instance.
     */
    public DeviceWrapper( final Device aDevice, final AcquisitionStatusListener aStatusListener )
    {
      this.device = aDevice;
      this.statusListener = aStatusListener;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public AcquisitionResult call() throws Exception
    {
      this.device.open();

      this.statusListener.acquisitionStarted();

      return this.device.call();
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BackgroundDataAcquisitionService.class.getName() );

  // VARIABLES

  private volatile Future<?> future;
  private volatile Device device;

  private final ExecutorService executorService;
  private final AcquisitionDataListenerHelper acquisitionDataListenerHelper;
  private final AcquisitionStatusListenerHelper acquisitionStatusListenerHelper;
  private final AcquisitionProgressListenerHelper acquisitionProgressListenerHelper;

  // CONSTRUCTORS

  /**
   * Creates a new BackgroundDataAcquisitionService instance.
   */
  public BackgroundDataAcquisitionService( final BundleContext aBundleContext )
  {
    this.executorService = Executors.newSingleThreadExecutor();

    this.acquisitionDataListenerHelper = new AcquisitionDataListenerHelper( aBundleContext );
    this.acquisitionStatusListenerHelper = new AcquisitionStatusListenerHelper( aBundleContext );
    this.acquisitionProgressListenerHelper = new AcquisitionProgressListenerHelper( aBundleContext );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final DeviceController aDeviceController ) throws IOException
  {
    if ( aDeviceController == null )
    {
      throw new IllegalArgumentException( "Device controller cannot be null!" );
    }
    if ( this.device != null )
    {
      throw new IllegalStateException( "Capture already in progress!" );
    }

    this.device = aDeviceController.createDevice( this.acquisitionProgressListenerHelper );

    final FutureTask<AcquisitionResult> acquisitionWrapper = new FutureTask<AcquisitionResult>( new DeviceWrapper(
        this.device, this.acquisitionStatusListenerHelper ) )
    {
      @Override
      protected void done()
      {
        AcquisitionResult acquisitionResult = null;
        AcquisitionResultStatus status = new AcquisitionResultStatus( ResultStatus.NORMAL );

        try
        {
          acquisitionResult = get();
        }
        catch ( InterruptedException exception )
        {
          status = new AcquisitionResultStatus( ResultStatus.ABORTED, "Cancelled by user!" );
        }
        catch ( ExecutionException exception )
        {
          status = AcquisitionResultStatus.create( exception.getCause() );
        }

        finalizeAcquisition( status, acquisitionResult );
      }
    };

    this.future = this.executorService.submit( acquisitionWrapper );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    if ( this.future != null )
    {
      if ( !this.future.cancel( true /* mayInterruptIfRunning */) )
      {
        throw new IllegalStateException();
      }
    }
  }

  /**
   * Closes this data acquisition service.
   */
  public void close()
  {
    this.acquisitionDataListenerHelper.close();
    this.acquisitionStatusListenerHelper.close();
    this.acquisitionProgressListenerHelper.close();

    this.executorService.shutdown();

    LOG.info( "Background data acquisition service closed ..." );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAcquiring()
  {
    return ( this.future != null ) && !this.future.isDone();
  }

  /**
   * Opens this data acquisition service for business.
   */
  public void open()
  {
    this.acquisitionDataListenerHelper.open( true /* trackAllServices */);
    this.acquisitionStatusListenerHelper.open( true /* trackAllServices */);
    this.acquisitionProgressListenerHelper.open( true /* trackAllServices */);

    LOG.info( "Background data acquisition service opened ..." );
  }

  /**
   * Finalizes the acquisition by invoking all interested listeners and cleaning
   * up things.
   * 
   * @param aStatus
   *          the status of the acquisition, never <code>null</code>;
   * @param aAcquisitionResult
   *          the acquisition result itself, can be <code>null</code> if the
   *          acquisition was aborted or failed.
   */
  final void finalizeAcquisition( final AcquisitionResultStatus aStatus, final AcquisitionResult aAcquisitionResult )
  {
    try
    {
      if ( aAcquisitionResult != null )
      {
        this.acquisitionDataListenerHelper.acquisitionComplete( aAcquisitionResult );
      }

      this.acquisitionStatusListenerHelper.acquisitionEnded( aStatus );
    }
    finally
    {
      // Close the device...
      HostUtils.closeResource( this.device );

      this.device = null;
      this.future = null;
    }
  }
}
