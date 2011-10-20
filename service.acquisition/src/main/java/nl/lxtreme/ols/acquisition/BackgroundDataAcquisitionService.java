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
package nl.lxtreme.ols.acquisition;


import java.io.*;
import java.util.concurrent.*;
import java.util.logging.*;

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

  private final AcquisitionListenerHelper acquisitionListenerHelper;

  final ExecutorService executorService;

  // CONSTRUCTORS

  /**
   * Creates a new BackgroundDataAcquisitionService instance.
   * 
   * @param aAcquisitionListenerHelper
   *          the helper class for delegating events to interested listeners,
   *          cannot be <code>null</code>.
   */
  public BackgroundDataAcquisitionService( final AcquisitionListenerHelper aAcquisitionListenerHelper )
  {
    this.acquisitionListenerHelper = aAcquisitionListenerHelper;
    this.executorService = Executors.newCachedThreadPool();
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

    this.device = aDeviceController.createDevice( this.acquisitionListenerHelper );

    final FutureTask<AcquisitionResult> acquisitionWrapper = new FutureTask<AcquisitionResult>( new DeviceWrapper(
        this.device, this.acquisitionListenerHelper ) )
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
          final Throwable cause = exception.getCause();
          // The inner future can also be cancelled, causing it to abort its
          // execution, so we should also check the cause of the execution
          // exception to differentiate between aborts and failures...
          if ( cause instanceof InterruptedException )
          {
            status = new AcquisitionResultStatus( ResultStatus.ABORTED, "Cancelled by user!" );
          }
          else
          {
            status = AcquisitionResultStatus.create( cause );
          }
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
        if ( !this.future.isDone() )
        {
          throw new InternalError( "Failed to cancel ongoing task. Possible resource leak!" );
        }
      }
    }
    else
    {
      throw new IllegalStateException( "No acquisition in progress!" );
    }
  }

  /**
   * Closes this data acquisition service.
   */
  public void close()
  {
    try
    {
      // Make sure we're cancelling any ongoing acquisitions...
      cancelAcquisition();
    }
    finally
    {
      if ( !this.executorService.isShutdown() )
      {
        // Always invoke this method; even when an exception is thrown during
        // cancellation!
        this.executorService.shutdown();

        int tries = 3;
        while ( !this.executorService.isTerminated() && ( tries-- >= 0 ) )
        {
          try
          {
            if ( this.executorService.awaitTermination( 100L, TimeUnit.MILLISECONDS ) )
            {
              LOG.fine( "All running threads are terminated ..." );
            }
          }
          catch ( InterruptedException exception )
          {
            // Make sure our thread administration is correct...
            Thread.currentThread().interrupt();
          }
        }

        LOG.info( "Background data acquisition service closed ..." );
      }
    }
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
        this.acquisitionListenerHelper.acquisitionComplete( aAcquisitionResult );
      }

      this.acquisitionListenerHelper.acquisitionEnded( aStatus );
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
