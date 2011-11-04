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
   * Wrapper for {@link AcquisitionTask#call()}.
   */
  static final class DeviceWrapper implements Callable<AcquisitionResult>
  {
    // VARIABLES

    private final AcquisitionTask device;
    private final AcquisitionStatusListener statusListener;

    // CONSTRUCTORS

    /**
     * Creates a new BackgroundDataAcquisitionService.DeviceWrapper instance.
     */
    public DeviceWrapper( final AcquisitionTask aDevice, final AcquisitionStatusListener aStatusListener )
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
  private volatile AcquisitionTask device;

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
  public void acquireData( final Device aDeviceController ) throws IOException
  {
    if ( aDeviceController == null )
    {
      throw new IllegalArgumentException( "Device controller cannot be null!" );
    }
    if ( this.device != null )
    {
      throw new IllegalStateException( "Capture already in progress!" );
    }

    this.device = aDeviceController.createAcquisitionTask( this.acquisitionListenerHelper );

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
        catch ( Throwable exception )
        {
          InterruptedException ie = unwrapInterruptedException( exception );
          if ( ie != null )
          {
            status = new AcquisitionResultStatus( ResultStatus.ABORTED, "Cancelled by user!" );
          }
          else
          {
            Throwable t = unwrapExecutionException( exception );
            status = AcquisitionResultStatus.create( t );
          }
        }
        finally
        {
          finalizeAcquisition( status, acquisitionResult );
        }
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
    try
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
    finally
    {
      cleanResources();
    }
  }

  /**
   * Closes this data acquisition service, cancelling any ongoing acquisitions
   * if needed.
   * <p>
   * After calling this method, this instance should <em>no longer</em> be used.
   * </p>
   */
  public void close()
  {
    try
    {
      if ( this.future != null )
      {
        // Make sure we're cancelling any ongoing acquisitions...
        cancelAcquisition();
      }

      // Make sure to leave no garbage behind...
      cleanResources();
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
      cleanResources();
    }
  }

  /**
   * Closes the device and sets it and the future to <code>null</code>.
   */
  private void cleanResources()
  {
    // Close the device (at best effort)...
    HostUtils.closeResource( this.device );

    this.device = null;
    this.future = null;
  }

  /**
   * Unwraps a given exception to an {@link InterruptedException} in case it is
   * either the given exception, or any cause-exception wrapped in the given
   * exception.
   * 
   * @param aException
   *          the exception to unwrap, may be <code>null</code>.
   * @return the unwrapped interrupted exception, or <code>null</code> if no
   *         such exception was wrapped.
   */
  private Throwable unwrapExecutionException( final Throwable aException )
  {
    Throwable ptr = aException;
    if ( ptr != null )
    {
      if ( ptr instanceof ExecutionException )
      {
        return ptr.getCause();
      }
    }

    return ptr;
  }

  /**
   * Unwraps a given exception to an {@link InterruptedException} in case it is
   * either the given exception, or any cause-exception wrapped in the given
   * exception.
   * 
   * @param aException
   *          the exception to unwrap, may be <code>null</code>.
   * @return the unwrapped interrupted exception, or <code>null</code> if no
   *         such exception was wrapped.
   */
  private InterruptedException unwrapInterruptedException( final Throwable aException )
  {
    Throwable ptr = aException;
    while ( ptr != null )
    {
      if ( ptr instanceof InterruptedException )
      {
        return ( InterruptedException )ptr;
      }
      ptr = ptr.getCause();
    }

    return null;
  }
}
