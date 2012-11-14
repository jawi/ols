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
package nl.lxtreme.ols.client.acquisition;


import java.io.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.acquisition.service.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.event.*;


/**
 * Provides a front end for acquiring data.
 */
public class DataAcquirerImpl implements EventHandler, IDataAcquirer
{
  // VARIABLES

  // Injected by Felix DM...
  private volatile DataAcquisitionService acquisitionService;

  private final ConcurrentMap<String, Future<AcquisitionData>> acquisitions;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DataAcquirerImpl} instance.
   */
  public DataAcquirerImpl()
  {
    this.acquisitions = new ConcurrentHashMap<String, Future<AcquisitionData>>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final Device aDevice ) throws IOException
  {
    final String name = aDevice.getName();
    if ( this.acquisitions.containsKey( name ) )
    {
      throw new IllegalStateException( "Acquisition for " + aDevice.getName() + " already in progress!" );
    }

    this.acquisitions.putIfAbsent( name, this.acquisitionService.acquireData( aDevice ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition( final Device aDevice ) throws IOException
  {
    final String name = aDevice.getName();

    cancelAcquisition( name );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( final Event aEvent )
  {
    String topic = aEvent.getTopic();
    if ( DataAcquisitionService.TOPIC_ACQUISITION_STATUS.equals( topic ) )
    {
      String status = ( String )aEvent.getProperty( DataAcquisitionService.KEY_STATUS );
      String name = ( String )aEvent.getProperty( DataAcquisitionService.KEY_DEVICE );

      if ( DataAcquisitionService.STATUS_SUCCESS.equals( status ) )
      {
        // Remove the future from our administration...
        this.acquisitions.remove( name );
      }
      else if ( DataAcquisitionService.STATUS_FAILED.equals( status ) )
      {
        // Remove the future from our administration...
        this.acquisitions.remove( name );
      }
      else if ( DataAcquisitionService.STATUS_CANCELLED.equals( status ) )
      {
        // Remove the future from our administration...
        this.acquisitions.remove( name );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAcquisitionInProgress( final Device aDevice )
  {
    final String name = aDevice.getName();
    final Future<AcquisitionData> future = this.acquisitions.get( name );
    return ( future != null ) && !future.isDone();
  }

  /**
   * Called by Felix DM upon stop of this component.
   */
  public void stop() throws InterruptedException
  {
    for ( String deviceName : this.acquisitions.keySet() )
    {
      cancelAcquisition( deviceName );
    }
  }

  /**
   * Cancels the potentially pending acquisition of the device with the given
   * name.
   * 
   * @param aDeviceName
   *          the name of the device to cancel the acquisition for, cannot be
   *          <code>null</code>.
   */
  private void cancelAcquisition( final String aDeviceName )
  {
    Future<AcquisitionData> future;
    do
    {
      future = this.acquisitions.get( aDeviceName );
      if ( future != null )
      {
        future.cancel( true /* mayInterruptIfRunning */);
      }
    }
    while ( !this.acquisitions.remove( aDeviceName, future ) );
  }
}
