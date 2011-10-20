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
package nl.lxtreme.ols.client.osgi;


import java.io.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.devices.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * 
 */
public class DataAcquisitionServiceTracker extends ServiceTracker implements DataAcquisitionService
{
  // CONSTRUCTORS

  /**
   * Creates a new DataAcquisitionServiceTracker instance.
   * 
   * @param aContext
   *          the bundle context to use.
   */
  public DataAcquisitionServiceTracker( final BundleContext aContext )
  {
    super( aContext, DataAcquisitionService.class.getName(), null /* aCustomizer */);
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final DeviceController aDeviceController ) throws IOException
  {
    final DataAcquisitionService dataAcquisitionService = ( DataAcquisitionService )getService();
    if ( dataAcquisitionService != null )
    {
      dataAcquisitionService.acquireData( aDeviceController );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    final DataAcquisitionService dataAcquisitionService = ( DataAcquisitionService )getService();
    if ( dataAcquisitionService != null )
    {
      dataAcquisitionService.cancelAcquisition();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAcquiring()
  {
    final DataAcquisitionService dataAcquisitionService = ( DataAcquisitionService )getService();
    if ( dataAcquisitionService != null )
    {
      return dataAcquisitionService.isAcquiring();
    }
    return false;
  }
}
