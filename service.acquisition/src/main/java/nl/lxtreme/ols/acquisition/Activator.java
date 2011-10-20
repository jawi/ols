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


import java.util.*;

import nl.lxtreme.ols.api.*;
import org.osgi.framework.*;


/**
 * Provides a bundle activator for the acquisition service.
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private BackgroundDataAcquisitionService service;
  private AcquisitionListenerHelper acquisitionListenerHelper;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.acquisitionListenerHelper = new AcquisitionListenerHelper( aContext );
    this.acquisitionListenerHelper.open();

    this.service = new BackgroundDataAcquisitionService( this.acquisitionListenerHelper );

    final Properties props = new Properties();
    props.put( "invocation", "asynchonous" );

    aContext.registerService( DataAcquisitionService.class.getName(), this.service, props );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    this.acquisitionListenerHelper.close();
    this.service.close();
  }
}
