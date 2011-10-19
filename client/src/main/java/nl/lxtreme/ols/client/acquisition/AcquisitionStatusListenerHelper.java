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


import nl.lxtreme.ols.api.acquisition.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides a helper class for passing events to any interested
 * {@link AcquisitionStatusListener}.
 */
final class AcquisitionStatusListenerHelper extends ServiceTracker implements AcquisitionStatusListener
{
  // CONSTRUCTORS

  /**
   * Creates a new AcquisitionStatusListenerHelper instance.
   * 
   * @param aContext
   *          the OSGi bundle context to use.
   */
  public AcquisitionStatusListenerHelper( final BundleContext aContext )
  {
    super( aContext, AcquisitionStatusListener.class.getName(), null /* aCustomizer */);
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionEnded( final AcquisitionResultStatus aStatus )
  {
    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object service : services )
      {
        ( ( AcquisitionStatusListener )service ).acquisitionEnded( aStatus );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionStarted()
  {
    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object service : services )
      {
        ( ( AcquisitionStatusListener )service ).acquisitionStarted();
      }
    }
  }

}
