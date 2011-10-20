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


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.util.osgi.*;
import nl.lxtreme.ols.util.osgi.WhiteboardHelper.*;

import org.osgi.framework.*;


/**
 * Provides a helper class for passing events to any interested
 * {@link AcquisitionDataListener}, {@link AcquisitionProgressListener} and
 * {@link AcquisitionStatusListener}.
 */
public class AcquisitionListenerHelper implements AcquisitionDataListener, AcquisitionProgressListener,
    AcquisitionStatusListener
{
  // VARIABLES

  private final WhiteboardHelper<Object> whiteboardHelper;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionListenerHelper} instance.
   * 
   * @param aContext
   *          the bundle context to use, cannot be <code>null</code>.
   */
  public AcquisitionListenerHelper( final BundleContext aContext )
  {
    this.whiteboardHelper = new WhiteboardHelper<Object>( aContext, createFilter() );
  }

  // METHODS

  /**
   * Creates a filter for listening to all registered instances of
   * {@link AcquisitionDataListener}, {@link AcquisitionProgressListener} and
   * {@link AcquisitionStatusListener}.
   * 
   * @return a filter instance, never <code>null</code>.
   * @throws RuntimeException
   *           in case the filter definition was invalid.
   */
  static Filter createFilter()
  {
    final String filter = String.format( "(|(%s=%s)(%s=%s)(%s=%s))", //
        Constants.OBJECTCLASS, AcquisitionDataListener.class.getName(), //
        Constants.OBJECTCLASS, AcquisitionProgressListener.class.getName(), //
        Constants.OBJECTCLASS, AcquisitionStatusListener.class.getName() );

    try
    {
      return FrameworkUtil.createFilter( filter );
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new RuntimeException( "Invalid filter syntax?!" );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionComplete( final AcquisitionResult aData )
  {
    this.whiteboardHelper.accept( new Visitor<Object>()
    {
      @Override
      public void visit( final Object aService )
      {
        if ( aService instanceof AcquisitionDataListener )
        {
          ( ( AcquisitionDataListener )aService ).acquisitionComplete( aData );
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionEnded( final AcquisitionResultStatus aStatus )
  {
    this.whiteboardHelper.accept( new Visitor<Object>()
    {
      @Override
      public void visit( final Object aService )
      {
        if ( aService instanceof AcquisitionStatusListener )
        {
          ( ( AcquisitionStatusListener )aService ).acquisitionEnded( aStatus );
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionInProgress( final int aPercentage )
  {
    this.whiteboardHelper.accept( new Visitor<Object>()
    {
      @Override
      public void visit( final Object aService )
      {
        if ( aService instanceof AcquisitionProgressListener )
        {
          ( ( AcquisitionProgressListener )aService ).acquisitionInProgress( aPercentage );
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionStarted()
  {
    this.whiteboardHelper.accept( new Visitor<Object>()
    {
      @Override
      public void visit( final Object aService )
      {
        if ( aService instanceof AcquisitionStatusListener )
        {
          ( ( AcquisitionStatusListener )aService ).acquisitionStarted();
        }
      }
    } );
  }

  /**
   * Stops listening to all interested listeners.
   */
  public final void close()
  {
    this.whiteboardHelper.close();
  }

  /**
   * Lets start this helper to listen for all interested listeners.
   */
  public final void open()
  {
    this.whiteboardHelper.open( true /* trackAllServices */);
  }
}
