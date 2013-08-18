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
package nl.lxtreme.ols.tool.base;


import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.util.osgi.*;

import org.osgi.framework.*;


/**
 * Service tracker for annotation listeners.
 */
public class AnnotationListenerServiceTracker implements AnnotationListener
{
  // VARIABLES

  private final WhiteboardHelper<AnnotationListener> annotationListenerHelper;

  // CONSTRUCTORS

  /**
   * Creates a new AnnotationListenerServiceTracker instance.
   */
  public AnnotationListenerServiceTracker( final BundleContext aContext )
  {
    this.annotationListenerHelper = new WhiteboardHelper<AnnotationListener>( aContext, AnnotationListener.class );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations()
  {
    this.annotationListenerHelper.accept( new WhiteboardHelper.Visitor<AnnotationListener>()
    {
      @Override
      public void visit( final AnnotationListener aService )
      {
        aService.clearAnnotations();
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations( final int aChannelIdx )
  {
    this.annotationListenerHelper.accept( new WhiteboardHelper.Visitor<AnnotationListener>()
    {
      @Override
      public void visit( final AnnotationListener aService )
      {
        aService.clearAnnotations( aChannelIdx );
      }
    } );
  }

  /**
   * Closes this annotation listener service tracker.
   */
  public void close()
  {
    try
    {
      this.annotationListenerHelper.close();
    }
    catch ( IllegalStateException exception )
    {
      // Ignore; bundle context probably is incorrect...
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onAnnotation( final Annotation<?> aAnnotation )
  {
    this.annotationListenerHelper.accept( new WhiteboardHelper.Visitor<AnnotationListener>()
    {
      @Override
      public void visit( final AnnotationListener aService )
      {
        aService.onAnnotation( aAnnotation );
      }
    } );
  }

  /**
   * Opens this annotation listener service tracker for business.
   */
  public void open()
  {
    this.annotationListenerHelper.open( true /* trackAllServices */);
  }

}
