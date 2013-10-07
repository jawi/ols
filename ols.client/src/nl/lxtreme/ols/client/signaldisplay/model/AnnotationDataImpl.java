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
package nl.lxtreme.ols.client.signaldisplay.model;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides a default implementation of {@link AnnotationData}, which also acts
 * as {@link AnnotationListener} to collect all emitted annotations.
 */
public class AnnotationDataImpl implements AnnotationData
{
  // VARIABLES

  private final ConcurrentMap<Integer, SortedSet<Annotation>> annotations;

  // Injected by Felix DM...
  // private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationDataImpl} instance.
   */
  public AnnotationDataImpl()
  {
    this.annotations = new ConcurrentHashMap<Integer, SortedSet<Annotation>>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void add( final Annotation aAnnotation )
  {
    Integer channelIndex = Integer.valueOf( aAnnotation.getChannelIndex() );
    SortedSet<Annotation> annotations = this.annotations.get( channelIndex );
    if ( annotations == null )
    {
      annotations = new ConcurrentSkipListSet<Annotation>();
      this.annotations.putIfAbsent( channelIndex, annotations );
    }

    // this.logService.log( LogService.LOG_DEBUG,
    // "Adding annotation for channel: " + channelIndex + ", " + aAnnotation );

    annotations.add( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clear( final int aChannelIdx )
  {
    // this.logService.log( LogService.LOG_DEBUG,
    // "Clearing annotations for channel: " + aChannelIdx );

    this.annotations.remove( Integer.valueOf( aChannelIdx ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAll()
  {
    // this.logService.log( LogService.LOG_DEBUG,
    // "Clearing annotations for all channels" );

    this.annotations.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SortedSet<Annotation> getAnnotations()
  {
    SortedSet<Annotation> result = new TreeSet<Annotation>();
    for ( SortedSet<Annotation> annotations : this.annotations.values() )
    {
      result.addAll( annotations );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SortedSet<Annotation> getAnnotations( final int aChannelIdx )
  {
    SortedSet<Annotation> result = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    return ( result == null ) ? new TreeSet<Annotation>() : result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAnnotations( final Class<? extends Annotation> aType, final int aChannelIdx )
  {
    SortedSet<Annotation> result = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( result != null )
    {
      for ( Annotation annotation : result )
      {
        if ( aType.isAssignableFrom( annotation.getClass() ) )
        {
          return true;
        }
      }
    }
    return false;
  }
}
