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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.session;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;


/**
 * Default implementation of {@link Session}.
 */
public class SessionImpl implements Session, AnnotationData
{
  // VARIABLES

  private final int id;
  private final SessionProviderImpl provider;
  private final AcquisitionData data;
  private final ConcurrentMap<Integer, SortedSet<Annotation>> annotations;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SessionImpl} instance.
   */
  public SessionImpl( int aId, SessionProviderImpl aProvider, AcquisitionData aData )
  {
    this.id = aId;
    this.provider = aProvider;
    this.data = aData;

    this.annotations = new ConcurrentHashMap<Integer, SortedSet<Annotation>>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void add( Annotation aAnnotation )
  {
    Integer channelIndex = Integer.valueOf( aAnnotation.getChannelIndex() );
    SortedSet<Annotation> annotations = this.annotations.get( channelIndex );
    if ( annotations == null )
    {
      annotations = new ConcurrentSkipListSet<Annotation>();
      this.annotations.putIfAbsent( channelIndex, annotations );
    }

    annotations.add( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clear( int aChannelIdx )
  {
    this.annotations.remove( Integer.valueOf( aChannelIdx ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAll()
  {
    this.annotations.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    this.provider.removeSession( this );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData getAcquiredData()
  {
    return this.data;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AnnotationData getAnnotationData()
  {
    return this;
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
  public SortedSet<Annotation> getAnnotations( int aChannelIdx )
  {
    SortedSet<Annotation> result = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    return ( result == null ) ? new TreeSet<Annotation>() : result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SortedSet<DataAnnotation> getAnnotations( int aChannelIdx, long aStartTime, long aEndTime )
  {
    SortedSet<Annotation> anns = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( anns == null )
    {
      return new TreeSet<DataAnnotation>();
    }
    SortedSet<DataAnnotation> result = new TreeSet<DataAnnotation>();
    for ( Annotation annotation : anns )
    {
      if ( annotation instanceof DataAnnotation )
      {
        long annStartTime = ( ( DataAnnotation )annotation ).getStartTimestamp();
        long annEndTime = ( ( DataAnnotation )annotation ).getEndTimestamp();

        if ( ( ( annStartTime < aStartTime ) && ( annEndTime < aStartTime ) )
            || ( ( annStartTime > aEndTime ) && ( annEndTime > aEndTime ) ) )
        {
          // Simple reject: annotation falls outside time interval...
          continue;
        }

        result.add( ( DataAnnotation )annotation );
      }
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getId()
  {
    return this.id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAnnotations( Class<? extends Annotation> aType, int aChannelIdx )
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
