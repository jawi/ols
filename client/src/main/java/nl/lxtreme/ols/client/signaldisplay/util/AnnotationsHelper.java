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
package nl.lxtreme.ols.client.signaldisplay.util;


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 * 
 */
@SuppressWarnings( { "rawtypes", "unchecked" } )
public final class AnnotationsHelper
{
  // VARIABLES

  private final Channel channel;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationsHelper} instance.
   * 
   * @param aChannel
   *          the channel to use, cannot be <code>null</code>.
   */
  public AnnotationsHelper( final Channel aChannel )
  {
    if ( aChannel == null )
    {
      throw new IllegalArgumentException( "Channel cannot be null!" );
    }
    this.channel = aChannel;
  }

  /**
   * Creates a new {@link AnnotationsHelper} instance.
   * 
   * @param aElement
   *          the element to get the annotations from, cannot be
   *          <code>null</code> and must represent a digital signal.
   */
  public AnnotationsHelper( final SignalElement aElement )
  {
    if ( aElement == null )
    {
      throw new IllegalArgumentException( "Element cannot be null!" );
    }
    if ( !aElement.isDigitalSignal() )
    {
      throw new IllegalArgumentException( "Can only work for digital channels!" );
    }
    this.channel = aElement.getChannel();
  }

  // METHODS

  /**
   * Finds the annotation that starts before the given timestamp, and ends at or
   * after the given timestamp.
   * 
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation<?> getAnnotation( final long aTimestamp )
  {
    DataAnnotation<?> result = null;
    for ( Annotation<?> annotation : this.channel.getAnnotations() )
    {
      if ( !( annotation instanceof DataAnnotation<?> ) )
      {
        continue;
      }

      final DataAnnotation<?> ann = ( DataAnnotation<?> )annotation;

      final long annStartTime = ann.getStartTimestamp();
      final long annEndTime = ann.getEndTimestamp();

      if ( ( annStartTime <= aTimestamp ) && ( annEndTime >= aTimestamp ) )
      {
        result = ann;
        break;
      }
    }
    return result;
  }

  /**
   * Finds the first annotation that starts and ends after the given timestamp.
   * 
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation<?> getAnnotationAfter( final long aTimestamp )
  {
    SortedSet<Annotation<?>> annotations = new TreeSet<Annotation<?>>( this.channel.getAnnotations() );
    for ( Annotation<?> annotation : annotations )
    {
      if ( !( annotation instanceof DataAnnotation<?> ) )
      {
        continue;
      }

      final DataAnnotation<?> ann = ( DataAnnotation<?> )annotation;

      final long annStartTime = ann.getStartTimestamp();
      final long annEndTime = ann.getEndTimestamp();

      if ( ( annStartTime >= aTimestamp ) && ( annEndTime >= aTimestamp ) )
      {
        return ann;
      }
    }

    return null;
  }

  /**
   * Finds the first annotation that starts and ends before the given timestamp.
   * 
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation<?> getAnnotationBefore( final long aTimestamp )
  {
    DataAnnotation<?> result = null;

    SortedSet<Annotation<?>> annotations = new TreeSet<Annotation<?>>( this.channel.getAnnotations() );
    for ( Annotation<?> annotation : annotations )
    {
      if ( !( annotation instanceof DataAnnotation<?> ) )
      {
        continue;
      }

      final DataAnnotation<?> ann = ( DataAnnotation<?> )annotation;

      final long annStartTime = ann.getStartTimestamp();
      final long annEndTime = ann.getEndTimestamp();

      if ( ( annStartTime < aTimestamp ) && ( annEndTime < aTimestamp ) )
      {
        result = ann;
      }
      else
      {
        break;
      }
    }

    return result;
  }

  /**
   * Returns all annotations that fall inside the given boundaries.
   * 
   * @param aStartTime
   *          the start timestamp;
   * @param aEndTime
   *          the end timestamp.
   * @return a list with annotations, never <code>null</code>.
   */
  public <T extends DataAnnotation<?>> List<T> getAnnotations( final Class<T> aType, final long aStartTime,
      final long aEndTime )
  {
    List<T> result = new ArrayList<T>();
    for ( Annotation<?> annotation : this.channel.getAnnotations() )
    {
      if ( !aType.isAssignableFrom( annotation.getClass() ) )
      {
        continue;
      }

      final DataAnnotation<?> ann = ( DataAnnotation<?> )annotation;

      final long annStartTime = ann.getStartTimestamp();
      final long annEndTime = ann.getEndTimestamp();

      if ( ( ( annStartTime < aStartTime ) && ( annEndTime < aStartTime ) )
          || ( ( annStartTime > aEndTime ) && ( annEndTime > aEndTime ) ) )
      {
        // Simple reject: annotation falls outside clip boundaries...
        continue;
      }

      result.add( ( T )ann );
    }
    return result;
  }

  /**
   * Returns all annotations that fall inside the given boundaries.
   * 
   * @param aStartTime
   *          the start timestamp;
   * @param aEndTime
   *          the end timestamp.
   * @return a list with annotations, never <code>null</code>.
   */
  public List<DataAnnotation> getDataAnnotations( final long aStartTime, final long aEndTime )
  {
    return getAnnotations( DataAnnotation.class, aStartTime, aEndTime );
  }
}
