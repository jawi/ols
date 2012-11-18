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
package nl.lxtreme.ols.client.signaldisplay;


import static nl.lxtreme.ols.tool.api.AnnotationHelper.*;

import java.awt.event.*;
import java.util.*;

import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides a helper class to deal with channel annotations.
 */
@SuppressWarnings( { "unchecked" } )
public final class AnnotationsHelper
{
  // VARIABLES

  private final Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationsHelper} instance.
   * 
   * @param aSession
   *          the session to use, cannot be <code>null</code>.
   */
  public AnnotationsHelper( final Session aSession )
  {
    if ( aSession == null )
    {
      throw new IllegalArgumentException( "Session cannot be null!" );
    }
    this.session = aSession;
  }

  // METHODS

  /**
   * Finds the annotation that starts before the given timestamp, and ends at or
   * after the given timestamp.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0;
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation getAnnotation( final int aChannelIdx, final long aTimestamp )
  {
    DataAnnotation result = null;
    for ( Annotation annotation : getAnnotations( aChannelIdx ) )
    {
      if ( !( annotation instanceof DataAnnotation ) )
      {
        continue;
      }

      final DataAnnotation ann = ( DataAnnotation )annotation;

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
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0;
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation getAnnotationAfter( final int aChannelIdx, final long aTimestamp )
  {
    SortedSet<Annotation> annotations = new TreeSet<Annotation>( getAnnotations( aChannelIdx ) );
    for ( Annotation annotation : annotations )
    {
      if ( !( annotation instanceof DataAnnotation ) )
      {
        continue;
      }

      final DataAnnotation ann = ( DataAnnotation )annotation;

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
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0;
   * @param aTimestamp
   *          the timestamp to search for annotations, >= 0L.
   * @return an annotation matching the given timestamp criteria,
   *         <code>null</code> if not found.
   */
  public DataAnnotation getAnnotationBefore( final int aChannelIdx, final long aTimestamp )
  {
    DataAnnotation result = null;

    SortedSet<Annotation> annotations = new TreeSet<Annotation>( getAnnotations( aChannelIdx ) );
    for ( Annotation annotation : annotations )
    {
      if ( !( annotation instanceof DataAnnotation ) )
      {
        continue;
      }

      final DataAnnotation ann = ( DataAnnotation )annotation;

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
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0;
   * @param aStartTime
   *          the start timestamp;
   * @param aEndTime
   *          the end timestamp.
   * @return a list with annotations, never <code>null</code>.
   */
  public <T extends DataAnnotation> List<T> getAnnotations( final int aChannelIdx, final Class<T> aType,
      final long aStartTime, final long aEndTime )
  {
    List<T> result = new ArrayList<T>();
    for ( Annotation annotation : getAnnotations( aChannelIdx ) )
    {
      if ( !aType.isAssignableFrom( annotation.getClass() ) )
      {
        continue;
      }

      final DataAnnotation ann = ( DataAnnotation )annotation;

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

    Collections.sort( result );

    return result;
  }

  /**
   * Returns all annotations that fall inside the given boundaries.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0;
   * @param aStartTime
   *          the start timestamp;
   * @param aEndTime
   *          the end timestamp.
   * @return a list with annotations, never <code>null</code>.
   */
  public List<DataAnnotation> getDataAnnotations( final int aChannelIdx, final long aStartTime, final long aEndTime )
  {
    return getAnnotations( aChannelIdx, DataAnnotation.class, aStartTime, aEndTime );
  }

  /**
   * Returns a text representation for the given annotation.
   * 
   * @param aAnnotation
   *          the annotation to get a text representation for, cannot be
   *          <code>null</code>.
   * @return a text representation, never <code>null</code>.
   */
  public String getText( final DataAnnotation aAnnotation )
  {
    String result = null;

    Object data = aAnnotation.getData();
    Map<String, Object> props = aAnnotation.getProperties();

    boolean symbol = Boolean.TRUE.equals( props.get( KEY_SYMBOL ) );
    if ( symbol )
    {
      if ( data instanceof Integer )
      {
        int value = ( ( Integer )data ).intValue();
        if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
        {
          result = String.format( "%1$c", data );
        }
      }
    }

    if ( result == null )
    {
      result = String.format( "(%1$s)", String.valueOf( data ) );
    }

    return result;
  }

  /**
   * Returns a text description for the given annotation, useful for tooltips.
   * 
   * @param aAnnotation
   *          the annotation to get a description for, cannot be
   *          <code>null</code>.
   * @return an annotation description, never <code>null</code>.
   */
  public String getDescription( final Annotation aAnnotation )
  {
    Object data = aAnnotation.getData();

    String description = null;
    boolean symbol = false;
    boolean error = false;

    if ( aAnnotation instanceof DataAnnotation )
    {
      Map<String, Object> props = ( ( DataAnnotation )aAnnotation ).getProperties();

      symbol = Boolean.TRUE.equals( props.get( KEY_SYMBOL ) );
      error = Boolean.TRUE.equals( props.get( KEY_ERROR ) );
      description = ( String )props.get( KEY_DESCRIPTION );
    }

    StringBuilder sb = new StringBuilder( "<html><head><style>th { text-align: left; }</style></head><body><table>" );

    if ( symbol )
    {
      sb.append( "<tr><th>Symbol:</th><td>" );
      if ( data instanceof Integer )
      {
        int value = ( ( Integer )data ).intValue();
        String text;
        if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
        {
          text = String.format( "'%1$c' (%1$d, 0x%1$x, 0%1$o)", data );
        }
        else
        {
          text = String.format( "%1$d (0x%1$x, 0%1$o)", data );
        }
        sb.append( text );
      }
      else
      {
        sb.append( data );
      }
      sb.append( "</td></tr>" );
    }

    if ( error )
    {
      sb.append( "<tr><th>Error</th><td>" ).append( data ).append( "</td></tr>" );
    }

    if ( description != null )
    {
      sb.append( "<tr><td colspan='2'>" ).append( description ).append( "</td></tr>" );
    }

    return sb.append( "</table></body></html>" ).toString();
  }

  /**
   * @param aChannelIdx
   * @return
   */
  private SortedSet<Annotation> getAnnotations( final int aChannelIdx )
  {
    final AnnotationData annotationData = this.session.getAnnotationData();
    return annotationData.getAnnotations( aChannelIdx );
  }

  /**
   * @param aChar
   * @return
   */
  private boolean isPrintableChar( final char aChar )
  {
    if ( Character.isISOControl( aChar ) || ( aChar == KeyEvent.CHAR_UNDEFINED ) )
    {
      return false;
    }
    Character.UnicodeBlock block = Character.UnicodeBlock.of( aChar );
    return ( block != null ) && ( block != Character.UnicodeBlock.SPECIALS );
  }
}
