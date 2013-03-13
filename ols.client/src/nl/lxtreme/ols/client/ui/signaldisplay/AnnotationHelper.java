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
package nl.lxtreme.ols.client.ui.signaldisplay;


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;

import java.awt.*;
import java.awt.event.*;
import java.math.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a helper class to deal with channel annotations.
 */
@SuppressWarnings( { "unchecked" } )
public final class AnnotationHelper
{
  // VARIABLES

  private final Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationHelper} instance.
   * 
   * @param aSession
   *          the session to use, cannot be <code>null</code>.
   */
  public AnnotationHelper( final Session aSession )
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
   * Returns the color to represent the given annotation in.
   * 
   * @param aAnnotation
   *          the annotation to obtain the color for, cannot be
   *          <code>null</code>;
   * @param aDefaultColor
   *          the default color to fall back to in case either the given
   *          annotation does not define a color, or when annotations shouldn't
   *          be colorized.
   * @return the color to paint the annotation in, never <code>null</code>.
   */
  public Color getColor( final DataAnnotation aAnnotation, final Color aDefaultColor )
  {
    Color result = null;
    if ( UIManager.getBoolean( UIManagerKeys.USE_COLORIZED_ANNOTATIONS ) )
    {
      Object value = aAnnotation.getProperties().get( KEY_COLOR );
      if ( value instanceof Color )
      {
        result = ( Color )value;
      }
      else if ( value != null )
      {
        result = ColorUtils.parseColor( String.valueOf( value ) );
      }
    }
    if ( result == null )
    {
      result = aDefaultColor;
    }
    return result;
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

    boolean symbol = TYPE_SYMBOL.equals( props.get( KEY_TYPE ) );
    if ( symbol && ( data instanceof Number ) )
    {
      if ( data instanceof Integer )
      {
        int value = ( ( Integer )data ).intValue();
        if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
        {
          result = String.format( "%1$c", data );
        }
      }
      else
      {
        result = String.format( "0x%1$x", data );
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
   *          <code>null</code>;
   * @param aIncludeTimingData
   *          <code>true</code> to include timing data (start & stop of
   *          annotation), <code>false</code> to omit timing data.
   * @return an annotation description, never <code>null</code>.
   */
  public String getDescription( final Annotation aAnnotation, final boolean aIncludeTimingData )
  {
    Object data = aAnnotation.getData();

    Map<String, Object> props = new HashMap<String, Object>();
    String description = null;
    boolean symbol = false;
    boolean error = false;
    boolean event = false;
    long startTime = -1L;
    long endTime = -1L;

    if ( aAnnotation instanceof DataAnnotation )
    {
      final DataAnnotation dataAnnotation = ( DataAnnotation )aAnnotation;
      props.putAll( dataAnnotation.getProperties() );

      startTime = dataAnnotation.getStartTimestamp();
      endTime = dataAnnotation.getEndTimestamp();

      description = ( String )props.remove( KEY_DESCRIPTION );
      Object type = props.remove( KEY_TYPE );
      props.remove( KEY_COLOR );

      error = TYPE_ERROR.equals( type );
      event = TYPE_EVENT.equals( type );
      symbol = TYPE_SYMBOL.equals( type );
    }

    StringBuilder sb = new StringBuilder(
        "<html><head><style>th{text-align:right;} td{padding:0; margin:0;}</style></head><body><table class='desc' border='0'>" );

    if ( description != null )
    {
      sb.append( "<tr><td colspan='2'>" ).append( description ).append( "</td></tr>" );
    }
    if ( aIncludeTimingData && ( startTime >= 0 ) )
    {
      sb.append( "<tr><th>Start</th><td>" ).append( formatTime( startTime ) ).append( "</td></tr>" );
    }
    if ( aIncludeTimingData && ( endTime >= 0 ) )
    {
      sb.append( "<tr><th>Stop</th><td>" ).append( formatTime( endTime ) ).append( "</td></tr>" );
    }

    if ( symbol )
    {
      sb.append( "<tr><th>Symbol</th><td>" );

      if ( data instanceof Number )
      {
        sb.append( "<table border='0'>" );
        if ( data instanceof Integer )
        {
          int value = ( ( Integer )data ).intValue();
          if ( ( value >= 0 ) && isPrintableChar( ( char )value ) )
          {
            sb.append( "<tr><th valign='top'>char</th><td>'" ).append( ( char )value ).append( "'</td></tr>" );
          }
        }

        sb.append( "<tr><th>dec</th><td>" ).append( String.format( "%d", data ) ).append( "</td></tr>" );
        sb.append( "<tr><th>hex</th><td>" ).append( String.format( "%x", data ) ).append( "</td></tr>" );
        sb.append( "<tr><th>oct</th><td>" ).append( String.format( "%o", data ) ).append( "</td></tr>" );

        String binary;
        if ( data instanceof BigInteger )
        {
          binary = ( ( BigInteger )data ).toString( 2 );
        }
        else
        {
          binary = Long.toBinaryString( ( ( Number )data ).longValue() );
        }

        sb.append( "<tr><th>bin</th><td>" ).append( binary ).append( "</td></tr>" );
        sb.append( "</table>" );
      }
      else
      {
        sb.append( data );
      }
      sb.append( "</td></tr>" );
    }

    if ( event )
    {
      sb.append( "<tr><th>Event</th><td>" ).append( data ).append( "</td></tr>" );
    }

    if ( error )
    {
      sb.append( "<tr><th>Error</th><td>" ).append( data ).append( "</td></tr>" );
    }

    for ( Map.Entry<String, Object> entry : props.entrySet() )
    {
      sb.append( "<tr><th>" ).append( entry.getKey() ).append( "</th><td>" );
      sb.append( entry.getValue() ).append( "</td></tr>" );
    }

    return sb.append( "</table></body></html>" ).toString();
  }

  /**
   * Returns the annotations for the channel with the given index.
   * 
   * @param aChannelIdx
   *          the channel index to retrieve the annotations for, >= 0.
   * @return a sorted set of annotations, never <code>null</code>.
   */
  private SortedSet<Annotation> getAnnotations( final int aChannelIdx )
  {
    final AnnotationData annotationData = this.session.getAnnotationData();
    return annotationData.getAnnotations( aChannelIdx );
  }

  /**
   * Formats a given timestamp to a human-readable representation.
   * 
   * @param aTimestamp
   *          the timestamp to format.
   * @return a human readable timestamp, never <code>null</code>.
   */
  private String formatTime( final long aTimestamp )
  {
    final AcquisitionData data = this.session.getAcquisitionData();
    double time = aTimestamp;
    if ( data != null )
    {
      if ( data.hasTriggerData() )
      {
        time -= data.getTriggerPosition();
      }
      time = time / data.getSampleRate();
    }
    return Unit.Time.toUnit( time ).formatHumanReadable( time );
  }

  /**
   * Determines if the given character is a printable character or not.
   * 
   * @param aChar
   *          the character to test.
   * @return <code>true</code> if the given character is a printable one,
   *         <code>false</code> otherwise.
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
