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
package nl.lxtreme.ols.tool.api;


import java.util.*;

import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides a helper class for dealing with annotations in tools.
 */
public class AnnotationHelper
{
  // INNER TYPES

  /**
   * Provides an annotation implementation for use as channel label annotation.
   */
  static class ChannelLabelAnnotation implements Annotation
  {
    // VARIABLES

    private final int channelIdx;
    private final String label;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelLabelAnnotation} instance.
     */
    public ChannelLabelAnnotation( final int aChannelIdx, final String aLabel )
    {
      this.channelIdx = aChannelIdx;
      this.label = aLabel;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Annotation aOther )
    {
      int result = ( this.channelIdx - aOther.getChannelIndex() );
      if ( result == 0 )
      {
        String d1 = getData();
        String d2 = String.valueOf( aOther.getData() );
        result = d1.compareTo( d2 );
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannelIndex()
    {
      return this.channelIdx;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getData()
    {
      return this.label;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return this.label;
    }
  }

  /**
   * Provides a data annotation implementation.
   */
  static class SampleDataAnnotation implements DataAnnotation
  {
    // VARIABLES

    private final int channelIdx;
    private final long startTimestamp;
    private final long endTimestamp;
    private final Object data;
    private final Map<String, Object> properties;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SampleDataAnnotation} instance.
     */
    public SampleDataAnnotation( final int aChannelIdx, final long aStartTimestamp, final long aEndTimestamp,
        final Object aText, final Map<String, Object> aProperties )
    {
      this.channelIdx = aChannelIdx;
      this.startTimestamp = aStartTimestamp;
      this.endTimestamp = aEndTimestamp;
      this.data = aText;
      this.properties = aProperties;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Annotation aOther )
    {
      int result = this.channelIdx - aOther.getChannelIndex();
      if ( result == 0 )
      {
        if ( aOther instanceof DataAnnotation )
        {
          DataAnnotation dataAnnotation = ( DataAnnotation )aOther;

          result = ( int )( this.startTimestamp - dataAnnotation.getStartTimestamp() );
          if ( result == 0 )
          {
            result = ( int )( this.endTimestamp - dataAnnotation.getEndTimestamp() );
          }
        }
      }

      if ( result == 0 )
      {
        String d1 = String.valueOf( getData() );
        String d2 = String.valueOf( aOther.getData() );
        result = d1.compareTo( d2 );
      }

      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannelIndex()
    {
      return this.channelIdx;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getData()
    {
      return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getEndTimestamp()
    {
      return this.endTimestamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getProperties()
    {
      return this.properties;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getStartTimestamp()
    {
      return this.startTimestamp;
    }
  }

  // CONSTANTS

  /**
   * Denotes a property to use an alternative color for the annotation. The
   * value of this property is either a {@link java.awt.Color} object, or a
   * triple hex-string value (e.g. "#aabbcc").
   */
  public static final String KEY_COLOR = "color";
  /**
   * Denotes a property that indicates that the annotation is in fact an error.
   * The value of this property should be a {@link Boolean} value.
   */
  public static final String KEY_ERROR = "error";
  /**
   * Denotes a property that indicates that the annotation is a decoded symbol.
   * The value of this property should be a {@link Boolean} value.
   */
  public static final String KEY_SYMBOL = "symbol";
  /**
   * Denotes a property that provides an additional description to the
   * annotation. The value of this property should be a string.
   */
  public static final String KEY_DESCRIPTION = "desc";

  // VARIABLES

  private final ToolContext context;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationHelper} instance.
   * 
   * @param aContext
   *          the tool context to use, cannot be <code>null</code>.
   */
  public AnnotationHelper( final ToolContext aContext )
  {
    this.context = aContext;
  }

  // METHODS

  /**
   * Adds a data or event annotation to a channel.
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate;
   * @param aStartTime
   *          the start time of the annotation;
   * @param aEndTime
   *          the ending time of the annotation;
   * @param aData
   *          the actual data of the annotation;
   * @param aProperties
   *          the optional properties of the returned data annotation, as
   *          key-value pairs (given elements must be a multiple of two).
   */
  public void addAnnotation( final int aChannelIdx, final long aStartTime, final long aEndTime, final Object aData,
      final Map<String, Object> aProperties )
  {
    boolean validChannel = ( this.context.getEnabledChannels() & ( 1 << aChannelIdx ) ) != 0;
    if ( validChannel )
    {
      this.context.addAnnotation( new SampleDataAnnotation( aChannelIdx, aStartTime, aEndTime, aData, aProperties ) );
    }
  }

  /**
   * Adds a data or event annotation to a channel.
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate;
   * @param aStartTime
   *          the start time of the annotation;
   * @param aEndTime
   *          the ending time of the annotation;
   * @param aData
   *          the actual data of the annotation;
   * @param aProperties
   *          the optional properties of the returned data annotation, as
   *          key-value pairs (given elements must be a multiple of two).
   */
  public void addAnnotation( final int aChannelIdx, final long aStartTime, final long aEndTime, final Object aData,
      final Object... aProperties )
  {
    addAnnotation( aChannelIdx, aStartTime, aEndTime, aData, toMap( aProperties ) );
  }

  /**
   * Adds an annotation denoting a change in the label of a channel.
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate with the new label;
   * @param aLabel
   *          the new label of the channel, may be <code>null</code> to use the
   *          default label.
   */
  public void addAnnotation( final int aChannelIdx, final String aLabel )
  {
    boolean validChannel = ( this.context.getEnabledChannels() & ( 1 << aChannelIdx ) ) != 0;
    if ( validChannel )
    {
      this.context.addAnnotation( new ChannelLabelAnnotation( aChannelIdx, aLabel ) );
    }
  }

  /**
   * Adds a data symbol annotation to a channel (having the property
   * "symbol=true").
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate;
   * @param aStartTime
   *          the start time of the annotation;
   * @param aEndTime
   *          the ending time of the annotation;
   * @param aSymbol
   *          the actual data of the annotation;
   * @param aProperties
   *          the optional properties of the returned data annotation, as
   *          key-value pairs (given elements must be a multiple of two).
   */
  public void addSymbolAnnotation( final int aChannelIdx, final long aStartTime, final long aEndTime,
      final int aSymbol, final Object... aProperties )
  {
    boolean validChannel = ( this.context.getEnabledChannels() & ( 1 << aChannelIdx ) ) != 0;
    if ( validChannel )
    {
      Map<String, Object> props = toMap( aProperties );
      props.put( KEY_SYMBOL, Boolean.TRUE );

      this.context.addAnnotation( new SampleDataAnnotation( aChannelIdx, aStartTime, aEndTime, Integer
          .valueOf( aSymbol ), props ) );
    }
  }

  /**
   * Clears the annotations on the channels denoted by the given indexes.
   * 
   * @param aChannelIdxs
   *          the indexes of the channel to clear the annotations for, cannot be
   *          <code>null</code>. Invalid channel indexes are silently ignored.
   */
  public void clearAnnotations( final int... aChannelIdxs )
  {
    this.context.clearAnnotations( aChannelIdxs );
  }

  /**
   * @param aProperties
   * @return
   * @throws IllegalArgumentException
   */
  private Map<String, Object> toMap( final Object[] aProperties ) throws IllegalArgumentException
  {
    Map<String, Object> result = new HashMap<String, Object>();
    if ( aProperties != null )
    {
      if ( ( aProperties.length % 2 ) != 0 )
      {
        throw new IllegalArgumentException( "Odd number of properties defined!" );
      }
      for ( int i = 0; i < aProperties.length; i += 2 )
      {
        String key = ( String )aProperties[i];
        Object value = aProperties[i + 1];
        result.put( key, value );
      }
    }
    return result;
  }
}
