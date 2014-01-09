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


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;

import java.awt.event.*;
import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides a helper class for dealing with annotations in tools.
 */
public class ToolAnnotationHelper
{
  // INNER TYPES

  /**
   * Provides an annotation implementation for use as channel label annotation.
   */
  static class ChannelLabelAnnotation implements LabelAnnotation
  {
    // VARIABLES

    private final Channel channel;
    private final String label;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelLabelAnnotation} instance.
     */
    public ChannelLabelAnnotation( Channel aChannel, String aLabel )
    {
      this.channel = aChannel;
      this.label = aLabel;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Annotation aOther )
    {
      int result = this.channel.compareTo( aOther.getChannel() );
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
    public Channel getChannel()
    {
      return this.channel;
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

    private final Channel channel;
    private final long startTimestamp;
    private final long endTimestamp;
    private final Object data;
    private final Map<String, Object> properties;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SampleDataAnnotation} instance.
     */
    public SampleDataAnnotation( Channel aChannel, long aStartTimestamp, long aEndTimestamp, Object aText,
        Map<String, Object> aProperties )
    {
      this.channel = aChannel;
      this.startTimestamp = aStartTimestamp;
      this.endTimestamp = aEndTimestamp;
      this.data = aText;
      this.properties = aProperties;
    }

    // METHODS

    private static boolean isSet( int aValue, int aBitMask )
    {
      return ( aValue & aBitMask ) != 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Annotation aOther )
    {
      int result = this.channel.compareTo( aOther.getChannel() );
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
    public Channel getChannel()
    {
      return this.channel;
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

    /**
     * {@inheritDoc}
     */
    @Override
    public String getText( int aOptions )
    {
      StringBuilder result = new StringBuilder();

      Map<String, Object> props = getProperties();
      Object desc = props.get( KEY_DESCRIPTION );
      Object type = props.get( KEY_TYPE );

      if ( TYPE_SYMBOL.equals( type ) )
      {
        if ( isSet( aOptions, OPTION_WITH_DESCRIPTION ) && ( desc != null ) )
        {
          result.append( desc ).append( " " );
        }

        Object data = getData();
        if ( data instanceof Number )
        {
          int value = ( ( Number )data ).intValue();

          if ( isSet( aOptions, OPTION_WITH_BIN_DATA ) )
          {
            result.append( "0b" ).append( Integer.toBinaryString( value ) ).append( " " );
          }
          if ( isSet( aOptions, OPTION_WITH_OCT_DATA ) )
          {
            result.append( "0" ).append( Integer.toOctalString( value ) ).append( " " );
          }
          if ( isSet( aOptions, OPTION_WITH_HEX_DATA ) )
          {
            result.append( "0x" ).append( Integer.toHexString( value ) ).append( " " );
          }
          if ( isSet( aOptions, OPTION_WITH_CHAR_DATA ) && isPrintableChar( ( char )value ) )
          {
            result.append( ( char )value ).append( " " );
          }
        }
        else
        {
          if ( isSet( aOptions, OPTION_WITH_CHAR_DATA ) )
          {
            result.append( data ).append( " " );
          }
        }
      }

      if ( isSet( aOptions, OPTION_WITH_DESCRIPTION ) && ( TYPE_EVENT.equals( type ) || TYPE_ERROR.equals( type ) )
          && ( desc != null ) )
      {
        result.append( desc ).append( " " );
      }

      if ( result.length() == 0 )
      {
        Object data = getData();
        result.append( "(" ).append( data ).append( ")" );
      }

      return result.toString().trim();
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

  // VARIABLES

  private final ToolContext context;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolAnnotationHelper} instance.
   * 
   * @param aContext
   *          the tool context to use, cannot be <code>null</code>.
   */
  public ToolAnnotationHelper( final ToolContext aContext )
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
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      this.context.addAnnotation( new SampleDataAnnotation( ch, aStartTime, aEndTime, aData, aProperties ) );
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
   * Adds an error annotation to a channel (having the property "type=error").
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate;
   * @param aStartTime
   *          the start time of the annotation;
   * @param aEndTime
   *          the ending time of the annotation;
   * @param aErrorID
   *          the identifier of this error annotation. This could be a plain
   *          string describing the error condition;
   * @param aProperties
   *          the optional properties of the returned data annotation, as
   *          key-value pairs (given elements must be a multiple of two).
   */
  public void addErrorAnnotation( final int aChannelIdx, final long aStartTime, final long aEndTime,
      final Object aErrorID, final Object... aProperties )
  {
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      Map<String, Object> props = toMap( aProperties );
      props.put( KEY_TYPE, TYPE_ERROR );

      this.context.addAnnotation( new SampleDataAnnotation( ch, aStartTime, aEndTime, aErrorID, props ) );
    }
  }

  /**
   * Adds an event annotation to a channel (having the property "type=event").
   * 
   * @param aChannelIdx
   *          the index of the channel to annotate;
   * @param aStartTime
   *          the start time of the annotation;
   * @param aEndTime
   *          the ending time of the annotation;
   * @param aEventID
   *          the identifier of this event annotation. This could be a plain
   *          string describing the event;
   * @param aProperties
   *          the optional properties of the returned data annotation, as
   *          key-value pairs (given elements must be a multiple of two).
   */
  public void addEventAnnotation( final int aChannelIdx, final long aStartTime, final long aEndTime,
      final Object aEventID, final Object... aProperties )
  {
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      Map<String, Object> props = toMap( aProperties );
      props.put( KEY_TYPE, TYPE_EVENT );

      this.context.addAnnotation( new SampleDataAnnotation( ch, aStartTime, aEndTime, aEventID, props ) );
    }
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
  public void addLabelAnnotation( final int aChannelIdx, final String aLabel )
  {
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      this.context.addAnnotation( new ChannelLabelAnnotation( ch, aLabel ) );
    }
  }

  /**
   * Adds a data symbol annotation to a channel (having the property
   * "type=symbol").
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
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      Map<String, Object> props = toMap( aProperties );
      props.put( KEY_TYPE, TYPE_SYMBOL );

      this.context.addAnnotation( new SampleDataAnnotation( ch, aStartTime, aEndTime, Integer.valueOf( aSymbol ), props ) );
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
   * Clears all annotations for the channel with the given index and sets its
   * label to the one given.
   * 
   * @param aChannelIdx
   *          the index of the channel to clear and annotate with the new label;
   * @param aLabel
   *          the new label of the channel, may be <code>null</code> to use the
   *          default label.
   */
  public void prepareChannel( final int aChannelIdx, final String aLabel )
  {
    Channel ch = getChannel( aChannelIdx );
    if ( ch != null )
    {
      this.context.clearAnnotations( ch.getIndex() );
      this.context.addAnnotation( new ChannelLabelAnnotation( ch, aLabel ) );
    }
  }

  /**
   * @param aChannelIdx
   * @return
   */
  private Channel getChannel( int aChannelIdx )
  {
    AcquisitionData data = this.context.getData();
    final int enabled = data.getEnabledChannels();
    if ( ( enabled & ( 1 << aChannelIdx ) ) != 0 )
    {
      return data.getChannels()[aChannelIdx];
    }
    return null;
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
