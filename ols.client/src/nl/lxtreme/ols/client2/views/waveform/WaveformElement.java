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
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.views.UIMgr.*;

import java.awt.*;

import nl.lxtreme.ols.client2.views.UIMgr.Alignment;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Represents an signal element on screen.
 */
final class WaveformElement
{
  // INNER TYPES

  public static enum Type
  {
    GROUP, CHANNEL, GROUP_SUMMARY, ANALOG_SCOPE;
  }

  /**
   * Defines a measurer for waveform elements.
   */
  public static interface WaveformElementMeasurer
  {
    public static final WaveformElementMeasurer STRICT_MEASURER = new StrictChannelElementMeasurer();
    public static final WaveformElementMeasurer LOOSE_MEASURER = new LooseChannelElementMeasurer();

    /**
     * Determines whether a signal element at a given Y-position with a given
     * height fits in the boundaries defined by [minY, maxY].
     * 
     * @param aYpos
     *          the Y-position of the signal element, in pixels;
     * @param aHeight
     *          the height of the signal element, in pixels;
     * @param aMinY
     *          the minimum Y-position to fit in;
     * @param aMaxY
     *          the maximum Y-position to fit in.
     * @return <code>true</code> if the signal element would fit,
     *         <code>false</code> otherwise.
     */
    boolean signalElementFits( int aYpos, int aHeight, int aMinY, int aMaxY );
  }

  /**
   * Provides a loose channel element measurer, which selects channel elements
   * that completely fit within the boundaries, and also channel elements that
   * partly fit within the boundaries.
   */
  private static class LooseChannelElementMeasurer implements WaveformElementMeasurer
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public boolean signalElementFits( final int aYpos, final int aHeight, final int aMinY, final int aMaxY )
    {
      final int y1 = aYpos;
      final int y2 = y1 + aHeight;
      return ( ( ( y1 >= aMinY ) || ( y2 >= aMinY ) ) && ( ( y1 <= aMaxY ) || ( y2 <= aMaxY ) ) );
    }
  }

  /**
   * Provides a strict channel element measurer, which only selects channel
   * elements that completely fit within the boundaries.
   */
  private static class StrictChannelElementMeasurer implements WaveformElementMeasurer
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public boolean signalElementFits( final int aYpos, final int aHeight, final int aMinY, final int aMaxY )
    {
      return ( aYpos >= aMinY ) && ( aYpos <= aMaxY );
    }
  }

  // VARIABLES

  private final Type type;

  private ChannelGroup group;
  private Channel channel;

  private Alignment alignment;
  private boolean enabled;
  private int height;
  private int signalHeight;
  private int yPos;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformElement} instance.
   */
  private WaveformElement( Type aType )
  {
    this.type = aType;
    this.alignment = Alignment.CENTER;
    this.enabled = true;
  }

  // METHODS

  /**
   * Creates a {@link WaveformElement} that represents an analog scope for a
   * given group.
   * 
   * @param aGroup
   *          the group to create the analog scope for, cannot be
   *          <code>null</code>.
   * @return a new {@link WaveformElement} instance, never <code>null</code>.
   */
  public static WaveformElement createAnalogScope( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.ANALOG_SCOPE );
    result.setAlignment( Alignment.valueOf( getString( SIGNALVIEW_SIGNAL_ALIGNMENT, "CENTER" ) ) );
    result.setHeight( getInt( ANALOG_SCOPE_HEIGHT, 50 ) );
    result.group = aGroup;
    return result;
  }

  /**
   * Creates a {@link WaveformElement} that represents a digital signal for a
   * given channel.
   * 
   * @param aChannel
   *          the channel to create the digital signal element for, cannot be
   *          <code>null</code>.
   * @return a new {@link WaveformElement} instance, never <code>null</code>.
   */
  public static WaveformElement createChannelElement( Channel aChannel )
  {
    WaveformElement result = new WaveformElement( Type.CHANNEL );
    result.setAlignment( Alignment.valueOf( getString( SIGNALVIEW_SIGNAL_ALIGNMENT, "CENTER" ) ) );
    result.setSignalHeight( getInt( DIGITAL_SIGNAL_HEIGHT, 24 ) );
    result.setHeight( getInt( CHANNEL_HEIGHT, 30 ) );
    result.channel = aChannel;
    return result;
  }

  /**
   * Creates a {@link WaveformElement} that represents the group-element for a
   * given group.
   * 
   * @param aGroup
   *          the group to create the group-element for, cannot be
   *          <code>null</code>.
   * @return a new {@link WaveformElement} instance, never <code>null</code>.
   */
  public static WaveformElement createGroupElement( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.GROUP );
    result.setAlignment( Alignment.valueOf( getString( SIGNALVIEW_SIGNAL_ALIGNMENT, "CENTER" ) ) );
    result.setHeight( getInt( GROUP_SUMMARY_HEIGHT, 25 ) );
    result.group = aGroup;
    return result;
  }

  /**
   * Creates a {@link WaveformElement} that represents a group-summary for a
   * given group.
   * 
   * @param aGroup
   *          the group to create the group-summary for, cannot be
   *          <code>null</code>.
   * @return a new {@link WaveformElement} instance, never <code>null</code>.
   */
  public static WaveformElement createGroupSummary( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.GROUP_SUMMARY );
    result.setAlignment( Alignment.valueOf( getString( SIGNALVIEW_SIGNAL_ALIGNMENT, "CENTER" ) ) );
    result.setHeight( getInt( GROUP_SUMMARY_HEIGHT, 25 ) );
    result.setLabel( aGroup.getName() );
    result.group = aGroup;
    return result;
  }

  /**
   * Returns the current value of alignment.
   * 
   * @return the alignment, never <code>null</code>.
   */
  public Alignment getAlignment()
  {
    return this.alignment;
  }

  /**
   * Returns the main/foreground color of this signal element.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getColor()
  {
    Color result;
    if ( this.channel != null )
    {
      result = getChannelColor( channel );
    }
    else
    {
      result = getGroupColor( group );
    }
    return result;
  }

  /**
   * @return a channel group index, >= 0.
   */
  public int getGroupIndex()
  {
    if ( this.channel != null )
    {
      return this.channel.getGroup().getIndex();
    }
    return this.group.getIndex();
  }

  /**
   * @return the height of this signal element on screen, in pixels.
   */
  public int getHeight()
  {
    return this.height;
  }

  /**
   * Returns the index of this element, which means that for elements with
   * {@link Type#CHANNEL}, this will return the index of the channel, otherwise
   * it will return <tt>-1</tt>.
   * 
   * @return an index, >= 0.
   */
  public int getIndex()
  {
    if ( this.channel != null )
    {
      return this.channel.getIndex();
    }
    return -1;
  }

  /**
   * Returns the label of this signal element.
   * 
   * @return a label, can be <code>null</code>.
   */
  public String getLabel()
  {
    if ( this.channel != null )
    {
      return this.channel.getLabel();
    }
    return this.group.getName();
  }

  /**
   * @return the bitmask.
   */
  public int getMask()
  {
    if ( this.channel != null )
    {
      return this.channel.getMask();
    }
    return this.group.getIndex(); // XXX
  }

  /**
   * @return a relative offset to display the contents of this signal element,
   *         >= 0.
   */
  public int getOffset()
  {
    return getOffset( this.alignment );
  }

  /**
   * @return a relative offset to display the contents of this signal element,
   *         >= 0.
   */
  public int getOffset( final Alignment aAlignment )
  {
    final int signalOffset;
    if ( Alignment.BOTTOM.equals( aAlignment ) )
    {
      signalOffset = ( this.height - this.signalHeight );
    }
    else if ( Alignment.CENTER.equals( aAlignment ) )
    {
      signalOffset = ( int )( ( this.height - this.signalHeight ) / 2.0 );
    }
    else
    {
      signalOffset = 0;
    }

    return signalOffset;
  }

  public int getSignalHeight()
  {
    return this.signalHeight;
  }

  /**
   * @return the type of this waveform element, never <code>null</code>.
   */
  public Type getType()
  {
    return this.type;
  }

  /**
   * @return the Y-position of this signal element on screen, >= 0, in pixels.
   */
  public int getYposition()
  {
    return this.yPos;
  }

  /**
   * Returns whether or not this signal element is enabled.
   * 
   * @return <code>true</code> if this element is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isEnabled()
  {
    return this.enabled;
  }

  /**
   * @param aAlignment
   *          the alignment to set, cannot be <code>null</code>.
   */
  public void setAlignment( Alignment aAlignment )
  {
    this.alignment = aAlignment;
  }

  /**
   * Sets the color for this element.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( Color aColor )
  {
    if ( this.channel != null )
    {
      this.channel.setColor( aColor );
    }
    else if ( this.group != null )
    {
      this.group.setColor( aColor );
    }
  }

  /**
   * Sets height to the given value.
   * 
   * @param aHeight
   *          the height to set.
   */
  public void setHeight( final int aHeight )
  {
    this.height = aHeight;
  }

  /**
   * Returns the label of this signal element.
   * 
   * @param aLabel
   *          the label to set, may be <code>null</code>.
   */
  public void setLabel( String aLabel )
  {
    if ( this.channel != null )
    {
      this.channel.setLabel( aLabel );
    }
    else if ( this.group != null )
    {
      this.group.setName( aLabel );
    }
  }

  public void setSignalHeight( int aHeight )
  {
    this.signalHeight = aHeight;
  }

  /**
   * Sets the Y-position of this element.
   * 
   * @param aYPos
   *          the Y-position to set, in pixels.
   */
  public void setYposition( int aYPos )
  {
    this.yPos = aYPos;
  }

}
