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


import java.awt.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Represents an signal element on screen.
 */
public class WaveformElement
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

  private Color color;
  private boolean enabled;
  private int height;
  private int mask;
  private int signalHeight;
  private int yPos;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformElement} instance.
   */
  private WaveformElement( Type aType )
  {
    this.type = aType;
    this.enabled = true;
  }

  // METHODS

  /**
   * @param aGroup
   * @return
   */
  public static WaveformElement createAnalogScope( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.ANALOG_SCOPE );
    result.setHeight( 50 ); // XXX
    result.group = aGroup;
    return result;
  }

  /**
   * @param aChannel
   * @return
   */
  public static WaveformElement createChannelElement( Channel aChannel )
  {
    WaveformElement result = new WaveformElement( Type.CHANNEL );
    result.setColor( Color.BLUE ); // XXX
    result.setSignalHeight( 20 ); // XXX
    result.setHeight( 30 ); // XXX
    result.mask = aChannel.getMask();
    result.channel = aChannel;
    return result;
  }

  /**
   * @param aGroup
   * @return
   */
  public static WaveformElement createGroupElement( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.GROUP );
    result.setHeight( 25 ); // XXX
    result.group = aGroup;
    return result;
  }

  /**
   * @param aGroup
   * @return
   */
  public static WaveformElement createGroupSummary( ChannelGroup aGroup )
  {
    WaveformElement result = new WaveformElement( Type.GROUP_SUMMARY );
    result.setLabel( aGroup.getName() );
    result.group = aGroup;
    result.setHeight( 25 ); // XXX
    return result;
  }

  /**
   * Returns the main/foreground color of this signal element.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getColor()
  {
    return this.color;
  }

  /**
   * @return the height of this signal element on screen, in pixels.
   */
  public int getHeight()
  {
    return this.height;
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

  public int getMask()
  {
    return this.mask;
  }

  public int getOffset()
  {
    return 2; // XXX
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
   * Sets the color for this element.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( Color aColor )
  {
    this.color = aColor;
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
    if ( this.group != null )
    {
      this.group.setName( aLabel );
    }
  }

  public void setOffset( int aOffset )
  {
    // TODO
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
