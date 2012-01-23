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
package nl.lxtreme.ols.client.signaldisplay.model;


import java.awt.*;

import nl.lxtreme.ols.client.signaldisplay.channel.*;


/**
 * Represents a signal element in the form of a digital signal, analog signal or
 * a group summary.
 */
public class SignalElement
{
  // INNER TYPES

  /**
   * Denotes the kind of a signal element, such as a digital signal, or an
   * analog signal.
   */
  static enum SignalElementType
  {
    SIGNAL_GROUP, //
    DIGITAL_SIGNAL, //
    GROUP_SUMMARY, //
    ANALOG_SIGNAL; //
  }

  // VARIABLES

  private final SignalElementType type;
  private final int yPosition;
  private final int height;
  private final int mask;

  private ChannelImpl channel;
  private ChannelGroup channelGroup;

  // TODO add label, color & font...

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElement} instance.
   * 
   * @param aType
   *          the type of this signal element, cannot be <code>null</code>;
   * @param aMask
   *          the mask of channel elements;
   * @param aYposition
   *          the Y-position on screen, >= 0;
   * @param aHeight
   *          the height of this channel element, in pixels, >= 0.
   */
  private SignalElement( final SignalElementType aType, final int aMask, final int aYposition, final int aHeight )
  {
    this.type = aType;
    this.mask = aMask;
    this.yPosition = aYposition;
    this.height = aHeight;
  }

  // METHODS

  /**
   * Factory method for creating a {@link SignalElement} instance representing
   * an analog scope for a channel group.
   * 
   * @param aChannelGroup
   *          the channel group to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createAnalogScopeElement( final ChannelGroup aChannelGroup, final int aYposition,
      final int aHeight )
  {
    final SignalElement channelElement = new SignalElement( SignalElementType.ANALOG_SIGNAL, aChannelGroup.getMask(),
        aYposition, aHeight );
    channelElement.channelGroup = aChannelGroup;
    return channelElement;
  }

  /**
   * Factory method for creating a {@link SignalElement} instance representing a
   * digital signal.
   * 
   * @param aChannel
   *          the channel to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createDigitalSignalElement( final ChannelImpl aChannel, final int aYposition,
      final int aHeight )
  {
    final SignalElement channelElement = new SignalElement( SignalElementType.DIGITAL_SIGNAL, aChannel.getMask(),
        aYposition, aHeight );
    channelElement.channel = aChannel;
    return channelElement;
  }

  /**
   * Factory method for creating a {@link SignalElement} instance representing a
   * summary for a group of signals.
   * 
   * @param aChannelGroup
   *          the channel group to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createGroupSummaryElement( final ChannelGroup aChannelGroup, final int aYposition,
      final int aHeight )
  {
    final SignalElement channelElement = new SignalElement( SignalElementType.GROUP_SUMMARY, aChannelGroup.getMask(),
        aYposition, aHeight );
    channelElement.channelGroup = aChannelGroup;
    return channelElement;
  }

  /**
   * Factory method for creating a {@link SignalElement} instance representing a
   * a group of signals.
   * 
   * @param aChannelGroup
   *          the channel group to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createSignalGroupElement( final ChannelGroup aChannelGroup, final int aYposition,
      final int aHeight )
  {
    final SignalElement channelElement = new SignalElement( SignalElementType.SIGNAL_GROUP, aChannelGroup.getMask(),
        aYposition, aHeight );
    channelElement.channelGroup = aChannelGroup;
    return channelElement;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof SignalElement ) )
    {
      return false;
    }

    final SignalElement other = ( SignalElement )aObject;
    if ( this.height != other.height )
    {
      return false;
    }
    if ( this.yPosition != other.yPosition )
    {
      return false;
    }
    if ( this.type != other.type )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the channel of this signal element, when this signal element
   * represents a digital signal.
   * 
   * @return the channel, never <code>null</code>.
   * @throws IllegalStateException
   *           in case this signal element does not represent a digital signal.
   * @see #isDigitalSignal()
   */
  public ChannelImpl getChannel()
  {
    if ( this.channel == null )
    {
      throw new IllegalStateException( "Can only be called for a digital signal!" );
    }
    return this.channel;
  }

  /**
   * Returns the main color of this signal element.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getColor()
  {
    if ( this.channel != null )
    {
      return this.channel.getColor();
    }
    else
    {
      return this.channelGroup.getColor();
    }
  }

  /**
   * Returns the current value of height.
   * 
   * @return the height, in pixels.
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
  public final String getLabel()
  {
    if ( this.channel != null )
    {
      return this.channel.getLabel();
    }
    else if ( this.channelGroup != null )
    {
      if ( isAnalogSignal() )
      {
        return this.channelGroup.getAnalogSignalLabel();
      }
      else if ( isGroupSummary() )
      {
        return this.channelGroup.getGroupSummaryLabel();
      }

      return this.channelGroup.getName();
    }
    return null;
  }

  /**
   * Returns the current value of mask.
   * 
   * @return the mask
   */
  public int getMask()
  {
    return this.mask;
  }

  /**
   * Returns the Y-position of this channel element on screen.
   * 
   * @return the Y-position, >= 0.
   */
  public int getYposition()
  {
    return this.yPosition;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + this.height;
    result = ( prime * result ) + this.type.hashCode();
    return result;
  }

  /**
   * Returns whether we should show the analog signal for this group.
   * 
   * @return <code>true</code> if the analog signal is to be shown,
   *         <code>false</code> to hide it.
   */
  public boolean isAnalogSignal()
  {
    return ( this.type == SignalElementType.ANALOG_SIGNAL );
  }

  /**
   * Returns whether we should show digital signals in this group.
   * 
   * @return <code>true</code> if the individual digital signals are to be
   *         shown, <code>false</code> to hide them.
   */
  public boolean isDigitalSignal()
  {
    return ( this.type == SignalElementType.DIGITAL_SIGNAL );
  }

  /**
   * Returns whether or not this signal element is enabled.
   * 
   * @return
   */
  public boolean isEnabled()
  {
    if ( this.channel != null )
    {
      return this.channel.isEnabled();
    }
    else if ( this.channelGroup != null )
    {
      if ( isGroupSummary() )
      {
        return this.channelGroup.isShowGroupSummary();
      }
      else if ( isAnalogSignal() )
      {
        return this.channelGroup.isShowAnalogSignal();
      }
      else if ( isSignalGroup() )
      {
        return this.channelGroup.isShowDigitalSignals();
      }
      else
      {
        throw new InternalError( "Unknown signal element?! Type = " + this.type );
      }
    }
    return false;
  }

  /**
   * Returns whether we should a summary for this group.
   * 
   * @return <code>true</code> if the data values are to be shown,
   *         <code>false</code> to hide them.
   */
  public boolean isGroupSummary()
  {
    return ( this.type == SignalElementType.GROUP_SUMMARY );
  }

  /**
   * Returns whether we should this group at all.
   * 
   * @return <code>true</code> if this group is to be shown, <code>false</code>
   *         to hide them.
   */
  public boolean isSignalGroup()
  {
    return ( this.type == SignalElementType.SIGNAL_GROUP );
  }

  /**
   * Sets the main color of this signal element.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    if ( this.channel != null )
    {
      this.channel.setColor( aColor );
    }
    else
    {
      this.channelGroup.setColor( aColor );
    }
  }

  /**
   * Sets whether or not this signal element is enabled.
   * 
   * @param aEnabled
   *          <code>true</code> to enable this signal element,
   *          <code>false</code> to disable it.
   */
  public void setEnabled( final boolean aEnabled )
  {
    if ( this.channel != null )
    {
      this.channel.setEnabled( aEnabled );
    }
    else if ( this.channelGroup != null )
    {
      if ( isGroupSummary() )
      {
        this.channelGroup.setGroupSummary( aEnabled );
      }
      else if ( isAnalogSignal() )
      {
        this.channelGroup.setShowAnalogSignal( aEnabled );
      }
      else if ( isSignalGroup() )
      {
        this.channelGroup.setShowDigitalSignals( aEnabled );
      }
      else
      {
        throw new InternalError( "Unknown signal element?! Type = " + this.type );
      }
    }
  }

  /**
   * Returns the label of this signal element.
   * 
   * @param aLabel
   *          the label to set, may be <code>null</code>.
   */
  public final void setLabel( final String aLabel )
  {
    if ( this.channel != null )
    {
      this.channel.setLabel( aLabel );
    }
    else if ( this.channelGroup != null )
    {
      if ( isAnalogSignal() )
      {
        this.channelGroup.setAnalogSignalLabel( aLabel );
      }
      else if ( isGroupSummary() )
      {
        this.channelGroup.setGroupSummaryLabel( aLabel );
      }
      else
      {
        this.channelGroup.setName( aLabel );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    StringBuilder builder = new StringBuilder();
    builder.append( "SignalElement [type=" );
    builder.append( this.type );
    builder.append( ", yPosition=" );
    builder.append( this.yPosition );
    builder.append( ", height=" );
    builder.append( this.height );
    if ( this.channel != null )
    {
      builder.append( ", channel=" );
      builder.append( this.channel );
    }
    if ( this.channelGroup != null )
    {
      builder.append( ", channelGroup=" );
      builder.append( this.channelGroup );
    }
    builder.append( "]" );
    return builder.toString();
  }
}
