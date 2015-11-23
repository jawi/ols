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
package nl.lxtreme.ols.client.signaldisplay.signalelement;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;

import java.awt.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;


/**
 * Represents a signal element in the form of a digital signal, analog signal or
 * a group summary.
 */
public final class SignalElement implements Comparable<SignalElement>, IUIElement
{
  // INNER TYPES

  /**
   * Denotes the kind of a signal element, such as a digital signal, or an
   * analog signal.
   */
  public static enum SignalElementType
  {
    DIGITAL_SIGNAL, //
    GROUP_SUMMARY, //
    ANALOG_SIGNAL; //
  }

  // VARIABLES

  private final SignalElementType type;
  private final int index;
  private final int mask;

  /** the wrapped digital channel, if type == DIGITAL_SIGNAL. */
  private Channel channel;
  /** the group we belong to, should always be non-null. */
  private ElementGroup group;

  private int height;
  private int yPosition;
  /** the signal height, if type == DIGITAL_SIGNAL. */
  private int signalHeight;
  /** the signal alignment, if type == DIGITAL_SIGNAL. */
  private SignalAlignment alignment;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElement} instance.
   *
   * @param aSignalElement
   *          the signal element to copy, cannot be <code>null</code>.
   */
  protected SignalElement( final SignalElement aSignalElement )
  {
    this.index = aSignalElement.index;
    this.type = aSignalElement.type;
    this.mask = aSignalElement.mask;
    this.channel = aSignalElement.channel;
    this.height = aSignalElement.height;
    this.yPosition = aSignalElement.yPosition;
    this.signalHeight = aSignalElement.signalHeight;
    this.alignment = aSignalElement.alignment;

    setEnabled( aSignalElement.isEnabled() );
    setLabel( aSignalElement.getLabel() );
  }

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
  private SignalElement( final SignalElementType aType, final int aIndex, final int aMask )
  {
    this.type = aType;
    this.index = aIndex;
    this.mask = aMask;
  }

  // METHODS

  /**
   * Factory method for creating a {@link SignalElement} instance representing
   * an analog scope for a channel group.
   *
   * @param aGroup
   *          the channel group to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createAnalogScopeElement( final ElementGroup aGroup )
  {
    int index = aGroup.getElementCount();
    SignalElement channelElement = new SignalElement( SignalElementType.ANALOG_SIGNAL, index, -1 );
    channelElement.group = aGroup;
    channelElement.height = UIManager.getInt( ANALOG_SCOPE_HEIGHT );
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
  public static SignalElement createDigitalSignalElement( final Channel aChannel, final ElementGroup aGroup )
  {
    int index = aGroup.getElementCount();
    SignalElement channelElement = new SignalElement( SignalElementType.DIGITAL_SIGNAL, index, aChannel.getMask() );
    channelElement.channel = aChannel;
    channelElement.group = aGroup;
    channelElement.height = UIManager.getInt( CHANNEL_HEIGHT );
    channelElement.signalHeight = UIManager.getInt( DIGITAL_SIGNAL_HEIGHT );
    channelElement.alignment = SignalAlignment.valueOf( getUIManagerValue( SIGNALVIEW_SIGNAL_ALIGNMENT, "CENTER" ) );
    return channelElement;
  }

  /**
   * Factory method for creating a {@link SignalElement} instance representing a
   * summary for a group of signals.
   *
   * @param aGroup
   *          the channel group to create a channel element for, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalElement} instance, never <code>null</code>.
   */
  public static SignalElement createGroupSummaryElement( final ElementGroup aGroup )
  {
    int index = aGroup.getElementCount();
    SignalElement channelElement = new SignalElement( SignalElementType.GROUP_SUMMARY, index, -1 );
    channelElement.group = aGroup;
    channelElement.height = UIManager.getInt( GROUP_SUMMARY_HEIGHT );
    return channelElement;
  }

  /**
   * @param aKey
   *          the key to lookup, cannot be <code>null</code>.
   * @param aDefault
   *          the default value to return in case the requested value is absent.
   * @return a value, never <code>null</code>.
   */
  @SuppressWarnings( "unchecked" )
  private static <TYPE> TYPE getUIManagerValue( final String aKey, final TYPE aDefault )
  {
    Object value = UIManager.get( aKey );
    if ( ( value == null ) || !( aDefault.getClass().isAssignableFrom( value.getClass() ) ) )
    {
      return aDefault;
    }
    return ( TYPE )value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final SignalElement aSignalElement )
  {
    int thisIdx = getVirtualIndex();
    int thatIdx = aSignalElement.getVirtualIndex();

    return thisIdx - thatIdx;
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
    if ( this.type != other.type )
    {
      return false;
    }
    if ( this.channel != other.channel )
    {
      return false;
    }
    if ( this.group != other.group )
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
  public Channel getChannel()
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
    Color result = null;

    if ( this.group != null )
    {
      if ( isDigitalSignal() )
      {
        String key = getColorKey();
        result = UIManager.getColor( key );
      }

      if ( result == null )
      {
        // Fall back to group color...
        result = this.group.getColor();
      }
    }

    if ( result == null )
    {
      // Fall back to a constant value...
      result = Color.WHITE;
    }

    return result;
  }

  /**
   * Returns the element group this element belongs to.
   *
   * @return a group, can be <code>null</code>.
   */
  public ElementGroup getGroup()
  {
    return this.group;
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
    else if ( this.group != null )
    {
      if ( isAnalogSignal() )
      {
        return this.group.getAnalogSignalLabel();
      }
      else if ( isGroupSummary() )
      {
        return this.group.getGroupSummaryLabel();
      }

      return this.group.getName();
    }
    return getDefaultName();
  }

  /**
   * Returns the current value of mask.
   *
   * @return the mask
   */
  public int getMask()
  {
    if ( !isDigitalSignal() && ( this.group != null ) )
    {
      // For group summary & analog scope, we always use the mask of the entire
      // group...
      return this.group.getMask();
    }
    return this.mask;
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
  public int getOffset( final SignalAlignment aAlignment )
  {
    assert isDigitalSignal() : "Can only be called for digital signals!";
    final int signalOffset;
    if ( SignalAlignment.BOTTOM.equals( aAlignment ) )
    {
      signalOffset = ( this.height - this.signalHeight );
    }
    else if ( SignalAlignment.CENTER.equals( aAlignment ) )
    {
      signalOffset = ( int )( ( this.height - this.signalHeight ) / 2.0 );
    }
    else
    {
      signalOffset = 0;
    }

    return signalOffset;
  }

  /**
   * Returns the current value of alignment.
   *
   * @return the alignment
   */
  public SignalAlignment getSignalAlignment()
  {
    return this.alignment;
  }

  /**
   * Returns the current value of signalHeight.
   *
   * @return the signalHeight
   */
  public int getSignalHeight()
  {
    return this.signalHeight;
  }

  /**
   * Returns the type of this signal element.
   *
   * @return a signal element type, never <code>null</code>.
   */
  public SignalElementType getType()
  {
    return this.type;
  }

  /**
   * @return the masked sample value.
   */
  public int getValue( final int aSampleValue )
  {
    int value = 0;
    if ( isDigitalSignal() && ( ( aSampleValue & this.mask ) != 0 ) )
    {
      value |= 1;
    }
    else if ( isAnalogSignal() || isGroupSummary() )
    {
      List<SignalElement> elements = getGroup().getElements();
      for ( int i = elements.size() - 1; i >= 0; i-- )
      {
        SignalElement el = elements.get( i );
        if ( el.isDigitalSignal() )
        {
          value = ( value << 1 ) | el.getValue( aSampleValue );
        }
      }
    }

    return value;
  }

  /**
   * Returns the virtual index of this channel.
   *
   * @return the virtualIndex, >= 0.
   */
  public int getVirtualIndex()
  {
    int result = -1;
    if ( this.group != null )
    {
      result = this.group.getVirtualIndex( this );
    }
    return result;
  }

  /**
   * @return the Y-position on screen, >= 0, in pixels.
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
    result = ( prime * result ) + this.group.hashCode();
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
    else if ( this.group != null )
    {
      if ( isGroupSummary() )
      {
        return this.group.isShowGroupSummary();
      }
      else if ( isAnalogSignal() )
      {
        return this.group.isShowAnalogSignal();
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
   * Sets the main color of this signal element.
   *
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Color cannot be null!" );
    }
    UIManager.put( getColorKey(), aColor );
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
    else if ( this.group != null )
    {
      if ( isGroupSummary() )
      {
        this.group.setShowGroupSummary( aEnabled );
      }
      else if ( isAnalogSignal() )
      {
        this.group.setShowAnalogSignal( aEnabled );
      }
      else
      {
        throw new InternalError( "Unknown signal element?! Type = " + this.type );
      }
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
  public final void setLabel( final String aLabel )
  {
    if ( this.channel != null )
    {
      this.channel.setLabel( aLabel );
    }
    else if ( this.group != null )
    {
      if ( isAnalogSignal() )
      {
        this.group.setAnalogSignalLabel( aLabel );
      }
      else if ( isGroupSummary() )
      {
        this.group.setGroupSummaryLabel( aLabel );
      }
      else
      {
        this.group.setName( aLabel );
      }
    }
  }

  /**
   * Sets alignment to the given value.
   *
   * @param aAlignment
   *          the alignment to set.
   */
  public void setSignalAlignment( final SignalAlignment aAlignment )
  {
    if ( !isDigitalSignal() )
    {
      throw new IllegalArgumentException( "Can only be called for digital signals!" );
    }
    this.alignment = aAlignment;
  }

  /**
   * Sets signalHeight to the given value.
   *
   * @param aSignalHeight
   *          the signalHeight to set.
   */
  public void setSignalHeight( final int aSignalHeight )
  {
    if ( !isDigitalSignal() )
    {
      throw new IllegalArgumentException( "Can only be called for digital signals!" );
    }
    this.signalHeight = aSignalHeight;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return String.format( "SignalElement[%s / %s]@%s", getLabel(), getGroup().getName(),
        Integer.toHexString( hashCode() ) );
  }

  /**
   * @return the (fixed) index of this signal element.
   */
  final int getIndex()
  {
    return this.index;
  }

  /**
   * Connects this signal element to a the given group.
   *
   * @param aGroup
   *          the group to connect this element to, can be <code>null</code> to
   *          disconnect this element from its current group.
   */
  final void setGroup( final ElementGroup aGroup )
  {
    synchronized ( this )
    {
      this.group = aGroup;
    }
  }

  /**
   * Sets yPosition to the given value.
   *
   * @param aYPosition
   *          the yPosition to set.
   */
  final void setYposition( final int aYPosition )
  {
    this.yPosition = aYPosition;
  }

  /**
   * @return
   */
  private String getColorKey()
  {
    Integer groupIdx = Integer.valueOf( ( this.group.getIndex() % 4 ) + 1 );
    // Issue #121: channel can be null for non-digital channels...
    if ( this.channel != null )
    {
      Integer channelIdx = Integer.valueOf( ( this.channel.getIndex() % 8 ) + 1 );
      return String.format( "ols.channelgroup%d.channel%d.default.color", groupIdx, channelIdx );
    }
    return String.format( "ols.channelgroup%d.default.color", groupIdx );
  }

  /**
   * Crafts a default channel name for use when a channel has no label set.
   *
   * @return a channel name, never <code>null</code>.
   */
  private String getDefaultName()
  {
    int index = 0;
    String name = "";
    if ( this.channel != null )
    {
      index = this.channel.getIndex();
      name = this.group.getName();
    }
    else if ( this.group != null )
    {
      index = this.group.getIndex();
      name = this.group.getName();
    }
    return String.format( "%s-%d", name, Integer.valueOf( index + 1 ) );
  }
}
