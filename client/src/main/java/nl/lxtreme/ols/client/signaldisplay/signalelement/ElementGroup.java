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


import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.SignalElementType;


/**
 * Indicates a number of grouped channels, with their own set of labels and
 * indexes of channels.
 * <p>
 * This class is thread-safe.
 * </p>
 */
public class ElementGroup
{
  // INNER TYPES

  /**
   * Denotes the type of the channel.
   */
  public static enum ChannelElementType
  {
    // CONSTANTS

    DIGITAL_SIGNAL( 1 ), //
    GROUP_SUMMARY( 2 ), //
    ANALOG_SIGNAL( 4 ); //

    // VARIABLES

    private int mask;

    // CONSTRUCTORS

    /**
     * Creates a new ChannelScreenElementType instance.
     * 
     * @param aValue
     *          the numeric value, >= 1.
     */
    private ChannelElementType( final int aValue )
    {
      this.mask = ( 1 << aValue );
    }

    // METHODS

    /**
     * Returns the mask of this {@link ChannelElementType}.
     * 
     * @return the mask value, >= 1.
     */
    public int getMask()
    {
      return this.mask;
    }
  }

  // VARIABLES

  private final List<SignalElement> elements;

  private final int index;
  private int mask;
  /** The name of this group. */
  private String name;
  /** The label used for the group summary. */
  private String summaryLabel;
  /** The label used for the analog signal. */
  private String analogSignalLabel;
  /** The color used as default for this channel group. */
  private Color color;
  private boolean visible;
  private int viewOptions;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelGroup instance.
   * 
   * @param aIndex
   *          the index of this channel group, >= 0;
   * @param aName
   *          the name of this channel group, cannot be <code>null</code> or
   *          empty.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  ElementGroup( final int aIndex, final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    this.index = aIndex;
    this.name = aName;
    this.mask = 0;
    // By default visible...
    this.visible = true;
    // By default only the digital signals are shown...
    this.viewOptions = ChannelElementType.DIGITAL_SIGNAL.mask;

    if ( UIManager.getBoolean( UIManagerKeys.ANALOG_SCOPE_VISIBLE_DEFAULT ) )
    {
      this.viewOptions |= ChannelElementType.ANALOG_SIGNAL.mask;
    }
    if ( UIManager.getBoolean( UIManagerKeys.GROUP_SUMMARY_VISIBLE_DEFAULT ) )
    {
      this.viewOptions |= ChannelElementType.GROUP_SUMMARY.mask;
    }

    this.elements = new ArrayList<SignalElement>();
  }

  /**
   * Adds a given signal element to this group.
   * <p>
   * If the given element is already contained by this group, this method is
   * effectively a no-op.
   * </p>
   * 
   * @param aElement
   *          the signal element to add, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given channel was <code>null</code>.
   */
  public void addElement( final SignalElement aElement )
  {
    if ( hasElement( aElement ) )
    {
      // Nothing to do; this channel already is in this group...
      return;
    }

    final SignalElement signalElement = aElement;

    // Make sure we've disconnected the element from its former group...
    final ElementGroup oldGroup = aElement.getGroup();
    if ( oldGroup != null )
    {
      oldGroup.removeElement( signalElement );
    }

    synchronized ( this.elements )
    {
      this.elements.add( signalElement );
      // Make sure the channel links back to this channel group...
      signalElement.setGroup( this );

      // Update our local mask...
      this.mask |= aElement.getMask();
    }
  }

  // METHODS

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
    if ( ( aObject == null ) || !( aObject instanceof ElementGroup ) )
    {
      return false;
    }

    final ElementGroup other = ( ElementGroup )aObject;
    if ( this.name == null )
    {
      if ( other.name != null )
      {
        return false;
      }
    }
    else if ( !this.name.equals( other.name ) )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the label used for the analog scope of this channel group.
   * 
   * @return the label for the analog scope, can be <code>null</code>.
   */
  public String getAnalogSignalLabel()
  {
    if ( ( this.analogSignalLabel == null ) || this.analogSignalLabel.trim().isEmpty() )
    {
      return getDefaultAnalogSignalName();
    }
    return this.analogSignalLabel;
  }

  /**
   * Returns the signal element that represents the channel with the given
   * index.
   * 
   * @param aChannelIndex
   *          the index of the channel to retrieve the corresponding signal
   *          element for.
   * @return a signal element matching the given channel index, or
   *         <code>null</code> if no such element was found.
   */
  public SignalElement getChannelByIndex( final int aChannelIndex )
  {
    synchronized ( this.elements )
    {
      for ( SignalElement element : this.elements )
      {
        if ( element.isDigitalSignal() && ( element.getChannel().getIndex() == aChannelIndex ) )
        {
          return element;
        }
      }
    }
    return null;
  }

  /**
   * Returns the color of this channel group.
   * 
   * @return the color used by this channel group, never <code>null</code>.
   */
  public Color getColor()
  {
    Color result = this.color;

    if ( result == null )
    {
      result = getDefaultGroupColor( this.index );
    }

    return result;
  }

  /**
   * Returns the number of all element in this group.
   * 
   * @return an element count, >= 0.
   */
  public int getElementCount()
  {
    synchronized ( this.elements )
    {
      return this.elements.size();
    }
  }

  /**
   * Returns the number of elements specified by the given type in this group.
   * 
   * @param aType
   *          the type to count, cannot be <code>null</code>.
   * @return an element count, >= 0.
   */
  public int getElementCount( final SignalElementType aType )
  {
    int count = 0;
    synchronized ( this.elements )
    {
      for ( SignalElement element : this.elements )
      {
        if ( aType.equals( element.getType() ) )
        {
          count++;
        }
      }
      return count;
    }
  }

  /**
   * Returns all channels assigned to this channel group.
   * 
   * @return an array of channels, never <code>null</code>.
   */
  public Collection<SignalElement> getElements()
  {
    Collection<SignalElement> result = new ArrayList<SignalElement>();
    synchronized ( this.elements )
    {
      result.addAll( this.elements );
    }
    return result;
  }

  /**
   * Returns the label used for the group summary of this channel group.
   * 
   * @return the label for the group summary, can be <code>null</code>.
   */
  public String getGroupSummaryLabel()
  {
    if ( ( this.summaryLabel == null ) || this.summaryLabel.trim().isEmpty() )
    {
      return getDefaultGroupSummaryName();
    }
    return this.summaryLabel;
  }

  /**
   * @return the index
   */
  public int getIndex()
  {
    return this.index;
  }

  /**
   * Returns the bitwise mask for all channels that belong to this channel
   * group.
   * 
   * @return a bitmask, >= 0.
   */
  public int getMask()
  {
    return this.mask;
  }

  /**
   * Returns the name of this channel group.
   * 
   * @return a name, never <code>null</code> or empty.
   */
  public String getName()
  {
    return this.name;
  }

  /**
   * Returns whether or not a element belong to this group.
   * 
   * @param aElement
   *          the element to test, cannot be <code>null</code>.
   * @return <code>true</code> if the given channel is contained by this channel
   *         group, <code>false</code> otherwise.
   * @throws IllegalArgumentException
   *           in case the given channel was <code>null</code>.
   */
  public boolean hasElement( final SignalElement aElement )
  {
    if ( aElement == null )
    {
      throw new IllegalArgumentException( "Channel cannot be null!" );
    }

    return this.elements.contains( aElement );
  }

  /**
   * Returns whether or not this group has any elements.
   * 
   * @return <code>true</code> if this group contains at least one signal
   *         element, <code>false</code> otherwise.
   */
  public boolean hasElements()
  {
    synchronized ( this.elements )
    {
      return !this.elements.isEmpty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.name == null ) ? 0 : this.name.hashCode() );
    return result;
  }

  /**
   * Returns whether we should show the analog signal for this group.
   * 
   * @return <code>true</code> if the analog signal is to be shown,
   *         <code>false</code> to hide it.
   */
  public boolean isShowAnalogSignal()
  {
    return ( this.viewOptions & ChannelElementType.ANALOG_SIGNAL.getMask() ) != 0;
  }

  /**
   * Returns whether we should show digital signals in this group.
   * 
   * @return <code>true</code> if the individual digital signals are to be
   *         shown, <code>false</code> to hide them.
   */
  public boolean isShowDigitalSignals()
  {
    return ( this.viewOptions & ChannelElementType.DIGITAL_SIGNAL.getMask() ) != 0;
  }

  /**
   * Returns whether we should show the summary for this group.
   * 
   * @return <code>true</code> if the summary is to be shown, <code>false</code>
   *         to hide this summary.
   */
  public boolean isShowGroupSummary()
  {
    return ( this.viewOptions & ChannelElementType.GROUP_SUMMARY.getMask() ) != 0;
  }

  /**
   * Returns whether or not this entire channel group is visible.
   * 
   * @return <code>true</code> if this channel group is visible,
   *         <code>false</code> otherwise.
   */
  public boolean isVisible()
  {
    return this.visible;
  }

  /**
   * Sets the label for the analog signal of this channel group.
   * 
   * @param aSignalLabel
   *          the label to set for the analog signal of this channel group.
   */
  public void setAnalogSignalLabel( final String aSignalLabel )
  {
    this.analogSignalLabel = aSignalLabel;
  }

  /**
   * Sets the color of this channel group.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    this.color = aColor;
  }

  /**
   * Sets the label for the group summary of this channel group.
   * 
   * @param aSummaryLabel
   *          the label to set for the group summary.
   */
  public void setGroupSummaryLabel( final String aSummaryLabel )
  {
    this.summaryLabel = aSummaryLabel;
  }

  /**
   * Sets name to the given value.
   * 
   * @param aName
   *          the name to set.
   * @throws IllegalArgumentException
   *           in case the given name is <code>null</code> or empty.
   */
  public void setName( final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }
    this.name = aName;
  }

  /**
   * Sets whether or not the analog signal is to be shown.
   * 
   * @param aShowAnalogSignal
   *          <code>true</code> to show the analog signal, <code>false</code> to
   *          hide it.
   */
  public void setShowAnalogSignal( final boolean aShowAnalogSignal )
  {
    int mask = ChannelElementType.ANALOG_SIGNAL.getMask();
    if ( aShowAnalogSignal )
    {
      this.viewOptions |= mask;
    }
    else
    {
      this.viewOptions &= ~mask;
    }
  }

  /**
   * Sets whether or not the individual digital signals are to be shown.
   * 
   * @param aShowDigitalSignals
   *          <code>true</code> to show the individual digital signals,
   *          <code>false</code> to hide them.
   */
  public void setShowDigitalSignals( final boolean aShowDigitalSignals )
  {
    int mask = ChannelElementType.DIGITAL_SIGNAL.getMask();
    if ( aShowDigitalSignals )
    {
      this.viewOptions |= mask;
    }
    else
    {
      this.viewOptions &= ~mask;
    }
  }

  /**
   * Sets whether or not the data values are to be shown.
   * 
   * @param aShowSummary
   *          <code>true</code> to show the group summary, <code>false</code> to
   *          hide it.
   */
  public void setShowGroupSummary( final boolean aShowSummary )
  {
    int mask = ChannelElementType.GROUP_SUMMARY.getMask();
    if ( aShowSummary )
    {
      this.viewOptions |= mask;
    }
    else
    {
      this.viewOptions &= ~mask;
    }
  }

  /**
   * Sets visible to the given value.
   * 
   * @param aVisible
   *          the visible to set.
   */
  public void setVisible( final boolean aVisible )
  {
    this.visible = aVisible;
  }

  /**
   * Returns the virtual index for a given channel.
   * 
   * @param aElement
   *          the channel to return the virtual index for, cannot be
   *          <code>null</code>.
   * @return the virtual index of the given channel (>= 0 && <
   *         {@link #getElementCount()}) or -1 if the given channel does not
   *         belong to this group.
   */
  final int getVirtualIndex( final SignalElement aElement )
  {
    Iterator<SignalElement> iter;
    synchronized ( this.elements )
    {
      iter = this.elements.iterator();
    }

    int i = 0;
    while ( iter.hasNext() )
    {
      if ( aElement == iter.next() )
      {
        return i;
      }
      i++;
    }

    return -1;
  }

  /**
   * Moves a given channel to a new index in this channel group.
   * 
   * @param aElement
   *          the channel to move, cannot be <code>null</code>;
   * @param aNewIndex
   *          the new index of the channel, >= 0.
   */
  final void moveChannel( final SignalElement aElement, final int aNewIndex )
  {
    // Make sure we've disconnected the element from its former group...
    final ElementGroup oldGroup = aElement.getGroup();
    if ( oldGroup != null )
    {
      oldGroup.removeElement( aElement );
    }

    synchronized ( this.elements )
    {
      if ( aNewIndex >= this.elements.size() )
      {
        this.elements.add( aElement );
      }
      else
      {
        this.elements.add( Math.max( 0, aNewIndex ), aElement );
      }
      // Make sure the channel links back to this channel group...
      aElement.setGroup( this );

      // Update our local mask...
      this.mask |= aElement.getMask();
    }
  }

  /**
   * Removes a given channel from this channel group.
   * <p>
   * If the given channel is <em>not</em> contained by this channel group, this
   * method is effectively a no-op.
   * </p>
   * 
   * @param aElement
   *          the channel to remove, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given channel was <code>null</code>.
   */
  final void removeElement( final SignalElement aElement )
  {
    synchronized ( this.elements )
    {
      if ( this.elements.remove( aElement ) )
      {
        // Make sure the channel no longer links back to this channel group...
        aElement.setGroup( null );

        // Remove channel's mask from our local mask...
        this.mask &= ~aElement.getMask();
      }
    }
  }

  /**
   * Crafts a default name for use when an analog scope has no label set.
   * 
   * @return an analog scope name, never <code>null</code>.
   */
  private String getDefaultAnalogSignalName()
  {
    return String.format( "Scope-%d", Integer.valueOf( getIndex() + 1 ) );
  }

  /**
   * Returns the default group color, as stored in the UIManager.
   * 
   * @param aIndex
   *          the group index to return the color for.
   * @return a color, never <code>null</code>.
   */
  private Color getDefaultGroupColor( final int aIndex )
  {
    String key = String.format( "ols.channelgroup%d.default.color", Integer.valueOf( ( aIndex + 1 ) % 4 ) );
    Color defaultColor = UIManager.getColor( key );
    if ( defaultColor == null )
    {
      defaultColor = Color.WHITE;
    }
    return defaultColor;
  }

  /**
   * Crafts a default name for use when a group summary has no label set.
   * 
   * @return a group summary name, never <code>null</code>.
   */
  private String getDefaultGroupSummaryName()
  {
    return String.format( "Summary-%d", Integer.valueOf( getIndex() + 1 ) );
  }
}
