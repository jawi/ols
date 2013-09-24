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


import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElementManager.SignalElementMeasurer;


class SignalElementModel
{
  // VARIABLES

  /** the total set of channel groups. */
  private final List<ElementGroup> groups;
  /** the total set of channels. */
  private final List<SignalElement> elements;

  private final Object lock = new Object();

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElementManager} instance.
   */
  public SignalElementModel()
  {
    this.groups = new ArrayList<ElementGroup>();
    this.elements = new ArrayList<SignalElement>();
  }

  /**
   * Creates a new {@link SignalElementManager} instance.
   */
  public SignalElementModel( SignalElementModel aElementModel )
  {
    this.groups = new ArrayList<ElementGroup>();
    this.elements = new ArrayList<SignalElement>( aElementModel.elements.size() );

    for ( ElementGroup oldGroup : aElementModel.groups )
    {
      ElementGroup newGroup = new ElementGroup( oldGroup );
      this.groups.add( newGroup );

      this.elements.addAll( newGroup.getElements() );
    }
  }

  // METHODS

  /**
   * Returns the absolute height of the screen.
   * 
   * @param aHeightProvider
   *          the provider for the various element's heights, cannot be
   *          <code>null</code>.
   * @return a screen height, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int calculateScreenHeight()
  {
    int height = 0;

    final int spacing = UIManager.getInt( UIManagerKeys.SIGNAL_ELEMENT_SPACING );

    for ( ElementGroup cg : getGroups() )
    {
      if ( !cg.isVisible() )
      {
        continue;
      }

      height += cg.getHeight() + spacing;

      for ( SignalElement element : cg.getElements() )
      {
        if ( element.isDigitalSignal() && cg.isShowDigitalSignals() )
        {
          height += element.getHeight() + spacing;
        }
        else if ( element.isGroupSummary() && cg.isShowGroupSummary() )
        {
          height += element.getHeight() + spacing;
        }
        else if ( element.isAnalogSignal() && cg.isShowAnalogSignal() )
        {
          height += element.getHeight() + spacing;
        }
      }
    }

    return height;
  }

  /**
   * Returns all signal elements.
   * 
   * @return a collection of all signal elements, never <code>null</code>.
   */
  public Collection<SignalElement> getAllElements()
  {
    Collection<SignalElement> result = new ArrayList<SignalElement>();

    synchronized ( this.lock )
    {
      if ( this.elements != null )
      {
        result.addAll( this.elements );
      }
    }

    return result;
  }

  /**
   * Returns a sorted set of all assigned (not available) signal elements.
   * 
   * @return a sorted set of all assigned signal elements, never
   *         <code>null</code>.
   */
  public SortedSet<SignalElement> getAssignedElements()
  {
    SortedSet<SignalElement> channelIndexes = new TreeSet<SignalElement>();

    synchronized ( this.lock )
    {
      for ( ElementGroup cg : this.groups )
      {
        channelIndexes.addAll( cg.getElements() );
      }
    }

    return channelIndexes;
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
  public SignalElement getDigitalSignalByChannelIndex( final int aChannelIndex )
  {
    SignalElement result = null;
    synchronized ( this.lock )
    {
      for ( ElementGroup cg : this.groups )
      {
        result = cg.getDigitalSignalByChannelIndex( aChannelIndex );
        if ( result != null )
        {
          break;
        }
      }
    }
    return result;
  }

  /**
   * Returns the channel group with a given name.
   * 
   * @param aName
   *          the name of the channel group to return, cannot be
   *          <code>null</code> or empty.
   * @return the channel group with the given name, or <code>null</code> if no
   *         such channel group exists.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  public ElementGroup getGroupByName( String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    List<ElementGroup> groups = getGroups();
    for ( ElementGroup group : groups )
    {
      if ( aName.equals( group.getName() ) )
      {
        return group;
      }
    }
    return null;
  }

  /**
   * Returns all current channel groups.
   * 
   * @return an array of channel groups, never <code>null</code>.
   */
  public List<ElementGroup> getGroups()
  {
    List<ElementGroup> result;
    synchronized ( this.groups )
    {
      result = new ArrayList<ElementGroup>( this.groups );
    }
    return Collections.unmodifiableList( result );
  }

  /**
   * Returns all signal elements the given range of all visible channel groups.
   * 
   * @param aY
   *          the screen Y-coordinate;
   * @param aHeight
   *          the screen height;
   * @param aMeasurer
   *          the measurer to use to determine whether or not a signal element
   *          fits in the given dimensions.
   * @return an array of channels, never <code>null</code>.
   */
  public IUIElement[] getUIElements( final int aY, final int aHeight, final SignalElementMeasurer aMeasurer )
  {
    final List<IUIElement> result = new ArrayList<IUIElement>();

    final int yMin = aY;
    final int yMax = aHeight + aY;

    final int spacing = UIManager.getInt( UIManagerKeys.SIGNAL_ELEMENT_SPACING );
    final int halfSpacing = spacing / 2;

    int yPos = 0;
    for ( ElementGroup group : getGroups() )
    {
      if ( !group.isVisible() )
      {
        continue;
      }
      if ( yPos > yMax )
      {
        // Optimization: no need to continue after the requested end position...
        break;
      }

      int height = group.getHeight();
      if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
      {
        group.setYposition( yPos );
        result.add( group );
      }
      yPos += height + spacing;

      for ( SignalElement element : group.getElements() )
      {
        if ( element.isDigitalSignal() && group.isShowDigitalSignals() )
        {
          // Does this individual channel fit?
          height = element.getHeight();
          if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
          {
            element.setYposition( yPos );
            result.add( element );
          }
          yPos += height + spacing;
        }
        else if ( element.isGroupSummary() && group.isShowGroupSummary() )
        {
          height = element.getHeight();
          if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
          {
            element.setYposition( yPos );
            result.add( element );
          }
          yPos += height + spacing;
        }
        else if ( element.isAnalogSignal() && group.isShowAnalogSignal() )
        {
          height = element.getHeight();
          if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
          {
            element.setYposition( yPos );
            result.add( element );
          }
          yPos += height + spacing;
        }
      }
    }

    return result.toArray( new IUIElement[result.size()] );
  }

  /**
   * Adds a new group to this manager.
   * 
   * @param aName
   *          the name of the new group, cannot be <code>null</code> or empty.
   * @return the newly added group, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty;
   * @throws IllegalStateException
   *           in case no signal elements are available for the new group.
   */
  protected ElementGroup addGroup( final String aName )
  {
    ElementGroup result;
    synchronized ( this.lock )
    {
      result = new ElementGroup( this.groups.size(), aName );

      this.groups.add( result );
    }

    return result;
  }

  /**
   * Adds a given signal element to the given group.
   * <p>
   * If the given group already contains the given signal element, then this
   * method is effectively a no-op.
   * </p>
   * 
   * @param aGroup
   *          the group to add the signal element to, cannot be
   *          <code>null</code>;
   * @param aSignalElement
   *          the signal element to add to the group, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case one of the given parameters was <code>null</code>.
   */
  protected void addSignalElement( final ElementGroup aGroup, final SignalElement aSignalElement )
  {
    if ( aGroup == null )
    {
      throw new IllegalArgumentException( "Group cannot be null!" );
    }
    if ( aSignalElement == null )
    {
      throw new IllegalArgumentException( "Signal element cannot be null!" );
    }

    if ( aGroup.hasElement( aSignalElement ) )
    {
      // Nothing to do; we're done...
      return;
    }

    synchronized ( this.lock )
    {
      if ( !this.elements.contains( aSignalElement ) )
      {
        this.elements.add( aSignalElement );
      }

      // Keep a reference to the former channel group...
      final ElementGroup oldGroup = aSignalElement.getGroup();
      // This will automatically remove the given channel from its former
      // channel group...
      aGroup.addElement( aSignalElement );
      // When there are no more channels left in this channel group, remove
      // it...
      if ( ( oldGroup != null ) && !oldGroup.hasElements() )
      {
        this.groups.remove( oldGroup );
      }
    }
  }

  /**
   * Returns a sorted set of all unassigned (= available) signal elements.
   * 
   * @return a sorted set of unassigned signal elements, never <code>null</code>
   *         , but can be empty.
   */
  protected List<SignalElement> getUnassignedElements()
  {
    List<SignalElement> channels = new ArrayList<SignalElement>( getAllElements() );

    for ( ElementGroup cg : getGroups() )
    {
      channels.removeAll( cg.getElements() );
    }

    return channels;
  }

  /**
   * Removes a given group.
   * 
   * @param aGroup
   *          the group to remove, cannot be <code>null</code> or empty.
   * @throws IllegalArgumentException
   *           in case the given group was <code>null</code>.
   */
  protected void removeGroup( final ElementGroup aGroup )
  {
    if ( aGroup == null )
    {
      throw new IllegalArgumentException( "Group cannot be null!" );
    }

    synchronized ( this.groups )
    {
      if ( this.groups.remove( aGroup ) )
      {
        for ( SignalElement element : aGroup.getElements() )
        {
          aGroup.removeElement( element );
        }
      }
    }
  }

  /**
   * Removes a given signal element from a given group.
   * 
   * @param aGroup
   *          the group to remove the signal element from, cannot be
   *          <code>null</code>;
   * @param aSignalElement
   *          the signal element to remove, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case one of the given parameters was <code>null</code>.
   */
  protected void removeSignalElement( final ElementGroup aGroup, final SignalElement aSignalElement )
  {
    if ( aGroup == null )
    {
      throw new IllegalArgumentException( "Group cannot be null!" );
    }
    if ( aSignalElement == null )
    {
      throw new IllegalArgumentException( "Signal element cannot be null!" );
    }

    aGroup.removeElement( aSignalElement );
  }
}
