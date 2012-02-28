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


import static nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.*;
import java.util.*;

import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.ISignalElementChangeListener.*;


/**
 * Manager for all channels and their grouping.
 * <p>
 * This class is thread-safe.
 * </p>
 */
public final class SignalElementManager implements IDataModelChangeListener
{
  // INNER TYPES

  /**
   * Defines a provider the heights of the various types of signal elements.
   */
  public static interface SignalElementHeightProvider
  {
    /**
     * @return a analog signal height, in pixels.
     */
    int getAnalogSignalHeight();

    /**
     * @return a digital channel height, in pixels.
     */
    int getDigitalSignalHeight();

    /**
     * @return a group summary height, in pixels.
     */
    int getGroupSummaryHeight();

    /**
     * @return a signal group height, in pixels.
     */
    int getSignalGroupHeight();
  }

  /**
   * Defines a measurer for signal elements.
   */
  public static interface SignalElementMeasurer
  {
    public static final SignalElementMeasurer STRICT_MEASURER = new StrictChannelElementMeasurer();
    public static final SignalElementMeasurer LOOSE_MEASURER = new LooseChannelElementMeasurer();

    /**
     * @param aYpos
     * @param aHeight
     * @param aMinY
     * @param aMaxY
     * @return
     */
    boolean signalElementFits( int aYpos, int aHeight, int aMinY, int aMaxY );
  }

  /**
   * Provides a loose channel element measurer, which selects channel elements
   * that completely fit within the boundaries, and also channel elements that
   * partly fit within the boundaries.
   */
  private static class LooseChannelElementMeasurer implements SignalElementMeasurer
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
  private static class StrictChannelElementMeasurer implements SignalElementMeasurer
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

  /** the total set of channel groups. */
  private final List<ElementGroup> groups;
  /** the total set of channels. */
  private final List<SignalElement> elements;

  private final EventListenerList eventListeners;
  private final Object lock = new Object();

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElementManager} instance.
   */
  public SignalElementManager()
  {
    this.groups = new ArrayList<ElementGroup>();
    this.elements = new ArrayList<SignalElement>();
    this.eventListeners = new EventListenerList();
  }

  // METHODS

  /**
   * Adds a channel change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addChannelChangeListener( final ISignalElementChangeListener aListener )
  {
    this.eventListeners.add( ISignalElementChangeListener.class, aListener );
  }

  /**
   * Returns the absolute height of the screen.
   * 
   * @param aHeightProvider
   *          the provider for the various element's heights, cannot be
   *          <code>null</code>.
   * @return a screen height, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int calculateScreenHeight( final SignalElementHeightProvider aHeightProvider )
  {
    int height = 0;
    for ( ElementGroup cg : getGroups() )
    {
      if ( !cg.isVisible() )
      {
        continue;
      }

      height += aHeightProvider.getSignalGroupHeight();

      if ( cg.isShowDigitalSignals() )
      {
        height += aHeightProvider.getDigitalSignalHeight() * cg.getElementCount();
      }
      // Always keep these heights into account...
      if ( cg.isShowGroupSummary() )
      {
        height += aHeightProvider.getGroupSummaryHeight();
      }
      if ( cg.isShowAnalogSignal() )
      {
        height += aHeightProvider.getAnalogSignalHeight();
      }
    }

    return height;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final DataSet aCapturedData )
  {
    // Make sure only a single thread at a time modifies us...
    synchronized ( this.lock )
    {
      final Channel[] newChannelList = aCapturedData.getChannels();

      // Reset channel groups so they align with the given data model...
      final int groupCount = Math.max( 1, ( int )Math.ceil( newChannelList.length / ( double )Ols.CHANNELS_PER_BLOCK ) );
      final int channelsPerGroup = ( int )Math.ceil( newChannelList.length / ( double )groupCount );

      // TODO property change listeners on SignalElements!

      this.elements.clear();
      this.groups.clear();

      for ( int g = 0; g < groupCount; g++ )
      {
        final ElementGroup group = addGroup( "Group " + ( g + 1 ) );

        // We start with a signal group element... XXX?
        addSignalElement( group, createSignalGroupElement( group ) );

        for ( int c = 0; c < channelsPerGroup; c++ )
        {
          final int channelIdx = ( g * channelsPerGroup ) + c;
          addSignalElement( group, createDigitalSignalElement( newChannelList[channelIdx], group ) );
        }

        addSignalElement( group, createGroupSummaryElement( group ) );
        addSignalElement( group, createAnalogScopeElement( group ) );
      }
    }

    fireGroupStructureChangeEvent( getAssignedElements() );
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
   * Returns all current channel groups.
   * 
   * @return an array of channel groups, never <code>null</code>.
   */
  public Collection<ElementGroup> getGroups()
  {
    Collection<ElementGroup> result;
    synchronized ( this.groups )
    {
      result = Collections.unmodifiableCollection( this.groups );
    }
    return result;
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
  public SignalElement[] getSignalElements( final int aY, final int aHeight, final SignalElementMeasurer aMeasurer,
      final SignalElementHeightProvider aHeightProvider )
  {
    final List<SignalElement> result = new ArrayList<SignalElement>();

    final int digitalSignalHeight = aHeightProvider.getDigitalSignalHeight();
    final int groupSummaryHeight = aHeightProvider.getGroupSummaryHeight();
    final int analogSignalHeight = aHeightProvider.getAnalogSignalHeight();
    final int signalGroupHeight = aHeightProvider.getSignalGroupHeight();

    final int yMin = aY;
    final int yMax = aHeight + aY;

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

      for ( SignalElement element : group.getElements() )
      {
        if ( element.isSignalGroup() )
        {
          if ( aMeasurer.signalElementFits( yPos, signalGroupHeight, yMin, yMax ) )
          {
            element.setYposition( yPos );
            element.setHeight( signalGroupHeight );
            result.add( element );
          }
          yPos += signalGroupHeight;
        }
        else if ( element.isDigitalSignal() && group.isShowDigitalSignals() )
        {
          // Does this individual channel fit?
          if ( aMeasurer.signalElementFits( yPos, digitalSignalHeight, yMin, yMax ) )
          {
            element.setYposition( yPos );
            element.setHeight( digitalSignalHeight );
            result.add( element );
          }
          yPos += digitalSignalHeight;
        }
        else if ( element.isGroupSummary() && group.isShowGroupSummary() )
        {
          if ( aMeasurer.signalElementFits( yPos, groupSummaryHeight, yMin, yMax ) )
          {
            element.setYposition( yPos );
            element.setHeight( groupSummaryHeight );
            result.add( element );
          }
          yPos += groupSummaryHeight;
        }
        else if ( element.isAnalogSignal() && group.isShowAnalogSignal() )
        {
          if ( aMeasurer.signalElementFits( yPos, analogSignalHeight, yMin, yMax ) )
          {
            element.setYposition( yPos );
            element.setHeight( analogSignalHeight );
            result.add( element );
          }
          yPos += analogSignalHeight;
        }
      }
    }

    return result.toArray( new SignalElement[result.size()] );
  }

  /**
   * Moves a given signal element from its current position to a new group with
   * a new index.
   * 
   * @param aMovedElement
   *          the signal element to move, cannot be <code>null</code>;
   * @param aNewGroup
   *          the new element group to move the signal element to, can be equal
   *          to the current group of the moved signal element but never
   *          <code>null</code>;
   * @param aNewIndex
   *          the new index of the moved signal element.
   */
  public void moveElement( final SignalElement aMovedElement, final ElementGroup aNewGroup, final int aNewIndex )
  {
    if ( aMovedElement == null )
    {
      throw new IllegalArgumentException( "Moved signal element cannot be null!" );
    }
    if ( aNewGroup == null )
    {
      throw new IllegalArgumentException( "New group cannot be null!" );
    }
    if ( ( aNewIndex < 0 ) || ( aNewIndex > Ols.MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid new index: " + aNewIndex + "!" );
    }

    final ElementGroup oldCG = aMovedElement.getGroup();
    final int oldIndex = aMovedElement.getVirtualIndex();

    // Perform the actual move itself...
    aNewGroup.moveChannel( aMovedElement, aNewIndex );

    // Fire an event to all interested listeners...
    fireChannelMoveEvent( new ElementMoveEvent( aMovedElement, oldCG, oldIndex ) );
  }

  /**
   * Removes a channel change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeChannelChangeListener( final ISignalElementChangeListener aListener )
  {
    this.eventListeners.remove( ISignalElementChangeListener.class, aListener );
  }

  /**
   * Fires a {@link ElementMoveEvent} to all interested listeners.
   * 
   * @param aEvent
   *          the event to fire,cannot be <code>null</code>.
   */
  final void fireChannelMoveEvent( final ElementMoveEvent aEvent )
  {
    final ISignalElementChangeListener[] listeners = this.eventListeners
        .getListeners( ISignalElementChangeListener.class );
    for ( ISignalElementChangeListener listener : listeners )
    {
      listener.signalElementMoved( aEvent );
    }
  }

  /**
   * Fires a "channelgroup structure changed"-event to all interested listeners.
   * 
   * @param aSignalElements
   *          the event to fire,cannot be <code>null</code>.
   */
  final void fireGroupStructureChangeEvent( final Collection<SignalElement> aSignalElements )
  {
    final ISignalElementChangeListener[] listeners = this.eventListeners
        .getListeners( ISignalElementChangeListener.class );
    for ( ISignalElementChangeListener listener : listeners )
    {
      listener.groupStructureChanged( aSignalElements );
    }
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
   * Removes the group with the given name.
   * 
   * @param aName
   *          the name of the group to remove, cannot be <code>null</code> or
   *          empty.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  protected void removeGroup( final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    synchronized ( this.groups )
    {
      ElementGroup group = getGroupByName( aName );
      if ( group != null )
      {
        this.groups.remove( group );
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
  private ElementGroup getGroupByName( final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    Iterator<ElementGroup> channelGroupIter;
    synchronized ( this.lock )
    {
      channelGroupIter = this.groups.iterator();
    }

    while ( channelGroupIter.hasNext() )
    {
      ElementGroup cg = channelGroupIter.next();
      if ( aName.equals( cg.getName() ) )
      {
        return cg;
      }
    }

    return null;
  }
}
