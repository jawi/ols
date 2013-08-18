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
import java.util.concurrent.atomic.*;

import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.ISignalElementChangeListener.ElementMoveEvent;


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
   * Defines a measurer for signal elements.
   */
  public static interface SignalElementMeasurer
  {
    public static final SignalElementMeasurer STRICT_MEASURER = new StrictChannelElementMeasurer();
    public static final SignalElementMeasurer LOOSE_MEASURER = new LooseChannelElementMeasurer();

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

  private final AtomicReference<SignalElementModel> modelRef;
  private final EventListenerList eventListeners;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElementManager} instance.
   */
  public SignalElementManager()
  {
    this.modelRef = new AtomicReference<SignalElementModel>( new SignalElementModel() );
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
  public int calculateScreenHeight()
  {
    return getSignalElementModel().calculateScreenHeight();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final DataSet aCapturedData )
  {
    SignalElementModel oldModel = getSignalElementModel();
    SignalElementModel newModel = new SignalElementModel();

    final Channel[] newChannelList = aCapturedData.getChannels();

    if ( oldModel == null || oldModel.getGroups().isEmpty() )
    {
      // Reset channel groups so they align with the given data model...
      final int groupCount = Math.max( 1, ( int )Math.ceil( newChannelList.length / ( double )Ols.CHANNELS_PER_BLOCK ) );
      final int channelsPerGroup = ( int )Math.ceil( newChannelList.length / ( double )groupCount );

      for ( int g = 0; g < groupCount; g++ )
      {
        final ElementGroup group = newModel.addGroup( "Group " + ( g + 1 ) );

        for ( int c = 0; c < channelsPerGroup; c++ )
        {
          final int channelIdx = ( g * channelsPerGroup ) + c;
          if ( newChannelList[channelIdx] == null )
          {
            continue;
          }
          newModel.addSignalElement( group, createDigitalSignalElement( newChannelList[channelIdx], group ) );
        }

        newModel.addSignalElement( group, createGroupSummaryElement( group ) );
        newModel.addSignalElement( group, createAnalogScopeElement( group ) );
      }
    }
    else
    {
      // Copy the structure of the existing model using the new channel data...
      Set<Integer> seenChannelIdxs = new HashSet<Integer>();
      ElementGroup newGroup = null;
      for ( ElementGroup oldGroup : oldModel.getGroups() )
      {
        newGroup = newModel.addGroup( oldGroup.getName() );

        for ( SignalElement oldElement : oldGroup.getElements() )
        {
          SignalElement element;
          if ( oldElement.isDigitalSignal() )
          {
            int channelIdx = oldElement.getChannel().getIndex();
            if ( channelIdx >= newChannelList.length )
            {
              // Not in the new data...
              continue;
            }

            Channel newChannel = newChannelList[channelIdx];
            seenChannelIdxs.add( Integer.valueOf( channelIdx ) );

            element = createDigitalSignalElement( newChannel, newGroup );
            element.setSignalAlignment( oldElement.getSignalAlignment() );
            element.setSignalHeight( oldElement.getSignalHeight() );
          }
          else if ( oldElement.isAnalogSignal() )
          {
            element = createAnalogScopeElement( newGroup );
          }
          else if ( oldElement.isGroupSummary() )
          {
            element = createGroupSummaryElement( newGroup );
          }
          else
          {
            throw new RuntimeException( "Unknown/unhandled signal element: " + oldElement );
          }

          element.setColor( oldElement.getColor() );
          element.setEnabled( oldElement.isEnabled() );
          element.setHeight( oldElement.getHeight() );
          element.setLabel( oldElement.getLabel() );

          newModel.addSignalElement( newGroup, element );
        }
      }

      if ( newGroup == null )
      {
        // Odd case, old model didn't have any groups?!
        newGroup = newModel.addGroup( "Group 1" );
      }

      // Handle all left-over channels...
      for ( Channel channel : newChannelList )
      {
        Integer channelIdx = Integer.valueOf( channel.getIndex() );
        if ( !seenChannelIdxs.contains( channelIdx ) )
        {
          newModel.addSignalElement( newGroup, createDigitalSignalElement( channel, newGroup ) );
        }
      }
    }

    setSignalElementModel( newModel );
  }

  /**
   * Returns all signal elements.
   * 
   * @return a collection of all signal elements, never <code>null</code>.
   */
  public Collection<SignalElement> getAllElements()
  {
    SignalElementModel model = getSignalElementModel();
    return model.getAllElements();
  }

  /**
   * Returns a sorted set of all assigned (not available) signal elements.
   * 
   * @return a sorted set of all assigned signal elements, never
   *         <code>null</code>.
   */
  public SortedSet<SignalElement> getAssignedElements()
  {
    SignalElementModel model = getSignalElementModel();
    return model.getAssignedElements();
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
    SignalElementModel model = getSignalElementModel();
    return model.getDigitalSignalByChannelIndex( aChannelIndex );
  }

  /**
   * Returns all current channel groups.
   * 
   * @return an array of channel groups, never <code>null</code>.
   */
  public Collection<ElementGroup> getGroups()
  {
    SignalElementModel model = getSignalElementModel();
    return model.getGroups();
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
    SignalElementModel model = getSignalElementModel();
    return model.getUIElements( aY, aHeight, aMeasurer );
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
   * @return a deep copy of the current signal element model, never
   *         <code>null</code>.
   */
  final SignalElementModel createSignalElementModelCopy()
  {
    SignalElementModel currentModel = getSignalElementModel();
    return new SignalElementModel( currentModel );
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
   * Returns the current signal element model.
   * 
   * @return the {@link SignalElementModel} instance, never <code>null</code>.
   */
  final SignalElementModel getSignalElementModel()
  {
    return this.modelRef.get();
  }

  /**
   * @param aModel
   */
  final void setSignalElementModel( SignalElementModel aModel )
  {
    SignalElementModel oldModel;
    do
    {
      oldModel = this.modelRef.get();
    }
    while ( !this.modelRef.compareAndSet( oldModel, aModel ) );

    fireGroupStructureChangeEvent( aModel.getAssignedElements() );
  }
}
