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
package nl.lxtreme.ols.client.signaldisplay.signalelement;


import java.util.*;


/**
 * Provides a listener for signal element(group) change events.
 */
public interface ISignalElementChangeListener extends EventListener
{
  // INNER TYPES

  /**
   * Provides the information of a channel movement.
   */
  public static final class ElementMoveEvent
  {
    // VARIABLES

    private final SignalElement element;
    private final ElementGroup oldGroup;
    private final int oldPosition;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ElementMoveEvent} instance.
     */
    public ElementMoveEvent( final SignalElement aElement, final ElementGroup aOldGroup, final int aOldPosition )
    {
      this.element = aElement;
      this.oldGroup = aOldGroup;
      this.oldPosition = aOldPosition;
    }

    // METHODS

    /**
     * Returns the new (after the move) channel group of the channel.
     * 
     * @return the new channel group, never <code>null</code>.
     */
    public ElementGroup getNewGroup()
    {
      return this.element.getGroup();
    }

    /**
     * Returns the new (after the move) channel position.
     * 
     * @return the new channel position, >= 0.
     */
    public int getNewPosition()
    {
      return this.element.getVirtualIndex();
    }

    /**
     * Returns the old (before the move) channel group of the channel.
     * 
     * @return the old channel group, never <code>null</code>.
     */
    public ElementGroup getOldGroup()
    {
      return this.oldGroup;
    }

    /**
     * Returns the old (before the move) channel position.
     * 
     * @return the old channel position, >= 0.
     */
    public int getOldPosition()
    {
      return this.oldPosition;
    }

    /**
     * Returns the channel that is moved.
     * 
     * @return the moved channel, never <code>null</code>.
     */
    public SignalElement getSignalElement()
    {
      return this.element;
    }

    /**
     * Returns whether or not the channel is moved between channel groups.
     * 
     * @return <code>true</code> if the channel moved between channel groups,
     *         <code>false</code> otherwise.
     */
    public boolean isGroupChange()
    {
      // We can safely check for reference!
      return getOldGroup() != getNewGroup();
    }
  }

  // METHODS

  /**
   * Called when the group structure is (re)defined.
   * 
   * @param aSignalList
   *          the new, immutable, list of assigned signal elements, never
   *          <code>null</code>.
   */
  void groupStructureChanged( Collection<SignalElement> aSignalList );

  /**
   * Called when a signal element is moved either inside a group or between
   * groups.
   * 
   * @param aEvent
   *          the event details, never <code>null</code>.
   */
  void signalElementMoved( ElementMoveEvent aEvent );

}
