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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.dnd;


import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.signaldisplay.*;


/**
 * Denotes a drag and drop target controller that allows handler to be plugged
 * in to add/remove custom drag and drop behavior.
 */
public final class DragAndDropTargetController extends DropTargetAdapter
{
  // INNER TYPES

  /**
   * Denotes a handler for DnD targets.
   */
  public static interface DragAndDropHandler
  {
    // METHODS

    /**
     * Implement this method to conditionally accept the given drop event.
     * 
     * @param aEvent
     *          the target drop event to accept, cannot be <code>null</code>.
     * @return <code>true</code> if the drop was accepted, <code>false</code> if
     *         the drop was declined.
     */
    boolean acceptDrop( SignalDiagramController aController, DropTargetDropEvent aEvent );

    /**
     * Returns the action-mask this drop handler supports.
     * 
     * @return a action mask, > 0.
     */
    int getDropAction();

    /**
     * Returns the data flavor supported by this handler.
     * 
     * @return a supported data flavor, never <code>null</code>.
     */
    DataFlavor getFlavor();
  }

  // VARIABLES

  private final SignalDiagramController ctlr;
  private final List<DragAndDropHandler> handlers;

  // CONSTRUCTORS

  /**
   * Creates a new DragAndDropTargetController instance.
   * 
   * @param aController
   *          the diagram controller to use in the handlers.
   */
  public DragAndDropTargetController( final SignalDiagramController aController )
  {
    this.ctlr = aController;

    this.handlers = new CopyOnWriteArrayList<DragAndDropHandler>();
  }

  // METHODS

  /**
   * Adds the given handler to this controller.
   * <p>
   * If this method is given twice the same handler, it will be added twice!
   * </p>
   * 
   * @param aHandler
   *          the handler to add, cannot be <code>null</code>.
   */
  public void addHandler( final DragAndDropHandler aHandler )
  {
    this.handlers.add( aHandler );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void drop( final DropTargetDropEvent aEvent )
  {
    final DataFlavor[] flavors = aEvent.getCurrentDataFlavors();

    boolean result = false;
    if ( ( flavors != null ) && ( flavors.length > 0 ) )
    {
      for ( int i = flavors.length - 1; !result && ( i >= 0 ); i-- )
      {
        final DragAndDropHandler handler = getHandler( flavors[i] );
        if ( ( handler != null ) && ( ( aEvent.getSourceActions() & handler.getDropAction() ) != 0 ) )
        {
          result = handler.acceptDrop( this.ctlr, aEvent );

          if ( result )
          {
            aEvent.acceptDrop( handler.getDropAction() );
          }
        }
      }
    }

    if ( !result )
    {
      // None of the handlers accepted the drop; so we reject it totally...
      aEvent.rejectDrop();
    }

    // Acknowledge that we've successfully dropped the item...
    aEvent.dropComplete( result );
  }

  /**
   * Removes a given handler from this controller.
   * 
   * @param aHandler
   *          the handler to remove, cannot be <code>null</code>.
   */
  public void removeHandler( final DragAndDropHandler aHandler )
  {
    this.handlers.remove( aHandler );
  }

  /**
   * Searches for a handler that accepts the given data flavor.
   * 
   * @param aFlavor
   *          the flavor to search for, cannot be <code>null</code>.
   * @return a handler that accepts the given data flavor, can be
   *         <code>null</code> in case no handler supports the given flavor.
   */
  private DragAndDropHandler getHandler( final DataFlavor aFlavor )
  {
    final List<DragAndDropHandler> handlersCopy = new ArrayList<DragAndDropHandler>( this.handlers );
    for ( DragAndDropHandler handler : handlersCopy )
    {
      if ( aFlavor.equals( handler.getFlavor() ) )
      {
        return handler;
      }
    }
    return null;
  }
}
