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
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.Point;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JPopupMenu;

import nl.lxtreme.ols.api.Ols;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.IActionManager;
import nl.lxtreme.ols.client.signaldisplay.MeasurementInfo;
import nl.lxtreme.ols.client.signaldisplay.SignalDiagramController;
import nl.lxtreme.ols.client.signaldisplay.action.DeleteCursorAction;
import nl.lxtreme.ols.client.signaldisplay.action.EditCursorPropertiesAction;
import nl.lxtreme.ols.client.signaldisplay.action.EditSignalElementPropertiesAction;
import nl.lxtreme.ols.client.signaldisplay.action.RemoveChannelAnnotations;
import nl.lxtreme.ols.client.signaldisplay.action.SetCursorAction;
import nl.lxtreme.ols.client.signaldisplay.action.SetSignalElementVisibilityAction;
import nl.lxtreme.ols.client.signaldisplay.action.SetSignalGroupVisibilityAction;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.SignalElementType;


/**
 * Provides a helper class for handling popups in the various views.
 */
final class PopupFactory
{
  // VARIABLES

  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PopupFactory} instance.
   */
  PopupFactory( final SignalDiagramController aController )
  {
    this.controller = aController;
  }

  // METHODS

  /**
   * Creates the context-sensitive popup menu for channel labels.
   * 
   * @param aRelativePoint
   *          the current mouse location to show the popup menu, cannot be
   *          <code>null</code>.
   * @param aLocationOnScreen
   *          the location on screen, cannot be <code>null</code>.
   * @return a popup menu, can be <code>null</code> if the given mouse point is
   *         not above a channel.
   */
  public JPopupMenu createChannelLabelPopup( final Point aRelativePoint, final Point aLocationOnScreen )
  {
    return createPopup( aRelativePoint, aLocationOnScreen, null, false /* aShowCursors */);
  }

  /**
   * Creates the context-sensitive popup menu for cursors.
   * 
   * @param aPoint
   *          the current mouse location to show the cursor, cannot be
   *          <code>null</code>;
   * @param aLocationOnScreen
   *          the location on screen, cannot be <code>null</code>.
   * @return a popup menu, never <code>null</code>.
   */
  public JPopupMenu createCursorPopup( final Cursor aHoveredCursor, final Point aPoint, final Point aLocationOnScreen )
  {
    return createPopup( aPoint, aLocationOnScreen, aHoveredCursor, true /* aShowCursors */);
  }

  /**
   * @param aMenu
   *          the popup menu to add the cursor actions to;
   * @param aHoveredCursor
   *          the current cursor, can be <code>null</code> if not hovering above
   *          a cursor;
   * @param aPoint
   *          the current mouse location, cannot be <code>null</code>.
   */
  private void addCursorActions( final JPopupMenu aMenu, final Cursor aHoveredCursor, final Point aPoint )
  {
    Cursor cursor = aHoveredCursor;
    if ( cursor != null )
    {
      // Hovering above existing cursor, show remove menu...
      aMenu.add( new EditCursorPropertiesAction( this.controller, cursor ) );

      aMenu.addSeparator();

      aMenu.add( new DeleteCursorAction( this.controller, cursor ) );
    }
    else
    {
      // Not hovering above existing cursor, show add menu...
      for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
      {
        final SetCursorAction action = new SetCursorAction( this.controller, i );
        aMenu.add( new JCheckBoxMenuItem( action ) );
      }

      aMenu.putClientProperty( SetCursorAction.KEY, getCursorDropPoint( aPoint ) );
    }
  }

  /**
   * @param aPoint
   * @param aLocationOnScreen
   * @param aHoveredCursor
   * @param aShowCursorSection
   * @return
   */
  private JPopupMenu createPopup( final Point aPoint, final Point aLocationOnScreen, final Cursor aHoveredCursor,
      final boolean aShowCursorSection )
  {
    IActionManager actionMgr = this.controller.getActionManager();

    final JPopupMenu contextMenu = new JPopupMenu();

    // when an action is selected, we *no* longer know where the point was
    // where the user clicked. Therefore, we need to store it separately
    // for later use...
    contextMenu.putClientProperty( "mouseLocation", aPoint );

    boolean elementAdded = false;

    if ( aHoveredCursor == null )
    {
      final IUIElement element = findUIElement( aPoint );
      if ( element == null )
      {
        // Not above a cursor, nor above a signal element...
        return null;
      }

      contextMenu.add( new ShowManagerViewAction( this.controller ) );

      contextMenu.addSeparator();

      if ( element instanceof ElementGroup )
      {
        ElementGroup group = ( ElementGroup )element;

        contextMenu
            .add( new SetSignalGroupVisibilityAction( this.controller, group, SignalElementType.DIGITAL_SIGNAL ) );
        contextMenu.add( new SetSignalGroupVisibilityAction( this.controller, group, SignalElementType.GROUP_SUMMARY ) );
        contextMenu.add( new SetSignalGroupVisibilityAction( this.controller, group, SignalElementType.ANALOG_SIGNAL ) );
      }
      else
      {
        SignalElement signalElement = ( SignalElement )element;

        contextMenu.add( new EditSignalElementPropertiesAction( this.controller, element, aLocationOnScreen ) );

        contextMenu.addSeparator();

        contextMenu.add( new SetSignalElementVisibilityAction( this.controller, signalElement ) );

        if ( signalElement.isDigitalSignal() )
        {
          contextMenu.add( new RemoveChannelAnnotations( this.controller, signalElement ) );
        }
      }

      elementAdded = true;
    }

    if ( aShowCursorSection )
    {
      if ( elementAdded )
      {
        contextMenu.addSeparator();
      }

      addCursorActions( contextMenu, aHoveredCursor, aPoint );
    }

    contextMenu.addSeparator();

    contextMenu.add( actionMgr.getAction( DeleteAllCursorsAction.ID ) );

    return contextMenu;
  }

  /**
   * Finds the channel under the given point.
   * 
   * @param aPoint
   *          the coordinate of the potential channel, cannot be
   *          <code>null</code>.
   * @return the channel index, or -1 if not found.
   */
  private IUIElement findUIElement( final Point aPoint )
  {
    return getModel().findUIElement( aPoint );
  }

  /**
   * Calculates the drop point for the cursor under the given coordinate.
   * 
   * @param aCoordinate
   *          the coordinate to return the channel drop point for, cannot be
   *          <code>null</code>.
   * @return a drop point, never <code>null</code>.
   */
  private Point getCursorDropPoint( final Point aCoordinate )
  {
    Point dropPoint = new Point( aCoordinate );

    if ( getModel().isSnapCursorMode() )
    {
      final MeasurementInfo signalHover = getModel().getSignalHover( aCoordinate );
      if ( ( signalHover != null ) && !signalHover.isEmpty() )
      {
        dropPoint.x = signalHover.getMidSamplePos().intValue();
      }
    }
    dropPoint.y = 0;

    return dropPoint;
  }

  /**
   * @return the signal diagram model, never <code>null</code>.
   */
  private SignalDiagramModel getModel()
  {
    return this.controller.getViewModel();
  }
}
