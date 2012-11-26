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
package nl.lxtreme.ols.client.signaldisplay.marker;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an action to set a marker to a certain location.
 */
public class SetMarkerAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String KEY = "SetMarkerAction";

  // VARIABLES

  private final Marker marker;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetMarkerAction} instance.
   * 
   * @param aMarker
   *          the marker to set, cannot be <code>null</code>.
   */
  public SetMarkerAction( final Marker aMarker )
  {
    if ( ( aMarker == null ) || !aMarker.isMoveable() )
    {
      throw new IllegalArgumentException( "Invalid marker, cannot be null or a trigger!" );
    }
    this.marker = aMarker;

    final Integer logicalIdx = Integer.valueOf( aMarker.getIndex() + 1 );
    putValue( NAME, String.format( "Set cursor %d", logicalIdx ) );

    int keyStroke = KeyEvent.VK_0 + ( ( aMarker.getIndex() + 1 ) % Ols.MAX_CURSORS );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createKeyMask( keyStroke ) );

    putValue( Action.SELECTED_KEY, Boolean.valueOf( this.marker.isDefined() ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JMenuItem menuitem = ( JMenuItem )aEvent.getSource();
    final long timestamp = getTimestampFromContextMenu( menuitem );

    this.marker.setTimestamp( timestamp );
  }

  /**
   * Returns the context menu location client property of the given menu item's
   * popup menu.
   * 
   * @param aMenuItem
   *          the menu item to return the client property of, cannot be
   *          <code>null</code>.
   * @return a location denoting the context menu's position, never
   *         <code>null</code>.
   */
  private long getTimestampFromContextMenu( final JMenuItem aMenuItem )
  {
    final JComponent container = ( JComponent )aMenuItem.getParent();

    Long location = ( Long )container.getClientProperty( KEY );
    if ( location == null )
    {
      // Make sure we return a defined timestamp...
      return Ols.NOT_AVAILABLE;
    }

    return location.longValue();
  }
}
