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
package nl.lxtreme.ols.client.ui.signaldisplay.marker;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a UI-action to delete a single marker.
 */
public class DeleteMarkerAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Marker marker;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DeleteMarkerAction} instance.
   */
  public DeleteMarkerAction( final Marker aMarker )
  {
    if ( ( aMarker == null ) || !aMarker.isMoveable() )
    {
      throw new IllegalArgumentException( "Marker cannot be null or a trigger!" );
    }

    this.marker = aMarker;

    putValue( NAME, "Delete cursor " + ( aMarker.getIndex() + 1 ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createKeyMask( KeyEvent.VK_DELETE ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    this.marker.clear();
  }
}
