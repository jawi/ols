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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an action to give a certain window the current focus.
 */
public class FocusWindowAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID_PREFIX = "FocusWindow.";

  // CONSTRUCTORS

  /**
   * Creates a new FocusWindowAction instance.
   * 
   * @param aWindow
   *          the window to give focus, cannot be <code>null</code>.
   */
  public FocusWindowAction( final Window aWindow )
  {
    super( ID_PREFIX + getTitle( aWindow ) );
    putValue( NAME, getTitle( aWindow ) );
  }

  // METHODS

  /**
   * Tries to determine the title of the given window.
   * 
   * @param aWindow
   *          the window to determine the title for, cannot be <code>null</code>
   *          .
   * @return a title, can be <code>null</code>.
   */
  static final String getTitle( final Window aWindow )
  {
    if ( aWindow instanceof Frame )
    {
      return ( ( Frame )aWindow ).getTitle();
    }
    else if ( aWindow instanceof Dialog )
    {
      return ( ( Dialog )aWindow ).getTitle();
    }
    return aWindow.getName();
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window window = findWindow( getValue( NAME ) );

    if ( ( window != null ) && !SwingComponentUtils.isActivelyShown( window ) )
    {
      if ( window instanceof Frame )
      {
        final Frame frame = ( Frame )window;
        int state = frame.getExtendedState();
        if ( ( state & Frame.ICONIFIED ) != 0 )
        {
          state &= ~Frame.ICONIFIED;
          frame.setExtendedState( state );
        }
      }
      window.toFront();
      window.requestFocus();
    }
  }

  /**
   * Finds the window with the given name/title.
   * 
   * @param aName
   *          the name/title of the window to find.
   * @return the window matching the given name, or <code>null</code> if no such
   *         window could be found.
   */
  private Window findWindow( final Object aName )
  {
    final String name = String.valueOf( aName );
    for ( Window window : Window.getWindows() )
    {
      if ( name.equals( getTitle( window ) ) )
      {
        return window;
      }
    }
    return null;
  }

}
