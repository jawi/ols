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
   * @param aWindowTitle
   *          the title of the window to give focus, cannot be <code>null</code>
   *          or empty.
   */
  public FocusWindowAction( final String aWindowTitle )
  {
    super( ID_PREFIX.concat( aWindowTitle ) );
    putValue( NAME, aWindowTitle );
    putValue( SELECTED_KEY, hasFocus( aWindowTitle ) );
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
  public static final String getTitle( final Window aWindow )
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
   * Finds the window with the given name/title.
   * 
   * @param aName
   *          the name/title of the window to find.
   * @return the window matching the given name, or <code>null</code> if no such
   *         window could be found.
   */
  private static Window findWindow( final Object aName )
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

  /**
   * Finds the window with the given name and returns whether it has the current
   * focus.
   * 
   * @param aName
   * @return
   */
  private static Boolean hasFocus( final Object aName )
  {
    final Window window = findWindow( aName );
    if ( window != null )
    {
      return Boolean.valueOf( window.hasFocus() );
    }
    return Boolean.FALSE;
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final Window window = findWindow( getValue( NAME ) );

        if ( ( window != null ) && !SwingComponentUtils.isActivelyShown( window ) )
        {
          if ( window instanceof Frame )
          {
            deiconifyWindow( ( Frame )window );
          }
          window.toFront();
          window.requestFocus();
        }
      }
    } );
  }

  /**
   * Deiconifies the given window/frame.
   * 
   * @param aWindow
   *          the window to deiconify, if it is not iconified, it will do
   *          nothing.
   */
  void deiconifyWindow( final Frame aWindow )
  {
    int state = aWindow.getExtendedState();
    if ( ( state & Frame.ICONIFIED ) != 0 )
    {
      state &= ~Frame.ICONIFIED;
      aWindow.setExtendedState( state );
    }
  }

}
