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
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Minimizes the current focused window.
 */
public class MinimizeWindowAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new MinimizeWindowAction instance.
   */
  public MinimizeWindowAction()
  {
    super();

    putValue( NAME, "Minimize" );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_M ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window[] windows = Window.getWindows();
    for ( Window window : windows )
    {
      if ( window instanceof Frame )
      {
        iconifyWindow( ( Frame )window );
      }
    }
  }

  /**
   * Iconifies the given window/frame.
   * 
   * @param aWindow
   *          the window to iconify, if it is not iconified, it will do nothing.
   */
  void iconifyWindow( final Frame aWindow )
  {
    int state = aWindow.getExtendedState();
    if ( ( state & Frame.ICONIFIED ) == 0 )
    {
      state |= Frame.ICONIFIED;
      aWindow.setExtendedState( state );
    }
  }
}
