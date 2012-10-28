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
package nl.lxtreme.ols.util.swing.component;


import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Provides a "pop down" button, which shows a popdown menu if clicked.
 * <p>
 * Based on code found on:
 * <http://explodingpixels.wordpress.com/2008/11/10/prevent
 * -popup-menu-dismissal/>
 * </p>
 */
public class JPopdownButton extends JToggleButton
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JPopupMenu popupMenu = new JPopupMenu();

  private boolean shouldHandlePopupWillBecomeInvisible = true;

  // CONSTRUCTORS

  /**
   * Creates a new {@link JPopdownButton} instance.
   * 
   * @param aIcon
   *          the icon of this popdown button.
   */
  public JPopdownButton( final Icon aIcon )
  {
    this( aIcon, aIcon );
  }

  /**
   * Creates a new {@link JPopdownButton} instance.
   * 
   * @param aDefaultIcon
   *          the default icon of this popdown button;
   * @param aPressedAndSelectedIcon
   *          the pressed and selected icon of this popdown button.
   */
  public JPopdownButton( final Icon aDefaultIcon, final Icon aPressedAndSelectedIcon )
  {
    // setup the default button state.
    setIcon( aDefaultIcon );
    setPressedIcon( aPressedAndSelectedIcon );
    setSelectedIcon( aPressedAndSelectedIcon );
    setFocusable( false );
    putClientProperty( "JButton.buttonType", "textured" );

    // install a mouse listener on the button to hide and show the popup
    // menu as appropriate.
    addMouseListener( createButtonMouseListener() );

    // add a popup menu listener to update the button's selection state
    // when the menu is being dismissed.
    this.popupMenu.addPopupMenuListener( createPopupMenuListener() );

    // install a special client property on the button to prevent it from
    // closing of the popup when the down arrow is pressed.
    final JComboBox box = new JComboBox();
    final Object preventHide = box.getClientProperty( "doNotCancelPopup" );
    putClientProperty( "doNotCancelPopup", preventHide );
  }

  /**
   * Returns the popup menu.
   * 
   * @return the popup menu, never <code>null</code>.
   */
  public JPopupMenu getPopupMenu()
  {
    return this.popupMenu;
  }

  /**
   * @return
   */
  private MouseListener createButtonMouseListener()
  {
    return new MouseAdapter()
    {
      @Override
      public void mousePressed( final MouseEvent e )
      {
        // if the popup menu is currently showing, then hide it.
        // else if the popup menu is not showing, then show it.
        if ( JPopdownButton.this.popupMenu.isShowing() )
        {
          hidePopupMenu();
        }
        else
        {
          showPopupMenu();
        }
      }
    };
  }

  /**
   * @return
   */
  private PopupMenuListener createPopupMenuListener()
  {
    return new PopupMenuListener()
    {
      @Override
      public void popupMenuCanceled( final PopupMenuEvent e )
      {
        // the popup menu has been canceled externally (either by
        // pressing escape or clicking off of the popup menu). update
        // the button's state to reflect the menu dismissal.
        JPopdownButton.this.setSelected( false );
      }

      @Override
      public void popupMenuWillBecomeInvisible( final PopupMenuEvent e )
      {
        // handle this event if so indicated. the only time we don't handle
        // this event is when the button itself is pressed, the press action
        // toggles the button selected state for us. this case handles when
        // the button has been toggled, but the user clicks outside the
        // button in order to dismiss the menu.
        if ( JPopdownButton.this.shouldHandlePopupWillBecomeInvisible )
        {
          JPopdownButton.this.setSelected( false );
        }
      }

      @Override
      public void popupMenuWillBecomeVisible( final PopupMenuEvent e )
      {
        // no implementation.
      }
    };
  }

  /**
   * 
   */
  private void hidePopupMenu()
  {
    this.shouldHandlePopupWillBecomeInvisible = false;
    this.popupMenu.setVisible( false );
    this.shouldHandlePopupWillBecomeInvisible = true;
  }

  /**
   * 
   */
  private void showPopupMenu()
  {
    // show the menu below the button, and slightly to the right.
    this.popupMenu.show( this, 5, getHeight() );
  }
}
