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
package nl.lxtreme.ols.util.swing;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * Provides a Swing {@link Action}-factory for some commonly used actions.
 */
public final class StandardActionFactory
{
  // INNER TYPES

  /**
   * Provides a generic close dialog action.
   */
  public static final class CloseAction extends AbstractAction
  {
    // INNER TYPES

    /**
     * Denotes a window/dialog/frame that can be closed through a
     * {@link CloseAction}.
     */
    public static interface Closeable
    {
      /**
       * Closes this dialog, effectively setting its visibilty to
       * <code>false</code> and disposes it.
       */
      void close();
    }

    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CloseAction} instance.
     */
    public CloseAction()
    {
      super( "Close" );
      putValue( SHORT_DESCRIPTION, "Closes this dialog" );

      if ( HostUtils.getHostInfo().isMacOS() )
      {
        // On Mac OS, the default Window-close accelerator is CMD + W
        putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_W ) );
      }
      else
      {
        // On Windows/Linux platforms it is ESCape...
        putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE, 0 ) );
      }
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      if ( aEvent.getSource() instanceof Component )
      {
        final Component source = ( Component )aEvent.getSource();

        final Closeable closeableParent = findCloseableParent( source );
        if ( closeableParent == null )
        {
          throw new RuntimeException( "Failed to find closeable parent?!" );
        }

        closeableParent.close();

        // Make sure the resources held by the window are released...
        if ( closeableParent instanceof Window )
        {
          ( ( Window )closeableParent ).dispose();
        }
      }
    }

    /**
     * Tries to find a parent container/component that implements the
     * {@link Closeable} interface.
     * 
     * @param aComponent
     *          the component to find the closeable parent for, may be
     *          <code>null</code>.
     * @return the closeable parent, or <code>null</code> if no such parent was
     *         found (or the given component was <code>null</code>).
     */
    private Closeable findCloseableParent( final Component aComponent )
    {
      Closeable closeableParent;
      // Some magic in order to also be able to find the parent's of menu items
      // or popup menu items...
      if ( aComponent instanceof JMenuItem )
      {
        final Component parent = ( ( JMenuItem )aComponent ).getParent();
        closeableParent = findCloseableParent( parent );
      }
      else if ( aComponent instanceof JPopupMenu )
      {
        final Component invoker = ( ( JPopupMenu )aComponent ).getInvoker();
        closeableParent = findCloseableParent( invoker );
      }
      else
      {
        closeableParent = SwingComponentUtils.getAncestorOfClass( Closeable.class, aComponent );
      }
      return closeableParent;
    }
  }

  // CONSTANTS

  /** Denotes the ID of a "close"/"cancel" action. */
  public static final String CLOSE_ACTION_ID = "CloseAction";

  // CONSTRUCTORS

  /**
   * Creates a new StandardActionFactory; never used.
   */
  private StandardActionFactory()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates a new close action instance.
   * <p>
   * The close action will have a default shortcut key of CTRL/CMD + W on Mac OS
   * platforms, and ESC on other platforms.
   * </p>
   * 
   * @return a close action instance, never <code>null</code>.
   */
  public static final Action createCloseAction()
  {
    return new CloseAction();
  }

  /**
   * Creates a new button instance with a close action assigned to it.
   * <p>
   * The close action shortcut will be set as default operation on the returned
   * button.
   * </p>
   * 
   * @return a new close-button instance, never <code>null</code>.
   */
  public static final JButton createCloseButton()
  {
    final Action action = createCloseAction();
    return createButton( action );
  }

  /**
   * Creates a new button with the given action and registers the (optional)
   * shortcut key as default action for the returned button.
   * 
   * @param aAction
   *          the action of the button to create, cannot be <code>null</code>.
   * @return a new JButton instance, never <code>null</code>.
   */
  private static JButton createButton( final Action aAction )
  {
    String actionName = ( String )aAction.getValue( Action.NAME );
    if ( actionName == null )
    {
      actionName = aAction.toString();
    }

    final JButton button = new JButton( aAction );
    SwingComponentUtils.registerKeystroke( button, aAction, actionName );
    return button;
  }
}
