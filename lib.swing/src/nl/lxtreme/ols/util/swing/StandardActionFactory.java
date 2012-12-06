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


/**
 * Provides a Swing {@link Action}-factory for some commonly used actions.
 */
public final class StandardActionFactory
{
  // INNER TYPES

  /**
   * Denotes an action for an "Cancel" button.
   */
  public static final class CancelAction extends AbstractStatusAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CancelAction} instance.
     */
    public CancelAction()
    {
      super( DialogStatus.CANCEL );
    }
  }

  /**
   * Provides a generic close dialog action.
   */
  public static final class CloseAction extends AbstractStatusAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CloseAction} instance.
     */
    public CloseAction()
    {
      super( DialogStatus.CANCEL );

      putValue( NAME, "Close" );
      putValue( SHORT_DESCRIPTION, "Closes this dialog" );

      if ( isMacOS() )
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
     * Returns whether the current host's operating system is Mac OS X.
     * 
     * @return <code>true</code> if running on Mac OS X, <code>false</code>
     *         otherwise.
     */
    private boolean isMacOS()
    {
      final String osName = System.getProperty( "os.name" );
      return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
    }
  }

  /**
   * Denotes the result status of a dialog.
   */
  public static enum DialogStatus
  {
    OK, CANCEL;

    /**
     * @return <code>true</code> if this status equals to {@link #CANCEL},
     *         <code>false</code> otherwise.
     */
    public boolean isCancel()
    {
      return this == CANCEL;
    }

    /**
     * @return <code>true</code> if this status equals to {@link #OK},
     *         <code>false</code> otherwise.
     */
    public boolean isOk()
    {
      return this == OK;
    }
  }

  /**
   * Denotes an action for an "Ok"/"Apply" button.
   */
  public static final class OkAction extends AbstractStatusAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link OkAction} instance.
     */
    public OkAction()
    {
      super( DialogStatus.OK );
    }
  }

  /**
   * Denotes a dialog that can be validated for its settings.
   */
  public static interface ValidatableDialog
  {
    // METHODS

    /**
     * Validates this dialog and returns an error message stating what might be
     * wrong with this dialog.
     * 
     * @return a non-null, non-empty string with a reason about the possible
     *         problem of this dialog, or an empty string or <code>null</code>
     *         if the dialog is valid.
     */
    String validateDialog();
  }

  /**
   * Denotes a dialog that is aware of its status.
   */
  public static interface StatusAwareDialog
  {
    // METHODS

    /**
     * Returns the dialog status, if set.
     * 
     * @return the dialog status, can be <code>null</code> as long as it is not
     *         set.
     * @see #setDialogStatus(DialogStatus)
     */
    DialogStatus getDialogStatus();

    /**
     * Reports back what the status of the implementing dialog is.
     * 
     * @param aStatus
     *          the status of the dialog, can be <code>null</code>.
     */
    void setDialogStatus( DialogStatus aStatus );
  }

  /**
   * Denotes an action for an "Ok"/"Apply" button.
   */
  abstract static class AbstractStatusAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final DialogStatus status;

    /**
     * Creates a new {@link AbstractStatusAction} instance.
     */
    public AbstractStatusAction( final DialogStatus aStatus )
    {
      super( aStatus.name().substring( 0, 1 ).concat( aStatus.name().substring( 1 ).toLowerCase() ) );

      this.status = aStatus;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      if ( aEvent.getSource() instanceof Component )
      {
        final Component source = ( Component )aEvent.getSource();

        final Window window = findParent( source, Window.class );

        if ( window instanceof StatusAwareDialog )
        {
          ( ( StatusAwareDialog )window ).setDialogStatus( this.status );
        }

        boolean closeOk = true;
        if ( ( this.status == DialogStatus.OK ) && ( window instanceof ValidatableDialog ) )
        {
          String validationResult = ( ( ValidatableDialog )window ).validateDialog();

          closeOk = ( validationResult == null ) || "".equals( validationResult );
        }

        if ( closeOk && ( window != null ) )
        {
          window.dispose();
        }
      }
    }
  }

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
   * Creates a new cancel action instance.
   * 
   * @return a cancel action instance, never <code>null</code>.
   */
  public static final Action createCancelAction()
  {
    return new CancelAction();
  }

  /**
   * Creates a new button instance with a cancel action assigned to it.
   * 
   * @return a new cancel-button instance, never <code>null</code>.
   */
  public static final JButton createCancelButton()
  {
    final Action action = createCancelAction();
    return createButton( action );
  }

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
   * Creates a new OK action instance.
   * 
   * @return an OK action instance, never <code>null</code>.
   */
  public static final Action createOkAction()
  {
    return new OkAction();
  }

  /**
   * Creates a new button instance with an OK action assigned to it.
   * 
   * @return a new OK-button instance, never <code>null</code>.
   */
  public static final JButton createOkButton()
  {
    final Action action = createOkAction();
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
  private static <T> T findParent( final Component aComponent, final Class<T> aImplementingType )
  {
    T closeableParent;

    // Some magic in order to also be able to find the parent's of menu items
    // or popup menu items...
    if ( aComponent instanceof JMenuItem )
    {
      final Component parent = ( ( JMenuItem )aComponent ).getParent();
      closeableParent = findParent( parent, aImplementingType );
    }
    else if ( aComponent instanceof JPopupMenu )
    {
      final Component invoker = ( ( JPopupMenu )aComponent ).getInvoker();
      closeableParent = findParent( invoker, aImplementingType );
    }
    else
    {
      closeableParent = SwingComponentUtils.getAncestorOfClass( aImplementingType, aComponent );
    }

    return closeableParent;
  }
}
