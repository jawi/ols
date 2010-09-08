/**
 * 
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
     * 
     */
    public CloseAction()
    {
      super( "Close" );
      putValue( SHORT_DESCRIPTION, "Closes this dialog" );

      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_W ) );
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

        final Closeable closeableParent = ( Closeable )SwingUtilities.getAncestorOfClass( Closeable.class, source );
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
   * The close action will have a default shortcut key of CTRL/CMD + W.
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
