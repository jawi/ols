/**
 * 
 */
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import nl.lxtreme.ols.client.*;


/**
 * Shows some information about this client (version stuff, legal stuff, and
 * such).
 */
public class HelpAboutAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "HelpAbout";

  // CONSTRUCTORS

  /**
   * Creates a new HelpAboutAction instance.
   * 
   * @param aController
   *          the client controller to use, cannot be <code>null</code>.
   */
  public HelpAboutAction( final ClientController aController )
  {
    super( ID, aController, "About " + Host.SHORT_NAME, "" );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().showAboutBox();
  }
}
