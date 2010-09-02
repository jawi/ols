package nl.lxtreme.ols.client.action;


import java.awt.event.*;
import java.net.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.icons.*;


/**
 */
public class ScrollPaneContextAction extends BaseAction implements PopupMenuListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ScrollPaneContext";

  // VARIABLES

  private final JPopupMenu popup;
  private final ActionProvider actionProvider;

  // CONSTRUCTORS

  /**
   * Creates a new ScrollPaneContextAction instance.
   */
  public ScrollPaneContextAction( final ActionProvider aActionProvider )
  {
    super( ID, "", "" );

    final URL url = IconLocator.class.getResource( IconLocator.ICON_DIAGRAM_SETTINGS );
    putValue( Action.LARGE_ICON_KEY, new ImageIcon( url ) );

    this.actionProvider = aActionProvider;

    this.popup = new JPopupMenu();
    this.popup.addPopupMenuListener( this );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JButton source = ( JButton )aEvent.getSource();
    final int x = -1;
    final int y = source.getHeight();

    this.popup.show( source, x, y );
  }

  /**
   * @see javax.swing.event.PopupMenuListener#popupMenuCanceled(javax.swing.event.PopupMenuEvent)
   */
  @Override
  public void popupMenuCanceled( final PopupMenuEvent aEvent )
  {
    // NO-op
  }

  /**
   * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent)
   */
  @Override
  public void popupMenuWillBecomeInvisible( final PopupMenuEvent aEvent )
  {
    // NO-op
  }

  /**
   * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent)
   */
  @Override
  public void popupMenuWillBecomeVisible( final PopupMenuEvent aEvent )
  {
    final MenuElement[] menuitems = this.popup.getSubElements();
    if ( ( menuitems == null ) || ( menuitems.length == 0 ) )
    {
      // First time we're going to show this menu, dynamically build it...
      this.popup.add( new JMenuItem( this.actionProvider.getAction( ShowDiagramSettingsAction.ID ) ) );
      this.popup.add( new JMenuItem( this.actionProvider.getAction( ShowDiagramLabelsAction.ID ) ) );
    }
  }
}
