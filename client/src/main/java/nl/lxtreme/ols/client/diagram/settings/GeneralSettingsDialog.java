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
package nl.lxtreme.ols.client.diagram.settings;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.*;

import org.osgi.service.prefs.*;


/**
 * Stores diagram "mode" settings and provides a dialog for changing them.
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public class GeneralSettingsDialog extends JDialog implements Configurable, Closeable
{
  // INNER TYPES

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Insets GROUP_INSETS = new Insets( 6, 4, 6, 6 );
  private static final Insets LABEL_INSETS = new Insets( 6, 2, 6, 6 );
  private static final Insets COMP_INSETS = new Insets( 6, 4, 6, 2 );

  // VARIABLES

  private final MutableDiagramSettings settings;
  private boolean result;

  // CONSTRUCTORS

  /**
   * Constructs diagram settings component.
   * 
   * @param aParent
   * @param aSettings
   */
  public GeneralSettingsDialog( final Window aParent, final DiagramSettings aSettings )
  {
    super( aParent, "Preferences", ModalityType.DOCUMENT_MODAL );

    this.settings = new MutableDiagramSettings( aSettings );

    initDialog();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * Returns the (mutated) diagram settings.
   * 
   * @return the diagram settings, never <code>null</code>.
   */
  public final DiagramSettings getDiagramSettings()
  {
    return this.settings;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  public void readPreferences( final Preferences aProperties )
  {
    // NO-op
  }

  /**
   * Display the settings dialog. If the user clicks ok, all changes are
   * reflected in the properties of this object. Otherwise changes are
   * discarded.
   * 
   * @return <code>OK</code> when user accepted changes, <code>CANCEL</code>
   *         otherwise
   */
  public boolean showDialog()
  {
    setVisible( true );
    return ( this.result );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  public void writePreferences( final Preferences aProperties )
  {
    // NO-op
  }

  /**
   * @return
   */
  private JPanel createButtonsPane()
  {
    final JButton cancel = StandardActionFactory.createCloseButton();
    final JButton ok = new JButton( "Ok" );
    ok.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        GeneralSettingsDialog.this.result = true;
        close();
      }
    } );
    // Make both buttons the same size...
    ok.setPreferredSize( cancel.getPreferredSize() );

    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );

    buttonPane.add( Box.createHorizontalGlue() );
    buttonPane.add( ok );
    buttonPane.add( Box.createHorizontalStrut( 16 ) );
    buttonPane.add( cancel );
    return buttonPane;
  }

  /**
   * 
   */
  private void initDialog()
  {
    final JPanel contentPane = new JPanel( new BorderLayout() );
    contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
    setContentPane( contentPane );

    add( new JPanel(), BorderLayout.CENTER );

    final JPanel buttonPane = createButtonsPane();
    add( buttonPane, BorderLayout.PAGE_END );

    pack();
  }
}
