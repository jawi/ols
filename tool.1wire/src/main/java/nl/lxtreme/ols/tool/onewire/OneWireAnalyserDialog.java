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
package nl.lxtreme.ols.tool.onewire;


import java.awt.*;

import javax.swing.*;

import org.osgi.service.prefs.*;

import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jawi
 */
public class OneWireAnalyserDialog extends BaseAsyncToolDialog<OneWireDataSet, OneWireAnalyserWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public OneWireAnalyserDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void readPreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void writePreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
  }

  /**
   * Creates the button pane for this dialog.
   * 
   * @return a button pane, never <code>null</code>.
   */
  private JComponent createButtonPane()
  {
    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = createExportButton();
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = createCloseButton();
    this.closeAction = closeButton.getAction();

    return SwingComponentUtils.createButtonPane( closeButton, runAnalysisButton, exportButton );
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    setMinimumSize( new Dimension( 640, 480 ) );

    final JComponent settingsPane = new JPanel();
    final JComponent previewPane = new JPanel();

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.add( settingsPane, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    contentPane.add( previewPane, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    final JComponent buttonPane = createButtonPane();

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane );

    pack();
  }
}
