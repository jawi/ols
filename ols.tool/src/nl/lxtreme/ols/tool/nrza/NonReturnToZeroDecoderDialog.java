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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.nrza;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.util.swing.*;


/**
 * Denotes the configuration dialog for the NRZA line decoder tool.
 */
public class NonReturnToZeroDecoderDialog extends BaseToolDialog<AcquisitionData>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JComboBox dataLine;
  private JComboBox clockLine;
  private JCheckBox inverted;

  private Action closeAction;
  private RestorableAction runAnalysisAction;

  // CONSTRUCTORS

  /**
   * Creates a new {@link NonReturnToZeroDecoderDialog} instance.
   * 
   * @param aOwner
   * @param aTool
   * @param aContext
   */
  public NonReturnToZeroDecoderDialog( Window aOwner, NonReturnToZeroDecoder aTool, ToolContext aContext )
  {
    super( aOwner, aTool, aContext );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.dataLine.setSelectedIndex( aSettings.getInt( "dataLine", this.dataLine.getSelectedIndex() ) );
    this.clockLine.setSelectedIndex( aSettings.getInt( "clockLine", this.clockLine.getSelectedIndex() ) );
    this.inverted.setSelected( aSettings.getBoolean( "inverted", this.inverted.isSelected() ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void reset()
  {
    this.runAnalysisAction.restore();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "dataLine", this.dataLine.getSelectedIndex() );
    aSettings.putInt( "clockLine", this.clockLine.getSelectedIndex() );
    aSettings.putBoolean( "inverted", this.inverted.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final AcquisitionData aResult )
  {
    this.closeAction.setEnabled( true );
    this.runAnalysisAction.restore();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolStarted()
  {
    this.closeAction.setEnabled( false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<AcquisitionData> aToolTask )
  {
    final NonReturnToZeroDecoderTask toolTask = ( NonReturnToZeroDecoderTask )aToolTask;
    toolTask.setClockIdx( this.clockLine.getSelectedIndex() ); // XXX
    toolTask.setDataIdx( this.dataLine.getSelectedIndex() ); // XXX
    toolTask.setInverted( this.inverted.isSelected() );
    toolTask.setDecodingArea( getMarkerAIndex(), getMarkerBIndex() );
  }

  /**
   * Creates the settings pane.
   * 
   * @return a settings pane, never <code>null</code>.
   */
  private JComponent createSettingsPane( final JPanel aSubSettingsPanel )
  {
    final JPanel panel = new JPanel( new SpringLayout() );

    addDecoderAreaPane( panel );

    SpringLayoutUtils.addSeparator( panel, "General settings" );

    final int channelCount = getData().getChannelCount();

    this.inverted = new JCheckBox( "", false /* selected */);

    panel.add( createRightAlignedLabel( "Inverted?" ) );
    panel.add( this.inverted );

    this.dataLine = SwingComponentUtils.createChannelSelector( channelCount );

    panel.add( createRightAlignedLabel( "Data line" ) );
    panel.add( this.dataLine );

    this.clockLine = SwingComponentUtils.createChannelSelector( channelCount );

    panel.add( createRightAlignedLabel( "Clock line" ) );
    panel.add( this.clockLine );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 4 );

    return panel;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    setMinimumSize( new Dimension( 640, 480 ) );

    final JPanel subSettingsPane = new JPanel( new SpringLayout() );
    final JComponent settingsPane = createSettingsPane( subSettingsPane );

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.add( settingsPane, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.NONE, //
            new Insets( 2, 0, 2, 0 ), 0, 0 ) );

    contentPane.add( subSettingsPane, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.NONE, //
            new Insets( 2, 0, 2, 0 ), 0, 0 ) );

    final JButton runAnalysisButton = ToolUtils.createRunAnalysisButton( this );
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton closeButton = ToolUtils.createCloseButton();
    this.closeAction = closeButton.getAction();

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( runAnalysisButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, contentPane, buttonPane, runAnalysisButton );

    pack();
  }

}
