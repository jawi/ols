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
package nl.lxtreme.ols.tool.linedecoder.impl;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.tool.linedecoder.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.osgi.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;


/**
 * Denotes the configuration dialog for the line decoder tool.
 */
public class LineDecoderToolDialog extends BaseToolDialog<AcquisitionResult>
{
  // INNER TYPES

  private static final class LineDecoderComboBoxRenderer extends DefaultListCellRenderer
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see javax.swing.DefaultListCellRenderer#getListCellRendererComponent(javax.swing.JList,
     *      java.lang.Object, int, boolean, boolean)
     */
    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      final String text = ( ( LineDecoder )aValue ).getName();

      return super.getListCellRendererComponent( aList, text, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final WhiteboardHelper<AcquisitionDataListener> acquisitionDataListenerHelper;

  private JComboBox lineDecoders;
  private JComboBox[] lines;
  private JCheckBox inverted;
  private JCheckBox recoverClock;
  private JTextField clockSpeed;

  private Action closeAction;
  private RestorableAction runAnalysisAction;

  // CONSTRUCTORS

  /**
   * Creates a new LineDecoderToolDialog instance.
   * 
   * @param aOwner
   * @param aContext
   * @param aBundleContext
   * @param aTool
   */
  public LineDecoderToolDialog( final Window aOwner, final ToolContext aContext, final BundleContext aBundleContext,
      final Tool<AcquisitionResult> aTool )
  {
    super( aOwner, aContext, aBundleContext, aTool );

    this.lines = new JComboBox[0];
    this.acquisitionDataListenerHelper = new WhiteboardHelper<AcquisitionDataListener>( aBundleContext,
        AcquisitionDataListener.class );

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
    this.lineDecoders.setSelectedIndex( aSettings.getInt( "decoder", this.lineDecoders.getSelectedIndex() ) );
    this.clockSpeed.setText( aSettings.get( "clockSpeed", this.clockSpeed.getText() ) );
    this.inverted.setSelected( aSettings.getBoolean( "inverted", this.inverted.isSelected() ) );
    this.recoverClock.setSelected( aSettings.getBoolean( "recoverClock", this.recoverClock.isSelected() ) );

    for ( int i = 0; i < this.lines.length; i++ )
    {
      // Issue #114: avoid setting illegal values...
      setComboBoxIndex( this.lines[i], aSettings, "decoder.line." + i );
    }
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
    aSettings.putInt( "decoder", this.lineDecoders.getSelectedIndex() );
    aSettings.put( "clockSpeed", this.clockSpeed.getText() );
    aSettings.putBoolean( "inverted", this.inverted.isSelected() );
    aSettings.putBoolean( "recoverClock", this.recoverClock.isSelected() );

    for ( int i = 0; i < this.lines.length; i++ )
    {
      aSettings.putInt( "decoder.line." + i, this.lines[i].getSelectedIndex() );
    }
  }

  /**
   * @return an array with all line decoders, never <code>null</code>.
   */
  protected final LineDecoder[] getLineDecoders()
  {
    return ( ( LineDecoderTool )getTool() ).getLineDecoders();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final AcquisitionResult aResult )
  {
    this.acquisitionDataListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionDataListener>()
    {
      @Override
      public void visit( final AcquisitionDataListener aService )
      {
        if ( aResult != null )
        {
          aService.acquisitionComplete( aResult );
        }
      }
    } );

    this.closeAction.setEnabled( true );
    this.runAnalysisAction.restore();

    this.acquisitionDataListenerHelper.close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolStarted()
  {
    this.closeAction.setEnabled( false );
    this.acquisitionDataListenerHelper.open();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<AcquisitionResult> aToolTask )
  {
    int[] channels = new int[this.lines.length];
    for ( int i = 0; i < channels.length; i++ )
    {
      channels[i] = this.lines[i].getSelectedIndex();
    }

    int clockSpeed = NumberUtils.safeParseInt( this.clockSpeed.getText() );

    final LineDecoderTask toolTask = ( LineDecoderTask )aToolTask;
    toolTask.setLineDecoder( ( LineDecoder )this.lineDecoders.getSelectedItem() );
    toolTask.setChannels( channels );
    toolTask.setInverted( this.inverted.isSelected() );
    toolTask.setRecoverClock( this.recoverClock.isSelected() );
    toolTask.setClockSpeed( clockSpeed );
  }

  /**
   * @param aPanel
   *          the panel to fill with the settings of the given line decoder;
   * @param aLineDecoder
   *          the line decoder to fill the settings panel for.
   */
  protected void updateSettingsPane( final JPanel aPanel, final LineDecoder aLineDecoder )
  {
    aPanel.removeAll();
    SpringLayoutUtils.addSeparator( aPanel, "Decoder settings" );

    this.clockSpeed = new JTextField();
    this.recoverClock = new JCheckBox( "", false /* selected */);
    this.recoverClock.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        LineDecoderToolDialog.this.clockSpeed.setEnabled( !LineDecoderToolDialog.this.recoverClock.isSelected() );
      }
    } );

    if ( aLineDecoder.canRecoverClock() )
    {
      aPanel.add( createRightAlignedLabel( "Recover clock?" ) );
      aPanel.add( this.recoverClock );

      aPanel.add( createRightAlignedLabel( "Clock speed" ) );
      aPanel.add( this.clockSpeed );
    }

    final int channelCount = getData().getChannels();

    final String[] lines = aLineDecoder.getLineNames();
    this.lines = new JComboBox[lines.length];

    this.inverted = new JCheckBox( "", false /* selected */);

    if ( aLineDecoder.canHandleInversion() )
    {
      aPanel.add( createRightAlignedLabel( "Inverted?" ) );
      aPanel.add( this.inverted );
    }

    for ( int i = 0; i < lines.length; i++ )
    {
      this.lines[i] = SwingComponentUtils.createChannelSelector( channelCount );

      String label = "Channel " + ( i + 1 );
      if ( ( lines[i] != null ) && !"".equals( lines[i].trim() ) )
      {
        label = lines[i];
      }

      aPanel.add( createRightAlignedLabel( label ) );
      aPanel.add( this.lines[i] );
    }

    SpringLayoutUtils.makeEditorGrid( aPanel, 10, 4 );
  }

  /**
   * Creates the settings pane.
   * 
   * @return a settings pane, never <code>null</code>.
   */
  private JComponent createSettingsPane( final JPanel aSubSettingsPanel )
  {
    final JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "General settings" );

    panel.add( createRightAlignedLabel( "Line Decoder" ) );
    this.lineDecoders = new JComboBox( getLineDecoders() );
    this.lineDecoders.setRenderer( new LineDecoderComboBoxRenderer() );
    this.lineDecoders.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final LineDecoder lineDecoder = ( LineDecoder )aEvent.getItem();
        updateSettingsPane( aSubSettingsPanel, lineDecoder );

        validate();
      }
    } );
    panel.add( this.lineDecoders );

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

    updateSettingsPane( subSettingsPane, ( LineDecoder )this.lineDecoders.getSelectedItem() );

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
