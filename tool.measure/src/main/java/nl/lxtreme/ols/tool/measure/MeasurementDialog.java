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
package nl.lxtreme.ols.tool.measure;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.logging.*;

import javax.swing.*;
import org.osgi.framework.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.measure.ClockFrequencyMeasureTask.ClockStats;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jajans
 */
public class MeasurementDialog extends BaseToolDialog<ClockFrequencyMeasureTask.ClockStats>
{
  // INNER TYPES

  /**
   * Action for measuring the clock frequency between two cursors.
   */
  final class MeasureClockFrequencyAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    /**
     * Creates a new MeasureClockFrequencyAction instance.
     */
    public MeasureClockFrequencyAction()
    {
      super( "Measure ..." );
      putValue( SHORT_DESCRIPTION, "Try to determine the (clock-)frequency of the selected channel." );
    }

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      if ( getData().getValues().length <= 0 )
      {
        ToolUtils.showErrorMessage( MeasurementDialog.this,
            "There is no data available to measure. Please perform a capture first..." );
      }
      else
      {
        invokeTool();
      }
    }
  }

  /**
   * Provides an action listener which updates the measurements in this dialog
   * on regular intervals.
   */
  final class TimerActionListener implements ActionListener
  {
    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      if ( SwingComponentUtils.isActivelyShown( MeasurementDialog.this ) )
      {
        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.fine( "Updating measurement ..." );
        }
        MeasurementDialog.this.updateMeasurement();
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( MeasurementDialog.class.getName() );

  static final String EMPTY_TEXT = "<???>        ";

  // VARIABLES

  private final Timer updateTimer;

  private JLabel[] cursorValueLabels;
  private JComboBox cursorB;
  private JComboBox cursorA;
  private JLabel frequencyLabel;
  private JLabel distanceLabel;
  private JLabel clockFrequencyLabel;
  private JLabel clockDutyCycleLabel;

  private JComboBox clockChannelChooser;

  // CONSTRUCTORS

  /**
   * Creates a new MeasurementDialog instance (non modal).
   * 
   * @param aOwner
   *          the owner of this dialog;
   * @param aToolContext
   *          the tool context;
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aTool
   *          the {@link MeasurementTool} tool.
   */
  public MeasurementDialog( final Window aOwner, final ToolContext aToolContext, final BundleContext aContext,
      final MeasurementTool aTool )
  {
    super( aOwner, ModalityType.MODELESS, aToolContext, aContext, aTool );

    initDialog();

    this.updateTimer = new Timer( 500, new TimerActionListener() );
    this.updateTimer.setRepeats( true );
    this.updateTimer.start();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * @see java.awt.Window#dispose()
   */
  @Override
  public void dispose()
  {
    // Make sure that if this dialog is disposed without our explicit
    // knowledge, the timer is stopped. Otherwise, this dialog would be kept
    // in memory and AWT cannot be shutdown normally...
    if ( ( this.updateTimer != null ) && this.updateTimer.isRunning() )
    {
      this.updateTimer.stop();
    }
    super.dispose();
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    this.cursorA.setSelectedIndex( aSettings.getInt( "selectedCursorA", this.cursorA.getSelectedIndex() ) );
    this.cursorB.setSelectedIndex( aSettings.getInt( "selectedCursorB", this.cursorB.getSelectedIndex() ) );
    this.clockChannelChooser.setSelectedIndex( aSettings.getInt( "selectedClockChannel",
        this.clockChannelChooser.getSelectedIndex() ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#reset()
   */
  @Override
  public void reset()
  {
    if ( !this.updateTimer.isRunning() )
    {
      this.updateTimer.start();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "selectedCursorA", this.cursorA.getSelectedIndex() );
    aSettings.putInt( "selectedCursorB", this.cursorB.getSelectedIndex() );
    aSettings.putInt( "selectedClockChannel", this.clockChannelChooser.getSelectedIndex() );
  }

  /**
   * Updates the measurement.
   */
  final void updateMeasurement()
  {
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      final String text = getCursorTimeDisplayValue( i );
      this.cursorValueLabels[i].setText( text );
    }

    final AcquisitionResult data = getData();
    final ToolContext context = getContext();

    final double rate = data.getSampleRate();

    final Long cursorApos = context.getCursorPosition( this.cursorA.getSelectedIndex() );
    final Long cursorBpos = context.getCursorPosition( this.cursorB.getSelectedIndex() );

    String distanceText = EMPTY_TEXT;
    String frequencyText = EMPTY_TEXT;

    if ( ( cursorApos != null ) && ( cursorBpos != null ) && ( !cursorApos.equals( cursorBpos ) ) )
    {
      final long diff = cursorApos.longValue() - cursorBpos.longValue();

      final double distance = Math.abs( diff / rate );
      distanceText = DisplayUtils.displayTime( distance );

      final double frequency = Math.abs( rate / diff );
      frequencyText = DisplayUtils.displayFrequency( frequency );
    }

    this.distanceLabel.setText( distanceText );
    this.frequencyLabel.setText( frequencyText );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final ClockStats aClockStats )
  {
    StringBuilder freqText = new StringBuilder();
    freqText.append( DisplayUtils.displayFrequency( aClockStats.getFrequency() ) );
    freqText.append( " (\u00b1 " ).append( DisplayUtils.displayFrequency( aClockStats.getError() ) ).append( ")" );

    this.clockFrequencyLabel.setText( freqText.toString() );
    this.clockDutyCycleLabel.setText( DisplayUtils.displayPercentage( aClockStats.getDutyCycle() ) );

    getRootPane().revalidate();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolStarted()
  {
    // NO-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<ClockStats> aToolTask )
  {
    ClockFrequencyMeasureTask toolTask = ( ClockFrequencyMeasureTask )aToolTask;
    toolTask.setChannel( this.clockChannelChooser.getSelectedIndex() );
  }

  /**
   * @param aCursorNames
   * @return
   */
  private JPanel createCursorListingPane( final String[] aCursorNames )
  {
    final JPanel cursorListing = new JPanel( new SpringLayout() );
    cursorListing.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    SpringLayoutUtils.addSeparator( cursorListing, "Cursors" );

    this.cursorValueLabels = new JLabel[Ols.MAX_CURSORS];
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      cursorListing.add( createRightAlignedLabel( aCursorNames[i] ) );

      this.cursorValueLabels[i] = new JLabel( getCursorTimeDisplayValue( i ) );
      cursorListing.add( this.cursorValueLabels[i] );
    }

    SpringLayoutUtils.makeEditorGrid( cursorListing, 10, 6 );

    return cursorListing;
  }

  /**
   * Creates the dialog content for the measurement dialog.
   * <p>
   * Should be called from the EDT!
   * </p>
   * 
   * @return a dialog panel, never <code>null</code>.
   */
  private JComponent createDialogContent()
  {
    final String[] cursorNames = new String[Ols.MAX_CURSORS];
    for ( int i = 0; i < cursorNames.length; i++ )
    {
      cursorNames[i] = String.format( "Cursor %d", Integer.valueOf( i + 1 ) );
    }

    final JPanel result = new JPanel( new GridBagLayout() );

    result.add( createMeasurementPane( cursorNames ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.6, 1.0, GridBagConstraints.NORTHEAST, GridBagConstraints.HORIZONTAL,
            new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( createCursorListingPane( cursorNames ), //
        new GridBagConstraints( 1, 0, 1, 1, 0.4, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL,
            new Insets( 0, 0, 0, 0 ), 0, 0 ) );

    return result;
  }

  /**
   * Creates the pane on which the measurement results are shown.
   * 
   * @return a measurement panel, never <code>null</code>.
   */
  private Component createMeasurementPane( final String[] aCursorNames )
  {
    final int channelCount = getData().getChannels();

    this.frequencyLabel = new JLabel( EMPTY_TEXT );
    this.distanceLabel = new JLabel( EMPTY_TEXT );

    this.cursorA = new JComboBox( aCursorNames );
    this.cursorA.setSelectedIndex( 0 );

    this.cursorB = new JComboBox( aCursorNames );
    this.cursorB.setSelectedIndex( 1 );

    final JPanel rightPane = new JPanel( new SpringLayout() );
    rightPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    SpringLayoutUtils.addSeparator( rightPane, "Reference" );

    rightPane.add( createRightAlignedLabel( "Cursor A" ) );
    rightPane.add( this.cursorA );

    rightPane.add( createRightAlignedLabel( "Cursor B" ) );
    rightPane.add( this.cursorB );

    SpringLayoutUtils.addSeparator( rightPane, "" );

    rightPane.add( createRightAlignedLabel( "Distance" ) );
    rightPane.add( this.distanceLabel );

    rightPane.add( createRightAlignedLabel( "Frequency" ) );
    rightPane.add( this.frequencyLabel );

    final MeasureClockFrequencyAction measureAction = new MeasureClockFrequencyAction();

    this.clockChannelChooser = SwingComponentUtils.createChannelSelector( channelCount );

    this.clockFrequencyLabel = new JLabel( EMPTY_TEXT );
    this.clockDutyCycleLabel = new JLabel( EMPTY_TEXT );
    final JButton measureClockFrequency = new JButton( measureAction );

    SpringLayoutUtils.addSeparator( rightPane, "Measure clock" );

    rightPane.add( createRightAlignedLabel( "Frequency" ) );
    rightPane.add( this.clockFrequencyLabel );
    rightPane.add( createRightAlignedLabel( "Duty cycle" ) );
    rightPane.add( this.clockDutyCycleLabel );
    rightPane.add( createRightAlignedLabel( "Channel" ) );
    rightPane.add( this.clockChannelChooser );
    rightPane.add( new JLabel() );
    rightPane.add( measureClockFrequency );

    SpringLayoutUtils.makeEditorGrid( rightPane, 10, 6 );

    return rightPane;
  }

  /**
   * Returns the time of a cursor with a given index as display value.
   * 
   * @param aIndex
   *          the cursor index to get the time for, >= 0 && < 10.
   * @return a display value for the cursor time, can be "not set" if the cursor
   *         is not set.
   */
  private String getCursorTimeDisplayValue( final int aIndex )
  {
    final AcquisitionResult data = getData();
    if ( data != null )
    {
      final ToolContext context = getContext();

      final Long cursorPosition = context.getCursorPosition( aIndex );
      if ( cursorPosition != null )
      {
        return DisplayUtils.displayTime( cursorPosition.doubleValue() / data.getSampleRate() );
      }
    }
    return "<not set>";
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JComponent contentPane = createDialogContent();
    final JButton closeButton = ToolUtils.createCloseButton();
    final JComponent buttonPane = SwingComponentUtils.createButtonPane( closeButton );

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane, closeButton );
  }
}
