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
package nl.lxtreme.ols.tool.measure;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.SwingWorker.StateValue;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.measure.ClockFrequencyMeasureWorker.ClockStats;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jajans
 */
public class MeasurementDialog extends BaseToolDialog
{
  // INNER TYPES

  /**
   * Action for measuring the clock frequency between two cursors.
   */
  final class MeasureClockFrequencyAction extends AbstractAction implements PropertyChangeListener
  {
    private static final long serialVersionUID = 1L;

    private int channel = 0;
    private ClockFrequencyMeasureWorker worker = null;

    /**
     *
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
      if ( this.worker != null )
      {
        if ( !this.worker.isDone() )
        {
          this.worker.cancel( true /* mayInterruptIfRunning */);
        }
        this.worker.removePropertyChangeListener( this );
        this.worker = null;
      }

      if ( this.worker == null )
      {
        final DataContainer container = MeasurementDialog.this.data;
        final int cursorAidx = MeasurementDialog.this.cursorA.getSelectedIndex();
        final int cursorBidx = MeasurementDialog.this.cursorB.getSelectedIndex();

        final ToolContext context = new ToolContext()
        {
          /**
           * @see nl.lxtreme.ols.api.tools.ToolContext#getEndSampleIndex()
           */
          @Override
          public int getEndSampleIndex()
          {
            if ( container.hasCapturedData() )
            {
              if ( container.isCursorPositionSet( cursorBidx ) )
              {
                final Long cursorPosition = container.getCursorPosition( cursorBidx );
                return container.getSampleIndex( cursorPosition.longValue() );
              }

              return container.getValues().length - 1;
            }
            return -1;
          }

          /**
           * @see nl.lxtreme.ols.api.tools.ToolContext#getLength()
           */
          @Override
          public int getLength()
          {
            return getEndSampleIndex() - getStartSampleIndex();
          }

          /**
           * @see nl.lxtreme.ols.api.tools.ToolContext#getStartSampleIndex()
           */
          @Override
          public int getStartSampleIndex()
          {
            if ( container.hasCapturedData() )
            {
              if ( container.isCursorPositionSet( cursorAidx ) )
              {
                final Long cursorPosition = container.getCursorPosition( cursorAidx );
                return container.getSampleIndex( cursorPosition.longValue() );
              }

              return 0;
            }
            return -1;
          }
        };

        this.worker = new ClockFrequencyMeasureWorker( MeasurementDialog.this.data, context, this.channel );

        if ( !this.worker.containsData() )
        {
          showErrorMessage( "There is no data available to measure. Please perform a capture first..." );
        }
        else
        {
          this.worker.addPropertyChangeListener( this );
          this.worker.execute();
        }
      }
    }

    /**
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
     */
    @Override
    public void propertyChange( final PropertyChangeEvent aEvent )
    {
      final String name = aEvent.getPropertyName();
      final Object value = aEvent.getNewValue();

      if ( "state".equals( name ) )
      {
        // State change...
        final StateValue state = ( StateValue )value;
        if ( StateValue.STARTED.equals( state ) )
        {
          // Set the "wait" cursor...
          setCursor( new Cursor( Cursor.WAIT_CURSOR ) );
          setEnabled( false );
        }
        else if ( StateValue.DONE.equals( state ) )
        {
          // Restore the original cursor...
          setCursor( new Cursor( Cursor.DEFAULT_CURSOR ) );
          setEnabled( true );

          final ClockFrequencyMeasureWorker worker = ( ClockFrequencyMeasureWorker )aEvent.getSource();

          try
          {
            final ClockStats stats = worker.get();

            MeasurementDialog.this.clockFrequencyLabel.setText( stats.getFrequencyDisplayText() );
            MeasurementDialog.this.clockDutyCycleLabel.setText( stats.getDutyCycleDisplayText() );
          }
          catch ( CancellationException exception )
          {
            LOG.log( Level.WARNING, "Dialog exception!", exception );
          }
          catch ( ExecutionException exception )
          {
            // Make sure to handle IO-interrupted exceptions properly!
            if ( !HostUtils.handleInterruptedException( exception.getCause() ) )
            {
              LOG.log( Level.WARNING, "Dialog exception!", exception );
            }
          }
          catch ( InterruptedException exception )
          {
            // Make sure to handle IO-interrupted exceptions properly!
            if ( !HostUtils.handleInterruptedException( exception ) )
            {
              LOG.log( Level.WARNING, "Dialog exception!", exception );
            }
          }
        }
      }
    }

    /**
     * Sets the channel on which the clock-frequency should be determined.
     * 
     * @param aChannel
     */
    public void setChannel( final int aChannel )
    {
      this.channel = aChannel;
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
  private volatile DataContainer data;

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
   *          the owning window;
   * @param aTitle
   *          the title of this dialog;
   * @param aData
   *          the data to show in this dialog;
   * @param aContext
   *          the tool context.
   */
  public MeasurementDialog( final Window aOwner, final String aTitle )
  {
    super( aOwner, aTitle, Dialog.ModalityType.MODELESS );

    initDialog();

    this.updateTimer = new Timer( 500, new TimerActionListener() );
    this.updateTimer.setRepeats( true );
    this.updateTimer.start();

    setLocationRelativeTo( getOwner() );
  }

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

  // METHODS

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
   * @see nl.lxtreme.ols.tool.base.BaseToolDialog#showDialog(nl.lxtreme.ols.api.data.DataContainer)
   */
  @Override
  public void showDialog( final DataContainer aData )
  {
    this.data = aData;
    super.showDialog( aData );
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
    for ( int i = 0; i < CapturedData.MAX_CURSORS; i++ )
    {
      final String text = getCursorTimeDisplayValue( i );
      this.cursorValueLabels[i].setText( text );
    }

    final double rate = this.data.getSampleRate();

    final Long cursorApos = this.data.getCursorPosition( this.cursorA.getSelectedIndex() );
    final Long cursorBpos = this.data.getCursorPosition( this.cursorB.getSelectedIndex() );

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
   * @param aCursorNames
   * @return
   */
  private JPanel createCursorListingPane( final String[] aCursorNames )
  {
    final JPanel cursorListing = new JPanel( new SpringLayout() );
    cursorListing.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    SpringLayoutUtils.addSeparator( cursorListing, "Cursors" );

    this.cursorValueLabels = new JLabel[CapturedData.MAX_CURSORS];
    for ( int i = 0; i < CapturedData.MAX_CURSORS; i++ )
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
    final String[] cursorNames = new String[CapturedData.MAX_CURSORS];
    for ( int i = 0; i < cursorNames.length; i++ )
    {
      cursorNames[i] = String.format( "Cursor %d", Integer.valueOf( i + 1 ) );
    }

    final JPanel result = new JPanel( new GridBagLayout() );

    result.add( createCursorListingPane( cursorNames ), //
        new GridBagConstraints( 1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHEAST, GridBagConstraints.NONE,
            new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( createMeasurementPane( cursorNames ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHEAST, GridBagConstraints.NONE,
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

    final String[] channelNames = new String[CapturedData.MAX_CHANNELS];
    for ( int i = 0; i < channelNames.length; i++ )
    {
      channelNames[i] = String.format( "Channel %d", Integer.valueOf( i ) );
    }
    this.clockChannelChooser = new JComboBox( channelNames );
    this.clockChannelChooser.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        measureAction.setChannel( combobox.getSelectedIndex() );
      }
    } );

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
    if ( this.data != null )
    {
      final Double cursorTimeValue = this.data.getCursorTimeValue( aIndex );
      if ( cursorTimeValue != null )
      {
        return DisplayUtils.displayTime( cursorTimeValue.doubleValue() );
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
    final JButton closeButton = createCloseButton();
    final JComponent buttonPane = SwingComponentUtils.createButtonPane( closeButton );

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane, closeButton );
  }
}
