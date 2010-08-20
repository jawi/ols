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


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.Timer;
import javax.swing.SwingWorker.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;
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
      if ( this.worker == null )
      {
        this.worker = new ClockFrequencyMeasureWorker( MeasurementDialog.this.data, this.channel );
        this.worker.addPropertyChangeListener( this );

        this.worker.execute();
      }
      else
      {
        if ( !this.worker.isDone() )
        {
          this.worker.cancel( true /* mayInterruptIfRunning */);
        }
        this.worker.removePropertyChangeListener( this );
        this.worker = null;
      }
    }

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
        }
        else if ( StateValue.DONE.equals( state ) )
        {
          // Restore the original cursor...
          setCursor( new Cursor( Cursor.DEFAULT_CURSOR ) );

          final ClockFrequencyMeasureWorker worker = ( ClockFrequencyMeasureWorker )aEvent.getSource();

          try
          {
            final String analysisResults = worker.get();
            MeasurementDialog.this.clockFrequencyLabel.setText( analysisResults );
          }
          catch ( CancellationException exception )
          {
            LOG.log( Level.WARNING, "Dialog exception!", exception );
          }
          catch ( ExecutionException exception )
          {
            LOG.log( Level.WARNING, "Dialog exception!", exception );
          }
          catch ( InterruptedException exception )
          {
            LOG.log( Level.WARNING, "Dialog exception!", exception );
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

  // VARIABLES

  private final Timer updateTimer;
  private volatile AnnotatedData data;

  private JLabel[] cursorValueLabels;
  private JComboBox cursorB;
  private JComboBox cursorA;
  private JLabel frequencyLabel;
  private JLabel distanceLabel;
  private JLabel clockFrequencyLabel;

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

    final JComponent contentPane = ( JComponent )getContentPane();
    contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
    contentPane.add( createDialogContent() );

    this.updateTimer = new Timer( 500, new TimerActionListener() );
    this.updateTimer.setRepeats( true );
    this.updateTimer.start();

    pack();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.BaseTool#readProperties(String,
   *      java.util.Properties)
   */
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    this.cursorA.setSelectedIndex( NumberUtils.smartParseInt(
        aProperties.getProperty( aNamespace + ".selectedCursorA" ), 0 ) );
    this.cursorB.setSelectedIndex( NumberUtils.smartParseInt(
        aProperties.getProperty( aNamespace + ".selectedCursorB" ), 1 ) );
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
   * @see nl.lxtreme.ols.tool.base.BaseToolDialog#showDialog(nl.lxtreme.ols.api.data.AnnotatedData)
   */
  @Override
  public void showDialog( final AnnotatedData aData )
  {
    this.data = aData;
    super.showDialog( aData );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseTool#writeProperties(String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    aProperties.put( aNamespace + ".selectedCursorA", String.valueOf( this.cursorA.getSelectedIndex() ) );
    aProperties.put( aNamespace + ".selectedCursorB", String.valueOf( this.cursorB.getSelectedIndex() ) );
  }

  /**
   * Updates the measurement.
   */
  final void updateMeasurement()
  {
    for ( int i = 0; i < AnnotatedData.MAX_CURSORS; i++ )
    {
      final String text = getCursorTimeDisplayValue( i );
      this.cursorValueLabels[i].setText( text );
    }

    final double rate = this.data.getSampleRate();

    final long cursorApos = this.data.getCursorPosition( this.cursorA.getSelectedIndex() );
    final long cursorBpos = this.data.getCursorPosition( this.cursorB.getSelectedIndex() );

    String distanceText = "<???>";
    String frequencyText = "<???>";

    if ( ( cursorApos >= 0 ) && ( cursorBpos >= 0 ) && ( cursorApos != cursorBpos ) )
    {
      final long diff = cursorApos - cursorBpos;

      final double distance = Math.abs( diff / rate );
      distanceText = DisplayUtils.displayTime( distance );

      final double frequency = Math.abs( rate / diff );
      frequencyText = DisplayUtils.displayFrequency( frequency );
    }

    this.distanceLabel.setText( distanceText );
    this.frequencyLabel.setText( frequencyText );

    // repaint();
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseToolDialog#close()
   */
  @Override
  protected void close()
  {
    this.updateTimer.stop();
    super.close();
  }

  /**
   * @return
   */
  private Component createButtonPane()
  {
    final JButton close = new JButton( new CloseAction() );

    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );
    buttonPane.setBorder( BorderFactory.createEmptyBorder( 8, 4, 8, 4 ) );

    buttonPane.add( Box.createHorizontalGlue() );
    buttonPane.add( close );

    return buttonPane;
  }

  /**
   * @param aData
   * @return
   */
  private Component createCursorsPane()
  {
    final String[] cursorNames = new String[AnnotatedData.MAX_CURSORS];
    for ( int i = 0; i < cursorNames.length; i++ )
    {
      cursorNames[i] = String.format( "Cursor %d", ( i + 1 ) );
    }

    final JPanel referenceCursors = new JPanel( new GridBagLayout() );
    referenceCursors.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Reference" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    this.cursorA = new JComboBox( cursorNames );
    this.cursorA.setSelectedIndex( 0 );

    this.cursorB = new JComboBox( cursorNames );
    this.cursorB.setSelectedIndex( 1 );

    referenceCursors.add( new JLabel( "Cursor A:" ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    referenceCursors.add( this.cursorA, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    referenceCursors.add( new JLabel( "Cursor B:" ), //
        new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    referenceCursors.add( this.cursorB, //
        new GridBagConstraints( 1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    final JPanel cursorListing = new JPanel( new GridBagLayout() );
    cursorListing.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Current cursors" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    int yIdx = 0;
    this.cursorValueLabels = new JLabel[AnnotatedData.MAX_CURSORS];
    for ( int i = 0; i < AnnotatedData.MAX_CURSORS; i++ )
    {
      cursorListing.add( new JLabel( cursorNames[i] ), //
          new GridBagConstraints( 0, yIdx, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
              GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );

      this.cursorValueLabels[i] = new JLabel( getCursorTimeDisplayValue( i ) );
      cursorListing.add( this.cursorValueLabels[i], //
          new GridBagConstraints( 1, yIdx, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
              GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );
      yIdx++;
    }

    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( referenceCursors, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    result.add( cursorListing, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    return result;
  }

  /**
   * Creates the dialog content for the measurement dialog.
   * <p>
   * Should be called from the EDT!
   * </p>
   * 
   * @return a dialog panel, never <code>null</code>.
   */
  private Component createDialogContent()
  {
    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( createCursorsPane(), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.VERTICAL,
            COMP_INSETS, 0, 0 ) );
    result.add( createMeasurePane(), //
        new GridBagConstraints( 1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHEAST, GridBagConstraints.VERTICAL,
            COMP_INSETS, 0, 0 ) );
    result.add( createButtonPane(), //
        new GridBagConstraints( 0, 1, 2, 1, 1.0, 1.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );
    return result;
  }

  /**
   * Creates the pane on which the measurement results are shown.
   * 
   * @return a measurement panel, never <code>null</code>.
   */
  private Component createMeasurePane()
  {
    this.frequencyLabel = new JLabel( "<???>        " );
    this.distanceLabel = new JLabel( "<???>         " );

    final JPanel basicMeasurements = new JPanel( new GridBagLayout() );
    basicMeasurements.setBorder( BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder( "Measurement results" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    basicMeasurements.add( new JLabel( "Distance:" ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    basicMeasurements.add( this.distanceLabel, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    basicMeasurements.add( new JLabel( "Frequency:" ), //
        new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    basicMeasurements.add( this.frequencyLabel, //
        new GridBagConstraints( 1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    final MeasureClockFrequencyAction measureAction = new MeasureClockFrequencyAction();

    final String[] channelNames = new String[AnnotatedData.MAX_CHANNELS];
    for ( int i = 0; i < channelNames.length; i++ )
    {
      channelNames[i] = String.format( "Channel %d", ( i + 1 ) );
    }
    final JComboBox channelChooser = new JComboBox( channelNames );
    channelChooser.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        measureAction.setChannel( combobox.getSelectedIndex() );
      }
    } );

    this.clockFrequencyLabel = new JLabel( "" );
    final JButton measureClockFrequency = new JButton( measureAction );

    final JPanel measureClock = new JPanel( new GridBagLayout() );
    measureClock.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Measure clock" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    measureClock.add( new JLabel( "Clock:" ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    measureClock.add( this.clockFrequencyLabel, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );
    measureClock.add( new JLabel( "Channel:" ), //
        new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    measureClock.add( channelChooser, //
        new GridBagConstraints( 1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );
    measureClock.add( measureClockFrequency, //
        new GridBagConstraints( 0, 2, 2, 1, 1.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );

    final JPanel result = new JPanel( new GridBagLayout() );

    result.add( basicMeasurements, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHEAST, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    result.add( measureClock, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHEAST, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    // Spacer
    result.add( Box.createVerticalGlue(), //
        new GridBagConstraints( 0, 2, 2, 1, 10.0, 10.0, GridBagConstraints.NORTHEAST, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    return result;
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
        return DisplayUtils.displayTime( cursorTimeValue );
      }
    }
    return "<not set>";
  }
}
