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
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a simple measurement component for measuring some common aspects of
 * signals, such as frequency, # of pulses and so on.
 */
public class MeasurementView extends AbstractViewLayer implements IToolWindow, ISignalElementChangeListener,
    ICursorChangeListener, IMeasurementListener
{
  // INNER TYPES

  /**
   * {@link ActionListener} implementation for the channel comboboxes.
   */
  final class ChannelActionListener implements ActionListener
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final Collection<Channel> channelList = getAllChannels();
      updateChannelModel( channelList );
    }
  }

  /**
   * Provides a renderer for the channels combobox.
   */
  static final class ChannelComboBoxRenderer extends DefaultListCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      StringBuilder sb = new StringBuilder();
      if ( ( aValue != null ) && ( aValue instanceof Channel ) )
      {
        final Channel channel = ( Channel )aValue;
        sb.append( channel.getIndex() );
        if ( channel.hasName() )
        {
          sb.append( ", " ).append( channel.getLabel() );
        }
      }

      return super.getListCellRendererComponent( aList, sb.toString(), aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * {@link ActionListener} implementation for the cursor comboboxes.
   */
  final class CursorActionListener implements ActionListener
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      updateCursorModels();
    }
  }

  /**
   * Provides a renderer for the cursor combobox.
   */
  static final class CursorComboBoxRenderer extends DefaultListCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      StringBuilder sb = new StringBuilder();
      if ( ( aValue != null ) && ( aValue instanceof Cursor ) )
      {
        final Cursor cursor = ( Cursor )aValue;
        sb.append( cursor.getIndex() );
        if ( cursor.hasLabel() )
        {
          sb.append( ": " ).append( cursor.getLabel() );
        }
      }

      return super.getListCellRendererComponent( aList, sb.toString(), aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Provides a {@link SwingWorker} to measure the frequency, dutycycle and such
   * asynchronously from the UI.
   */
  final class SignalMeasurer extends SwingWorker<String, Boolean>
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String doInBackground() throws Exception
    {
      final int mask = ( ( Channel )MeasurementView.this.channel.getSelectedItem() ).getMask();
      final long startTimestamp = ( ( Cursor )MeasurementView.this.cursorA.getSelectedItem() ).getTimestamp();
      final long endTimestamp = ( ( Cursor )MeasurementView.this.cursorB.getSelectedItem() ).getTimestamp();

      final SignalDiagramModel model = getSignalDiagramModel();

      final int startIdx = model.getTimestampIndex( startTimestamp );
      final int endIdx = model.getTimestampIndex( endTimestamp );

      final boolean hasTimingData = model.hasTimingData();

      final int[] values = model.getValues();
      final long[] timestamps = model.getTimestamps();

      final double measureTime = ( double )Math.abs( endTimestamp - startTimestamp ) / model.getSampleRate();

      int highCount = 0;
      long highTime = 0;
      int lowCount = 0;
      long lowTime = 0;

      int i = startIdx;
      long lastTransition = timestamps[i];
      int lastBitValue = values[i++] & mask;

      for ( ; !isCancelled() && ( i <= endIdx ); i++ )
      {
        final int bitValue = values[i] & mask;

        if ( lastBitValue != bitValue )
        {
          final long periodTime = timestamps[i] - lastTransition;

          if ( lastBitValue < bitValue )
          {
            // Low to high transition: previously seen a low-state...
            lowCount++;
            lowTime += periodTime;
          }
          else if ( lastBitValue > bitValue )
          {
            // High to low transition: previously seen a high-state...
            highCount++;
            highTime += periodTime;
          }

          lastTransition = timestamps[i];
        }

        lastBitValue = bitValue;
      }

      int pulseCount = ( lowCount + highCount ) / 2;

      // Take the average high & low time per pulse...
      double avgHighTime = ( highTime / ( double )highCount );
      double avgLowTime = ( lowTime / ( double )lowCount );

      double frequency = model.getSampleRate() / ( avgHighTime + avgLowTime );
      double dutyCycle = avgHighTime / ( avgHighTime + avgLowTime );

      String timeText = UnitOfTime.format( measureTime );
      String frequencyText = FrequencyUnit.format( frequency );
      String dutyCycleText = String.format( "%.3f%%", Double.valueOf( 100.0 * dutyCycle ) );
      String pulseCountText = String.format( "%d (\u2191%d, \u2193%d)", Integer.valueOf( pulseCount ),
          Integer.valueOf( lowCount ), Integer.valueOf( highCount ) );

      final StringBuilder sb = new StringBuilder( "<html><table>" );
      if ( hasTimingData )
      {
        sb.append( "<tr><th align='right'>Time:</th><td>" ).append( timeText ).append( "</td>" );
        sb.append( "<tr><th align='right'>Frequency:</th><td>" ).append( frequencyText ).append( "</td>" );
        sb.append( "<tr><th align='right'>Duty cycle:</th><td>" ).append( dutyCycleText ).append( "</td>" );
        sb.append( "<tr><th align='right'># of pulses:</th><td>" ).append( pulseCountText ).append( "</td>" );
      }
      else
      {
        sb.append( "<tr><th align='right'># of states:</th><td>" ).append( pulseCountText ).append( "</td>" );
      }
      sb.append( "</table></html>" );

      return sb.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void done()
    {
      try
      {
        MeasurementView.this.measurementInfo.setText( get() );
      }
      catch ( Exception exception )
      {
        exception.printStackTrace();
      }
      finally
      {
        MeasurementView.this.indicator.setVisible( false );
      }
    }
  }

  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Measure";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JComboBox channel;
  private JComboBox cursorA;
  private JComboBox cursorB;
  private JBusyIndicator indicator;

  private JLabel measurementInfo;

  private volatile boolean listening;

  // CONSTRUCTORS

  /**
   * Creates a new MeasurementView instance.
   * 
   * @param aController
   */
  public MeasurementView( final SignalDiagramController aController )
  {
    super( aController );

    initComponent();

    this.listening = true;
  }

  // METHODS

  /**
   * Factory method to create a new {@link MeasurementView} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link MeasurementView} instance, never <code>null</code>.
   */
  public static MeasurementView create( final SignalDiagramController aController )
  {
    final MeasurementView result = new MeasurementView( aController );

    aController.addChannelChangeListener( result );
    aController.addCursorChangeListener( result );
    aController.addMeasurementListener( result );

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorAdded( final Cursor aCursor )
  {
    if ( this.listening )
    {
      updateCursorModels();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorChanged( final String aPropertyName, final Cursor aOldCursor, final Cursor aNewCursor )
  {
    if ( this.listening && !ICursorChangeListener.PROPERTY_COLOR.equals( aPropertyName ) )
    {
      updateCursorModels();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorRemoved( final Cursor aOldCursor )
  {
    if ( this.listening )
    {
      updateCursorModels();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsInvisible()
  {
    if ( this.listening )
    {
      updateCursorModels();
    }

    setState( false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsVisible()
  {
    if ( this.listening )
    {
      updateCursorModels();
    }

    setState( true );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void disableMeasurementMode()
  {
    if ( this.listening )
    {
      updateCursorModels();
    }

    setState( false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void enableMeasurementMode()
  {
    if ( this.listening )
    {
      updateCursorModels();
    }

    setState( true );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon getIcon()
  {
    return null; // XXX
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return ID;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void groupStructureChanged( final Collection<SignalElement> aSignalElements )
  {
    if ( this.listening )
    {
      updateChannelModel( getAllChannels( aSignalElements ) );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleMeasureEvent( final MeasurementInfo aEvent )
  {
    // No-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isListening()
  {
    return this.listening;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void signalElementMoved( final ElementMoveEvent aEvent )
  {
    // NO-op
  }

  /**
   * Returns all available channels.
   * 
   * @return a collection of all channels, never <code>null</code>.
   */
  final Collection<Channel> getAllChannels()
  {
    return getAllChannels( getSignalElementManager().getAllElements() );
  }

  /**
   * Updates the channel model to the current list of channels.
   */
  protected void updateChannelModel( final Collection<Channel> aChannels )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        updateChannelComboBoxModel( MeasurementView.this.channel, aChannels );

        updateMeasurementInfo();

        repaint( 50L );
      }
    } );
  }

  /**
   * Updates both combobox models to the current list of available cursors.
   */
  protected void updateCursorModels()
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        updateCursorComboBoxModel( MeasurementView.this.cursorA );
        updateCursorComboBoxModel( MeasurementView.this.cursorB );

        updateMeasurementInfo();

        repaint( 50L );
      }
    } );
  }

  /**
   * Determines whether all preconditions are met to perform a measurement.
   * 
   * @return <code>true</code> if a measurement can be performed,
   *         <code>false</code> otherwise.
   */
  private boolean canPerformMeasurement()
  {
    final SignalDiagramModel model = getSignalDiagramModel();

    Channel channel = ( Channel )this.channel.getSelectedItem();
    if ( ( channel == null ) || !channel.isEnabled() )
    {
      return false;
    }

    if ( !model.isCursorMode() )
    {
      return false;
    }

    Cursor selectedCursorA = ( Cursor )this.cursorA.getSelectedItem();
    if ( ( selectedCursorA == null ) || !selectedCursorA.isDefined() )
    {
      return false;
    }

    Cursor selectedCursorB = ( Cursor )this.cursorB.getSelectedItem();
    if ( ( selectedCursorB == null ) || !selectedCursorB.isDefined() )
    {
      return false;
    }

    return selectedCursorA != selectedCursorB;
  }

  /**
   * Returns all available channels.
   * 
   * @return a collection of all channels, never <code>null</code>.
   */
  private Collection<Channel> getAllChannels( final Collection<SignalElement> aSignalElements )
  {
    final List<Channel> channels = new ArrayList<Channel>();
    for ( SignalElement element : aSignalElements )
    {
      if ( element.isDigitalSignal() )
      {
        channels.add( element.getChannel() );
      }
    }
    return channels;
  }

  /**
   * @return
   */
  private SignalDiagramModel getSignalDiagramModel()
  {
    return getController().getSignalDiagramModel();
  }

  /**
   * Returns the channel group manager.
   * 
   * @return a channel group manager, never <code>null</code>.
   */
  private SignalElementManager getSignalElementManager()
  {
    return getSignalDiagramModel().getSignalElementManager();
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    this.channel = updateChannelComboBoxModel( new JComboBox(), Collections.<Channel> emptyList() );
    this.channel.setRenderer( new ChannelComboBoxRenderer() );
    this.channel.addActionListener( new CursorActionListener() );
    // Make the component a bit smaller and a pop-down on OSX...
    this.channel.putClientProperty( "JComponent.sizeVariant", "small" );
    this.channel.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.channel.setEnabled( false );

    this.cursorA = updateCursorComboBoxModel( new JComboBox() );
    this.cursorA.setRenderer( new CursorComboBoxRenderer() );
    this.cursorA.addActionListener( new ChannelActionListener() );
    // Make the component a bit smaller and a pop-down on OSX...
    this.cursorA.putClientProperty( "JComponent.sizeVariant", "small" );
    this.cursorA.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.cursorA.setEnabled( false );

    this.cursorB = updateCursorComboBoxModel( new JComboBox() );
    this.cursorB.setRenderer( new CursorComboBoxRenderer() );
    this.cursorB.addActionListener( new ChannelActionListener() );
    // Make the component a bit smaller and a pop-down on OSX...
    this.cursorB.putClientProperty( "JComponent.sizeVariant", "small" );
    this.cursorB.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.cursorB.setEnabled( false );

    this.indicator = new JBusyIndicator();
    this.indicator.setVisible( false );

    this.measurementInfo = new JLabel();

    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Measurement" );
    setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ) );

    GridBagConstraints gbc = new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, 0, GridBagConstraints.HORIZONTAL,
        new Insets( 0, 0, 0, 0 ), 0, 0 );

    JPanel cursorPanel = new JPanel( new GridBagLayout() );

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    cursorPanel.add( new JLabel( "Channel" ), gbc );

    gbc.gridx = 1;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    cursorPanel.add( this.channel, gbc );

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    cursorPanel.add( new JLabel( "Cursor A" ), gbc );

    gbc.gridx = 1;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    cursorPanel.add( this.cursorA, gbc );

    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    cursorPanel.add( new JLabel( "Cursor B" ), gbc );

    gbc.gridx = 1;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    cursorPanel.add( this.cursorB, gbc );

    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.gridwidth = 3;
    gbc.anchor = GridBagConstraints.CENTER;

    cursorPanel.add( this.indicator, gbc );

    add( cursorPanel, BorderLayout.NORTH );
    add( this.measurementInfo, BorderLayout.CENTER );
  }

  /**
   * Enables/disables the various components on this view.
   */
  private void setState( final boolean aEnabled )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        MeasurementView.this.channel.setEnabled( aEnabled );
        MeasurementView.this.cursorA.setEnabled( aEnabled );
        MeasurementView.this.cursorB.setEnabled( aEnabled );
        MeasurementView.this.measurementInfo.setText( "" );
      }
    } );
  }

  /**
   * Updates a given combobox' model to contain the current list of defined
   * cursors.
   * 
   * @return the given combobox.
   */
  private JComboBox updateChannelComboBoxModel( final JComboBox aComboBox, final Collection<Channel> aChannels )
  {
    ComboBoxModel model = new DefaultComboBoxModel( new Vector<Channel>( aChannels ) );

    final Object oldSelectedItem = aComboBox.getSelectedItem();
    aComboBox.setModel( model );
    aComboBox.setSelectedItem( oldSelectedItem );

    return aComboBox;
  }

  /**
   * Updates a given combobox' model to contain the current list of defined
   * cursors.
   * 
   * @return the given combobox.
   */
  private JComboBox updateCursorComboBoxModel( final JComboBox aComboBox )
  {
    final Cursor[] cursors = getSignalDiagramModel().getDefinedCursors();
    ComboBoxModel model = new DefaultComboBoxModel( cursors );

    final Object oldSelectedItem = aComboBox.getSelectedItem();
    aComboBox.setModel( model );
    aComboBox.setSelectedItem( oldSelectedItem );

    return aComboBox;
  }

  /**
   * Updates the actual measurement information.
   */
  private void updateMeasurementInfo()
  {
    this.listening = false;

    try
    {
      if ( canPerformMeasurement() )
      {
        this.indicator.setVisible( true );
        ( new SignalMeasurer() ).execute();
      }
      else
      {
        this.measurementInfo.setText( "" );
      }
    }
    finally
    {
      this.listening = true;
    }
  }
}
