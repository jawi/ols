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


import static nl.lxtreme.ols.client.signaldisplay.view.ViewUtils.*;

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
   * Represents a small DTO for measured pulse count information.
   */
  static final class PulseCountInfo
  {
    final double measureTime;
    final int risingEdgeCount;
    final int fallingEdgeCount;
    final long totalLowTime;
    final long totalHighTime;
    final int sampleRate;
    final boolean hasTimingData;

    /**
     * Creates a new {@link PulseCountInfo} instance.
     */
    public PulseCountInfo( final double aMeasureTime, final int aRisingEdgeCount, final int aFallingEdgeCount,
        final long aTotalLowTime, final long aTotalHighTime, final int aSampleRate, final boolean aHasTimingData )
    {
      this.measureTime = aMeasureTime;
      this.risingEdgeCount = aRisingEdgeCount;
      this.fallingEdgeCount = aFallingEdgeCount;
      this.totalLowTime = aTotalLowTime;
      this.totalHighTime = aTotalHighTime;
      this.sampleRate = aSampleRate;
      this.hasTimingData = aHasTimingData;
    }

    /**
     * @return
     */
    public double getDutyCycle()
    {
      final double avgHighTime = getAveragePulseHighTime();
      final double avgLowTime = getAveragePulseLowTime();
      return 100.0 * ( avgHighTime / ( avgHighTime + avgLowTime ) );
    }

    /**
     * @return
     */
    public double getFrequency()
    {
      return this.sampleRate / ( getAveragePulseHighTime() + getAveragePulseLowTime() );
    }

    /**
     * @return
     */
    public int getPulseCount()
    {
      return ( this.risingEdgeCount + this.fallingEdgeCount ) / 2;
    }

    /**
     * @return
     */
    private double getAveragePulseHighTime()
    {
      return ( this.totalHighTime / ( double )this.fallingEdgeCount );
    }

    /**
     * @return
     */
    private double getAveragePulseLowTime()
    {
      return ( this.totalLowTime / ( double )this.risingEdgeCount );
    }
  }

  /**
   * Provides a {@link SwingWorker} to measure the frequency, dutycycle and such
   * asynchronously from the UI.
   */
  final class SignalMeasurer extends SwingWorker<PulseCountInfo, Boolean>
  {
    // VARIABLES

    private final int mask;
    private final long startTimestamp;
    private final long endTimestamp;

    // CONSTRUCTORS

    /**
     * Creates a new MeasurementView.SignalMeasurer instance.
     */
    public SignalMeasurer( final Channel aChannel, final Cursor aCursorA, final Cursor aCursorB )
    {
      this.mask = aChannel.getMask();
      this.startTimestamp = aCursorA.getTimestamp();
      this.endTimestamp = aCursorB.getTimestamp();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected PulseCountInfo doInBackground() throws Exception
    {
      final SignalDiagramModel model = getSignalDiagramModel();

      final int startIdx = model.getTimestampIndex( this.startTimestamp );
      final int endIdx = model.getTimestampIndex( this.endTimestamp );

      final boolean hasTimingData = model.hasTimingData();

      final int[] values = model.getValues();
      final long[] timestamps = model.getTimestamps();

      int fallingEdgeCount = 0;
      long highTime = 0;
      int risingEdgeCount = 0;
      long lowTime = 0;

      int i = startIdx;
      long lastTransition = timestamps[i];
      int lastBitValue = values[i++] & this.mask;

      for ( ; !isCancelled() && ( i <= endIdx ); i++ )
      {
        final int bitValue = values[i] & this.mask;
        final Edge edge = Edge.toEdge( lastBitValue, bitValue );

        if ( !edge.isNone() )
        {
          final long periodTime = timestamps[i] - lastTransition;
          lastTransition = timestamps[i];

          if ( edge.isRising() )
          {
            // Low to high transition: previously seen a low-state...
            risingEdgeCount++;
            lowTime += periodTime;
          }
          else
          /* if ( edge.isFalling() ) */
          {
            // High to low transition: previously seen a high-state...
            fallingEdgeCount++;
            highTime += periodTime;
          }
        }

        lastBitValue = bitValue;
      }

      final double measureTime = ( double )Math.abs( this.endTimestamp - this.startTimestamp ) / model.getSampleRate();

      return new PulseCountInfo( measureTime, risingEdgeCount, fallingEdgeCount, lowTime, highTime,
          model.getSampleRate(), hasTimingData );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void done()
    {
      try
      {
        updatePulseCountInformation( get() );

        repaint( 50L );
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

  private JComboBox measureChannel;
  private JComboBox cursorA;
  private JComboBox cursorB;
  private JBusyIndicator indicator;

  private final JLabel mi_channel;
  private final JLabel mi_referenceLabel;
  private final JLabel mi_reference;
  private final JLabel mi_period;
  private final JLabel mi_frequency;
  private final JLabel mi_widthHigh;
  private final JLabel mi_widthLow;
  private final JLabel mi_dutyCycle;

  private final JLabel pci_time;
  private final JLabel pci_frequency;
  private final JLabel pci_dutyCycle;
  private final JLabel pci_pulseCountLabel;
  private final JLabel pci_pulseCount;

  private volatile boolean listening;
  private volatile SignalMeasurer signalMeasurer;

  // CONSTRUCTORS

  /**
   * Creates a new MeasurementView instance.
   * 
   * @param aController
   */
  public MeasurementView( final SignalDiagramController aController )
  {
    super( aController );

    this.mi_channel = new JLabel();
    this.mi_referenceLabel = new JLabel( "Time:" );
    this.mi_reference = new JLabel();
    this.mi_period = new JLabel();
    this.mi_frequency = new JLabel();
    this.mi_widthHigh = new JLabel();
    this.mi_widthLow = new JLabel();
    this.mi_dutyCycle = new JLabel();

    this.pci_time = new JLabel();
    this.pci_frequency = new JLabel();
    this.pci_dutyCycle = new JLabel();
    this.pci_pulseCountLabel = new JLabel( "# of pulses:" );
    this.pci_pulseCount = new JLabel();

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
    updateMeasurementInformation( aEvent );

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        repaint( 50L );
      }
    } );
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
   * @param aMeasurementInfo
   * @return
   */
  final void updateMeasurementInformation( final MeasurementInfo aMeasurementInfo )
  {
    String channelId = "-";
    String referenceLabel = "Time:";
    String reference = "-";
    String totalWidth = "-";
    String frequency = "-";
    String pwHigh = "-";
    String pwLow = "-";
    String dc = "-";
    boolean hasTimingData = true;

    if ( aMeasurementInfo != null )
    {
      hasTimingData = aMeasurementInfo.hasTimingData();

      channelId = Integer.toString( aMeasurementInfo.getChannelIndex() );
      if ( aMeasurementInfo.getChannelLabel() != null )
      {
        channelId = channelId.concat( ", " ).concat( aMeasurementInfo.getChannelLabel() );
      }

      reference = formatReference( hasTimingData, aMeasurementInfo.getReferenceTime() );

      if ( hasTimingData )
      {
        totalWidth = formatTime( aMeasurementInfo.getTotalTime() );
        frequency = "XXX";
        pwHigh = formatTime( aMeasurementInfo.getHighTime() );
        pwLow = formatTime( aMeasurementInfo.getLowTime() );
        dc = formatDutyCycle( aMeasurementInfo.getDutyCycle() );
      }
      else
      {
        referenceLabel = "State:";
      }
    }

    this.mi_channel.setText( channelId );
    this.mi_referenceLabel.setText( referenceLabel );
    this.mi_reference.setText( reference );
    this.mi_frequency.setText( frequency );
    this.mi_period.setText( totalWidth );
    this.mi_widthHigh.setText( pwHigh );
    this.mi_widthLow.setText( pwLow );
    this.mi_dutyCycle.setText( dc );
  }

  /**
   * @param aPulseCountInfo
   * @return
   */
  final void updatePulseCountInformation( final PulseCountInfo aPulseCountInfo )
  {
    boolean hasTimingData = true;
    boolean hasPulses = false;

    String timeText = "";
    String frequencyText = "-";
    String dutyCycleText = "-";
    String pulseCountLabel = "# of pulses:";
    String pulseCountText = "-";

    if ( aPulseCountInfo != null )
    {
      hasTimingData = aPulseCountInfo.hasTimingData;
      hasPulses = aPulseCountInfo.getPulseCount() != 0;

      timeText = UnitOfTime.format( aPulseCountInfo.measureTime );
      if ( hasPulses )
      {
        frequencyText = FrequencyUnit.format( aPulseCountInfo.getFrequency() );
        dutyCycleText = String.format( "%.3f%%", Double.valueOf( aPulseCountInfo.getDutyCycle() ) );
      }

      if ( hasTimingData )
      {
        pulseCountText = String.format( "%d (\u2191%d, \u2193%d)", Integer.valueOf( aPulseCountInfo.getPulseCount() ),
            Integer.valueOf( aPulseCountInfo.risingEdgeCount ), Integer.valueOf( aPulseCountInfo.fallingEdgeCount ) );
      }
      else
      {
        pulseCountLabel = "# of states:";
        pulseCountText = String.format( "%d", Integer.valueOf( aPulseCountInfo.getPulseCount() ) );
      }
    }

    if ( hasTimingData )
    {
      this.pci_time.setText( timeText );

      if ( hasPulses )
      {
        this.pci_frequency.setText( frequencyText );
        this.pci_dutyCycle.setText( dutyCycleText );
      }
      else
      {
        this.pci_frequency.setText( "-" );
        this.pci_dutyCycle.setText( "-" );
      }

      this.pci_pulseCountLabel.setText( pulseCountLabel );
      this.pci_pulseCount.setText( pulseCountText );
    }
    else
    {
      this.pci_pulseCountLabel.setText( pulseCountLabel );
      this.pci_pulseCount.setText( pulseCountText );
    }
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
        updateChannelComboBoxModel( MeasurementView.this.measureChannel, aChannels );

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

    Channel channel = ( Channel )this.measureChannel.getSelectedItem();
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
    this.measureChannel = updateChannelComboBoxModel( new JComboBox(), Collections.<Channel> emptyList() );
    this.measureChannel.setRenderer( new ChannelComboBoxRenderer() );
    this.measureChannel.addActionListener( new CursorActionListener() );
    // Make the component a bit smaller and a pop-down on OSX...
    this.measureChannel.putClientProperty( "JComponent.sizeVariant", "small" );
    this.measureChannel.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.measureChannel.setEnabled( false );

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

    setOpaque( false );
    setLayout( new GridBagLayout() );
    setName( "Measurement" );
    setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ) );

    final Font boldFont;

    final Insets defaultInsets = new Insets( 2, 4, 2, 4 );
    final Insets headerInsets = new Insets( 8, 4, 2, 4 );

    GridBagConstraints gbc = new GridBagConstraints( 0, 0, 1, 1, 1.0, 0.0, 0, 0, defaultInsets, 0, 0 );

    // ROW 0 -- HEADER
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;
    gbc.insets = headerInsets;

    final JLabel miHeader = new JLabel( "Measurement" );
    boldFont = miHeader.getFont().deriveFont( Font.BOLD );
    miHeader.setFont( boldFont );

    add( miHeader, gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;
    gbc.fill = GridBagConstraints.HORIZONTAL;

    add( new JSeparator(), gbc );

    // ROW 1
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;
    gbc.fill = GridBagConstraints.NONE;
    gbc.insets = defaultInsets;

    add( new JLabel( "Channel:" ), gbc );

    gbc.gridx = 1;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_channel, gbc );

    // ROW 2
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( this.mi_referenceLabel, gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_reference, gbc );

    // ROW 3
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Period:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_period, gbc );

    // ROW 4
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Frequency:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_frequency, gbc );

    // ROW 5
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Width (H):" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_widthHigh, gbc );

    // ROW 6
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Width (L):" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_widthLow, gbc );

    // ROW 7
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Duty cycle:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.mi_dutyCycle, gbc );

    // ROW 8 -- HEADER
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;
    gbc.insets = headerInsets;

    final JLabel pcHeader = new JLabel( "Pulse counter" );
    pcHeader.setFont( boldFont );

    add( pcHeader, gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;
    gbc.fill = GridBagConstraints.HORIZONTAL;

    add( new JSeparator(), gbc );

    // ROW 9
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.gridwidth = 1;
    gbc.weightx = 0.3;
    gbc.weighty = 0.0;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;
    gbc.fill = GridBagConstraints.NONE;
    gbc.insets = defaultInsets;

    add( new JLabel( "Channel:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.measureChannel, gbc );

    // ROW 10
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Cursor A:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.cursorA, gbc );

    // ROW 11
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Cursor B:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.cursorB, gbc );

    // ROW 12
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( this.pci_pulseCountLabel, gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.pci_pulseCount, gbc );

    // ROW 13
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "\u0394T:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.pci_time, gbc );

    // ROW 14
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Frequency:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.pci_frequency, gbc );

    // ROW 15
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.weightx = 0.3;
    gbc.anchor = GridBagConstraints.BASELINE_TRAILING;

    add( new JLabel( "Duty cycle:" ), gbc );

    gbc.gridx++;
    gbc.weightx = 0.7;
    gbc.anchor = GridBagConstraints.BASELINE_LEADING;

    add( this.pci_dutyCycle, gbc );

    // ROW 16
    gbc.gridx = 0;
    gbc.gridy++;
    gbc.gridwidth = 2;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;
    gbc.anchor = GridBagConstraints.CENTER;
    gbc.fill = GridBagConstraints.HORIZONTAL;

    add( this.indicator, gbc );

    // ROW 17
    gbc.gridy++;
    gbc.anchor = GridBagConstraints.NORTH;
    gbc.fill = GridBagConstraints.BOTH;

    add( new JLabel( " " ), gbc );
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
        MeasurementView.this.measureChannel.setEnabled( aEnabled );
        MeasurementView.this.cursorA.setEnabled( aEnabled );
        MeasurementView.this.cursorB.setEnabled( aEnabled );
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
        if ( ( this.signalMeasurer == null ) || this.signalMeasurer.isDone() )
        {
          this.indicator.setVisible( true );

          Channel channel = ( Channel )MeasurementView.this.measureChannel.getSelectedItem();
          Cursor cursorA = ( Cursor )MeasurementView.this.cursorA.getSelectedItem();
          Cursor cursorB = ( Cursor )MeasurementView.this.cursorB.getSelectedItem();

          this.signalMeasurer = new SignalMeasurer( channel, cursorA, cursorB );
          this.signalMeasurer.execute();
        }
      }
      else
      {
        updatePulseCountInformation( null );
      }
    }
    finally
    {
      this.listening = true;
    }
  }
}
