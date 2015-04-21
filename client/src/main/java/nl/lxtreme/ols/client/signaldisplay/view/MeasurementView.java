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
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.util.CursorFlagTextFormatter.*;
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
  final class CursorComboBoxRenderer extends DefaultListCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      String text;
      if ( ( aValue != null ) && ( aValue instanceof Cursor ) )
      {
        final Cursor cursor = ( Cursor )aValue;
        text = CursorFlagTextFormatter.getCursorFlagText( getSignalDiagramModel(), cursor, LabelStyle.LABEL_TIME );
      }
      else if ( aValue != null )
      {
        text = String.valueOf( aValue );
      }
      else
      {
        text = "";
      }

      return super.getListCellRendererComponent( aList, text, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Represents a small DTO for measured pulse count information.
   */
  static final class PulseCountInfo
  {
    final Double measureTime;
    final Integer risingEdgeCount;
    final Integer fallingEdgeCount;
    final Integer totalEdgeCount;
    final long totalLowTime;
    final long totalHighTime;
    final Integer pulseCount;
    final int sampleRate;
    final boolean hasTimingData;

    /**
     * Creates a new {@link PulseCountInfo} instance.
     */
    public PulseCountInfo( final double aMeasureTime, final int aRisingEdgeCount, final int aFallingEdgeCount,
        final long aTotalLowTime, final long aTotalHighTime, final int aSampleRate, final boolean aHasTimingData )
    {
      this.measureTime = Double.valueOf( aMeasureTime );
      this.risingEdgeCount = Integer.valueOf( aRisingEdgeCount );
      this.fallingEdgeCount = Integer.valueOf( aFallingEdgeCount );
      this.totalEdgeCount = Integer.valueOf( aRisingEdgeCount + aFallingEdgeCount );
      this.totalLowTime = aTotalLowTime;
      this.totalHighTime = aTotalHighTime;
      this.pulseCount = Integer.valueOf( this.totalEdgeCount.intValue() / 2 );
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
    public Double getFrequency()
    {
      return Double.valueOf( this.sampleRate / ( getAveragePulseHighTime() + getAveragePulseLowTime() ) );
    }

    /**
     * @return
     */
    private double getAveragePulseHighTime()
    {
      return ( this.totalHighTime / this.fallingEdgeCount.doubleValue() );
    }

    /**
     * @return
     */
    private double getAveragePulseLowTime()
    {
      return ( this.totalLowTime / this.risingEdgeCount.doubleValue() );
    }
  }

  /**
   * Does the actual measurement of the signal.
   */
  static final class SignalMeasurer
  {
    // VARIABLES

    private final AcquisitionResult result;
    private final int mask;
    private final long startTimestamp;
    private final long endTimestamp;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SignalMeasurer} instance.
     */
    public SignalMeasurer( final AcquisitionResult aResult, final int aIndex, final long aStartTimestamp,
        final long aEndTimestamp )
    {
      this.result = aResult;
      this.mask = ( 1 << aIndex );
      this.startTimestamp = aStartTimestamp;
      this.endTimestamp = aEndTimestamp;
    }

    // METHODS

    /**
     * Executes the actual measurement.
     *
     * @return the measurement information, never <code>null</code>.
     */
    public PulseCountInfo run()
    {
      // Issue #215 - we need to start *before* the first index and end *before*
      // the last index
      final int startIdx = Math.max( 0, this.result.getSampleIndex( this.startTimestamp ) - 1 );
      final int endIdx = Math.max( 0, this.result.getSampleIndex( this.endTimestamp ) - 1 );

      final boolean hasTimingData = this.result.hasTimingData();

      final int[] values = this.result.getValues();
      final long[] timestamps = this.result.getTimestamps();

      int fallingEdgeCount = 0;
      long highTime = 0;
      int risingEdgeCount = 0;
      long lowTime = 0;

      int i = startIdx;
      long lastTransition = timestamps[i];
      int lastBitValue = values[i++] & this.mask;

      for ( ; !Thread.currentThread().isInterrupted() && ( i <= endIdx ); i++ )
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

      final double measureTime = Math.abs( ( this.endTimestamp - this.startTimestamp )
          / ( double )this.result.getSampleRate() );

      return new PulseCountInfo( measureTime, risingEdgeCount, fallingEdgeCount, lowTime, highTime,
          this.result.getSampleRate(), hasTimingData );
    }
  }

  /**
   * Provides a {@link SwingWorker} to measure the frequency, dutycycle and such
   * asynchronously from the UI.
   */
  final class SignalMeasurerWorker extends SwingWorker<PulseCountInfo, Boolean>
  {
    // VARIABLES

    private final int index;
    private final long startTimestamp;
    private final long endTimestamp;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SignalMeasurerWorker} instance.
     *
     * @param aChannel
     *          the channel to measure;
     * @param aCursorA
     *          the cursor denoting the start of measurement;
     * @param aCursorB
     *          the cursor denoting the end of measurement.
     */
    public SignalMeasurerWorker( final Channel aChannel, final Cursor aCursorA, final Cursor aCursorB )
    {
      this.index = aChannel.getIndex();
      this.startTimestamp = aCursorA != null ? aCursorA.getTimestamp() : -1L;
      this.endTimestamp = aCursorB != null ? aCursorB.getTimestamp() : -1L;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected PulseCountInfo doInBackground() throws Exception
    {
      final SignalDiagramModel model = getSignalDiagramModel();

      long start = this.startTimestamp;
      if ( start < 0L )
      {
        start = model.getTimestamps()[0];
      }
      long end = this.endTimestamp;
      if ( end < 0L )
      {
        end = model.getAbsoluteLength();
      }

      return new SignalMeasurer( model.getCapturedData(), this.index, start, end ).run();
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

  private JCheckBox enableMeasurementMode;
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

  private final JLabel pci_timeLabel;
  private final JLabel pci_time;
  private final JLabel pci_frequency;
  private final JLabel pci_dutyCycle;
  private final JLabel pci_pulseCountLabel;
  private final JLabel pci_pulseCount;

  private volatile boolean listening;
  private volatile SignalMeasurerWorker signalMeasurerWorker;

  private final List<Component> comps;

  // CONSTRUCTORS

  /**
   * Creates a new MeasurementView instance.
   *
   * @param aController
   */
  public MeasurementView( final SignalDiagramController aController )
  {
    super( aController );

    this.comps = new ArrayList<Component>();

    this.mi_channel = new JLabel();
    this.mi_referenceLabel = new JLabel( "Time:" );
    this.mi_referenceLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.mi_reference = new JLabel();
    this.mi_period = new JLabel();
    this.mi_frequency = new JLabel();
    this.mi_widthHigh = new JLabel();
    this.mi_widthLow = new JLabel();
    this.mi_dutyCycle = new JLabel();

    this.pci_timeLabel = new JLabel();
    this.pci_timeLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.pci_time = new JLabel();
    this.pci_frequency = new JLabel();
    this.pci_dutyCycle = new JLabel();
    this.pci_pulseCountLabel = new JLabel( "# of pulses:" );
    this.pci_pulseCountLabel.setHorizontalAlignment( SwingConstants.RIGHT );
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
   * @return a signal diagram model, never <code>null</code>.
   */
  final SignalDiagramModel getSignalDiagramModel()
  {
    return getController().getViewModel();
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
        frequency = formatPeriodAsFrequency( aMeasurementInfo.getTotalTime() );
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

    String timeTextLabel = "\u0394T (B-A):";
    String timeText = "-";
    String frequencyText = "-";
    String dutyCycleText = "-";
    String pulseCountLabel = "Pulses:";
    String pulseCountText = "-";

    // Issue #150: use alternative characters to denote rising/falling edges...
    String upArrow = "\u02C4"; // \u2191 is not supported on Windows
    String downArrow = "\u02C5"; // \u2193 is not supported on Windows

    if ( aPulseCountInfo != null )
    {
      hasTimingData = aPulseCountInfo.hasTimingData;
      hasPulses = aPulseCountInfo.pulseCount.intValue() != 0;

      if ( hasTimingData )
      {
        timeText = formatTime( aPulseCountInfo.measureTime );

        pulseCountText = String.format( "%d (%s%d, %s%d)", aPulseCountInfo.pulseCount, upArrow,
            aPulseCountInfo.risingEdgeCount, downArrow, aPulseCountInfo.fallingEdgeCount );

        if ( hasPulses )
        {
          frequencyText = formatFrequency( aPulseCountInfo.getFrequency() );
          dutyCycleText = formatDutyCycle( Double.valueOf( aPulseCountInfo.getDutyCycle() ) );
        }
      }
      else
      {
        timeTextLabel = "\u0394S (B-A):";
        timeText = Integer.toString( aPulseCountInfo.measureTime.intValue() );

        pulseCountLabel = "Transitions:";
        pulseCountText = String.format( "%d", aPulseCountInfo.pulseCount );
        pulseCountText = String.format( "%d (%s%d, %s%d)", aPulseCountInfo.totalEdgeCount, upArrow,
            aPulseCountInfo.risingEdgeCount, downArrow, aPulseCountInfo.fallingEdgeCount );
      }
    }

    this.pci_timeLabel.setText( timeTextLabel );
    this.pci_time.setText( timeText );

    this.pci_frequency.setText( frequencyText );
    this.pci_dutyCycle.setText( dutyCycleText );

    this.pci_pulseCountLabel.setText( pulseCountLabel );
    this.pci_pulseCount.setText( pulseCountText );
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

        updatePulseCountInformation( null );

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

        updatePulseCountInformation( null );

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
    if ( !getController().getViewModel().isMeasurementMode() )
    {
      return false;
    }

    Channel channel = ( Channel )this.measureChannel.getSelectedItem();
    if ( ( channel == null ) || !channel.isEnabled() )
    {
      return false;
    }

    Cursor selectedCursorA = ( Cursor )this.cursorA.getSelectedItem();
    if ( ( selectedCursorA != null ) && !selectedCursorA.isDefined() )
    {
      return false;
    }

    Cursor selectedCursorB = ( Cursor )this.cursorB.getSelectedItem();
    if ( ( selectedCursorB != null ) && !selectedCursorB.isDefined() )
    {
      return false;
    }

    if ( ( selectedCursorA != null ) && ( selectedCursorB != null ) )
    {
      return selectedCursorA != selectedCursorB;
    }

    return true;
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
    IActionManager actionManager = getController().getActionManager();
    Action enableMeasurementModeAction = actionManager.getAction( SetMeasurementModeAction.ID );

    this.enableMeasurementMode = new JCheckBox( enableMeasurementModeAction );
    this.enableMeasurementMode.setText( "" );

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
    setLayout( new BorderLayout() );
    setName( "Measurement" );

    JPanel panel = new JPanel( new SpringLayout() );

    // ROW 0 -- HEADER
    SpringLayoutUtils.addSeparator( panel, "Measurement" );

    panel.add( createRightAlignedLabel( "Enabled" ) );
    panel.add( this.enableMeasurementMode );

    this.comps.add( panel.add( createRightAlignedLabel( "Channel:" ) ) );
    this.comps.add( panel.add( this.mi_channel ) );

    this.comps.add( panel.add( this.mi_referenceLabel ) );
    this.comps.add( panel.add( this.mi_reference ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Period:" ) ) );
    this.comps.add( panel.add( this.mi_period ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Frequency:" ) ) );
    this.comps.add( panel.add( this.mi_frequency ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Width (H):" ) ) );
    this.comps.add( panel.add( this.mi_widthHigh ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Width (L):" ) ) );
    this.comps.add( panel.add( this.mi_widthLow ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Duty cycle:" ) ) );
    this.comps.add( panel.add( this.mi_dutyCycle ) );

    // ROW 8 -- HEADER
    SpringLayoutUtils.addSeparator( panel, "Pulse counter" );

    this.comps.add( panel.add( createRightAlignedLabel( "Channel:" ) ) );
    this.comps.add( panel.add( this.measureChannel ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Cursor A:" ) ) );
    this.comps.add( panel.add( this.cursorA ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Cursor B:" ) ) );
    this.comps.add( panel.add( this.cursorB ) );

    this.comps.add( panel.add( this.pci_pulseCountLabel ) );
    this.comps.add( panel.add( this.pci_pulseCount ) );

    this.comps.add( panel.add( this.pci_timeLabel ) );
    this.comps.add( panel.add( this.pci_time ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Frequency:" ) ) );
    this.comps.add( panel.add( this.pci_frequency ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Duty cycle:" ) ) );
    this.comps.add( panel.add( this.pci_dutyCycle ) );

    panel.add( new JLabel( "" ) );
    panel.add( this.indicator );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 10 );

    // Synchronize model state with UI state...
    setState( this.enableMeasurementMode.isSelected() );

    updateMeasurementInformation( null );
    updatePulseCountInformation( null );

    add( panel, BorderLayout.NORTH );
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
        for ( Component label : MeasurementView.this.comps )
        {
          label.setEnabled( aEnabled );
        }
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
        if ( ( this.signalMeasurerWorker == null ) || this.signalMeasurerWorker.isDone() )
        {
          this.indicator.setVisible( true );

          Channel channel = ( Channel )MeasurementView.this.measureChannel.getSelectedItem();
          Cursor cursorA = ( Cursor )MeasurementView.this.cursorA.getSelectedItem();
          Cursor cursorB = ( Cursor )MeasurementView.this.cursorB.getSelectedItem();

          this.signalMeasurerWorker = new SignalMeasurerWorker( channel, cursorA, cursorB );
          this.signalMeasurerWorker.execute();
        }
      }
    }
    finally
    {
      this.listening = true;
    }
  }
}
