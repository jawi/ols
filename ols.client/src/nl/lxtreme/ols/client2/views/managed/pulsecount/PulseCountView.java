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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.pulsecount;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.event.*;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the current measurement information.
 */
public class PulseCountView extends AbstractManagedView implements EventHandler
{
  // CONSTANTS

  public static final String ID = "PulseCounter";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final List<Component> cursorComps;
  private final List<Component> otherComps;

  private JComboBox measureChannel;
  private JComboBox cursorA;
  private JComboBox cursorB;
  private JBusyIndicator indicator;

  private JLabel pci_timeLabel;
  private JLabel pci_time;
  private JLabel pci_frequency;
  private JLabel pci_dutyCycle;
  private JLabel pci_pulseCountLabel;
  private JLabel pci_pulseCount;
  private JLabel pci_risingEdgeCount;
  private JLabel pci_fallingEdgeCount;

  private volatile SignalMeasurerWorker signalMeasurerWorker;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PulseCountView} instance.
   */
  public PulseCountView()
  {
    super( ID );

    this.cursorComps = new ArrayList<Component>();
    this.otherComps = new ArrayList<Component>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeNotify()
  {
    setAcquiredData( null );

    super.removeNotify();
  }

  /**
   * Starts a pulse count, if not already busy doing so.
   */
  final void startPulseCount()
  {
    Channel channel = ( Channel )this.measureChannel.getSelectedItem();

    Cursor cursorA = null;
    Object selectedItem = this.cursorA.getSelectedItem();
    if ( selectedItem instanceof Cursor )
    {
      cursorA = ( Cursor )selectedItem;
    }

    Cursor cursorB = null;
    selectedItem = this.cursorB.getSelectedItem();
    if ( selectedItem instanceof Cursor )
    {
      cursorB = ( Cursor )selectedItem;
    }

    if ( canPerformMeasurement() )
    {
      if ( ( this.signalMeasurerWorker == null ) || this.signalMeasurerWorker.isDone() )
      {
        this.indicator.setVisible( true );

        this.signalMeasurerWorker = new SignalMeasurerWorker( this, getAcquiredData(), channel, cursorA, cursorB );
        this.signalMeasurerWorker.execute();
      }
    }
    else
    {
      updatePulseCountInformation( null );
    }
  }

  /**
   * @param aPulseCountInfo
   * @return
   */
  final void updatePulseCountInformation( PulseCountInfo aPulseCountInfo )
  {
    boolean hasTimingData = true;
    boolean hasPulses = false;

    String timeTextLabel = "\u0394T (B-A):";
    String timeText = "-";
    String frequencyText = "-";
    String dutyCycleText = "-";
    String pulseCountLabel = "Pulses:";
    String risingCountText = "-";
    String fallingCountText = "-";
    String pulseCountText = "-";

    if ( aPulseCountInfo != null )
    {
      hasTimingData = aPulseCountInfo.hasTimingData;
      hasPulses = aPulseCountInfo.pulseCount.intValue() != 0;

      pulseCountText = Integer.toString( aPulseCountInfo.pulseCount );
      risingCountText = Integer.toString( aPulseCountInfo.risingEdgeCount );
      fallingCountText = Integer.toString( aPulseCountInfo.fallingEdgeCount );

      if ( hasTimingData )
      {
        timeText = formatTime( aPulseCountInfo.measureTime );

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
      }
    }

    this.pci_timeLabel.setText( timeTextLabel );
    this.pci_time.setText( timeText );

    this.pci_frequency.setText( frequencyText );
    this.pci_dutyCycle.setText( dutyCycleText );

    this.pci_pulseCountLabel.setText( pulseCountLabel );
    this.pci_pulseCount.setText( pulseCountText );
    this.pci_risingEdgeCount.setText( risingCountText );
    this.pci_fallingEdgeCount.setText( fallingCountText );

    repaint( 50L );

    this.indicator.setVisible( false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void build()
  {
    JPanel panel = new JPanel( new SpringLayout() );

    this.otherComps.add( panel.add( createRightAlignedLabel( "Channel:" ) ) );
    this.otherComps.add( panel.add( this.measureChannel ) );

    this.cursorComps.add( panel.add( createRightAlignedLabel( "Cursor A:" ) ) );
    this.cursorComps.add( panel.add( this.cursorA ) );

    this.cursorComps.add( panel.add( createRightAlignedLabel( "Cursor B:" ) ) );
    this.cursorComps.add( panel.add( this.cursorB ) );

    SpringLayoutUtils.addSeparator( panel, "Pulse count" );

    this.otherComps.add( panel.add( this.pci_timeLabel ) );
    this.otherComps.add( panel.add( this.pci_time ) );

    this.otherComps.add( panel.add( createRightAlignedLabel( "Frequency:" ) ) );
    this.otherComps.add( panel.add( this.pci_frequency ) );

    this.otherComps.add( panel.add( createRightAlignedLabel( "Duty cycle:" ) ) );
    this.otherComps.add( panel.add( this.pci_dutyCycle ) );

    this.otherComps.add( panel.add( this.pci_pulseCountLabel ) );
    this.otherComps.add( panel.add( this.pci_pulseCount ) );

    this.otherComps.add( panel.add( createRightAlignedLabel( "Rising:" ) ) );
    this.otherComps.add( panel.add( this.pci_risingEdgeCount ) );

    this.otherComps.add( panel.add( createRightAlignedLabel( "Falling:" ) ) );
    this.otherComps.add( panel.add( this.pci_fallingEdgeCount ) );

    panel.add( new JLabel( "" ) );
    panel.add( this.indicator );

    makeEditorGrid( panel );

    updatePulseCountInformation( null );

    add( panel, BorderLayout.NORTH );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Pulse count" );

    this.pci_timeLabel = new JLabel();
    this.pci_timeLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.pci_time = new JLabel();
    this.pci_frequency = new JLabel();
    this.pci_dutyCycle = new JLabel();
    this.pci_pulseCountLabel = new JLabel( "# of pulses:" );
    this.pci_pulseCountLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.pci_pulseCount = new JLabel();
    this.pci_risingEdgeCount = new JLabel();
    this.pci_fallingEdgeCount = new JLabel();

    final ActionListener actionListener = new ActionListener()
    {
      @Override
      public void actionPerformed( ActionEvent aEvent )
      {
        startPulseCount();
      }
    };

    this.measureChannel = updateChannelComboBoxModel( new JComboBox() );
    this.measureChannel.setRenderer( new ChannelComboBoxRenderer() );
    this.measureChannel.addActionListener( actionListener );
    // Make the component a bit smaller and a pop-down on OSX...
    this.measureChannel.putClientProperty( "JComponent.sizeVariant", "small" );
    this.measureChannel.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.measureChannel.setEnabled( false );

    this.cursorA = updateCursorComboBoxModel( new JComboBox() );
    this.cursorA.setRenderer( new CursorComboBoxRenderer() );
    this.cursorA.setSelectedIndex( 0 );
    this.cursorA.addActionListener( actionListener );
    // Make the component a bit smaller and a pop-down on OSX...
    this.cursorA.putClientProperty( "JComponent.sizeVariant", "small" );
    this.cursorA.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.cursorA.setEnabled( false );

    this.cursorB = updateCursorComboBoxModel( new JComboBox() );
    this.cursorB.setRenderer( new CursorComboBoxRenderer() );
    this.cursorB.setSelectedIndex( 0 );
    this.cursorB.addActionListener( actionListener );
    // Make the component a bit smaller and a pop-down on OSX...
    this.cursorB.putClientProperty( "JComponent.sizeVariant", "small" );
    this.cursorB.putClientProperty( "JComboBox.isPopDown", Boolean.TRUE );
    this.cursorB.setEnabled( false );

    this.indicator = new JBusyIndicator();
    this.indicator.setVisible( false );

    aFrame.setInitIndex( 0 );

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    if ( aController != null )
    {
      setCursorState( aData.areCursorsVisible() );
      setOtherState( true );

      updateChannelModel( aData.getChannels() );
      updateCursorModels( aData.getCursors() );

      setAcquiredData( aData );
    }
    else
    {
      setCursorState( false );
      setOtherState( false );

      updateChannelModel();
      updateCursorModels();

      setAcquiredData( null );
      updatePulseCountInformation( null );
    }
  }

  /**
   * Updates the channel model to the current list of channels.
   */
  protected void updateChannelModel( final Channel... aChannels )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        updateChannelComboBoxModel( PulseCountView.this.measureChannel, aChannels );

        updatePulseCountInformation( null );
        startPulseCount();

        repaint( 50L );
      }
    } );
  }

  /**
   * Updates both combobox models to the current list of available cursors.
   */
  protected void updateCursorModels( final Cursor... aCursors )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        updateCursorComboBoxModel( PulseCountView.this.cursorA, aCursors );
        updateCursorComboBoxModel( PulseCountView.this.cursorB, aCursors );

        updatePulseCountInformation( null );
        startPulseCount();

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
    Channel channel = ( Channel )this.measureChannel.getSelectedItem();
    if ( ( channel == null ) || !channel.isEnabled() )
    {
      return false;
    }

    Object selectedCursorA = this.cursorA.getSelectedItem();
    if ( selectedCursorA == null )
    {
      return false;
    }

    Object selectedCursorB = this.cursorB.getSelectedItem();
    if ( selectedCursorB == null )
    {
      return false;
    }

    int idxA = 0;
    if ( selectedCursorA instanceof Cursor )
    {
      idxA = ( ( Cursor )selectedCursorA ).getIndex() + 1;
    }

    int idxB = 0;
    if ( selectedCursorB instanceof Cursor )
    {
      idxB = ( ( Cursor )selectedCursorB ).getIndex() + 1;
    }

    return ( idxB - idxA ) > 0 || ( idxA == 0 ) || ( idxB == 0 );
  }

  private AcquisitionData getAcquiredData()
  {
    return ( AcquisitionData )getClientProperty( "data" );
  }

  private void setAcquiredData( AcquisitionData aData )
  {
    putClientProperty( "data", aData );
  }

  /**
   * Enables/disables the various components on this view.
   */
  private void setCursorState( final boolean aEnabled )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        if ( !aEnabled )
        {
          cursorA.setSelectedIndex( 0 );
          cursorB.setSelectedIndex( 0 );
        }

        for ( Component comp : cursorComps )
        {
          comp.setEnabled( aEnabled );
        }
      }
    } );
  }

  /**
   * Enables/disables the various components on this view.
   */
  private void setOtherState( final boolean aEnabled )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        for ( Component label : otherComps )
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
  private JComboBox updateChannelComboBoxModel( JComboBox aComboBox, Channel... aChannels )
  {
    ComboBoxModel model = new DefaultComboBoxModel( new Vector<Channel>( Arrays.asList( aChannels ) ) );

    Object oldSelectedItem = aComboBox.getSelectedItem();
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
  private JComboBox updateCursorComboBoxModel( JComboBox aComboBox, Cursor... aCursors )
  {
    Vector<Object> cursors = new Vector<Object>();
    cursors.add( 0, "<none>" );

    for ( Cursor cursor : aCursors )
    {
      if ( cursor.isDefined() )
      {
        cursors.add( cursor );
      }
    }

    ComboBoxModel model = new DefaultComboBoxModel( cursors );

    Object oldSelectedItem = aComboBox.getSelectedItem();
    aComboBox.setModel( model );
    aComboBox.setSelectedItem( oldSelectedItem );

    return aComboBox;
  }
}
