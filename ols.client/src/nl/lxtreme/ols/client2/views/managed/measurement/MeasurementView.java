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
package nl.lxtreme.ols.client2.views.managed.measurement;


import static nl.lxtreme.ols.client2.ClientConstants.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.views.MeasurementInfoBuilder.MeasurementInfo;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.event.Event;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the current measurement information.
 */
public class MeasurementView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "MeasurementDetails";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final List<Component> comps;
  // Injected by Felix DM...
  private volatile ActionManager actionManager;

  private JCheckBox enableMeasurementMode;
  private JCheckBox measurementFrozen;
  private JLabel channel;
  private JLabel referenceLabel;
  private JLabel reference;
  private JLabel period;
  private JLabel frequency;
  private JLabel widthHigh;
  private JLabel widthLow;
  private JLabel dutyCycle;

  // CONSTRUCTORS

  /**
   * Creates a new {@link MeasurementView} instance.
   */
  public MeasurementView()
  {
    super( ID );

    this.comps = new ArrayList<Component>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected void build()
  {
    JPanel panel = new JPanel( new SpringLayout() );

    // ROW 0 -- HEADER
    panel.add( createRightAlignedLabel( "Enabled" ) );
    panel.add( this.enableMeasurementMode );

    this.comps.add( panel.add( createRightAlignedLabel( "Frozen?" ) ) );
    panel.add( this.measurementFrozen );

    this.comps.add( panel.add( createRightAlignedLabel( "Channel:" ) ) );
    this.comps.add( panel.add( this.channel ) );

    this.comps.add( panel.add( this.referenceLabel ) );
    this.comps.add( panel.add( this.reference ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Period:" ) ) );
    this.comps.add( panel.add( this.period ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Frequency:" ) ) );
    this.comps.add( panel.add( this.frequency ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Width (H):" ) ) );
    this.comps.add( panel.add( this.widthHigh ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Width (L):" ) ) );
    this.comps.add( panel.add( this.widthLow ) );

    this.comps.add( panel.add( createRightAlignedLabel( "Duty cycle:" ) ) );
    this.comps.add( panel.add( this.dutyCycle ) );

    makeEditorGrid( panel );

    // Synchronize model state with UI state...
    setState( this.enableMeasurementMode.isSelected() );

    add( panel, BorderLayout.NORTH );

    updateMeasurementInformation( null, false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Measurement" );

    this.channel = new JLabel();
    this.referenceLabel = new JLabel( "Time:" );
    this.referenceLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.reference = new JLabel();
    this.period = new JLabel();
    this.frequency = new JLabel();
    this.widthHigh = new JLabel();
    this.widthLow = new JLabel();
    this.dutyCycle = new JLabel();

    this.measurementFrozen = new JCheckBox( "" );
    this.measurementFrozen.setEnabled( false );
    this.measurementFrozen.setSelected( false );

    ManagedAction setMeasurementModeAction = this.actionManager.getAction( SetMeasurementModeAction.ID );

    this.enableMeasurementMode = new JCheckBox( setMeasurementModeAction );
    this.enableMeasurementMode.setText( "" );
    this.enableMeasurementMode.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( ActionEvent aEvent )
      {
        AbstractButton source = ( AbstractButton )aEvent.getSource();
        setState( source.isSelected() );
      }
    } );

    aFrame.setInitIndex( 0 );
    try
    {
      aFrame.setActive( Boolean.TRUE.equals( setMeasurementModeAction.getValue( Action.SELECTED_KEY ) ) );
    }
    catch ( PropertyVetoException exception )
    {
      // Ignore, we just wanted to give a hint that we should be active...
    }

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    setState( aController != null );
    if ( aController == null )
    {
      updateMeasurementInformation( null, false );
    }
    else
    {
      ViewModel model = aController.getModel();

      updateMeasurementInformation( model.getMeasurementInfo(), model.isMeasurementFrozen() );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String[] getEventTopics()
  {
    return new String[] { TOPIC_CLIENT_STATE.concat( "/*" ), TOPIC_MEASUREMENTS };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected boolean handleEvent( String aTopic, Event aEvent )
  {
    if ( TOPIC_MEASUREMENTS.equals( aTopic ) )
    {
      final MeasurementInfo info = ( MeasurementInfo )aEvent.getProperty( "measurement" );
      final boolean frozen = Boolean.TRUE.equals( aEvent.getProperty( "frozen" ) );

      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          setState( true );

          updateMeasurementInformation( info, frozen );
        }
      } );

      return true;
    }

    return false;
  }

  /**
   * Enables/disables the various components on this view.
   */
  private void setState( final boolean aEnabled )
  {
    for ( Component label : MeasurementView.this.comps )
    {
      label.setEnabled( aEnabled );
    }
  }

  /**
   * @param aMeasurementInfo
   * @return
   */
  private void updateMeasurementInformation( MeasurementInfo aMeasurementInfo, boolean aFrozen )
  {
    String channelId = "-";
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
      Channel channel = aMeasurementInfo.getChannel();

      channelId = Integer.toString( channel.getIndex() );
      if ( channel.getLabel() != null )
      {
        channelId = channelId.concat( ", " ).concat( channel.getLabel() );
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
    }

    this.measurementFrozen.setSelected( aFrozen );

    this.channel.setText( channelId );
    this.referenceLabel.setText( hasTimingData ? "Time:" : "State:" );
    this.reference.setText( reference );
    this.frequency.setText( frequency );
    this.period.setText( totalWidth );
    this.widthHigh.setText( pwHigh );
    this.widthLow.setText( pwLow );
    this.dutyCycle.setText( dc );
  }
}
