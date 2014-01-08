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


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
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
  private JLabel mi_channel;
  private JLabel mi_referenceLabel;
  private JLabel mi_reference;
  private JLabel mi_period;
  private JLabel mi_frequency;
  private JLabel mi_widthHigh;
  private JLabel mi_widthLow;
  private JLabel mi_dutyCycle;

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

    makeEditorGrid( panel );

    // Synchronize model state with UI state...
    setState( this.enableMeasurementMode.isSelected() );

    add( panel, BorderLayout.NORTH );

    updateMeasurementInformation( null );
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

    this.mi_channel = new JLabel();
    this.mi_referenceLabel = new JLabel( "Time:" );
    this.mi_referenceLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.mi_reference = new JLabel();
    this.mi_period = new JLabel();
    this.mi_frequency = new JLabel();
    this.mi_widthHigh = new JLabel();
    this.mi_widthLow = new JLabel();
    this.mi_dutyCycle = new JLabel();

    this.enableMeasurementMode = new JCheckBox( this.actionManager.getAction( SetMeasurementModeAction.ID ) );
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

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    // NOP
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected boolean handleEvent( String aTopic, Event aEvent )
  {
    // TODO handle measurement events
    return false;
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
   * @param aMeasurementInfo
   * @return
   */
  private void updateMeasurementInformation( final Object aMeasurementInfo )
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
      // hasTimingData = aMeasurementInfo.hasTimingData();
      //
      // channelId = Integer.toString( aMeasurementInfo.getChannelIndex() );
      // if ( aMeasurementInfo.getChannelLabel() != null )
      // {
      // channelId = channelId.concat( ", " ).concat(
      // aMeasurementInfo.getChannelLabel() );
      // }
      //
      // reference = formatReference( hasTimingData,
      // aMeasurementInfo.getReferenceTime() );
      //
      // if ( hasTimingData )
      // {
      // totalWidth = formatTime( aMeasurementInfo.getTotalTime() );
      // frequency = formatPeriodAsFrequency( aMeasurementInfo.getTotalTime() );
      // pwHigh = formatTime( aMeasurementInfo.getHighTime() );
      // pwLow = formatTime( aMeasurementInfo.getLowTime() );
      // dc = formatDutyCycle( aMeasurementInfo.getDutyCycle() );
      // }
    }

    this.mi_channel.setText( channelId );
    this.mi_referenceLabel.setText( hasTimingData ? "Time:" : "State:" );
    this.mi_reference.setText( reference );
    this.mi_frequency.setText( frequency );
    this.mi_period.setText( totalWidth );
    this.mi_widthHigh.setText( pwHigh );
    this.mi_widthLow.setText( pwLow );
    this.mi_dutyCycle.setText( dc );
  }
}
