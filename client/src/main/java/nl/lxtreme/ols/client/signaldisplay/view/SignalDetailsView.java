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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.util.swing.*;

import static nl.lxtreme.ols.client.signaldisplay.view.ViewUtils.*;


/**
 * 
 */
public class SignalDetailsView extends AbstractViewLayer implements IToolWindow, IMeasurementListener
{
  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Signal";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JLabel measureInfoField;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDetailsView instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  private SignalDetailsView( final SignalDiagramController aController )
  {
    super( aController );

    this.measureInfoField = new JLabel();

    initComponent();
  }

  // METHODS

  /**
   * Factory method to create a new {@link SignalDetailsView} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link SignalDetailsView} instance, never <code>null</code>.
   */
  public static SignalDetailsView create( final SignalDiagramController aController )
  {
    final SignalDetailsView result = new SignalDetailsView( aController );

    aController.addMeasurementListener( result );

    return result;
  }

  /**
   * @param aMeasurementInfo
   * @return
   */
  private static String asText( final MeasurementInfo aMeasurementInfo )
  {
    String channelIdx = "", channelLabel = "";
    String reference = "-", totalWidth = "-", pwHigh = "-", pwLow = "-", dc = "-";
    boolean hasTimingData = true;

    if ( aMeasurementInfo != null )
    {
      hasTimingData = aMeasurementInfo.hasTimingData();

      channelLabel = aMeasurementInfo.getChannelLabel();
      channelIdx = Integer.toString( aMeasurementInfo.getChannelIndex() );
      totalWidth = formatTime( aMeasurementInfo.getTotalTime() );
      pwHigh = formatTime( aMeasurementInfo.getHighTime() );
      pwLow = formatTime( aMeasurementInfo.getLowTime() );
      dc = formatDutyCycle( aMeasurementInfo.getDutyCycle() );
      reference = formatReference( hasTimingData, aMeasurementInfo.getReferenceTime() );
    }

    final StringBuilder sb = new StringBuilder( "<html><table>" );
    sb.append( "<tr><th align='right'>Channel:</th><td>" );
    sb.append( channelIdx );
    if ( channelLabel != null )
    {
      sb.append( ", " ).append( channelLabel );
    }
    sb.append( "</td>" );
    if ( hasTimingData )
    {
      sb.append( "<tr><th align='right'>Time:</th><td>" ).append( reference ).append( "</td>" );
      sb.append( "<tr><th align='right'>Period:</th><td>" ).append( totalWidth ).append( "</td>" );
      sb.append( "<tr><th align='right'>Width (H):</th><td>" ).append( pwHigh ).append( "</td>" );
      sb.append( "<tr><th align='right'>Width (L):</th><td>" ).append( pwLow ).append( "</td>" );
      sb.append( "<tr><th align='right'>Duty cycle:</th><td>" ).append( dc ).append( "</td>" );
    }
    else
    {
      sb.append( "<tr><th align='right'>State:</th><td>" ).append( reference ).append( "</td>" );
    }
    sb.append( "</table></html>" );

    return sb.toString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void disableMeasurementMode()
  {
    updateViewText( null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void enableMeasurementMode()
  {
    updateViewText( null );
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
  public void handleMeasureEvent( final MeasurementInfo aEvent )
  {
    updateViewText( aEvent );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isListening()
  {
    return true;
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Signal details" );

    add( this.measureInfoField, BorderLayout.NORTH );

    updateViewText( null );
  }

  /**
   * Updates this view's text and schedules it for a repaint job.
   */
  private void updateViewText( final MeasurementInfo aEvent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        SignalDetailsView.this.measureInfoField.setText( asText( aEvent ) );
        repaint( 50L );
      }
    } );
  }
}
