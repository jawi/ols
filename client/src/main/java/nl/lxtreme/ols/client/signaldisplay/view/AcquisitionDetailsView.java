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
import java.text.*;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a dockable tool window that shows details on the acquisition, like
 * sample rate, sample count, total capture time, etc.
 */
public class AcquisitionDetailsView extends AbstractViewLayer implements IToolWindow, IDataModelChangeListener,
    ZoomListener
{
  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Acquisition";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private volatile String sampleRate = "-";
  private volatile String sampleCount = "-";
  private volatile String totalWidth = "-";
  private volatile String tickInterval = "-";
  private volatile String displayedTime = "-";

  private volatile String scaleFactor = "-";

  private final JLabel captureInfoField;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDetailsView instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  private AcquisitionDetailsView( final SignalDiagramController aController )
  {
    super( aController );

    this.captureInfoField = new JLabel( asText() );
  }

  // METHODS

  /**
   * Factory method to create a new {@link AcquisitionDetailsView} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link AcquisitionDetailsView} instance, never
   *         <code>null</code>.
   */
  public static AcquisitionDetailsView create( final SignalDiagramController aController )
  {
    final AcquisitionDetailsView result = new AcquisitionDetailsView( aController );
    result.initComponent();

    aController.addDataModelChangeListener( result );

    aController.getZoomController().addZoomListener( result );

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final DataSet aDataSet )
  {
    this.sampleRate = "-";
    this.sampleCount = "-";
    this.totalWidth = "-";

    this.tickInterval = null;
    this.displayedTime = null;
    this.scaleFactor = null;

    if ( ( aDataSet != null ) && ( aDataSet.getCapturedData() != null ) )
    {
      final AcquisitionResult model = aDataSet.getCapturedData();

      this.sampleRate = FrequencyUnit.format( model.getSampleRate() );
      this.sampleCount = new DecimalFormat().format( model.getValues().length );
      this.totalWidth = UnitOfTime.format( model.getAbsoluteLength() / ( double )model.getSampleRate() );
    }

    updateView();
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
  public void notifyZoomChange( final ZoomEvent aEvent )
  {
    this.tickInterval = null;
    this.displayedTime = null;

    updateView();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    final SignalDiagramModel model = getController().getSignalDiagramModel();

    if ( this.tickInterval == null )
    {
      final Double timeInterval = model.getTimelineTimeIncrement();
      if ( timeInterval != null )
      {
        this.tickInterval = UnitOfTime.format( timeInterval.doubleValue() / model.getSampleRate() );
      }
      else
      {
        this.tickInterval = "-";
      }
    }

    if ( this.scaleFactor == null )
    {
      final Double scaleFactor = model.getTimelineScale();
      if ( scaleFactor != null )
      {
        this.scaleFactor = UnitOfTime.format( scaleFactor.doubleValue() / model.getSampleRate() );
      }
      else
      {
        this.scaleFactor = "-";
      }
    }

    if ( this.displayedTime == null )
    {
      this.displayedTime = ViewUtils.formatTime( model.getDisplayedTimeInterval() );
    }

    this.captureInfoField.setText( asText() );

    super.paintComponent( aGraphics );
  }

  /**
   * @param aEvent
   * @return
   */
  private String asText()
  {
    final StringBuilder sb = new StringBuilder( "<html><table>" );
    sb.append( "<tr><th align='right'>Sample count:</th><td>" ).append( this.sampleCount ).append( "</td>" );
    sb.append( "<tr><th align='right'>Sample rate:</th><td align='right'>" ).append( this.sampleRate ).append( "</td>" );
    sb.append( "<tr><th align='right'>Sample time:</th><td align='right'>" ).append( this.totalWidth ).append( "</td>" );
    sb.append( "<tr><th align='right'>Displayed time:</th><td align='right'>" ).append( this.displayedTime )
        .append( "</td>" );
    sb.append( "<tr><th align='right'>Tick interval:</th><td align='right'>" ).append( this.tickInterval )
        .append( "</td>" );
    sb.append( "<tr><th align='right'>Timeline scale:</th><td align='right'>" ).append( this.scaleFactor )
        .append( "</td>" );
    sb.append( "</table></html>" );

    return sb.toString();
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Acquisition details" );

    add( this.captureInfoField, BorderLayout.NORTH );
  }

  /**
   * Updates this view by repainting it.
   */
  private void updateView()
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        repaint( 25L );
      }
    } );
  }
}
