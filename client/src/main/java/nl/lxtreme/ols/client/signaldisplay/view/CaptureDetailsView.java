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


import static nl.lxtreme.ols.util.DisplayUtils.*;

import java.awt.*;
import java.beans.*;
import java.text.*;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * 
 */
public class CaptureDetailsView extends AbstractViewLayer implements IToolWindow, IDataModelChangeListener,
    PropertyChangeListener
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

  private final JLabel captureInfoField;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDetailsView instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  private CaptureDetailsView( final SignalDiagramController aController )
  {
    super( aController );

    this.captureInfoField = new JLabel( asText() );
  }

  // METHODS

  /**
   * Factory method to create a new {@link CaptureDetailsView} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link CaptureDetailsView} instance, never <code>null</code>.
   */
  public static CaptureDetailsView create( final SignalDiagramController aController )
  {
    final CaptureDetailsView result = new CaptureDetailsView( aController );
    result.initComponent();

    aController.addDataModelChangeListener( result );
    aController.addPropertyChangeListener( result );

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

    if ( aDataSet != null )
    {
      final AcquisitionResult model = aDataSet.getCapturedData();

      this.sampleRate = displayFrequency( model.getSampleRate() );
      this.sampleCount = new DecimalFormat().format( model.getValues().length );
      this.totalWidth = displayTime( model.getAbsoluteLength() / ( double )model.getSampleRate() );
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
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    final String name = aEvent.getPropertyName();
    if ( "zoomFactor".equals( name ) )
    {
      this.tickInterval = null;
      this.displayedTime = null;

      updateView();
    }
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
      this.tickInterval = displayTime( model.getTimeInterval() );
    }
    if ( this.displayedTime == null )
    {
      this.displayedTime = displayTime( model.getDisplayedTimeInterval() );
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
    sb.append( "<tr><th align='right'>Tick interval:</th><td align='right'>" ).append( this.tickInterval )
        .append( "</td>" );
    sb.append( "<tr><th align='right'>Displayed time:</th><td align='right'>" ).append( this.displayedTime )
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
