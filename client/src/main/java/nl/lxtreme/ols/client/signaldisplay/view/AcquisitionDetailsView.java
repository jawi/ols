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


import static nl.lxtreme.ols.client.signaldisplay.view.ViewUtils.*;

import java.awt.*;
import java.text.*;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomEvent;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomListener;
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

  private final JLabel sampleRate;
  private final JLabel sampleCount;
  private final JLabel totalWidth;
  private final JLabel displayedTimeLabel;
  private final JLabel displayedTime;
  private final JLabel secondsPerPixel;
  private final JLabel unitOfTime;

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

    this.sampleRate = new JLabel( "-" );
    this.sampleCount = new JLabel( "-" );
    this.totalWidth = new JLabel( "-" );
    this.displayedTimeLabel = new JLabel( "" );
    this.displayedTimeLabel.setHorizontalAlignment( SwingConstants.RIGHT );
    this.displayedTime = new JLabel( "-" );
    this.secondsPerPixel = new JLabel( "-" );
    this.unitOfTime = new JLabel( "-" );
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
    final String srText;
    final String scText;
    final String twText;

    if ( ( aDataSet != null ) && ( aDataSet.getCapturedData() != null ) )
    {
      final AcquisitionResult model = aDataSet.getCapturedData();

      if ( model.hasTimingData() )
      {
        final double sr = model.getSampleRate();
        final double tw = model.getAbsoluteLength() / sr;

        srText = formatFrequency( Double.valueOf( sr ) );
        twText = formatTime( Double.valueOf( tw ) );
      }
      else
      {
        srText = "n/a";
        twText = "n/a";
      }

      scText = new DecimalFormat().format( model.getValues().length );

    }
    else
    {
      srText = "-";
      scText = "-";
      twText = "-";
    }

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      public void run()
      {
        AcquisitionDetailsView.this.sampleRate.setText( srText );
        AcquisitionDetailsView.this.sampleCount.setText( scText );
        AcquisitionDetailsView.this.totalWidth.setText( twText );

        repaint( 25L );
      };
    } );
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

    final SignalDiagramModel model = getController().getViewModel();

    final Double dt = model.getDisplayedTimeInterval();
    final String dtText, dtTextLabel;

    if ( model.hasTimingData() )
    {
      dtTextLabel = "Displayed time:";
    }
    else
    {
      dtTextLabel = "Displayed states:";
    }

    if ( dt != null )
    {
      dtText = formatReference( model.hasTimingData(), model.getDisplayedTimeInterval().doubleValue() );
    }
    else
    {
      dtText = "-";
    }

    final Double spp = model.getTimelineSecondsPerPixel();
    final String sppText;
    if ( spp != null )
    {
      sppText = formatTime( spp );
    }
    else
    {
      sppText = "-";
    }

    final Double uot = model.getTimelineUnitOfTime();
    final String uotText;
    if ( uot != null )
    {
      uotText = formatTime( uot );
    }
    else
    {
      uotText = "-";
    }

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      public void run()
      {
        AcquisitionDetailsView.this.displayedTimeLabel.setText( dtTextLabel );
        AcquisitionDetailsView.this.displayedTime.setText( dtText );
        AcquisitionDetailsView.this.secondsPerPixel.setText( sppText );
        AcquisitionDetailsView.this.unitOfTime.setText( uotText );

        repaint( 25L );
      };
    } );
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Acquisition details" );

    JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "Acquisition details" );

    // ROW 1
    panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample count:" ) );
    panel.add( this.sampleCount );

    // ROW 2
    panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample rate:" ) );
    panel.add( this.sampleRate );

    // ROW 3
    panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample time:" ) );
    panel.add( this.totalWidth );

    // ROW 4
    panel.add( this.displayedTimeLabel );
    panel.add( this.displayedTime );

    // ROW 5
    panel.add( SwingComponentUtils.createRightAlignedLabel( "Time/pixel:" ) );
    panel.add( this.secondsPerPixel );

    // ROW 6
    panel.add( SwingComponentUtils.createRightAlignedLabel( "Unit of time:" ) );
    panel.add( this.unitOfTime );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 10 );

    add( panel, BorderLayout.NORTH );
  }
}
