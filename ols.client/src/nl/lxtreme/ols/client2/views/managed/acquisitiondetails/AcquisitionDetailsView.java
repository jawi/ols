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
package nl.lxtreme.ols.client2.views.managed.acquisitiondetails;


import static nl.lxtreme.ols.client2.views.ViewUtils.*;

import java.awt.*;
import java.text.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the acquisition details.
 */
public class AcquisitionDetailsView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "AcquisitionDetails";

  private static final long serialVersionUID = 1L;

  /**
   * Creates a new {@link AcquisitionDetailsView} instance.
   */
  public AcquisitionDetailsView()
  {
    super( ID );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void build()
  {
    updateViewText( null, null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Acquisition details" );

    aFrame.setInitIndex( 1 );
    aFrame.setDockedHeight( 200 /* px */);

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    updateViewText( aController, aData );
  }

  /**
   * Updates the text contents of this component.
   * 
   * @param aController
   * @param aData
   */
  private void updateViewText( ViewController aController, AcquisitionData aData )
  {
    String sampleRate = "-";
    String channelCount = "-";
    String sampleCount = "-";
    String totalWidth = "-";
    String displayInterval = "-";
    String timePerPixel = "-";
    String unitOfTime = "-";

    boolean withTimingData = true;

    if ( aController != null )
    {
      DecimalFormat decimalFormat = new DecimalFormat();

      ViewModel model = aController.getModel();
      BaseView view = aController.getView();

      sampleCount = decimalFormat.format( aData.getValues().length );
      channelCount = decimalFormat.format( aData.getChannelCount() );

      withTimingData = model.hasTimingData();
      if ( withTimingData )
      {
        double sr = aData.getSampleRate();
        // total width
        double tw = aData.getAbsoluteLength() / sr;
        // time per pixel...
        double tpp = 1.0 / ( view.getZoomFactor() * sr );

        sampleRate = formatFrequency( Double.valueOf( sr ) );
        totalWidth = formatTime( Double.valueOf( tw ) );
        displayInterval = formatReference( aData.hasTimingData(), view.getDisplayedInterval() );
        timePerPixel = formatTime( tpp );
        unitOfTime = formatTime( Math.pow( 10, Math.ceil( Math.log10( tpp ) ) ) );
      }
      else
      {
        displayInterval = decimalFormat.format( view.getDisplayedInterval() );
      }
    }

    JPanel panel = new JPanel( new SpringLayout() );

    panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample count:" ) );
    panel.add( new JLabel( sampleCount ) );

    panel.add( SwingComponentUtils.createRightAlignedLabel( "Channel count:" ) );
    panel.add( new JLabel( channelCount ) );

    if ( withTimingData )
    {
      panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample rate:" ) );
      panel.add( new JLabel( sampleRate ) );

      panel.add( SwingComponentUtils.createRightAlignedLabel( "Sample time:" ) );
      panel.add( new JLabel( totalWidth ) );

      panel.add( SwingComponentUtils.createRightAlignedLabel( "Displayed time:" ) );
      panel.add( new JLabel( displayInterval ) );

      panel.add( SwingComponentUtils.createRightAlignedLabel( "Time/pixel:" ) );
      panel.add( new JLabel( timePerPixel ) );

      panel.add( SwingComponentUtils.createRightAlignedLabel( "Unit of time:" ) );
      panel.add( new JLabel( unitOfTime ) );
    }
    else
    {
      panel.add( SwingComponentUtils.createRightAlignedLabel( "Displayed states:" ) );
      panel.add( new JLabel( displayInterval ) );
    }

    makeEditorGrid( panel );

    removeAll();
    add( panel, BorderLayout.NORTH );

    validate();
    repaint();
  }
}
