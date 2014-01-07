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
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.*;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides a custom model for the {@link WaveformView}.
 */
public class WaveformModel extends ViewModel
{
  // VARIABLES

  private final ViewController controller;
  private final List<WaveformElement> elements;

  private ZoomController zoomController;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformModel} instance.
   * 
   * @param aSession
   *          the session to use for this model, cannot be <code>null</code>.
   */
  public WaveformModel( ViewController aController, Session aSession )
  {
    super( aSession );

    this.controller = aController;
    this.elements = new CopyOnWriteArrayList<WaveformElement>();

    AcquisitionData data = aSession.getAcquiredData();
    for ( ChannelGroup group : data.getChannelGroups() )
    {
      this.elements.add( createGroupElement( group ) );

      for ( Channel channel : group.getChannels() )
      {
        this.elements.add( createChannelElement( channel ) );
      }

      this.elements.add( createGroupSummary( group ) );
      this.elements.add( createAnalogScope( group ) );
    }
  }

  // METHODS

  /**
   * Returns the absolute height of the screen.
   * 
   * @param aHeightProvider
   *          the provider for the various element's heights, cannot be
   *          <code>null</code>.
   * @return a screen height, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int calculateScreenHeight()
  {
    int height = 0;

    final int spacing = 5; // XXX UIManager.getInt(
                           // UIManagerKeys.SIGNAL_ELEMENT_SPACING );

    for ( WaveformElement element : this.elements )
    {
      height += element.getHeight() + spacing;
    }

    return height;
  }

  /**
   * @return
   */
  public long getAbsoluteLength()
  {
    return getData().getAbsoluteLength();
  }

  /**
   * @param aClip
   * @return
   */
  public int getEndIndex( Rectangle aClip, int aLength )
  {
    final Point location = new Point( aClip.x + aClip.width, 0 );
    int index = locationToSampleIndex( location );
    return Math.min( index + 1, aLength - 1 );
  }

  /**
   * @return the number of samples available, >= 0.
   */
  public int getSampleCount()
  {
    return getData().getValues().length;
  }

  /**
   * @param aClip
   * @return
   */
  public int getStartIndex( Rectangle aClip )
  {
    final Point location = aClip.getLocation();
    int index = locationToSampleIndex( location );
    return Math.max( index - 1, 0 );
  }

  /**
   * @param aY
   *          the starting position on the screen;
   * @param aHeight
   *          the height of the screen, in pixels;
   * @param aMeasurer
   *          the element measurer to use, cannot be <code>null</code>.
   * @return the waveform elements that fit in the given area.
   */
  public WaveformElement[] getWaveformElements( int aY, int aHeight, WaveformElementMeasurer aMeasurer )
  {
    final List<WaveformElement> result = new ArrayList<WaveformElement>();

    final int yMin = aY;
    final int yMax = aHeight + aY;

    final int spacing = 5; // XXX UIManager.getInt(
                           // UIManagerKeys.SIGNAL_ELEMENT_SPACING );
    final int halfSpacing = spacing / 2;

    int yPos = 0;
    for ( WaveformElement element : this.elements )
    {
      if ( yPos > yMax )
      {
        // Optimization: no need to continue after the requested end position...
        break;
      }

      int height = element.getHeight();
      if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
      {
        element.setYposition( yPos );
        result.add( element );
      }
      yPos += height + spacing;
    }

    return result.toArray( new WaveformElement[result.size()] );
  }

  /**
   * @return a zoom factor.
   */
  public double getZoomFactor()
  {
    return this.zoomController.getFactor();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void initialize()
  {
    this.zoomController = new ZoomController( this, ( WaveformView )this.controller.getView() );
  }

  /**
   * Converts the given coordinate to the corresponding sample index.
   * 
   * @param aCoordinate
   *          the coordinate to convert to a sample index, cannot be
   *          <code>null</code>.
   * @return a sample index, >= 0, or -1 if no corresponding sample index could
   *         be found.
   */
  public int locationToSampleIndex( final Point aCoordinate )
  {
    final long timestamp = locationToTimestamp( aCoordinate );
    final int idx = getData().getSampleIndex( timestamp );
    if ( idx < 0 )
    {
      return -1;
    }
    final int sampleCount = getSampleCount() - 1;
    if ( idx > sampleCount )
    {
      return sampleCount;
    }

    return idx;
  }

  /**
   * Converts the given coordinate to the corresponding sample index.
   * 
   * @param aCoordinate
   *          the coordinate to convert to a sample index, cannot be
   *          <code>null</code>.
   * @return a sample index, >= 0, or -1 if no corresponding sample index could
   *         be found.
   */
  public long locationToTimestamp( final Point aCoordinate )
  {
    final long timestamp = ( long )Math.ceil( aCoordinate.x / getZoomFactor() );
    if ( timestamp < 0 )
    {
      return -1;
    }
    return timestamp;
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    this.zoomController.zoomAll();
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    this.zoomController.zoomIn();
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    this.zoomController.zoomOriginal();
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    this.zoomController.zoomOut();
  }
}
