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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a waveform view of {@link AcquisitionData}, in which data is shown
 * graphically as rows.
 */
public class WaveformView extends BaseView implements Scrollable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** The maximum number of points in a polyline. */
  private static final int POINT_COUNT = 50000;

  // VARIABLES

  private final int[] x = new int[2 * POINT_COUNT];
  private final int[] y = new int[2 * POINT_COUNT];

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  public WaveformView( ViewController aController, ViewModel aModel )
  {
    super( aController, aModel );
  }

  // METHODS

  /**
   * Creates the rendering hints for this the drawing of arrows.
   */
  private static RenderingHints createArrowRenderingHints()
  {
    RenderingHints hints = new RenderingHints( RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC );
    hints.put( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    hints.put( RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED );
    hints.put( RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_SPEED );
    hints.put( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY );
    return hints;
  }

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createCursorRenderingHints()
  {
    RenderingHints hints = new RenderingHints( RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR );
    hints.put( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    hints.put( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * Creates the rendering hints for this view.
   * 
   * @param aUseAA
   *          <code>true</code> if anti aliasing should be used,
   *          <code>false</code> if anti aliasing shouldn't be used.
   */
  private static RenderingHints createSignalRenderingHints( final boolean aUseAA )
  {
    RenderingHints hints = new RenderingHints( RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BILINEAR );
    hints.put( RenderingHints.KEY_ANTIALIASING, aUseAA ? RenderingHints.VALUE_ANTIALIAS_ON
        : RenderingHints.VALUE_ANTIALIAS_OFF );
    hints.put( RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED );
    hints.put( RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_SPEED );
    hints.put( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    final int inc;
    if ( aOrientation == SwingConstants.VERTICAL )
    {
      inc = getVerticalBlockIncrement( getSize(), aVisibleRect, aDirection );
    }
    else
    /* if ( aOrientation == SwingConstants.HORIZONTAL ) */
    {
      inc = getHorizontalBlockIncrement( aVisibleRect, aDirection );
    }

    return inc;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    return getScrollableBlockIncrement( aVisibleRect, aOrientation, aDirection );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void initialize()
  {
    // add( new JLabel( "WaveForm " + this ), BorderLayout.CENTER );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( Graphics aGraphics )
  {
    WaveformModel model = ( WaveformModel )this.model;

    try
    {
      final Rectangle clip = aGraphics.getClipBounds();
      final WaveformElement[] elements = model.getWaveformElements( clip.y, clip.height, LOOSE_MEASURER );

      Graphics2D canvas = ( Graphics2D )aGraphics.create();

      try
      {
        if ( elements.length > 0 )
        {
          paintSignals( canvas, elements );
        }
      }
      finally
      {
        canvas.dispose();
        canvas = null;
      }

      // Use the *original* graphics object, as the one defined above is
      // translated to some unknown coordinate system...
      canvas = ( Graphics2D )aGraphics;

      // Draw the cursor "flags"...
      // if ( model.isCursorMode() )
      // {
      // paintCursors( canvas, model );
      // }

      // Draw the measurement stuff...
      // if ( model.isMeasurementMode() && MeasurementInfo.isDefined(
      // this.measurementInfo ) )
      // {
      // paintMeasurementArrow( canvas, model, this.measurementInfo );
      // }

      // Draw the annotations...
      // paintAnnotations( canvas, model, elements );
    }
    finally
    {
      // this.listening = true;
    }
  }

  /**
   * Calculates the horizontal block increment.
   * <p>
   * The following rules are adhered for scrolling horizontally:
   * </p>
   * <ol>
   * <li>unless the first or last sample is not shown, scroll a full block;
   * otherwise</li>
   * <li>do not scroll.</li>
   * </ol>
   * 
   * @param aVisibleRect
   *          the visible rectangle of the component, never <code>null</code>;
   * @param aDirection
   *          the direction in which to scroll (&gt; 0 to scroll left, &lt; 0 to
   *          scroll right);
   * @return a horizontal block increment, determined according to the rules
   *         described.
   */
  private int getHorizontalBlockIncrement( Rectangle aVisibleRect, int aDirection )
  {
    final int blockIncr = 50;
    final WaveformModel model = ( WaveformModel )this.model;

    final int firstVisibleSample = model.locationToSampleIndex( aVisibleRect.getLocation() );
    final int lastVisibleSample = model.locationToSampleIndex( new Point( aVisibleRect.x + aVisibleRect.width, 0 ) );
    final int lastSampleIdx = model.getSampleCount();

    int inc = 0;
    if ( aDirection < 0 )
    {
      // Scroll left
      if ( firstVisibleSample >= 0 )
      {
        inc = blockIncr;
      }
    }
    else if ( aDirection > 0 )
    {
      // Scroll right
      if ( lastVisibleSample < lastSampleIdx )
      {
        inc = blockIncr;
      }
    }

    return inc;
  }

  /**
   * Calculates the vertical block increment.
   * <p>
   * The following rules are adhered for scrolling vertically:
   * </p>
   * <ol>
   * <li>if the first shown channel is not completely visible, it will be made
   * fully visible; otherwise</li>
   * <li>scroll down to show the succeeding channel fully;</li>
   * <li>if the last channel is fully shown, and there is some room left at the
   * bottom, show the remaining space.</li>
   * </ol>
   * 
   * @param aVisibleRect
   *          the visible rectangle of the component, never <code>null</code>;
   * @param aDirection
   *          the direction in which to scroll (&gt; 0 to scroll down, &lt; 0 to
   *          scroll up).
   * @return a vertical block increment, determined according to the rules
   *         described.
   */
  private int getVerticalBlockIncrement( final Dimension aViewDimensions, final Rectangle aVisibleRect,
      final int aDirection )
  {
    final WaveformModel model = ( WaveformModel )this.model;

    WaveformElementMeasurer measurer = WaveformElementMeasurer.LOOSE_MEASURER;

    WaveformElement[] elements = model.getWaveformElements( aVisibleRect.y + 1, 1, measurer );
    if ( elements.length == 0 )
    {
      return 0;
    }

    final int spacing = 5; // XXX UIManager.getInt(
                           // UIManagerKeys.SIGNAL_ELEMENT_SPACING );

    int inc = 0;
    int yPos = elements[0].getYposition();

    if ( aDirection > 0 )
    {
      // Scroll down...
      int height = elements[0].getHeight() + spacing;
      inc = height - ( aVisibleRect.y - yPos );
      if ( inc < 0 )
      {
        inc = -inc;
      }
    }
    else if ( aDirection < 0 )
    {
      // Scroll up...
      inc = ( aVisibleRect.y - yPos );
      if ( inc <= 0 )
      {
        // Determine the height of the element *before* the current one, as we
        // need to scroll up its height...
        elements = model.getWaveformElements( yPos - spacing, 1, measurer );
        if ( elements.length > 0 )
        {
          inc += elements[0].getHeight() + spacing;
        }
      }
    }

    return inc;
  }

  /**
   * Paints the individual signal channels, group bytes and analogue scope
   * signals.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aElements
   *          the UI-elements to draw, cannot be <code>null</code> or empty!
   */
  private void paintSignals( Graphics2D aCanvas, WaveformElement[] aElements )
  {
    final WaveformModel model = ( WaveformModel )this.model;
    final AcquisitionData data = model.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();
    final long absLength = data.getAbsoluteLength();

    final Rectangle clip = aCanvas.getClipBounds();

    aCanvas.setBackground( Color.WHITE /* aModel.getBackgroundColor() */);
    aCanvas.clearRect( clip.x, clip.y, clip.width, clip.height );

    final int startIdx = model.getStartIndex( clip );
    final int endIdx = model.getEndIndex( clip, values.length );

    final double zoomFactor = model.getZoomFactor();

    if ( data.hasTriggerData() )
    {
      final long triggerOffset = data.getTriggerPosition();
      if ( ( timestamps[startIdx] <= triggerOffset ) && ( timestamps[endIdx] >= triggerOffset ) )
      {
        // Draw a line denoting the trigger position...
        final int x = ( int )Math.round( triggerOffset * zoomFactor ) - 1;

        aCanvas.setColor( Color.RED /* aModel.getTriggerColor() */);
        aCanvas.drawLine( x, clip.y, x, clip.y + clip.height );
      }
    }

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aElements[0].getYposition() );

    final boolean enableSloppyScopePainting = false; // aModel.isSloppyScopeRenderingAllowed();
    int lastP = 0;

    int elementSpacing = 5; // aModel.getSignalElementSpacing();

    for ( WaveformElement element : aElements )
    {
      Type type = element.getType();
      if ( Type.GROUP.equals( type ) )
      {
        // Draw nothing...

        // advance to the next element...
        aCanvas.translate( 0, element.getHeight() + elementSpacing );

        continue;
      }

      aCanvas.setColor( element.getColor() );

      if ( Type.CHANNEL.equals( type ) )
      {
        int signalHeight = element.getSignalHeight();
        int signalOffset = element.getOffset();

        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( false /* aUseAA */) );

        aCanvas.translate( 0, signalOffset );

        if ( !element.isEnabled() || ( startIdx == endIdx ) )
        {
          // Forced zero'd channel is *very* easy to draw...
          aCanvas.drawLine( clip.x, signalHeight, clip.x + clip.width, signalHeight );
        }

        else
        {
          // "Normal" data set; draw as accurate as possible...
          final int mask = element.getMask();

          // Make sure we always start with time 0...
          long prevTimestamp = timestamps[startIdx];
          int prevSampleValue = ( values[startIdx] & mask );

          x[0] = ( int )( zoomFactor * prevTimestamp );
          y[0] = ( prevSampleValue == 0 ? signalHeight : 0 );
          int p = 1;

          for ( int sampleIdx = startIdx + 1; sampleIdx <= endIdx; sampleIdx++ )
          {
            int sampleValue = ( values[sampleIdx] & mask );
            if ( prevSampleValue != sampleValue )
            {
              long timestamp = timestamps[sampleIdx];

              int xValue = ( int )( zoomFactor * timestamp );

              x[p] = xValue;
              y[p] = ( prevSampleValue == 0 ? signalHeight : 0 );
              p++;

              x[p] = xValue;
              y[p] = ( sampleValue == 0 ? signalHeight : 0 );
              p++;

              prevSampleValue = sampleValue;
              prevTimestamp = timestamp;
            }

            if ( p >= ( x.length - 2 ) )
            {
              aCanvas.drawPolyline( x, y, p );
              p = 0;
            }
          }

          x[p] = ( int )( zoomFactor * absLength );
          y[p] = ( values[endIdx] & mask ) == 0 ? signalHeight : 0;
          p++;

          aCanvas.drawPolyline( x, y, p );

          lastP = ( int )( ( p * 0.1 ) + ( lastP * 0.9 ) );
        }

        // Move back to the original position...
        aCanvas.translate( 0, -signalOffset );
      }

      int sampleIncr = 1;
      // if ( enableSloppyScopePainting && ( lastP > SLOPPY_DRAW_THRESHOLD ) )
      // {
      // sampleIncr = ( int )Math.max( 1.0, ( 1.0 / zoomFactor ) );
      // }

      if ( Type.GROUP_SUMMARY.equals( type ) )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( false /*
                                                                      * aModel.
                                                                      * isRenderGroupSummaryAntiAliased
                                                                      * ()
                                                                      */) );

        int mask = element.getMask();

        int padding = 5; // model.getGroupSummaryPadding();

        int prevSampleValue = values[startIdx] & mask;
        int prevX = ( int )( zoomFactor * timestamps[startIdx] );

        // aCanvas.setFont( aModel.getGroupSummaryTextFont() );

        FontMetrics fm = aCanvas.getFontMetrics();
        int textYpos = ( int )( ( element.getHeight() + fm.getLeading() + fm.getMaxAscent() ) / 2.0 ) - padding;

        for ( int sampleIdx = startIdx + 1; sampleIdx < endIdx; sampleIdx += sampleIncr )
        {
          int sampleValue = ( values[sampleIdx] & mask );

          if ( sampleValue != prevSampleValue )
          {
            int x = ( int )( zoomFactor * timestamps[sampleIdx] );

            String text = String.format( "%02X", Integer.valueOf( prevSampleValue ) );

            int textWidth = fm.stringWidth( text ) + ( 2 * padding );
            int cellWidth = x - prevX;
            if ( textWidth < cellWidth )
            {
              int textXpos = prevX + ( int )( ( cellWidth - textWidth ) / 2.0 ) + padding;

              aCanvas.setColor( element.getColor() );

              aCanvas.drawString( text, textXpos, textYpos );
            }

            aCanvas.setColor( Color.GRAY /* aModel.getGroupSummaryBarColor() */);

            // draw a small line...
            aCanvas.drawLine( x, padding, x, element.getHeight() - padding );

            prevX = x;
          }

          prevSampleValue = sampleValue;
        }
      }

      if ( Type.ANALOG_SCOPE.equals( type ) )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( false /*
                                                                      * aModel.
                                                                      * isRenderScopeSignalAntiAliased
                                                                      * ()
                                                                      */) );

        aCanvas.setColor( element.getColor() );

        long mask = element.getMask() & 0xFFFFFFFFL;
        final int trailingZeros = Long.numberOfTrailingZeros( mask );
        final int onesCount = Long.SIZE - Long.numberOfLeadingZeros( mask ) - trailingZeros;
        final long maxValue = ( ( 1L << onesCount ) - 1L ) & 0xFFFFFFFFL;
        double scaleFactor = ( maxValue == 0L ) ? 1.0 : element.getHeight() / ( double )maxValue;

        // Make sure we always start with time 0...
        int p = 0;
        if ( startIdx == endIdx )
        {
          x[p] = clip.x;
          y[p] = element.getHeight();
          p++;
        }
        else
        {
          for ( int sampleIdx = startIdx; ( p < POINT_COUNT ) && ( sampleIdx < endIdx ); sampleIdx += sampleIncr )
          {
            long timestamp = timestamps[sampleIdx];

            int sampleValue = ( int )( ( values[sampleIdx] & mask ) >> trailingZeros );
            final int i_max = Math.min( endIdx, ( sampleIdx + sampleIncr ) - 1 );
            for ( int i = sampleIdx + 1; i < i_max; i++ )
            {
              sampleValue += ( ( values[i] & mask ) >> trailingZeros );
            }
            sampleValue = ( int )( maxValue - ( sampleValue / ( double )sampleIncr ) );

            x[p] = ( int )( zoomFactor * timestamp );
            y[p] = ( int )( scaleFactor * sampleValue );
            p++;
          }
        }

        // Make sure we end at the last visible sample index...
        x[p] = clip.x + clip.width;
        y[p] = y[p - 1];
        p++;

        aCanvas.drawPolyline( x, y, p );
      }

      // advance to the next element...
      aCanvas.translate( 0, element.getHeight() + elementSpacing );
    }
  }
}
