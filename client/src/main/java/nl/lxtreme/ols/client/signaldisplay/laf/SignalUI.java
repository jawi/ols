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
package nl.lxtreme.ols.client.signaldisplay.laf;


import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides the LaF for displaying the signal data.
 */
public class SignalUI extends ComponentUI
{
  // CONSTANTS

  /** The maximum number of points in a polyline. */
  private static final int POINT_COUNT = 1000000;
  /**
   * The number of samples that are shown in a single view upon which is decided
   * to draw the group summary and scope a bit sloppy.
   */
  private static final int SLOPPY_DRAW_THRESHOLD = 10000;

  // VARIABLES

  private volatile boolean listening = true;
  private volatile MeasurementInfo measurementInfo;
  private volatile Rectangle measurementRect;

  private static final int[] x = new int[2 * POINT_COUNT];
  private static final int[] y = new int[2 * POINT_COUNT];

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
   * Returns the current value of measurementRect.
   *
   * @return the measurementRect
   */
  public Rectangle getMeasurementRect()
  {
    return this.measurementRect;
  }

  /**
   * {@inheritDoc}
   */
  public void handleMeasureEvent( final MeasurementInfo aMeasurementInfo )
  {
    this.measurementInfo = aMeasurementInfo;

    if ( aMeasurementInfo != null )
    {
      this.measurementRect = new Rectangle( aMeasurementInfo.getRectangle() );
      this.measurementRect.grow( ArrowRenderer.HEAD_WIDTH, ArrowRenderer.HEAD_HEIGHT );
    }
    else
    {
      this.measurementRect = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  public boolean isListening()
  {
    return this.listening;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void paint( final Graphics aGraphics, final JComponent aComponent )
  {
    final SignalView view = ( SignalView )aComponent;
    final SignalViewModel model = view.getModel();
    if ( !model.hasData() )
    {
      // Nothing to do!
      return;
    }

    this.listening = false;

    try
    {
      final Rectangle clip = aGraphics.getClipBounds();
      final IUIElement[] elements = model.getSignalElements( clip.y, clip.height );

      Graphics2D canvas = ( Graphics2D )aGraphics.create();

      try
      {
        if ( elements.length > 0 )
        {
          paintSignals( canvas, model, elements );
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
      if ( model.isCursorMode() )
      {
        paintCursors( canvas, model );
      }

      // Draw the measurement stuff...
      if ( model.isMeasurementMode() && MeasurementInfo.isDefined( this.measurementInfo ) )
      {
        paintMeasurementArrow( canvas, model, this.measurementInfo );
      }

      // Draw the annotations...
      paintAnnotations( canvas, model, elements );
    }
    finally
    {
      this.listening = true;
    }
  }

  /**
   * Returns the stroke to use to render the annotation lines.
   *
   * @param renderStyle
   * @param zoomFactor
   * @return
   */
  private Stroke getAnnotationLineStroke( final boolean renderStyle, final double zoomFactor )
  {
    final float strokeWidth = ( float )( 3.0f / Math.max( 1.0f, ( 1.0f / zoomFactor ) ) );

    final BasicStroke stroke;
    if ( renderStyle )
    {
      stroke = new BasicStroke( strokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_ROUND, 0.9f );
    }
    else
    {
      stroke = new BasicStroke( strokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0.9f );
    }

    return stroke;
  }

  /**
   * @param aCanvas
   * @param aModel
   * @param aSignalElements
   */
  private void paintAnnotations( final Graphics2D aCanvas, final SignalViewModel aModel,
      final IUIElement[] aSignalElements )
  {
    final long[] timestamps = aModel.getTimestamps();
    if ( ( timestamps == null ) || ( timestamps.length == 0 ) || ( aSignalElements.length == 0 ) )
    {
      // Nothing to do...
      return;
    }

    final Rectangle clip = aCanvas.getClipBounds();
    final int startIdx = aModel.getStartIndex( clip );
    final int endIdx = aModel.getEndIndex( clip, timestamps.length );

    final long startTimestamp = timestamps[startIdx];
    final long endTimestamp = timestamps[endIdx];

    final double zoomFactor = aModel.getZoomFactor();

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aSignalElements[0].getYposition() );

    final boolean annotationRenderStyle = aModel.isRenderAnnotationAlternatively();

    // Some drawing primitives we're going to re-use over and over...
    final Stroke stroke = getAnnotationLineStroke( annotationRenderStyle, zoomFactor );

    final AlphaComposite alphaComposite = AlphaComposite.SrcOver.derive( aModel.getAnnotationAlpha() );

    for ( IUIElement element : aSignalElements )
    {
      if ( element instanceof SignalElement )
      {
        SignalElement signalElement = ( SignalElement )element;
        if ( signalElement.isDigitalSignal() )
        {
          // Tell Swing how we would like to render ourselves...
          aCanvas.setRenderingHints( createSignalRenderingHints( aModel.isRenderAnnotationAntiAliased() ) );

          aCanvas.setColor( signalElement.getColor() );

          if ( signalElement.isEnabled() )
          {
            final AnnotationsHelper helper = new AnnotationsHelper( signalElement );

            aCanvas.setFont( aModel.getAnnotationFont() );

            final FontMetrics fm = aCanvas.getFontMetrics();
            final int fontHeight = fm.getHeight();

            for ( DataAnnotation<?> ann : helper.getAnnotations( DataAnnotation.class, startTimestamp, endTimestamp ) )
            {
              final long annStartTime = ann.getStartTimestamp();
              final long annEndTime = ann.getEndTimestamp();

              int x1 = ( int )( annStartTime * zoomFactor );
              int x2 = ( int )( annEndTime * zoomFactor );
              int y1 = signalElement.getOffset( aModel.getAnnotationAlignment() );
              int y2 = y1 + signalElement.getSignalHeight();
              int midY = y1 + ( ( y2 - y1 ) / 2 );

              final String annText = ann.getAnnotation().toString();

              final int annotationWidth = ( x2 - x1 ) + 2;

              final Composite oldComposite = aCanvas.getComposite();
              final Stroke oldStroke = aCanvas.getStroke();

              aCanvas.setComposite( alphaComposite );

              // Fade out the signal itself...
              aCanvas.setColor( aModel.getBackgroundColor() );
              if ( annotationRenderStyle )
              {
                aCanvas.fillRect( x1, y1 + 0, annotationWidth, ( y2 - y1 ) + 1 );
              }
              else
              {
                aCanvas.fillRect( x1, y1 + 1, annotationWidth, ( y2 - y1 ) - 1 );
              }

              aCanvas.setComposite( oldComposite );

              // Draw the thick white boundaries...
              aCanvas.setColor( aModel.getAnnotationColor() );
              aCanvas.setStroke( stroke );
              aCanvas.drawLine( x1, y1 + 2, x1, y2 - 2 );
              aCanvas.drawLine( x2, y1 + 2, x2, y2 - 2 );

              aCanvas.setStroke( oldStroke );

              final int textWidth = fm.stringWidth( annText );
              final int textXoffset = ( int )( ( annotationWidth - textWidth ) / 2.0 );

              if ( textXoffset > 0 )
              {
                int x3 = ( x1 + textXoffset );
                if ( annotationRenderStyle && ( ( x3 - 4 ) > 0 ) )
                {
                  aCanvas.drawLine( x1, midY, x3 - 4, midY );
                  aCanvas.drawLine( x3 + textWidth + 8, midY, x2, midY );
                }

                aCanvas.drawString( annText, x1 + textXoffset, y1 + fontHeight );
              }
            }
          }
        }
      }

      // Advance to the next channel...
      aCanvas.translate( 0, element.getHeight() + aModel.getSignalElementSpacing() );
    }
  }

  /**
   * Paints the cursors over the signals.
   *
   * @param aCanvas
   *          the canvas to paint the cursor on;
   * @param aModel
   *          the model to use;
   */
  private void paintCursors( final Graphics2D aCanvas, final SignalViewModel aModel )
  {
    final Rectangle clip = aCanvas.getClipBounds();

    // Tell Swing how we would like to render ourselves...
    aCanvas.setRenderingHints( createCursorRenderingHints() );

    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      int cursorXpos = aModel.getCursorScreenCoordinate( i );

      if ( ( cursorXpos < 0 ) || !clip.contains( cursorXpos, clip.y ) )
      {
        // Trivial reject: don't paint undefined cursors, or cursors outside the
        // clip boundaries...
        continue;
      }

      aCanvas.setColor( aModel.getCursorColor( i ) );

      aCanvas.drawLine( cursorXpos, clip.y, cursorXpos, clip.y + clip.height );
    }
  }

  /**
   * Renders the measurement information arrows.
   *
   * @param aCanvas
   *          the canvas to paint the measurement arrows on, cannot be
   *          <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aMeasurementInfo
   *          the measurement information, cannot be <code>null</code> or empty.
   */
  private void paintMeasurementArrow( final Graphics2D aCanvas, final SignalViewModel aModel,
      final MeasurementInfo aMeasurementInfo )
  {
    Rectangle signalHoverRect = aMeasurementInfo.getRectangle();
    int x = signalHoverRect.x;
    int y = ( int )signalHoverRect.getCenterY();
    int w = signalHoverRect.width;
    int middlePos = aMeasurementInfo.getMidSamplePos().intValue() - x;

    // Tell Swing how we would like to render ourselves...
    aCanvas.setRenderingHints( createArrowRenderingHints() );

    aCanvas.setColor( aModel.getMeasurementArrowColor() );

    // Allow arrow renderer to start with [0, 0] as coordinate system...
    aCanvas.translate( x, y );

    ArrowRenderer.render( aCanvas, w, middlePos );

    // Restore to original coordinate system...
    aCanvas.translate( -x, -y );
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
  private void paintSignals( final Graphics2D aCanvas, final SignalViewModel aModel, final IUIElement[] aElements )
  {
    final int[] values = aModel.getDataValues();
    final long[] timestamps = aModel.getTimestamps();

    final Rectangle clip = aCanvas.getClipBounds();

    aCanvas.setBackground( aModel.getBackgroundColor() );
    aCanvas.clearRect( clip.x, clip.y, clip.width, clip.height );

    final int startIdx = aModel.getStartIndex( clip );
    final int endIdx = aModel.getEndIndex( clip, values.length );

    final double zoomFactor = aModel.getZoomFactor();

    if ( aModel.hasTriggerData() )
    {
      final long triggerOffset = aModel.getTriggerOffset();
      if ( ( timestamps[startIdx] <= triggerOffset ) && ( timestamps[endIdx] >= triggerOffset ) )
      {
        // Draw a line denoting the trigger position...
        final int x = ( int )Math.round( triggerOffset * zoomFactor ) - 1;

        aCanvas.setColor( aModel.getTriggerColor() );
        aCanvas.drawLine( x, clip.y, x, clip.y + clip.height );
      }
    }

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aElements[0].getYposition() );

    final boolean enableSloppyScopePainting = aModel.isSloppyScopeRenderingAllowed();
    int lastP = 0;

    for ( IUIElement element : aElements )
    {
      if ( element instanceof ElementGroup )
      {
        // Draw nothing...

        // advance to the next element...
        aCanvas.translate( 0, element.getHeight() + aModel.getSignalElementSpacing() );

        continue;
      }

      aCanvas.setColor( element.getColor() );

      final SignalElement signalElement = ( SignalElement )element;

      if ( signalElement.isDigitalSignal() )
      {
        int signalHeight = signalElement.getSignalHeight();
        int signalOffset = signalElement.getOffset();

        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( false /* aUseAA */) );

        aCanvas.translate( 0, signalOffset );

        if ( !signalElement.isEnabled() || ( startIdx == endIdx ) )
        {
          // Forced zero'd channel is *very* easy to draw...
          aCanvas.drawLine( clip.x, signalHeight, clip.x + clip.width, signalHeight );
        }
        else
        {
          // "Normal" data set; draw as accurate as possible...
          final int mask = signalElement.getMask();

          // Make sure we always start with time 0...
          long timestamp = timestamps[startIdx];
          int prevSampleValue = ( values[startIdx] & mask );

          int xValue = ( int )( zoomFactor * timestamp );
          int yValue = ( prevSampleValue == 0 ? signalHeight : 0 );

          x[0] = xValue;
          y[0] = yValue;
          int p = 1;

          for ( int sampleIdx = startIdx + 1; ( p < POINT_COUNT ) && ( sampleIdx <= endIdx ); sampleIdx++ )
          {
            timestamp = timestamps[sampleIdx];
            int sampleValue = ( values[sampleIdx] & mask );

            xValue = ( int )( zoomFactor * timestamp );

            if ( prevSampleValue != sampleValue )
            {
              x[p] = xValue;
              y[p] = ( prevSampleValue == 0 ? signalHeight : 0 );
              p++;
            }

            x[p] = xValue;
            y[p] = ( sampleValue == 0 ? signalHeight : 0 );
            p++;

            prevSampleValue = sampleValue;
          }

          aCanvas.drawPolyline( x, y, p );

          lastP = ( int )( ( p * 0.1 ) + ( lastP * 0.9 ) );
        }

        // Move back to the original position...
        aCanvas.translate( 0, -signalOffset );
      }

      int sampleIncr = 1;
      if ( enableSloppyScopePainting && ( lastP > SLOPPY_DRAW_THRESHOLD ) )
      {
        sampleIncr = ( int )Math.max( 1.0, ( 1.0 / zoomFactor ) );
      }

      if ( signalElement.isGroupSummary() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( aModel.isRenderGroupSummaryAntiAliased() ) );

        int mask = signalElement.getMask();

        int padding = aModel.getGroupSummaryPadding();

        int prevSampleValue = values[startIdx] & mask;
        int prevX = ( int )( zoomFactor * timestamps[startIdx] );

        aCanvas.setFont( aModel.getGroupSummaryTextFont() );

        FontMetrics fm = aCanvas.getFontMetrics();
        int textYpos = ( int )( ( signalElement.getHeight() + fm.getLeading() + fm.getMaxAscent() ) / 2.0 ) - padding;

        for ( int sampleIdx = startIdx + 1; sampleIdx < endIdx; sampleIdx += sampleIncr )
        {
          int sampleValue = ( values[sampleIdx] & mask );

          if ( sampleValue != prevSampleValue )
          {
            int x = ( int )( zoomFactor * timestamps[sampleIdx] );

            String text = String.format( "%02X", Integer.valueOf( signalElement.getValue( prevSampleValue ) ) );

            int textWidth = fm.stringWidth( text ) + ( 2 * padding );
            int cellWidth = x - prevX;
            if ( textWidth < cellWidth )
            {
              int textXpos = prevX + ( int )( ( cellWidth - textWidth ) / 2.0 ) + padding;

              aCanvas.setColor( signalElement.getColor() );

              aCanvas.drawString( text, textXpos, textYpos );
            }

            aCanvas.setColor( aModel.getGroupSummaryBarColor() );

            // draw a small line...
            aCanvas.drawLine( x, padding, x, signalElement.getHeight() - padding );

            prevX = x;
          }

          prevSampleValue = sampleValue;
        }
      }

      if ( signalElement.isAnalogSignal() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( aModel.isRenderScopeSignalAntiAliased() ) );

        aCanvas.setColor( signalElement.getColor() );

        long mask = signalElement.getMask() & 0xFFFFFFFFL;
        final int trailingZeros = Long.numberOfTrailingZeros( mask );
        final int onesCount = Long.SIZE - Long.numberOfLeadingZeros( mask ) - trailingZeros;
        final long maxValue = ( ( 1L << onesCount ) - 1L ) & 0xFFFFFFFFL;
        double scaleFactor = ( maxValue == 0L ) ? 1.0 : signalElement.getHeight() / ( double )maxValue;

        // Make sure we always start with time 0...
        int p = 0;
        if ( startIdx == endIdx )
        {
          x[p] = clip.x;
          y[p] = signalElement.getHeight();
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
      aCanvas.translate( 0, signalElement.getHeight() + aModel.getSignalElementSpacing() );
    }
  }
}
