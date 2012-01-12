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

import nl.lxtreme.ols.api.tools.annotation.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.cursor.Cursor;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.Renderer;


/**
 * 
 */
public class SignalUI extends ComponentUI
{
  // CONSTANTS

  /** XXX The threshold when we're going to draw a bit more sloppy... */
  private static final int SLOPPY_THRESHOLD = 1000000;

  private static final int PADDING_X = 2;
  private static final int PADDING_Y = 2;

  // VARIABLES

  private final Renderer arrowRenderer = new ArrowRenderer();

  private volatile boolean listening = true;
  private volatile MeasurementInfo measurementInfo;
  private volatile Rectangle measurementRect;

  private static final int[] x = new int[2 * SLOPPY_THRESHOLD];
  private static final int[] y = new int[2 * SLOPPY_THRESHOLD];

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
    this.listening = false;

    final SignalView view = ( SignalView )aComponent;
    final SignalViewModel model = view.getModel();

    try
    {
      final Rectangle clip = aGraphics.getClipBounds();
      final SignalElement[] signalElements = model.getSignalElements( clip.y, clip.height );

      Graphics2D canvas = ( Graphics2D )aGraphics.create();

      try
      {

        if ( signalElements.length > 0 )
        {
          paintSignals( canvas, model, signalElements );
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
      paintAnnotations( canvas, model, signalElements );
    }
    finally
    {
      this.listening = true;
    }
  }

  /**
   * @param aCanvas
   * @param aModel
   * @param aSignalElements
   */
  private void paintAnnotations( final Graphics2D aCanvas, final SignalViewModel aModel,
      final SignalElement[] aSignalElements )
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

    final int signalHeight = aModel.getSignalHeight();
    final int signalOffset = aModel.getSignalOffset();
    final double zoomFactor = aModel.getZoomFactor();

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aSignalElements[0].getYposition() + signalOffset );

    // Some drawing primitives we're going to re-use over and over...
    final float strokeWidth = ( float )( 3.0f / Math.max( 1.0f, ( 1.0f / zoomFactor ) ) );
    final BasicStroke stroke;
    if ( aModel.isRenderAnnotationsAlternatively() )
    {
      stroke = new BasicStroke( strokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_ROUND, 0.9f );
    }
    else
    {
      stroke = new BasicStroke( strokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0.9f );
    }

    final AlphaComposite alphaComposite = AlphaComposite.SrcOver.derive( 0.875f );

    for ( SignalElement signalElement : aSignalElements )
    {
      aCanvas.setColor( signalElement.getColor() );

      if ( signalElement.isSignalGroup() )
      {
        // Draw nothing...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      if ( signalElement.isDigitalSignal() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( true /* aUseAA */) );

        if ( signalElement.isEnabled() )
        {
          final Annotation<?>[] annotations = aModel.getAnnotationsFor( signalElement.getChannel().getIndex() );

          final FontMetrics fm = aCanvas.getFontMetrics();

          final int fontHeight = fm.getHeight();

          for ( Annotation<?> annotation : annotations )
          {
            if ( annotation instanceof DataAnnotation<?> )
            {
              final DataAnnotation<?> ann = ( DataAnnotation<?> )annotation;

              final long annStartTime = ann.getStartTimestamp();
              final long annEndTime = ann.getEndTimestamp();

              if ( ( ( annStartTime < startTimestamp ) && ( annEndTime < startTimestamp ) )
                  || ( ( annStartTime > endTimestamp ) && ( annEndTime > endTimestamp ) ) )
              {
                // Simple reject: annotation falls outside clip boundaries...
                continue;
              }

              int x1 = ( int )( annStartTime * zoomFactor );
              int x2 = ( int )( annEndTime * zoomFactor );
              int y1 = 0;
              int y2 = signalHeight;
              int midY = signalHeight / 2;

              final String annText = ann.getAnnotation().toString();

              final int annotationWidth = ( x2 - x1 ) + 2;
              final int textWidth = fm.stringWidth( annText );
              final int textXoffset = ( int )( ( annotationWidth - textWidth ) / 2.0 );

              final Composite oldComposite = aCanvas.getComposite();
              final Stroke oldStroke = aCanvas.getStroke();

              aCanvas.setComposite( alphaComposite );

              // Fade out the signal itself...
              aCanvas.setColor( aModel.getBackgroundColor() );
              if ( aModel.isRenderAnnotationsAlternatively() )
              {
                aCanvas.fillRect( x1, y1 + 0, annotationWidth, y2 + 1 );
              }
              else
              {
                aCanvas.fillRect( x1, y1 + 1, annotationWidth, y2 - 1 );
              }

              aCanvas.setComposite( oldComposite );

              // Draw the thick white boundaries...
              aCanvas.setColor( Color.WHITE );
              aCanvas.setStroke( stroke );
              aCanvas.drawLine( x1, y1 + 2, x1, y2 - 2 );
              aCanvas.drawLine( x2, y1 + 2, x2, y2 - 2 );

              aCanvas.setStroke( oldStroke );

              if ( textXoffset > 0 )
              {
                int x3 = ( x1 + textXoffset );
                if ( aModel.isRenderAnnotationsAlternatively() && ( ( x3 - 4 ) > 0 ) )
                {
                  aCanvas.drawLine( x1, midY, x3 - 4, midY );
                  aCanvas.drawLine( x3 + textWidth + 4, midY, x2, midY );
                }

                aCanvas.drawString( annText, x1 + textXoffset, y1 + fontHeight );
              }
            }
          }
        }

        // Advance to the next channel...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      // remove the signal offset...
      aCanvas.translate( 0, -signalOffset );

      if ( signalElement.isGroupSummary() )
      {
        // Draw nothing...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      if ( signalElement.isAnalogSignal() )
      {
        // Draw nothing...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      // remove the signal offset...
      aCanvas.translate( 0, signalOffset );
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

    for ( int i = 0; i < Cursor.MAX_CURSORS; i++ )
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

    this.arrowRenderer.setContext( Integer.valueOf( w ), Integer.valueOf( middlePos ) );
    this.arrowRenderer.render( aCanvas, x, y );
  }

  /**
   * Paints the individual signal channels, group bytes and analogue scope
   * signals.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aSignalElements
   *          the signal elements to draw, cannot be <code>null</code> or empty!
   */
  private void paintSignals( final Graphics2D aCanvas, final SignalViewModel aModel,
      final SignalElement[] aSignalElements )
  {
    final int[] values = aModel.getDataValues();
    final long[] timestamps = aModel.getTimestamps();
    if ( ( timestamps == null ) || ( timestamps.length == 0 ) || ( aSignalElements.length == 0 ) )
    {
      // Nothing to do...
      return;
    }

    final Rectangle clip = aCanvas.getClipBounds();

    aCanvas.setBackground( aModel.getBackgroundColor() );
    aCanvas.clearRect( clip.x, clip.y, clip.width, clip.height );

    final int startIdx = aModel.getStartIndex( clip );
    final int endIdx = aModel.getEndIndex( clip, values.length );

    final int signalHeight = aModel.getSignalHeight();
    // Where is the signal to be drawn?
    final int signalOffset = aModel.getSignalOffset();
    final double zoomFactor = aModel.getZoomFactor();

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aSignalElements[0].getYposition() + signalOffset );

    final int sampleIncr = ( int )Math.max( 1.0, ( 1.0 / zoomFactor ) );

    for ( SignalElement signalElement : aSignalElements )
    {
      aCanvas.setColor( signalElement.getColor() );

      if ( signalElement.isSignalGroup() )
      {
        // Draw nothing...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      if ( signalElement.isDigitalSignal() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( false /* aUseAA */) );

        if ( !signalElement.isEnabled() )
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

          for ( int sampleIdx = startIdx + 1; sampleIdx < endIdx; sampleIdx++ )
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
        }

        // Advance to the next channel...
        aCanvas.translate( 0, signalElement.getHeight() );
      }

      // remove the signal offset...
      aCanvas.translate( 0, -signalOffset );

      if ( signalElement.isGroupSummary() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( aModel.isRenderGroupSummaryAntiAliased() ) );

        int mask = signalElement.getMask();

        int prevSampleValue = values[startIdx] & mask;
        int prevX = ( int )( zoomFactor * timestamps[startIdx] );

        FontMetrics fm = aCanvas.getFontMetrics();
        int textYpos = ( int )( ( signalElement.getHeight() + fm.getLeading() + fm.getMaxAscent() ) / 2.0 ) - 2;

        for ( int sampleIdx = startIdx + 1; sampleIdx < endIdx; sampleIdx += sampleIncr )
        {
          int sampleValue = ( values[sampleIdx] & mask );

          if ( sampleValue != prevSampleValue )
          {
            int x = ( int )( zoomFactor * timestamps[sampleIdx] );

            String text = String.format( "%02x", Integer.valueOf( prevSampleValue ) );

            int textWidth = fm.stringWidth( text ) + ( 2 * PADDING_X );
            int cellWidth = x - prevX;
            if ( textWidth < cellWidth )
            {
              int textXpos = prevX + ( int )( ( cellWidth - textWidth ) / 2.0 ) + PADDING_X;

              aCanvas.drawString( text, textXpos, textYpos );
            }

            // draw a small line...
            aCanvas.drawLine( x, PADDING_Y, x, signalElement.getHeight() - ( 2 * PADDING_Y ) );

            prevX = x;
          }

          prevSampleValue = sampleValue;
        }

        aCanvas.translate( 0, signalElement.getHeight() );
      }

      if ( signalElement.isAnalogSignal() )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( aModel.isRenderScopeSignalAntiAliased() ) );

        int mask = signalElement.getMask();
        final int trailingZeros = Integer.numberOfTrailingZeros( mask );
        final int onesCount = Integer.SIZE - Integer.numberOfLeadingZeros( mask ) - trailingZeros;
        final int maxValue = ( int )( 1L << onesCount );
        double scaleFactor = ( signalElement.getHeight() - ( 2 * PADDING_Y ) ) / ( maxValue + 1.0 );

        // Make sure we always start with time 0...
        int p = 0;
        for ( int sampleIdx = startIdx + sampleIncr; sampleIdx < endIdx; sampleIdx += sampleIncr )
        {
          long timestamp = timestamps[sampleIdx - sampleIncr];
          int sampleValue = 0;
          for ( int i = sampleIdx - sampleIncr; i < sampleIdx; i++ )
          {
            sampleValue += ( ( values[sampleIdx] & mask ) >> trailingZeros );
          }
          sampleValue = maxValue - ( sampleValue / sampleIncr );

          x[p] = ( int )( zoomFactor * timestamp );
          y[p] = PADDING_Y + ( int )( scaleFactor * sampleValue );
          p++;
        }

        aCanvas.drawPolyline( x, y, p );

        aCanvas.translate( 0, signalElement.getHeight() );
      }

      // remove the signal offset...
      aCanvas.translate( 0, signalOffset );
    }
  }
}
