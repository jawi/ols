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


import static java.awt.RenderingHints.*;
import static javax.swing.UIManager.*;
import static nl.lxtreme.ols.client2.views.UIMgr.*;
import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Alignment;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides the actual view of waveform elements.
 */
final class WaveformViewComponent extends JComponent implements Scrollable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** The maximum number of points in a polyline. */
  private static final int POINT_COUNT = 50000;
  /**
   * The number of samples that are shown in a single view upon which is decided
   * to draw the group summary and scope a bit sloppy.
   */
  private static final int SLOPPY_DRAW_THRESHOLD = 10000;

  // VARIABLES

  private final WaveformModel model;

  private final int[] x = new int[2 * POINT_COUNT];
  private final int[] y = new int[2 * POINT_COUNT];

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformViewComponent} instance.
   */
  public WaveformViewComponent( WaveformModel aModel )
  {
    this.model = aModel;

    // Inherit the popup menu from our parent...
    setInheritsPopupMenu( true );
    setOpaque( true );
    // Enable synthetic drag events (even when mouse is outside window)...
    setAutoscrolls( true );
    // We can receive the focus...
    setFocusable( true );
    setRequestFocusEnabled( true );
  }

  // METHODS

  /**
   * Creates the rendering hints for this the drawing of arrows.
   */
  @SuppressWarnings( "unused" )
  private static RenderingHints createArrowRenderingHints()
  {
    RenderingHints hints = new RenderingHints( KEY_INTERPOLATION, VALUE_INTERPOLATION_BICUBIC );
    hints.put( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON );
    hints.put( KEY_ALPHA_INTERPOLATION, VALUE_ALPHA_INTERPOLATION_SPEED );
    hints.put( KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_SPEED );
    hints.put( KEY_RENDERING, VALUE_RENDER_QUALITY );
    return hints;
  }

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createCursorRenderingHints()
  {
    RenderingHints hints = new RenderingHints( KEY_INTERPOLATION, VALUE_INTERPOLATION_NEAREST_NEIGHBOR );
    hints.put( KEY_ANTIALIASING, VALUE_ANTIALIAS_OFF );
    hints.put( KEY_RENDERING, VALUE_RENDER_SPEED );
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
    RenderingHints hints = new RenderingHints( KEY_INTERPOLATION, VALUE_INTERPOLATION_BILINEAR );
    hints.put( KEY_ANTIALIASING, aUseAA ? VALUE_ANTIALIAS_ON : VALUE_ANTIALIAS_OFF );
    hints.put( KEY_ALPHA_INTERPOLATION, VALUE_ALPHA_INTERPOLATION_SPEED );
    hints.put( KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_SPEED );
    hints.put( KEY_RENDERING, VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    super.addNotify();

    // Bootstrap the width of this component...
    int width = ( int )( this.model.getZoomFactor() * this.model.getAbsoluteLength() );
    int height = this.model.calculateScreenHeight();

    setPreferredSize( new Dimension( width, height ) );

    revalidate();
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
  protected void paintComponent( Graphics aGraphics )
  {
    try
    {
      final Rectangle clip = aGraphics.getClipBounds();
      final WaveformElement[] elements = this.model.getWaveformElements( clip.y, clip.height, LOOSE_MEASURER );

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
      if ( this.model.areCursorsVisible() )
      {
        paintCursors( canvas );
      }

      // Draw the measurement stuff...
      // if ( this.model.isMeasurementMode() && MeasurementInfo.isDefined(
      // this.measurementInfo ) )
      // {
      // paintMeasurementArrow( canvas, model, this.measurementInfo );
      // }
    }
    finally
    {
      // this.listening = true;
    }
  }

  /**
   * Returns the stroke to use to render the annotation lines.
   * 
   * @param aAltRenderStyle
   * @return
   */
  private Stroke getAnnotationLineStroke( final boolean aAltRenderStyle )
  {
    final double zoomFactor = this.model.getZoomFactor();
    final float strokeWidth = ( float )( 3.0f / Math.max( 1.0f, ( 1.0f / zoomFactor ) ) );

    final BasicStroke stroke;
    if ( aAltRenderStyle )
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
    int blockIncr = 50;

    int firstVisibleSample = this.model.coordinateToSampleIndex( aVisibleRect.getLocation() );
    int lastVisibleSample = this.model.coordinateToSampleIndex( new Point( aVisibleRect.x + aVisibleRect.width, 0 ) );
    int lastSampleIdx = this.model.getSampleCount();

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
    WaveformElement[] elements = this.model.getWaveformElements( aVisibleRect.y + 1, 1, LOOSE_MEASURER );
    if ( elements.length == 0 )
    {
      return 0;
    }

    final int spacing = UIManager.getInt( SIGNAL_ELEMENT_SPACING );

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
        elements = this.model.getWaveformElements( yPos - spacing, 1, LOOSE_MEASURER );
        if ( elements.length > 0 )
        {
          inc += elements[0].getHeight() + spacing;
        }
      }
    }

    return inc;
  }

  /**
   * Paints the cursors over the signals.
   * 
   * @param aCanvas
   *          the canvas to paint the cursor on;
   * @param aModel
   *          the model to use;
   */
  private void paintCursors( Graphics2D aCanvas )
  {
    Rectangle clip = aCanvas.getClipBounds();
    AcquisitionData data = this.model.getData();

    // Tell Swing how we would like to render ourselves...
    aCanvas.setRenderingHints( createCursorRenderingHints() );

    nl.lxtreme.ols.common.acquisition.Cursor[] cursors = data.getCursors();
    for ( int i = 0; i < cursors.length; i++ )
    {
      nl.lxtreme.ols.common.acquisition.Cursor cursor = cursors[i];
      if ( !cursor.isDefined() )
      {
        continue;
      }

      int cursorXpos = this.model.timestampToCoordinate( cursor.getTimestamp() );
      if ( ( cursorXpos < 0 ) || !clip.contains( cursorXpos, clip.y ) )
      {
        // Trivial reject: don't paint undefined cursors, or cursors outside the
        // clip boundaries...
        continue;
      }

      aCanvas.setColor( getCursorColor( cursor ) );
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
  // private void paintMeasurementArrow( Graphics2D aCanvas, MeasurementInfo
  // aMeasurementInfo )
  // {
  // Rectangle signalHoverRect = aMeasurementInfo.getRectangle();
  // int x = signalHoverRect.x;
  // int y = ( int )signalHoverRect.getCenterY();
  // int w = signalHoverRect.width;
  // int middlePos = aMeasurementInfo.getMidSamplePos().intValue() - x;
  //
  // // Tell Swing how we would like to render ourselves...
  // aCanvas.setRenderingHints( createArrowRenderingHints() );
  //
  // aCanvas.setColor( aModel.getMeasurementArrowColor() );
  //
  // // Allow arrow renderer to start with [0, 0] as coordinate system...
  // aCanvas.translate( x, y );
  //
  // ArrowRenderer.render( aCanvas, w, middlePos );
  //
  // // Restore to original coordinate system...
  // aCanvas.translate( -x, -y );
  // }

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
    final AcquisitionData data = this.model.getData();
    final AnnotationData annotations = this.model.getAnnotations();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();
    final long absLength = data.getAbsoluteLength();

    final Rectangle clip = aCanvas.getClipBounds();

    Color background = getColor( SIGNALVIEW_BACKGROUND_COLOR, Color.WHITE );

    // Some drawing primitives we're going to re-use over and over...
    boolean annotationRenderStyle = getBoolean( SIGNALVIEW_ANNOTATION_USE_ALTSTYLE );
    Stroke stroke = getAnnotationLineStroke( annotationRenderStyle );
    AlphaComposite alphaComposite = AlphaComposite.SrcOver.derive( getInt( SIGNALVIEW_ANNOTATION_ALPHA ) / 100.0f );

    aCanvas.setBackground( background );
    aCanvas.clearRect( clip.x, clip.y, clip.width, clip.height );

    final int startIdx = this.model.getStartIndex( clip );
    final long startTime = timestamps[startIdx];
    final int endIdx = this.model.getEndIndex( clip, values.length );
    final long endTime = timestamps[endIdx];

    final double zoomFactor = this.model.getZoomFactor();

    if ( data.hasTriggerData() )
    {
      final long triggerOffset = data.getTriggerPosition();
      if ( ( timestamps[startIdx] <= triggerOffset ) && ( timestamps[endIdx] >= triggerOffset ) )
      {
        // Draw a line denoting the trigger position...
        final int x = ( int )Math.round( triggerOffset * zoomFactor ) - 1;

        aCanvas.setColor( getColor( SIGNALVIEW_TRIGGER_COLOR, Color.RED ) );
        aCanvas.drawLine( x, clip.y, x, clip.y + clip.height );
      }
    }

    // Start drawing at the correct position in the clipped region...
    aCanvas.translate( 0, aElements[0].getYposition() );

    final boolean enableSloppyScopePainting = !getBoolean( DISABLE_SLOPPY_SCOPE_PAINTING );
    int lastP = 0;

    int elementSpacing = getInt( SIGNAL_ELEMENT_SPACING );

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
        int idx = element.getIndex();
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

          aCanvas.setFont( UIMgr.getFont( SIGNALVIEW_ANNOTATION_FONT ) );

          FontMetrics fm = aCanvas.getFontMetrics();
          int fontHeight = fm.getHeight();
          Alignment alignment = Alignment.valueOf( getString( SIGNALVIEW_ANNOTATION_ALIGNMENT, "CENTER" ) );

          // Draw the annotations...
          for ( DataAnnotation ann : annotations.getAnnotations( idx, startTime, endTime ) )
          {
            long annStartTime = ann.getStartTimestamp();
            long annEndTime = ann.getEndTimestamp();

            int x1 = ( int )( annStartTime * zoomFactor );
            int x2 = ( int )( annEndTime * zoomFactor );
            int y1 = element.getOffset( alignment );
            int y2 = y1 + element.getSignalHeight();
            int midY = y1 + ( ( y2 - y1 ) / 2 );

            String annText = ann.getText( DataAnnotation.OPTION_DEFAULT );

            int annotationWidth = ( x2 - x1 ) + 2;

            Composite oldComposite = aCanvas.getComposite();
            Stroke oldStroke = aCanvas.getStroke();

            aCanvas.setComposite( alphaComposite );

            // Fade out the signal itself...
            aCanvas.setColor( background );
            if ( annotationRenderStyle )
            {
              aCanvas.fillRect( x1, y1 + 0, annotationWidth, ( y2 - y1 ) + 1 );
            }
            else
            {
              aCanvas.fillRect( x1, y1 + 1, annotationWidth, ( y2 - y1 ) - 1 );
            }

            aCanvas.setComposite( oldComposite );

            // Draw the thick boundaries...
            aCanvas.setColor( getColor( SIGNALVIEW_ANNOTATION_COLOR, Color.LIGHT_GRAY ) );
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

        // Move back to the original position...
        aCanvas.translate( 0, -signalOffset );
      }

      int sampleIncr = 1;
      if ( enableSloppyScopePainting && ( lastP > SLOPPY_DRAW_THRESHOLD ) )
      {
        sampleIncr = ( int )Math.max( 1.0, ( 1.0 / zoomFactor ) );
      }

      if ( Type.GROUP_SUMMARY.equals( type ) )
      {
        // Tell Swing how we would like to render ourselves...
        aCanvas.setRenderingHints( createSignalRenderingHints( getBoolean( SIGNALVIEW_GROUP_SUMMARY_RENDER_AA ) ) );

        int mask = element.getMask();

        int padding = getInt( SIGNALVIEW_GROUP_SUMMARY_PADDING );

        int prevSampleValue = values[startIdx] & mask;
        int prevX = ( int )( zoomFactor * timestamps[startIdx] );

        aCanvas.setFont( UIMgr.getFont( SIGNALVIEW_GROUP_SUMMARY_TEXT_FONT ) );

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

            aCanvas.setColor( getColor( SIGNALVIEW_GROUP_SUMMARY_BAR_COLOR, Color.GRAY ) );

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
        aCanvas.setRenderingHints( createSignalRenderingHints( getBoolean( SIGNALVIEW_ANALOG_SCOPE_RENDER_AA ) ) );

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
