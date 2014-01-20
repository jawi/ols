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
import static java.lang.Math.*;
import static javax.swing.UIManager.*;
import static nl.lxtreme.ols.client2.views.UIMgr.*;

import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.Unit.Value;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor.LabelStyle;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides the timeline for use in {@link WaveformView}.
 */
final class WaveformTimelineComponent extends JComponent
{
  // INNER TYPES

  /**
   * Helper class for the placement of cursor labels.
   */
  private class CursorLabel implements Comparable<CursorLabel>
  {
    // VARIABLES

    final List<LabelPlacement> placements = new ArrayList<LabelPlacement>();
    final FontMetrics fm;
    final Cursor cursor;
    final Rectangle boundaries;

    volatile String text;
    volatile boolean mirrored;
    volatile ListIterator<LabelPlacement> placementIter;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CursorLabel} instance.
     */
    public CursorLabel( Cursor aCursor, String aText, FontMetrics aFM, Rectangle aBoundaries )
    {
      this.cursor = aCursor;
      this.text = aText;
      this.fm = aFM;
      this.boundaries = aBoundaries;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( CursorLabel aLabel )
    {
      int result = this.boundaries.x - aLabel.boundaries.x;
      if ( result == 0 )
      {
        result = this.boundaries.width - aLabel.boundaries.width;
      }
      if ( result == 0 )
      {
        result = this.boundaries.y - aLabel.boundaries.y;
      }
      return result;
    }

    public Polygon getPolygon()
    {
      int x = this.boundaries.x;
      int w = this.boundaries.width;
      int y = this.boundaries.y;
      int h = this.boundaries.height;

      Polygon flagPoly = new Polygon();
      if ( this.mirrored )
      {
        flagPoly.xpoints = new int[] { x, x + w, x + w, x + ( h / 3 ), x };
        flagPoly.ypoints = new int[] { y, y, y + h, y + h, y + ( ( 2 * h ) / 3 ) };
        flagPoly.npoints = 5;
      }
      else
      {
        flagPoly.xpoints = new int[] { x, x + w, x + w, ( x + w ) - ( h / 3 ), x };
        flagPoly.ypoints = new int[] { y, y, y + ( ( 2 * h ) / 3 ), y + h, y + h };
        flagPoly.npoints = 5;
      }
      return flagPoly;
    }

    /**
     * @return <code>true</code> if there are more placements to use,
     *         <code>false</code> if no more placements.
     */
    public boolean hasMoreStyles()
    {
      return this.placementIter.hasNext();
    }

    /**
     * Returns whether or not this cursor label intersects with the given cursor
     * label.
     * 
     * @param aLabel
     *          the cursor label to test against, cannot be <code>null</code>.
     * @return <code>true</code> if this cursor label intersects the given
     *         cursor label, <code>false</code> otherwise.
     */
    public boolean intersects( CursorLabel aLabel )
    {
      return this.boundaries.intersects( aLabel.boundaries );
    }

    /**
     * Determines if this cursor label is entirely visible using the given clip
     * boundaries.
     * 
     * @param aClipBounds
     *          the clip bounds to check against, cannot be <code>null</code>.
     * @return <code>true</code> if this cursor label is completely visible,
     *         <code>false</code> otherwise.
     */
    public boolean isCompletelyVisible( Rectangle aClipBounds )
    {
      return aClipBounds.contains( this.boundaries );
    }

    /**
     * Returns to the default label placement style.
     */
    public void resetStyle( Rectangle aClipBounds )
    {
      // Determine what possible placements we've got for this label...
      this.placements.clear();

      for ( LabelPlacement element : PLACEMENTS )
      {
        if ( isValidPlacement( element, aClipBounds ) )
        {
          this.placements.add( element );
        }
      }

      this.placementIter = this.placements.listIterator();
      recalculateBoundaries();
    }

    /**
     * Recalculates the boundaries for this label.
     */
    public void useNextStyle()
    {
      if ( this.placementIter.hasNext() )
      {
        recalculateBoundaries();
      }
    }

    /**
     * @param labelPlacement
     * @param aClipBounds
     * @param aModel
     * @param aFM
     * @return
     */
    private boolean isValidPlacement( LabelPlacement labelPlacement, Rectangle aClipBounds )
    {
      // Modify the label style of the previous label...
      String flagText = this.cursor.getLabel( labelPlacement.style );

      Rectangle rect = new Rectangle( this.boundaries );
      rect.width = this.fm.stringWidth( flagText ) + PADDING_WIDTH;
      rect.x = model.timestampToCoordinate( this.cursor.getTimestamp() );
      if ( labelPlacement.mirrored )
      {
        rect.x = Math.max( 0, rect.x - rect.width );
      }

      return aClipBounds.contains( rect );
    }

    /**
     * @param aModel
     * @param aFM
     */
    private void recalculateBoundaries()
    {
      if ( ( this.placementIter == null ) || !this.placementIter.hasNext() )
      {
        return;
      }
      final LabelPlacement labelPlacement = this.placementIter.next();

      // Modify the label style of the previous label...
      String flagText = this.cursor.getLabel( labelPlacement.style );

      this.text = flagText;
      this.boundaries.width = this.fm.stringWidth( flagText ) + PADDING_WIDTH;
      this.boundaries.x = model.timestampToCoordinate( this.cursor.getTimestamp() );
      this.mirrored = labelPlacement.mirrored;

      if ( this.mirrored )
      {
        this.boundaries.x = Math.max( 0, this.boundaries.x - this.boundaries.width );
      }
    }
  }

  /**
   * Provides an enumeration on how labels are to be placed on screen.
   */
  private static enum LabelPlacement
  {
    INDEX_ONLY_RIGHT( LabelStyle.INDEX_ONLY, false /* mirrored */), //
    INDEX_ONLY_LEFT( LabelStyle.INDEX_ONLY, true /* mirrored */), //
    TIME_ONLY_RIGHT( LabelStyle.TIME_ONLY, false /* mirrored */), //
    TIME_ONLY_LEFT( LabelStyle.TIME_ONLY, true /* mirrored */), //
    LABEL_ONLY_RIGHT( LabelStyle.LABEL_ONLY, false /* mirrored */), //
    LABEL_ONLY_LEFT( LabelStyle.LABEL_ONLY, true /* mirrored */), //
    INDEX_LABEL_RIGHT( LabelStyle.INDEX_LABEL, false /* mirrored */), //
    INDEX_LABEL_LEFT( LabelStyle.INDEX_LABEL, true /* mirrored */), //
    LABEL_TIME_RIGHT( LabelStyle.LABEL_TIME, false /* mirrored */), //
    LABEL_TIME_LEFT( LabelStyle.LABEL_TIME, true /* mirrored */);

    // VARIABLES

    final LabelStyle style;
    final boolean mirrored;

    // CONSTRUCTORS

    /**
     * Creates a new LabelPlacement instance.
     */
    private LabelPlacement( final LabelStyle aStyle, final boolean aMirrored )
    {
      this.style = aStyle;
      this.mirrored = aMirrored;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final LabelPlacement[] PLACEMENTS = { LabelPlacement.LABEL_TIME_RIGHT, LabelPlacement.LABEL_TIME_LEFT,
      LabelPlacement.LABEL_ONLY_LEFT, LabelPlacement.TIME_ONLY_LEFT, LabelPlacement.INDEX_ONLY_LEFT,
      LabelPlacement.INDEX_ONLY_RIGHT, LabelPlacement.TIME_ONLY_RIGHT, LabelPlacement.LABEL_ONLY_RIGHT };

  private static final int PADDING_TOP = 2;
  private static final int PADDING_LEFT = 2;
  private static final int PADDING_RIGHT = 1;

  private static final int PADDING_WIDTH = PADDING_LEFT + PADDING_RIGHT;
  private static final int PADDING_HEIGHT = 2 * PADDING_TOP;

  /** The horizontal padding for all texts. */
  private static final int TEXT_PADDING_X = 2;
  /** The vertical padding (in px) of the timeline view. */
  private static final int VERTICAL_PADDING = 1;

  // VARIABLES

  private final WaveformModel model;
  private final Formatter formatter;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformTimelineComponent} instance.
   */
  public WaveformTimelineComponent( WaveformModel aModel )
  {
    this.model = aModel;

    this.formatter = new Formatter();

    setOpaque( true );
    // Inherit the popup menu from our parent...
    setInheritsPopupMenu( true );
    // Enable synthetic drag events (even when mouse is outside window)...
    setAutoscrolls( true );
    // We can NOT receive the focus...
    setFocusable( false );
  }

  // METHODS

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createRenderingHints()
  {
    RenderingHints hints = new RenderingHints( KEY_INTERPOLATION, VALUE_INTERPOLATION_NEAREST_NEIGHBOR );
    hints.put( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON );
    hints.put( KEY_ALPHA_INTERPOLATION, VALUE_ALPHA_INTERPOLATION_QUALITY );
    hints.put( KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_QUALITY );
    hints.put( KEY_RENDERING, VALUE_RENDER_QUALITY );
    return hints;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( Graphics aGraphics )
  {
    int baseTickYpos = getInt( TIMELINE_HEIGHT, 40 ) - VERTICAL_PADDING;
    int tickYpos = baseTickYpos - getInt( TIMELINE_TICK_HEIGHT, 2 );
    int majorTickYpos = baseTickYpos - getInt( TIMELINE_MAJOR_TICK_HEIGHT, 10 );
    int minorTickYpos = baseTickYpos - getInt( TIMELINE_MAJOR_TICK_HEIGHT, 5 );

    AcquisitionData data = this.model.getData();

    Rectangle clip = aGraphics.getClipBounds();

    Graphics2D canvas = ( Graphics2D )aGraphics.create();

    try
    {
      // Tell Swing how we would like to render ourselves...
      canvas.setRenderingHints( createRenderingHints() );

      canvas.setBackground( getColor( SIGNALVIEW_BACKGROUND_COLOR, Color.WHITE ) );
      canvas.clearRect( clip.x, clip.y, clip.width, clip.height );

      Rectangle visibleRect = getVisibleRect();

      double zoomFactor = this.model.getZoomFactor();
      double triggerOffset = data.hasTriggerData() ? data.getTriggerPosition() * zoomFactor : 0;

      // s denotes the amount of pixels per second...
      double s = zoomFactor * data.getSampleRate();
      // p denotes the amount of time per pixel...
      double p = 1.0 / s;
      // UoT represents a logical unit-of-time...
      double uot = pow( 10, ceil( log10( p ) ) );
      // ts denotes the number of pixels per unit-of-time...
      double ts = uot * s;

      // determine the tick increment based on the current width of the visible
      // timeline; this will make it a factor of 1, 10, 100, 1000, ...
      int majorTickInc = ( int )max( 1.0, pow( 10, floor( log10( visibleRect.width / 2 ) ) ) );
      int minorTickInc = ( majorTickInc / 2 );
      double minorTickTime = ( minorTickInc * uot );
      int tickInc = max( 1, majorTickInc / 10 );

      // tts denotes rounding factor to use ...
      double tts = majorTickInc * ts;
      // to denotes the trigger offset...
      double to = triggerOffset % tts;

      int startX = clip.x;
      int endX = ( clip.x + clip.width );

      // Start *before* the clip region starts; but at a multiple of our
      // rounding factor...
      double x = ( ( floor( startX / tts ) - 1 ) * tts ) + to;

      int tick = 0;
      for ( ; x <= endX; tick++, x += ts )
      {
        int xPos = ( int )round( x );
        if ( ( xPos < startX ) || ( xPos > endX ) )
        {
          // Trivial reject: only paint visible items...
          continue;
        }

        final double time = ( x - triggerOffset );

        if ( abs( time ) < 1.0e-5 )
        {
          canvas.setColor( getColor( SIGNALVIEW_TRIGGER_COLOR, Color.RED ) );
        }
        else
        {
          canvas.setColor( getColor( TIMELINE_TICK_COLOR, Color.DARK_GRAY ) );
        }

        String text = null;
        int textYpos = -1;

        if ( ( tick % majorTickInc ) == 0 )
        {
          textYpos = majorTickYpos;

          canvas.drawLine( xPos, majorTickYpos, xPos, baseTickYpos );

          canvas.setFont( UIMgr.getFont( TIMELINE_MAJOR_TICK_LABEL_FONT ) );

          double t = round( ( time * p ) / uot ) * uot;

          text = getMajorTimestamp( t, uot );
        }
        else if ( ( tick % minorTickInc ) == 0 )
        {
          textYpos = minorTickYpos;

          canvas.drawLine( xPos, minorTickYpos, xPos, baseTickYpos );

          canvas.setFont( UIMgr.getFont( TIMELINE_MINOR_TICK_LABEL_FONT ) );

          if ( getBoolean( TIMELINE_MINOR_TICKS_VISIBLE ) )
          {
            text = getMinorTimestamp( time < 0 ? -minorTickTime : minorTickTime, uot );
          }
        }
        else if ( ( tick % tickInc ) == 0 )
        {
          canvas.drawLine( xPos, tickYpos, xPos, baseTickYpos );
        }

        if ( text != null )
        {
          FontMetrics fm = canvas.getFontMetrics();

          canvas.setColor( getColor( TIMELINE_TEXT_COLOR, Color.WHITE ) );

          int textWidth = ( fm.stringWidth( text ) + TEXT_PADDING_X );

          int textXpos = Math.max( visibleRect.x, ( int )( xPos - ( textWidth / 2.0 ) ) ) + 1;
          textYpos = Math.max( visibleRect.y, ( textYpos - fm.getDescent() ) );

          // canvas.drawString( text, textXpos, textYpos );
          drawLabel( canvas, text, textXpos, textYpos );
        }
      }

      // Draw the cursor "flags"...
      if ( this.model.areCursorsVisible() )
      {
        paintCursorFlags( canvas );
      }
    }
    finally
    {
      canvas.dispose();
      canvas = null;
    }
  }

  /**
   * @param aTime
   * @param aTimeScale
   * @return
   */
  private String displayTime( double aTime, double aTimeScale, int aPrecision )
  {
    Unit.Time timeScale = Unit.Time.toUnit( aTimeScale );
    Value time = Value.asTime( aTime );

    // determine the magnitude of difference...
    int mag = Math.abs( timeScale.ordinal() - time.getUnit().ordinal() );

    if ( mag > 1 )
    {
      // More than a factor 1000 difference...
      // return BaseView.formatTime( ptr, aTime );
    }

    StringBuilder out = ( StringBuilder )this.formatter.out();
    out.setLength( 0 );

    time.formatTo( this.formatter, 0, -1, aPrecision );

    return out.toString();
  }

  /**
   * Draws the label (and optionally its drop shadow).
   * 
   * @param aCanvas
   *          the canvas to paint on;
   * @param aModel
   *          the model to use;
   * @param aText
   *          the text to draw;
   * @param aXpos
   *          the X position where the text should be drawn;
   * @param aYpos
   *          the Y position where the text should be drawn.
   */
  private void drawLabel( final Graphics2D aCanvas, final String aText, final int aXpos, final int aYpos )
  {
    if ( getBoolean( TIMELINE_DRAW_TEXT_SHADOW ) )
    {
      aCanvas.setColor( getColor( TIMELINE_TEXT_SHADOW_COLOR, Color.BLACK ) );
      aCanvas.drawString( aText, aXpos + 2, aYpos + 2 );
    }

    aCanvas.setColor( getColor( TIMELINE_TEXT_COLOR, Color.WHITE ) );
    aCanvas.drawString( aText, aXpos, aYpos );
  }

  /**
   * Returns the cursor color to paint.
   * 
   * @param aCursor
   *          the cursor to get the color for, cannot be <code>null</code>.
   * @return a cursor color, never <code>null</code>.
   */
  private Color getCursorColor( Cursor aCursor )
  {
    Color cursorColor = UIMgr.getCursorColor( aCursor );
    if ( aCursor.equals( this.model.getSelectedCursor() ) )
    {
      cursorColor = cursorColor.brighter();
    }
    return cursorColor;
  }

  /**
   * @param sampleRate
   * @param majorTimestamp
   * @return
   */
  private String getMajorTimestamp( double aTime, double aTimeScale )
  {
    return displayTime( aTime, aTimeScale, 2 );
  }

  /**
   * @param aTime
   * @param sampleRate
   * @return
   */
  private String getMinorTimestamp( double aTime, double aTimeScale )
  {
    return Value.asTime( aTime ).toString();
  }

  /**
   * Paints the cursors on this timeline.
   * 
   * @param aCanvas
   *          the canvas to paint the cursor (flags) on;
   */
  private void paintCursorFlags( Graphics2D aCanvas )
  {
    LinkedList<CursorLabel> labels = new LinkedList<CursorLabel>();

    Rectangle clip = aCanvas.getClipBounds();

    // Phase 0: preparation...
    aCanvas.setFont( UIMgr.getFont( TIMELINE_CURSOR_FLAG_FONT ) );

    FontMetrics fm = aCanvas.getFontMetrics();
    int yOffset = fm.getLeading() + fm.getAscent() + PADDING_TOP;

    AcquisitionData data = this.model.getData();
    Cursor[] cursors = data.getCursors();

    // Phase 1: determine the boundaries of each defined cursor that should be
    // shown in the current clip boundaries...
    for ( Cursor cursor : cursors )
    {
      if ( !cursor.isDefined() )
      {
        continue;
      }

      Rectangle boundaries = new Rectangle();

      String flagText = cursor.getLabel( LabelStyle.LABEL_TIME );
      int cursorXpos = this.model.timestampToCoordinate( cursor.getTimestamp() );

      boundaries.height = fm.getHeight() + PADDING_HEIGHT;
      boundaries.width = fm.stringWidth( flagText ) + PADDING_WIDTH;
      boundaries.x = cursorXpos;
      boundaries.y = getHeight() - boundaries.height - 1;

      if ( ( boundaries.x < 0 ) || !clip.intersects( boundaries ) )
      {
        // Trivial reject: don't paint undefined cursors...
        continue;
      }

      labels.add( new CursorLabel( cursor, flagText, fm, boundaries ) );
    }

    // Phase 2: sort the labels so they are ordered from left to right on
    // screen...
    Collections.sort( labels );

    // Phase 3: try to optimize the overlapping labels...
    placeLabels( clip, labels );

    final AlphaComposite alphaComposite = AlphaComposite.SrcOver.derive( 0.7f );

    // Phase 4: draw the labels...
    for ( CursorLabel label : labels )
    {
      Rectangle boundaries = label.boundaries;

      Color cursorColor = getCursorColor( label.cursor );
      Color cursorTextColor = ColorUtils.getContrastColor( cursorColor );

      Composite oldComposite = aCanvas.getComposite();
      Stroke oldStroke = aCanvas.getStroke();

      aCanvas.setComposite( alphaComposite );

      Polygon flagPoly = label.getPolygon();

      aCanvas.setColor( cursorTextColor );
      // It appears that the fill/draw polygon routines are using some kind of
      // optimization that fails for large values, causing the flags to be
      // painted in a weird way...
      // aCanvas.drawPolygon( flagPoly );
      aCanvas.fillRect( label.boundaries.x, label.boundaries.y, label.boundaries.width, label.boundaries.height );

      aCanvas.setComposite( oldComposite );
      aCanvas.setStroke( oldStroke );

      aCanvas.setColor( cursorColor );
      // It appears that the fill/draw polygon routines are using some kind of
      // optimization that fails for large values, causing the flags to be
      // painted in a weird way...
      // aCanvas.drawPolygon( flagPoly );

      aCanvas.drawLine( flagPoly.xpoints[0], flagPoly.ypoints[0], flagPoly.xpoints[1], flagPoly.ypoints[1] );
      aCanvas.drawLine( flagPoly.xpoints[1], flagPoly.ypoints[1], flagPoly.xpoints[2], flagPoly.ypoints[2] );
      aCanvas.drawLine( flagPoly.xpoints[2], flagPoly.ypoints[2], flagPoly.xpoints[3], flagPoly.ypoints[3] );
      aCanvas.drawLine( flagPoly.xpoints[3], flagPoly.ypoints[3], flagPoly.xpoints[4], flagPoly.ypoints[4] );
      aCanvas.drawLine( flagPoly.xpoints[4], flagPoly.ypoints[4], flagPoly.xpoints[0], flagPoly.ypoints[0] );

      final int textXpos = boundaries.x + ( label.mirrored ? PADDING_LEFT : PADDING_RIGHT );
      final int textYpos = boundaries.y + yOffset;

      aCanvas.drawString( label.text, textXpos, textYpos );
    }
  }

  /**
   * Tries to place all labels in such way that they do not overlap.
   * <p>
   * This method performs a best effort in the sense that it tries to shorten
   * labels if needed. The used algorithm does not always succeed in finding the
   * optimal solution, hence overlap still may occur.
   * </p>
   * 
   * @param aClipBounds
   *          the clipping region to use;
   * @param aModel
   *          the model to use;
   * @param aLabels
   *          the labels to optimize.
   */
  private void placeLabels( Rectangle aClipBounds, List<CursorLabel> aLabels )
  {
    for ( int i = 0; i < aLabels.size(); i++ )
    {
      aLabels.get( i ).resetStyle( aClipBounds );
    }

    // The labels are sorted from left to right (regarding screen coordinates),
    // place them in reverse order to minimize the overlap...
    for ( int li = aLabels.size() - 1; li > 0; li-- )
    {
      final CursorLabel previousLabel = aLabels.get( li - 1 );
      final CursorLabel currentLabel = aLabels.get( li );

      while ( previousLabel.intersects( currentLabel ) && currentLabel.hasMoreStyles() )
      {
        do
        {
          // Modify the label style of the previous label...
          previousLabel.useNextStyle();
        }
        while ( previousLabel.hasMoreStyles()
            && ( previousLabel.intersects( currentLabel ) || !previousLabel.isCompletelyVisible( aClipBounds ) ) );

        if ( previousLabel.intersects( currentLabel ) || !currentLabel.isCompletelyVisible( aClipBounds ) )
        {
          // Ok; still overlapping labels, use an alternative style for the
          // current label and try again...
          previousLabel.resetStyle( aClipBounds );

          // Try next style in the next loop...
          currentLabel.useNextStyle();
        }
      }
    }
  }
}
