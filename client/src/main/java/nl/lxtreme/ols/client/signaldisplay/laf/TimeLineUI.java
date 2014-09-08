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


import static java.awt.RenderingHints.*;
import static java.lang.Math.*;

import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.util.CursorFlagTextFormatter.LabelStyle;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Represents the UI-implementation of the timeline.
 */
public class TimeLineUI extends ComponentUI
{
  // INNER TYPES

  /**
   * Helper class for the placement of cursor labels.
   */
  private static class CursorLabel implements Comparable<CursorLabel>
  {
    // CONSTANTS

    private static final LabelPlacement[] PLACEMENTS = { LabelPlacement.LABEL_TIME_RIGHT,
        LabelPlacement.LABEL_TIME_LEFT, LabelPlacement.LABEL_ONLY_LEFT, LabelPlacement.TIME_ONLY_LEFT,
        LabelPlacement.INDEX_ONLY_LEFT, LabelPlacement.INDEX_ONLY_RIGHT, LabelPlacement.TIME_ONLY_RIGHT,
        LabelPlacement.LABEL_ONLY_RIGHT };

    // VARIABLES

    final int index;
    final Rectangle boundaries;
    String text;
    boolean mirrored;

    private final List<LabelPlacement> placements = new ArrayList<LabelPlacement>();
    private volatile ListIterator<LabelPlacement> placementIter;

    // CONSTRUCTORS

    /**
     * Creates a new TimeLineUI.CursorLabel instance.
     */
    public CursorLabel( final int aIndex, final String aText, final Rectangle aBoundaries )
    {
      this.index = aIndex;
      this.text = aText;
      this.boundaries = aBoundaries;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final CursorLabel aLabel )
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
    public boolean intersects( final CursorLabel aLabel )
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
    public boolean isCompletelyVisible( final Rectangle aClipBounds )
    {
      return aClipBounds.contains( this.boundaries );
    }

    /**
     * Returns to the default label placement style.
     */
    public void resetStyle( final Rectangle aClipBounds, final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      // Determine what possible placements we've got for this label...
      this.placements.clear();

      for ( LabelPlacement element : PLACEMENTS )
      {
        if ( isValidPlacement( element, aClipBounds, aModel, aFM ) )
        {
          this.placements.add( element );
        }
      }

      this.placementIter = this.placements.listIterator();
      recalculateBoundaries( aModel, aFM );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return "CursorLabel [index=" + this.index + ", boundaries=" + this.boundaries + ", text=" + this.text + "]";
    }

    /**
     * Recalculates the boundaries for this label.
     */
    public void useNextStyle( final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      if ( this.placementIter.hasNext() )
      {
        recalculateBoundaries( aModel, aFM );
      }
    }

    /**
     * @param labelPlacement
     * @param aClipBounds
     * @param aModel
     * @param aFM
     * @return
     */
    private boolean isValidPlacement( final LabelPlacement labelPlacement, final Rectangle aClipBounds,
        final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      // Modify the label style of the previous label...
      String flagText = aModel.getCursorFlagText( this.index, labelPlacement.style );

      Rectangle rect = new Rectangle( this.boundaries );
      rect.width = aFM.stringWidth( flagText ) + PADDING_WIDTH;
      rect.x = aModel.getCursorScreenCoordinate( this.index );
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
    private void recalculateBoundaries( final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      if ( ( this.placementIter == null ) || !this.placementIter.hasNext() )
      {
        return;
      }
      final LabelPlacement labelPlacement = this.placementIter.next();

      // Modify the label style of the previous label...
      String flagText = aModel.getCursorFlagText( this.index, labelPlacement.style );

      this.text = flagText;
      this.boundaries.width = aFM.stringWidth( flagText ) + PADDING_WIDTH;
      this.boundaries.x = aModel.getCursorScreenCoordinate( this.index );
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

  private static final int PADDING_TOP = 2;
  private static final int PADDING_LEFT = 2;
  private static final int PADDING_RIGHT = 1;

  private static final int PADDING_WIDTH = PADDING_LEFT + PADDING_RIGHT;
  private static final int PADDING_HEIGHT = 2 * PADDING_TOP;

  /** The horizontal padding for all texts. */
  private static final int TEXT_PADDING_X = 2;
  /** The vertical padding (in px) of the timeline view. */
  private static final int VERTICAL_PADDING = 1;

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

  @Override
  public Dimension getPreferredSize( JComponent aComponent )
  {
    TimeLineView view = ( TimeLineView )aComponent;
    TimeLineViewModel model = view.getModel();

    int height = 0;
    int width = 0;
    if ( model.hasData() )
    {
      height = model.getPreferredHeight();
      width = model.getPreferredWidth();
    }

    return new Dimension( width, height );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void paint( final Graphics aGraphics, final JComponent aComponent )
  {
    final TimeLineView view = ( TimeLineView )aComponent;
    final TimeLineViewModel model = view.getModel();
    if ( !model.hasData() )
    {
      // Nothing to do!
      return;
    }

    final int baseTickYpos = model.getPreferredHeight() - VERTICAL_PADDING;
    final int tickYpos = baseTickYpos - model.getTickHeight();
    final int majorTickYpos = baseTickYpos - model.getMajorTickHeight();
    final int minorTickYpos = baseTickYpos - model.getMinorTickHeight();

    Graphics2D canvas = ( Graphics2D )aGraphics.create();

    try
    {
      final Rectangle clip = canvas.getClipBounds();
      // Tell Swing how we would like to render ourselves...
      canvas.setRenderingHints( createRenderingHints() );

      canvas.setBackground( model.getBackgroundColor() );
      canvas.clearRect( clip.x, clip.y, clip.width, clip.height );

      final Rectangle visibleRect = view.getVisibleRect();

      final double zoomFactor = model.getZoomFactor();
      final double triggerOffset = ( model.getTriggerOffset() * zoomFactor );

      // s denotes the amount of pixels per second...
      final double s = model.getPixelsPerSecond();
      // p denotes the amount of time per pixel...
      final double p = model.getSecondsPerPixel();
      // UoT represents a logical unit-of-time...
      final double uot = model.getUnitOfTime();
      // ts denotes the number of pixels per unit-of-time...
      final double ts = uot * s;

      // determine the tick increment based on the current width of the visible
      // timeline; this will make it a factor of 1, 10, 100, 1000, ...
      final int majorTickInc = ( int )max( 1.0, pow( 10, floor( log10( visibleRect.width / 2 ) ) ) );
      final int minorTickInc = ( majorTickInc / 2 );
      final double minorTickTime = ( minorTickInc * uot );
      final int tickInc = max( 1, majorTickInc / 10 );

      // tts denotes rounding factor to use ...
      final double tts = majorTickInc * ts;
      // to denotes the trigger offset...
      final double to = triggerOffset % tts;

      final int startX = clip.x;
      final int endX = ( clip.x + clip.width );

      // Start *before* the clip region starts; but at a multiple of our
      // rounding factor...
      double x = ( ( floor( startX / tts ) - 1 ) * tts ) + to;

      int tick = 0;
      for ( ; x <= endX; tick++, x += ts )
      {
        int xPos = ( int )round( x ) - 1;
        if ( ( xPos < startX ) || ( xPos > endX ) )
        {
          // Trivial reject: only paint visible items...
          continue;
        }

        final double time = ( x - triggerOffset );

        if ( abs( time ) < 1.0e-5 )
        {
          canvas.setColor( model.getTriggerColor() );
        }
        else
        {
          canvas.setColor( model.getTickColor() );
        }

        String text = null;
        int textYpos = -1;

        if ( ( tick % majorTickInc ) == 0 )
        {
          textYpos = majorTickYpos;

          canvas.drawLine( xPos, majorTickYpos, xPos, baseTickYpos );

          canvas.setFont( model.getMajorTickLabelFont() );

          double t = round( ( time * p ) / uot ) * uot;

          text = getMajorTimestamp( model, t, uot );
        }
        else if ( ( tick % minorTickInc ) == 0 )
        {
          textYpos = minorTickYpos;

          canvas.drawLine( xPos, minorTickYpos, xPos, baseTickYpos );

          canvas.setFont( model.getMinorTickLabelFont() );

          if ( model.isMinorTimestampVisible() )
          {
            text = getMinorTimestamp( model, time < 0 ? -minorTickTime : minorTickTime, uot );
          }
        }
        else if ( ( tick % tickInc ) == 0 )
        {
          canvas.drawLine( xPos, tickYpos, xPos, baseTickYpos );
        }

        if ( text != null )
        {
          FontMetrics fm = canvas.getFontMetrics();

          canvas.setColor( model.getTextColor() );

          int textWidth = ( fm.stringWidth( text ) + TEXT_PADDING_X );

          int textXpos = Math.max( visibleRect.x, ( int )( xPos - ( textWidth / 2.0 ) ) ) + 1;
          textYpos = Math.max( visibleRect.y, ( textYpos - fm.getDescent() ) );

          // canvas.drawString( text, textXpos, textYpos );
          drawLabel( canvas, model, text, textXpos, textYpos );
        }
      }

      // Draw the cursor "flags"...
      if ( model.isCursorMode() )
      {
        paintCursorFlags( model, canvas, aComponent );
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
  private String displayTime( final double aTime, final double aTimeScale, final int aPrecision )
  {
    final Unit.Time timeScale = Unit.Time.toUnit( aTimeScale );
    final Unit.Time time = Unit.Time.toUnit( aTime );

    // determine the magnitude of difference...
    int mag = 0;
    Unit.Time ptr = timeScale;
    while ( ( ptr != null ) && ( ptr != time ) )
    {
      ptr = ptr.predecessor();
      mag++;
    }

    if ( mag > 1 )
    {
      ptr = time.successor();
      if ( ptr == null )
      {
        ptr = timeScale.predecessor();
      }
      // More than a factor 1000 difference...
      return ptr.formatHumanReadable( aTime );
    }

    return time.formatHumanReadable( aTime );
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
  private void drawLabel( final Graphics2D aCanvas, final TimeLineViewModel aModel, final String aText,
      final int aXpos, final int aYpos )
  {
    if ( aModel.isDrawTextShadow() )
    {
      aCanvas.setColor( aModel.getTextShadowColor() );
      aCanvas.drawString( aText, aXpos + 2, aYpos + 2 );
    }

    aCanvas.setColor( aModel.getTextColor() );
    aCanvas.drawString( aText, aXpos, aYpos );
  }

  /**
   * @param sampleRate
   * @param majorTimestamp
   * @return
   */
  private String getMajorTimestamp( final TimeLineViewModel aModel, final double aTime, final double aTimeScale )
  {
    if ( aModel.hasTimingData() )
    {
      return displayTime( aTime, aTimeScale, 2 );
    }

    return Integer.toString( ( int )aTime );
  }

  /**
   * @param aTime
   * @param sampleRate
   * @return
   */
  private String getMinorTimestamp( final TimeLineViewModel aModel, final double aTime, final double aTimeScale )
  {
    if ( aModel.hasTimingData() )
    {
      final Unit.Time time = Unit.Time.toUnit( aTime );
      return time.formatHumanReadable( aTime );
    }

    return Integer.toString( ( int )aTime );
  }

  /**
   * Paints the cursors on this timeline.
   * 
   * @param aCanvas
   *          the canvas to paint the cursor (flags) on;
   */
  private void paintCursorFlags( final TimeLineViewModel aModel, final Graphics2D aCanvas, final JComponent aComponent )
  {
    final LinkedList<CursorLabel> labels = new LinkedList<CursorLabel>();

    final Rectangle clip = aCanvas.getClipBounds();

    // Phase 0: preparation...
    aCanvas.setFont( aModel.getCursorFlagFont() );

    final FontMetrics fm = aCanvas.getFontMetrics();
    final int yOffset = fm.getLeading() + fm.getAscent() + PADDING_TOP;

    // Phase 1: determine the boundaries of each defined cursor that should be
    // shown in the current clip boundaries...
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      // TODO persist the cursor labels between paints so we can perform smart
      // redraws...
      final Rectangle boundaries = new Rectangle();

      final String flagText = aModel.getCursorFlagText( i, LabelStyle.LABEL_TIME );

      boundaries.height = fm.getHeight() + PADDING_HEIGHT;
      boundaries.width = fm.stringWidth( flagText ) + PADDING_WIDTH;
      boundaries.x = aModel.getCursorScreenCoordinate( i );
      boundaries.y = aComponent.getHeight() - boundaries.height - 1;

      if ( ( boundaries.x < 0 ) || !clip.intersects( boundaries ) )
      {
        // Trivial reject: don't paint undefined cursors...
        continue;
      }

      labels.add( new CursorLabel( i, flagText, boundaries ) );
    }

    // Phase 2: sort the labels so they are ordered from left to right on
    // screen...
    Collections.sort( labels );

    // Phase 3: try to optimize the overlapping labels...
    placeLabels( clip, aModel, labels, fm );

    final AlphaComposite alphaComposite = AlphaComposite.SrcOver.derive( 0.7f );

    // Phase 4: draw the labels...
    for ( CursorLabel label : labels )
    {
      final Rectangle boundaries = label.boundaries;
      final Color cursorColor = aModel.getCursorColor( label.index );
      final Color cursorTextColor = aModel.getCursorTextColor( label.index );

      final Composite oldComposite = aCanvas.getComposite();
      final Stroke oldStroke = aCanvas.getStroke();

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
   *          the labels to optimize;
   * @param aFM
   *          the font metrics to use.
   */
  private void placeLabels( final Rectangle aClipBounds, final TimeLineViewModel aModel,
      final List<CursorLabel> aLabels, final FontMetrics aFM )
  {
    for ( int i = 0; i < aLabels.size(); i++ )
    {
      aLabels.get( i ).resetStyle( aClipBounds, aModel, aFM );
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
          previousLabel.useNextStyle( aModel, aFM );
        }
        while ( previousLabel.hasMoreStyles()
            && ( previousLabel.intersects( currentLabel ) || !previousLabel.isCompletelyVisible( aClipBounds ) ) );

        if ( previousLabel.intersects( currentLabel ) || !currentLabel.isCompletelyVisible( aClipBounds ) )
        {
          // Ok; still overlapping labels, use an alternative style for the
          // current label and try again...
          previousLabel.resetStyle( aClipBounds, aModel, aFM );

          // Try next style in the next loop...
          currentLabel.useNextStyle( aModel, aFM );
        }
      }
    }
  }
}
