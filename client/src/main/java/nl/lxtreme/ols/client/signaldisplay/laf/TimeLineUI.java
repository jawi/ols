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

import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.model.AbstractViewModel.LabelStyle;
import nl.lxtreme.ols.client.signaldisplay.model.*;
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

    private int altStyleIdx;

    // CONSTRUCTORS

    /**
     * Creates a new TimeLineUI.CursorLabel instance.
     */
    public CursorLabel( final int aIndex, final String aText, final Rectangle aBoundaries )
    {
      this.index = aIndex;
      this.text = aText;
      this.boundaries = aBoundaries;
      this.altStyleIdx = 0;
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

    /**
     * @return
     */
    public boolean hasMoreStyles()
    {
      return ( this.altStyleIdx < PLACEMENTS.length );
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
     * Returns to the default label placement style.
     */
    public void resetStyle( final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      this.altStyleIdx = 0;
      recalculateBoundaries( aModel, aFM );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return "CursorLabel [index=" + this.index + ", boundaries=" + this.boundaries + ", text=" + this.text
          + ", altStyleIdx=" + this.altStyleIdx + "]";
    }

    /**
     * Recalculates the boundaries for this label.
     */
    public void useNextStyle( final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      if ( this.altStyleIdx++ < ( PLACEMENTS.length - 1 ) )
      {
        recalculateBoundaries( aModel, aFM );
      }
    }

    /**
     * @param aModel
     * @param aFM
     */
    private void recalculateBoundaries( final TimeLineViewModel aModel, final FontMetrics aFM )
    {
      final LabelPlacement labelPlacement = PLACEMENTS[this.altStyleIdx];

      // Modify the label style of the previous label...
      String flagText = aModel.getCursorFlagText( this.index, labelPlacement.style );

      this.text = flagText;
      this.boundaries.width = aFM.stringWidth( flagText ) + PADDING_WIDTH;
      this.boundaries.x = aModel.getCursorScreenCoordinate( this.index );
      if ( labelPlacement.mirrored )
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
  private static final int PADDING_LEFT = 3;

  private static final int PADDING_WIDTH = 2 * PADDING_LEFT;
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

    final int baseTickYpos = model.getTimeLineHeight() - VERTICAL_PADDING;
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

      final long startX = clip.x;
      final long endX = ( clip.x + clip.width );

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

      final double correctedVisibleTime = Math.pow( 10, Math.ceil( Math.log10( clip.width * p ) ) );
      final int majorTickInc = ( int )( correctedVisibleTime / uot );
      final int minorTickInc = ( majorTickInc / 2 );
      final int tickInc = Math.max( 1, majorTickInc / 10 );

      // tts denotes the tick increment...
      final double tts = majorTickInc * ts;
      // to denotes the trigger offset...
      final double to = triggerOffset % ts;

      double x = ( Math.floor( startX / ts ) * ts ) + to;
      double majorTime = x - triggerOffset;

      System.out.println( "X = [" + startX + ".." + endX + "]; " + x + " (triggerOffset = " + triggerOffset
          + ", tts = " + tts + ", to = " + to + ")" );
      System.out.println( "majorTime = " + majorTime + "; time = " + ( x - triggerOffset ) + "; visibleTime = "
          + UnitOfTime.format( correctedVisibleTime ) + "; ratio = " + majorTickInc );

      int tick = 0;
      for ( ; x <= endX; tick++, x += ts )
      {
        int xPos = ( int )Math.round( x ) - 1; // XXX why the minus one???
        if ( ( xPos < startX ) || ( xPos > endX ) )
        {
          // Trivial reject: only paint visible items...
          continue;
        }

        double time = x - triggerOffset;

        canvas.setColor( model.getTickColor() );

        String text = null;

        if ( ( tick % majorTickInc ) == 0 )
        {
          canvas.drawLine( xPos, majorTickYpos, xPos, tickYpos );

          canvas.setFont( model.getMajorTickLabelFont() );

          text = getMajorTimestamp( model, time * p, uot );
          majorTime = time;
        }
        else if ( ( tick % minorTickInc ) == 0 )
        {
          canvas.drawLine( xPos, minorTickYpos, xPos, tickYpos );

          canvas.setFont( model.getMinorTickLabelFont() );

          text = UnitOfTime.format( ( time - majorTime ) * p );
        }
        else if ( ( tick % tickInc ) == 0 )
        {
          canvas.drawLine( xPos, baseTickYpos, xPos, tickYpos );
        }

        if ( text != null )
        {
          FontMetrics fm = canvas.getFontMetrics();

          canvas.setColor( model.getTextColor() );

          int textWidth = ( fm.stringWidth( text ) + TEXT_PADDING_X );
          int textHeight = fm.getHeight();

          int textXpos = Math.max( 0, ( int )( xPos - ( textWidth / 2.0 ) ) ) + 1;
          int textYpos = Math.max( 1, ( baseTickYpos - textHeight ) );

          canvas.drawString( text, textXpos, textYpos );
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
    final UnitOfTime timeScale = UnitOfTime.toUnit( aTimeScale );
    final UnitOfTime time = UnitOfTime.toUnit( aTime );

    if ( ( timeScale != time ) && ( timeScale != time.successor() ) )
    {
      // More than a factor 1000 difference...
      return timeScale.predecessor().format( aTime, aPrecision );
    }

    return time.format( aTime, aPrecision );
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
      return UnitOfTime.format( aTime );
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
      boundaries.y = aComponent.getHeight() - boundaries.height - PADDING_TOP;

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
    placeLabels( aModel, labels, fm );

    // Phase 4: draw the labels...
    for ( CursorLabel label : labels )
    {
      final Rectangle boundaries = label.boundaries;
      final Color cursorColor = aModel.getCursorColor( label.index );

      // aCanvas.setColor( cursorColor );
      // aCanvas.fillRect( boundaries.x, boundaries.y, boundaries.width,
      // boundaries.height );

      aCanvas.setColor( cursorColor );
      aCanvas.drawRect( boundaries.x, boundaries.y, boundaries.width, boundaries.height );

      final int textXpos = boundaries.x + PADDING_LEFT;
      final int textYpos = boundaries.y + yOffset;

      aCanvas.setColor( aModel.getCursorTextColor( label.index ) );
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
   * @param aModel
   *          the model to use;
   * @param aLabels
   *          the labels to optimize;
   * @param aFM
   *          the font metrics to use.
   */
  private void placeLabels( final TimeLineViewModel aModel, final List<CursorLabel> aLabels, final FontMetrics aFM )
  {
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
        while ( previousLabel.intersects( currentLabel ) && previousLabel.hasMoreStyles() );

        if ( previousLabel.intersects( currentLabel ) )
        {
          // Ok; still overlapping labels, use an alternative style for the
          // current label and try again...
          previousLabel.resetStyle( aModel, aFM );

          // Try next style in the next loop...
          currentLabel.useNextStyle( aModel, aFM );
        }
      }
    }
  }
}
