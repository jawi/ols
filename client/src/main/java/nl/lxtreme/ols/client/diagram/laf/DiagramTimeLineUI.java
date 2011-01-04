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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.diagram.laf;


import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.settings.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides the Look&Feel for the diagram timeline component.
 */
public class DiagramTimeLineUI extends ComponentUI
{
  // INNER TYPES

  /** The tick increment (in pixels). */
  public static final int TIMELINE_INCREMENT = 20;
  /** The height of this component. */
  public static final int TIMELINE_HEIGHT = 30;

  private static final int LONG_TICK_INTERVAL = 10;
  private static final int TIME_INTERVAL = 20;

  private static final int SHORT_TICK_HEIGHT = 4;
  private static final int PADDING_Y = 1;

  // VARIABLES

  private Font labelFont;
  private Font cursorFont;

  // METHODS

  /**
   * @see javax.swing.plaf.ComponentUI#getMaximumSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getMaximumSize( final JComponent aComponent )
  {
    return new Dimension( Short.MAX_VALUE, Short.MAX_VALUE );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#getMinimumSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getMinimumSize( final JComponent aComponent )
  {
    return new Dimension( 0, TIMELINE_HEIGHT );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#getPreferredSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getPreferredSize( final JComponent aComponent )
  {
    final DiagramTimeLine timeline = ( DiagramTimeLine )aComponent;
    final Diagram diagram = timeline.getDiagram();
    return new Dimension( diagram.getPreferredSize().width, TIMELINE_HEIGHT );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#installUI(javax.swing.JComponent)
   */
  @Override
  public void installUI( final JComponent aComponent )
  {
    final Font defaultFont = LafHelper.getDefaultFont();
    this.labelFont = defaultFont.deriveFont( 0.9f * defaultFont.getSize2D() );
    this.cursorFont = defaultFont.deriveFont( 0.9f * defaultFont.getSize2D() );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#paint(java.awt.Graphics,
   *      javax.swing.JComponent)
   */
  @Override
  public void paint( final Graphics aCanvas, final JComponent aComponent )
  {
    final Graphics2D canvas = ( Graphics2D )aCanvas;

    final DiagramTimeLine timeLine = ( DiagramTimeLine )aComponent;

    final DataContainer dataContainer = timeLine.getDataContainer();
    if ( !dataContainer.hasCapturedData() )
    {
      return;
    }

    final Diagram diagram = timeLine.getDiagram();
    final DiagramSettings settings = diagram.getDiagramSettings();

    final double scale = timeLine.getScale();

    final int tickInc = ( int )Math.max( 1.0, Diagram.MAX_SCALE / scale );
    final long triggerPosition = dataContainer.hasTriggerData() ? dataContainer.getTriggerPosition() : 0L;
    final long timeLineShift = triggerPosition % tickInc;

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = canvas.getClipBounds();
    // for some reason, this component gets scrolled vertically although it has
    // no reasons to do so. Resetting the Y-position & height of the clip-area
    // seems to solve this problem...
    clipArea.y = 0;
    clipArea.height = TIMELINE_HEIGHT;

    // find index of first sample we should draw...
    final long startSample = diagram.convertPointToSampleIndex( new Point( clipArea.x, 0 ) );
    // find index of last sample we should draw...
    final long endSample = diagram.convertPointToSampleIndex( new Point( clipArea.x + clipArea.width, 0 ) ) + 1;

    canvas.setFont( this.labelFont );
    final FontMetrics fm = canvas.getFontMetrics();

    canvas.setColor( settings.getBackgroundColor() );
    canvas.fillRect( clipArea.x, clipArea.y, clipArea.width, clipArea.height );

    canvas.setColor( settings.getTimeColor() );

    final int baselineYpos = clipArea.y + TIMELINE_HEIGHT - PADDING_Y;
    final int longTickYpos = ( int )( baselineYpos - ( 3.5 * SHORT_TICK_HEIGHT ) );
    final int shortTickYpos = baselineYpos - SHORT_TICK_HEIGHT;

    for ( long time = ( startSample / tickInc ) * tickInc + timeLineShift; time < endSample; time += tickInc )
    {
      final int xPos = Math.max( 0, ( int )( scale * time ) );

      final long absoluteTime = time - triggerPosition;
      final long scaledTime = absoluteTime / tickInc;

      if ( scaledTime % LONG_TICK_INTERVAL == 0 )
      {
        final String timeValue = indexToTime( dataContainer, time );

        final int labelYpos = longTickYpos - 2 * PADDING_Y;
        final int labelXpos = Math.max( clipArea.x, xPos - ( fm.stringWidth( timeValue ) / 2 ) );

        canvas.drawLine( xPos, baselineYpos, xPos, longTickYpos );
        if ( scaledTime % TIME_INTERVAL == 0 )
        {
          canvas.drawString( timeValue, labelXpos, labelYpos );
        }
      }
      else
      {
        canvas.drawLine( xPos, baselineYpos, xPos, shortTickYpos );
      }
    }

    // If cursors are disabled entirely, we're done; otherwise we need to draw
    // them for the range of samples we're currently showing...
    if ( dataContainer.isCursorsEnabled() )
    {
      paintCursorFlags( dataContainer, canvas, timeLine, startSample, endSample );
    }
  }

  /**
   * Convert sample count to time string.
   * 
   * @param count
   *          sample count (or index)
   * @return string containing time information
   */
  private String indexToTime( final DataContainer aDataContainer, final long aTimeValue )
  {
    if ( !aDataContainer.hasTimingData() )
    {
      return String.format( "%d", aTimeValue );
    }

    long timeValue = aTimeValue;
    if ( aDataContainer.hasTriggerData() )
    {
      timeValue -= aDataContainer.getTriggerPosition();
    }

    return DisplayUtils.displayScaledTime( timeValue, aDataContainer.getSampleRate() );
  }

  /**
   * Paints the cursor flags, denoting which cursor line belongs to which
   * cursor.
   * 
   * @param aDataContainer
   *          the data container to take the cursor information from;
   * @param aCanvas
   *          the graphics canvas to draw on;
   * @param aTimeLine
   *          the time line component;
   * @param aStartSample
   *          the first sample index to draw;
   * @param aEndSample
   *          the last sample index to draw.
   */
  private void paintCursorFlags( final DataContainer aDataContainer, final Graphics2D aCanvas,
      final DiagramTimeLine aTimeLine, final long aStartSample, final long aEndSample )
  {
    final Diagram diagram = aTimeLine.getDiagram();
    final DiagramSettings settings = diagram.getDiagramSettings();

    final double scale = aTimeLine.getScale();

    aCanvas.setFont( this.cursorFont );
    final FontMetrics fm = aCanvas.getFontMetrics();

    final int textHeight = fm.getHeight();
    final int flagHeight = textHeight;

    for ( int i = 0, size = DataContainer.MAX_CURSORS; i < size; i++ )
    {
      final long cursorPosition = aDataContainer.getCursorPosition( i );
      if ( ( cursorPosition >= aStartSample ) && ( cursorPosition <= aEndSample ) )
      {
        final int cursorPos = ( int )( cursorPosition * scale );

        final double cursorTimeValue = aDataContainer.getCursorTimeValue( i );
        final String cursorTime = DisplayUtils.displayTime( cursorTimeValue );

        final Color cursorColor = settings.getCursorColor( i );
        final Color cursorTextColor = ColorUtils.getContrastColor( cursorColor );

        String text = String.format( "T%d", i + 1 );
        if ( settings.isShowCursorTiming() )
        {
          text = text.concat( ": " ).concat( cursorTime );
        }

        final int textWidth = fm.stringWidth( text );
        final int flagWidth = textWidth + 4;

        aCanvas.setColor( cursorColor );
        aCanvas.drawLine( cursorPos, TIMELINE_HEIGHT - flagHeight, cursorPos, TIMELINE_HEIGHT );
        aCanvas.fillRect( cursorPos, TIMELINE_HEIGHT - flagHeight - 1, flagWidth, flagHeight - 1 );

        aCanvas.setColor( cursorColor.darker() );
        aCanvas.drawRect( cursorPos, TIMELINE_HEIGHT - flagHeight - 1, flagWidth, flagHeight - 2 );

        aCanvas.setColor( cursorTextColor );
        aCanvas.drawString( text, cursorPos + 3, TIMELINE_HEIGHT - 5 );
      }
    }
  }
}
