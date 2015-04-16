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
package nl.lxtreme.ols.client.signaldisplay.model;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides a custom model specific for the {@link TimeLineView} component.
 */
public class TimeLineViewModel extends AbstractViewModel
{
  // CONSTRUCTORS

  /**
   * Creates a new TimeLineModel instance.
   *
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public TimeLineViewModel( final SignalDiagramController aController )
  {
    super( aController );
  }

  // METHODS

  /**
   * Returns the background color for the timeline.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getBackgroundColor()
  {
    Color color = UIManager.getColor( TIMELINE_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * Returns the font for the cursor flags.
   *
   * @return a font, never <code>null</code>.
   */
  public Font getCursorFlagFont()
  {
    Font font = UIManager.getFont( TIMELINE_CURSOR_FLAG_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * Determines the ending time stamp until which the time line should be drawn
   * given the clip boundaries of this component.
   *
   * @param aClip
   *          the clip boundaries of the timeline component, cannot be
   *          <code>null</code>.
   * @return the ending time stamp, as long value.
   */
  public double getEndTimestamp( final Rectangle aClip )
  {
    final double zf = getZoomFactor();
    return ( ( aClip.x + aClip.width ) / zf );
  }

  /**
   * Returns the color in which the major ticks should be painted.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getMajorTickColor()
  {
    Color color = UIManager.getColor( TIMELINE_MAJOR_TICK_COLOR );
    if ( color == null )
    {
      color = Color.LIGHT_GRAY;
    }
    return color;
  }

  /**
   * Returns the major tick height.
   *
   * @return a height, in pixels.
   */
  public int getMajorTickHeight()
  {
    return UIManager.getInt( TIMELINE_MAJOR_TICK_HEIGHT );
  }

  /**
   * Returns the font for the major tick labels.
   *
   * @return a font, never <code>null</code>.
   */
  public Font getMajorTickLabelFont()
  {
    Font font = UIManager.getFont( TIMELINE_MAJOR_TICK_LABEL_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * Returns the color in which the minor ticks should be painted.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getMinorTickColor()
  {
    Color color = UIManager.getColor( TIMELINE_MINOR_TICK_COLOR );
    if ( color == null )
    {
      color = Color.DARK_GRAY;
    }
    return color;
  }

  /**
   * Returns the minor tick height.
   *
   * @return a height, in pixels.
   */
  public int getMinorTickHeight()
  {
    return UIManager.getInt( TIMELINE_MINOR_TICK_HEIGHT );
  }

  /**
   * Returns the font for the minor tick labels.
   *
   * @return a font, never <code>null</code>.
   */
  public Font getMinorTickLabelFont()
  {
    Font font = UIManager.getFont( TIMELINE_MINOR_TICK_LABEL_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * @see SignalDiagramModel#getTimelinePixelsPerSecond()
   */
  public double getPixelsPerSecond()
  {
    final Double value = getSignalDiagramModel().getTimelinePixelsPerSecond();
    if ( value == null )
    {
      return 1;
    }
    return value.doubleValue();
  }

  /**
   * Returns the height of the time line component.
   *
   * @return a height, in pixels, > 0.
   */
  public int getPreferredHeight()
  {
    return UIManager.getInt( TIMELINE_HEIGHT );
  }

  /**
   * Returns the width of the time line component.
   *
   * @return the width of the timeline, in pixels, > 0.
   */
  public int getPreferredWidth()
  {
    // timeline width *always* follows the width of the main component...
    return this.controller.getViewComponent().getWidth();
  }

  /**
   * Returns the sample rate of the sampled data.
   *
   * @return a sample rate, in Hertz.
   */
  public int getSampleRate()
  {
    return getSignalDiagramModel().getSampleRate();
  }

  /**
   * @see SignalDiagramModel#getTimelineSecondsPerPixel()
   */
  public double getSecondsPerPixel()
  {
    final Double value = getSignalDiagramModel().getTimelineSecondsPerPixel();
    if ( value == null )
    {
      return 1;
    }
    return value.doubleValue();
  }

  /**
   * Determines the starting time stamp from which the time line should be drawn
   * given the clip boundaries of this component.
   *
   * @param aClip
   *          the clip boundaries of the timeline component, cannot be
   *          <code>null</code>.
   * @return the starting time stamp, as long value.
   */
  public double getStartTimestamp( final Rectangle aClip )
  {
    final double zf = getZoomFactor();
    return ( aClip.x / zf );
  }

  /**
   * Returns the color in which the texts should be painted.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getTextColor()
  {
    Color color = UIManager.getColor( TIMELINE_TEXT_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * @return the drop shadow color for texts, never <code>null</code>.
   */
  public Color getTextShadowColor()
  {
    Color color = UIManager.getColor( TIMELINE_TEXT_SHADOW_COLOR );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * Returns the color of the individual ticks.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getTickColor()
  {
    Color color = UIManager.getColor( TIMELINE_TICK_COLOR );
    if ( color == null )
    {
      color = Color.DARK_GRAY;
    }
    return color;
  }

  /**
   * Returns the height of the individual ticks.
   *
   * @return a height, in pixels.
   */
  public int getTickHeight()
  {
    return UIManager.getInt( TIMELINE_TICK_HEIGHT );
  }

  /**
   * @see SignalDiagramModel#getTimelineUnitOfTime()
   */
  public double getUnitOfTime()
  {
    final Double timelineUnitOfTime = getSignalDiagramModel().getTimelineUnitOfTime();
    if ( timelineUnitOfTime == null )
    {
      return 1;
    }
    return timelineUnitOfTime.doubleValue();
  }

  /**
   * Returns whether the data is a timed-capture or a state-capture.
   *
   * @return <code>true</code> if there is timing data available,
   *         <code>false</code> if not.
   */
  public boolean hasTimingData()
  {
    return getSignalDiagramModel().hasTimingData();
  }

  /**
   * @return <code>true</code> if a shadow should be drawn behind the text,
   *         <code>false</code> otherwise.
   */
  public boolean isDrawTextShadow()
  {
    return UIManager.getBoolean( TIMELINE_DRAW_TEXT_SHADOW );
  }

  /**
   * Returns whether or not the minor timestamps on the timeline are to be
   * painted.
   *
   * @return <code>true</code> if the minor timestamps are to be painted,
   *         <code>false</code> if they should be hidden.
   */
  public boolean isMinorTimestampVisible()
  {
    return UIManager.getBoolean( TIMELINE_MINOR_TICKS_VISIBLE );
  }
}
