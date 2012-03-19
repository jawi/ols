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


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides a custom model specific for the {@link TimeLineView} component.
 */
public class TimeLineViewModel extends AbstractViewModel
{
  // CONSTANTS

  public static final String COMPONENT_BACKGROUND_COLOR = "timeline.color.background";
  public static final String COMPONENT_HEIGHT = "timeline.height";
  public static final String COMPONENT_VERTICAL_PADDING = "timeline.vertical.padding";

  public static final String CURSOR_FLAG_FONT = "timeline.cursor.font";
  public static final String TEXT_COLOR = "timeline.text.color";
  public static final String TICK_HEIGHT = "timeline.tick.height";
  public static final String TICK_COLOR = "timeline.tick.color";

  public static final String MAJOR_TICK_HEIGHT = "timeline.majortick.height";
  public static final String MAJOR_TICK_LABEL_FONT = "timeline.majortick.label.font";
  public static final String MAJOR_TICK_COLOR = "timeline.majortick.color";

  public static final String MINOR_TICK_HEIGHT = "timeline.minortick.height";
  public static final String MINOR_TICK_LABEL_FONT = "timeline.minortick.label.font";
  public static final String MINOR_TICK_COLOR = "timeline.minortick.color";

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
    Color color = UIManager.getColor( COMPONENT_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_BACKGROUND_COLOR;
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
    Font font = UIManager.getFont( CURSOR_FLAG_FONT );
    if ( font == null )
    {
      font = LafDefaults.DEFAULT_CURSOR_FLAG_FONT;
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
    Color color = UIManager.getColor( MAJOR_TICK_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_MAJOR_TICK_COLOR;
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
    int value = UIManager.getInt( MAJOR_TICK_HEIGHT );
    if ( value <= 0 )
    {
      return LafDefaults.DEFAULT_MAJOR_TICK_HEIGHT;
    }
    return value;
  }

  /**
   * Returns the font for the major tick labels.
   * 
   * @return a font, never <code>null</code>.
   */
  public Font getMajorTickLabelFont()
  {
    Font font = UIManager.getFont( MAJOR_TICK_LABEL_FONT );
    if ( font == null )
    {
      font = LafDefaults.DEFAULT_MAJOR_TICK_FONT;
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
    Color color = UIManager.getColor( MINOR_TICK_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_MINOR_TICK_COLOR;
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
    int value = UIManager.getInt( MINOR_TICK_HEIGHT );
    if ( value <= 0 )
    {
      return LafDefaults.DEFAULT_MINOR_TICK_HEIGHT;
    }
    return value;
  }

  /**
   * Returns the font for the minor tick labels.
   * 
   * @return a font, never <code>null</code>.
   */
  public Font getMinorTickLabelFont()
  {
    Font font = UIManager.getFont( MINOR_TICK_LABEL_FONT );
    if ( font == null )
    {
      font = LafDefaults.DEFAULT_MINOR_TICK_FONT;
    }
    return font;
  }

  /**
   * @see SignalDiagramModel#getTimelinePixelsPerSecond()
   */
  public double getPixelsPerSecond()
  {
    return getSignalDiagramModel().getTimelinePixelsPerSecond().doubleValue();
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
    return getSignalDiagramModel().getTimelineSecondsPerPixel().doubleValue();
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
    Color color = UIManager.getColor( TEXT_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_TEXT_COLOR;
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
    Color value = UIManager.getColor( TICK_COLOR );
    if ( value == null )
    {
      value = LafDefaults.DEFAULT_TICK_COLOR;
    }
    return value;
  }

  /**
   * Returns the height of the individual ticks.
   * 
   * @return a height, in pixels.
   */
  public int getTickHeight()
  {
    int value = UIManager.getInt( TICK_HEIGHT );
    if ( value <= 0 )
    {
      return LafDefaults.DEFAULT_TICK_HEIGHT;
    }
    return value;
  }

  /**
   * Returns the height of the time line component.
   * 
   * @return a height, in pixels, > 0.
   */
  public int getTimeLineHeight()
  {
    int value = UIManager.getInt( COMPONENT_HEIGHT );
    if ( value <= 0 )
    {
      return LafDefaults.DEFAULT_TIMELINE_HEIGHT;
    }
    return value;
  }

  /**
   * @see SignalDiagramModel#getTimelineUnitOfTime()
   */
  public double getUnitOfTime()
  {
    return getSignalDiagramModel().getTimelineUnitOfTime().doubleValue();
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
   * Returns whether or not the minor timestamps on the timeline are to be
   * painted.
   * 
   * @return <code>true</code> if the minor timestamps are to be painted,
   *         <code>false</code> if they should be hidden.
   */
  public boolean isMinorTimestampVisible()
  {
    // TODO Auto-generated method stub
    return false;
  }
}
