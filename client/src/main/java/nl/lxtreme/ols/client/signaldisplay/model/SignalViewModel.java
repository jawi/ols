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
 * Provides a custom model specific for the {@link SignalView} component.
 */
public class SignalViewModel extends AbstractViewModel
{
  // CONSTANTS

  public static final String COMPONENT_BACKGROUND_COLOR = "signal.color.background";
  public static final String MEASUREMENT_ARROW_COLOR = "signal.color.arrow";
  public static final String CURSOR_FLAG_FONT = TimeLineViewModel.CURSOR_FLAG_FONT;

  // CONSTRUCTORS

  /**
   * Creates a new SignalViewModel instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public SignalViewModel( final SignalDiagramController aController )
  {
    super( aController );
  }

  // METHODS

  /**
   * Returns the background color for the signal view.
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
   * @return
   */
  public int[] getDataValues()
  {
    return this.controller.getSignalDiagramModel().getValues();
  }

  /**
   * @param aClip
   * @return
   */
  public int getEndIndex( final Rectangle aClip, final int aLength )
  {
    final Point location = new Point( aClip.x + aClip.width, 0 );
    int index = locationToSampleIndex( location );
    return Math.min( index + 1, aLength - 1 );
  }

  /**
   * Returns the color for the arrows shown in the measurement view.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getMeasurementArrowColor()
  {
    Color color = UIManager.getColor( MEASUREMENT_ARROW_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_ARROW_COLOR;
    }
    return color;
  }

  /**
   * @param aClip
   * @return
   */
  public int getStartIndex( final Rectangle aClip )
  {
    final Point location = aClip.getLocation();
    int index = locationToSampleIndex( location );
    return Math.max( index - 1, 0 );
  }

  /**
   * @return
   */
  public long[] getTimestamps()
  {
    return this.controller.getSignalDiagramModel().getTimestamps();
  }

  /**
   * Returns whether or not the analog scope signal itself is rendered in
   * anti-aliased mode.
   * 
   * @return <code>true</code> if anti-aliasing should be applied to the scope
   *         rendering, <code>false</code> otherwise.
   */
  public boolean isRenderGroupSummaryAntiAliased()
  {
    return false; // XXX
  }

  /**
   * Returns whether or not the analog scope signal itself is rendered in
   * anti-aliased mode.
   * 
   * @return <code>true</code> if anti-aliasing should be applied to the scope
   *         rendering, <code>false</code> otherwise.
   */
  public boolean isRenderScopeSignalAntiAliased()
  {
    return false; // XXX
  }
}
