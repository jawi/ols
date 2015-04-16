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
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides a custom model specific for the {@link SignalView} component.
 */
public class SignalViewModel extends AbstractViewModel
{
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
   * @return the alignment for the annotations, never <code>null</code>.
   */
  public SignalAlignment getAnnotationAlignment()
  {
    String alignment = UIManager.getString( SIGNALVIEW_ANNOTATION_ALIGNMENT );
    if ( alignment == null )
    {
      alignment = "CENTER";
    }
    return SignalAlignment.valueOf( alignment );
  }

  /**
   * @return a translucency alpha value, &gt;= 0.0 && &lt;= 100.0f
   */
  public float getAnnotationAlpha()
  {
    int value = UIManager.getInt( SIGNALVIEW_ANNOTATION_ALPHA );
    return value / 100.0f;
  }

  /**
   * Returns the color for the annotations.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getAnnotationColor()
  {
    Color color = UIManager.getColor( SIGNALVIEW_ANNOTATION_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * Returns the font for the annotations.
   * 
   * @return a font, never <code>null</code>.
   */
  public Font getAnnotationFont()
  {
    Font font = UIManager.getFont( SIGNALVIEW_ANNOTATION_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * Returns the background color for the signal view.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getBackgroundColor()
  {
    Color color = UIManager.getColor( SIGNALVIEW_BACKGROUND_COLOR );
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
    Font font = UIManager.getFont( SIGNALVIEW_CURSOR_FLAG_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * @return
   */
  public int[] getDataValues()
  {
    return this.controller.getViewModel().getValues();
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
   * Returns the color for the group summary bars.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getGroupSummaryBarColor()
  {
    Color font = UIManager.getColor( SIGNALVIEW_GROUP_SUMMARY_BAR_COLOR );
    if ( font == null )
    {
      font = Color.WHITE;
    }
    return font;
  }

  /**
   * Returns the padding used for the group summary.
   * 
   * @return a (horizontal + vertical) padding, in pixels.
   */
  public int getGroupSummaryPadding()
  {
    return UIManager.getInt( SIGNALVIEW_GROUP_SUMMARY_PADDING );
  }

  /**
   * Returns the font for the group summary text.
   * 
   * @return a font, never <code>null</code>.
   */
  public Font getGroupSummaryTextFont()
  {
    Font font = UIManager.getFont( SIGNALVIEW_GROUP_SUMMARY_TEXT_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * Returns the color for the arrows shown in the measurement view.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getMeasurementArrowColor()
  {
    Color color = UIManager.getColor( SIGNALVIEW_MEASUREMENT_ARROW_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
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
    return this.controller.getViewModel().getTimestamps();
  }

  /**
   * Returns whether or not the alternative rendering style for annotations
   * should be used.
   * 
   * @return <code>true</code> if an alternative rendering style for annotations
   *         should be used, <code>false</code> otherwise.
   */
  public boolean isRenderAnnotationAlternatively()
  {
    return UIManager.getBoolean( SIGNALVIEW_ANNOTATION_USE_ALTSTYLE );
  }

  /**
   * Returns whether or not the annotations itself is rendered in anti-aliased
   * mode.
   * 
   * @return <code>true</code> if anti-aliasing should be applied to the
   *         annotation rendering, <code>false</code> otherwise.
   */
  public boolean isRenderAnnotationAntiAliased()
  {
    return UIManager.getBoolean( SIGNALVIEW_ANNOTATION_RENDER_AA );
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
    return UIManager.getBoolean( SIGNALVIEW_GROUP_SUMMARY_RENDER_AA );
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
    return UIManager.getBoolean( SIGNALVIEW_ANALOG_SCOPE_RENDER_AA );
  }

  /**
   * Returns whether or not the "sloppy" rendering for scopes and signal groups
   * should be used.
   * 
   * @return <code>true</code> if sloppy rendering is allowed,
   *         <code>false</code> otherwise.
   */
  public boolean isSloppyScopeRenderingAllowed()
  {
    return !UIManager.getBoolean( DISABLE_SLOPPY_SCOPE_PAINTING );
  }
}
