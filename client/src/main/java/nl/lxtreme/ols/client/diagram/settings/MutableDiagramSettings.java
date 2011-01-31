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
package nl.lxtreme.ols.client.diagram.settings;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Provides mutable diagram settings.
 */
public class MutableDiagramSettings implements DiagramSettings, Configurable
{
  // VARIABLES

  private int channelHeight;
  private int scopeHeight;
  private int signalHeight;

  private boolean showCursorTiming;

  private EdgeSlope edgeSlope;

  private Color triggerColor;
  private Color gridColor;
  private Color backgroundColor;
  private Color groupBackgroundColor;
  private Color groupByteColor;
  private Color scopeColor;
  private Color signalColor;
  private Color textColor;
  private Color timeColor;
  private Color labelColor;

  private SignalAlignment signalAlignment;
  private ColorScheme colorScheme;

  private final Color[] cursorColors;
  private final Color[] channelColors;

  /**
   * Display settings for each group. Can be any combinations (OR-ed) of the
   * defined MODE_* values.
   */
  private final int[] groupSettings;

  // CONSTRUCTORS

  private ColorTarget colorTarget;

  /**
   * Creates a new MutableDiagramSettings instance.
   */
  public MutableDiagramSettings()
  {
    this.colorScheme = ColorScheme.DARK;

    this.showCursorTiming = true;

    this.cursorColors = new Color[CapturedData.MAX_CURSORS];
    this.channelColors = new Color[CapturedData.MAX_CHANNELS];

    this.colorTarget = ColorTarget.SIGNALS;

    setDefaultColorScheme();

    this.groupSettings = new int[4];
    for ( int i = 0; i < this.groupSettings.length; i++ )
    {
      this.groupSettings[i] = DISPLAY_CHANNELS;
    }

    setChannelHeight( 35 );
    setSignalHeight( 20 );
    setScopeHeight( 133 );
    setSignalAlignment( SignalAlignment.CENTER );
    setEdgeSlope( EdgeSlope.NON_PERPENDICULAR );
  }

  // METHODS

  /**
   * Creates a new MutableDiagramSettings instance.
   * 
   * @param aDiagramSettings
   *          the diagram settings to use as default, may be <code>null</code>
   *          in which case this constructor falls back to the default settings.
   */
  public MutableDiagramSettings( final DiagramSettings aDiagramSettings )
  {
    this();

    if ( aDiagramSettings != null )
    {
      setShowCursorTiming( aDiagramSettings.isShowCursorTiming() );

      setColorTarget( aDiagramSettings.getColorTarget() );
      setColorScheme( aDiagramSettings.getColorScheme() );

      setBackgroundColor( aDiagramSettings.getBackgroundColor() );
      setTriggerColor( aDiagramSettings.getTriggerColor() );
      setGridColor( aDiagramSettings.getGridColor() );
      setGroupBackgroundColor( aDiagramSettings.getGroupBackgroundColor() );
      setTextColor( aDiagramSettings.getTextColor() );
      setTimeColor( aDiagramSettings.getTimeColor() );
      setLabelColor( aDiagramSettings.getLabelColor() );
      setGroupByteColor( aDiagramSettings.getGroupByteColor() );
      setScopeColor( aDiagramSettings.getScopeColor() );

      setColorScheme( aDiagramSettings.getColorScheme() );

      for ( int i = 0; i < CapturedData.MAX_CURSORS; i++ )
      {
        setCursorColor( i, aDiagramSettings.getCursorColor( i ) );
      }

      for ( int i = 0; i < CapturedData.MAX_CHANNELS; i++ )
      {
        setChannelColor( i, aDiagramSettings.getChannelColor( i ) );
      }

      for ( int i = 0; i < 4; i++ )
      {
        setShowByte( i, aDiagramSettings.isShowByte( i ) );
        setShowChannels( i, aDiagramSettings.isShowChannels( i ) );
        setShowScope( i, aDiagramSettings.isShowScope( i ) );
      }

      setChannelHeight( aDiagramSettings.getChannelHeight() );
      setScopeHeight( aDiagramSettings.getScopeHeight() );
      setSignalHeight( aDiagramSettings.getSignalHeight() );
      setSignalAlignment( aDiagramSettings.getSignalAlignment() );
      setEdgeSlope( aDiagramSettings.getEdgeSlope() );
    }
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getBackgroundColor()
   */
  @Override
  public final Color getBackgroundColor()
  {
    return this.backgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getChannelColor(int)
   */
  @Override
  public final Color getChannelColor( final int aChannelIdx )
  {
    return this.channelColors[aChannelIdx];
  }

  /**
   * @return the channelColors
   */
  public final Color[] getChannelColors()
  {
    return this.channelColors;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getChannelHeight()
   */
  @Override
  public final int getChannelHeight()
  {
    return this.channelHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getColorScheme()
   */
  @Override
  public final ColorScheme getColorScheme()
  {
    return this.colorScheme;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getColorTarget()
   */
  @Override
  public final ColorTarget getColorTarget()
  {
    return this.colorTarget;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getCursorColor(int)
   */
  @Override
  public final Color getCursorColor( final int aCursorIdx )
  {
    return this.cursorColors[aCursorIdx];
  }

  /**
   * @return the cursorColors
   */
  public final Color[] getCursorColors()
  {
    return this.cursorColors;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getEdgeSlope()
   */
  @Override
  public final EdgeSlope getEdgeSlope()
  {
    return this.edgeSlope;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGridColor()
   */
  @Override
  public final Color getGridColor()
  {
    return this.gridColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGroupBackgroundColor()
   */
  @Override
  public final Color getGroupBackgroundColor()
  {
    return this.groupBackgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getGroupByteColor()
   */
  @Override
  public final Color getGroupByteColor()
  {
    return this.groupByteColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getLabelColor()
   */
  @Override
  public final Color getLabelColor()
  {
    return this.labelColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getScopeColor()
   */
  @Override
  public final Color getScopeColor()
  {
    return this.scopeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getScopeHeight()
   */
  @Override
  public final int getScopeHeight()
  {
    return this.scopeHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalAlignment()
   */
  @Override
  public final SignalAlignment getSignalAlignment()
  {
    return this.signalAlignment;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalColor()
   */
  @Override
  public Color getSignalColor()
  {
    return this.signalColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getSignalHeight()
   */
  @Override
  public final int getSignalHeight()
  {
    return this.signalHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTextColor()
   */
  @Override
  public final Color getTextColor()
  {
    return this.textColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTimeColor()
   */
  @Override
  public final Color getTimeColor()
  {
    return this.timeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#getTriggerColor()
   */
  @Override
  public final Color getTriggerColor()
  {
    return this.triggerColor;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowByte(int)
   */
  public final boolean isShowByte( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_BYTE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowChannels(int)
   */
  public final boolean isShowChannels( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_CHANNELS ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowCursorTiming()
   */
  @Override
  public boolean isShowCursorTiming()
  {
    return this.showCursorTiming;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#isShowScope(int)
   */
  public final boolean isShowScope( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_SCOPE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    setChannelHeight( aSettings.getInt( "channelHeight", this.channelHeight ) );
    setSignalHeight( aSettings.getInt( "signalHeight", this.signalHeight ) );
    setScopeHeight( aSettings.getInt( "scopeHeight", this.scopeHeight ) );

    setShowCursorTiming( aSettings.getBoolean( "showCursorTiming", this.showCursorTiming ) );

    String edgeSlopeName = aSettings.get( "edgeSlope", this.edgeSlope.name() );
    setEdgeSlope( EdgeSlope.valueOf( edgeSlopeName ) );

    String colorTargetName = aSettings.get( "colorTarget", this.colorTarget.name() );
    setColorTarget( ColorTarget.valueOf( colorTargetName ) );

    String colorSchemeName = aSettings.get( "colorScheme", this.colorScheme.name() );
    setColorScheme( ColorScheme.valueOf( colorSchemeName ) );
  }

  /**
   * @param aBackgroundColor
   *          the backgroundColor to set
   */
  public final void setBackgroundColor( final Color aBackgroundColor )
  {
    if ( aBackgroundColor == null )
    {
      throw new IllegalArgumentException( "Background color cannot be null!" );
    }
    this.backgroundColor = aBackgroundColor;
  }

  /**
   * @param aChannelColor
   *          the channelColors to set
   */
  public final void setChannelColor( final int aChannelIdx, final Color aChannelColor )
  {
    if ( aChannelColor == null )
    {
      throw new IllegalArgumentException( "Channel color cannot be null!" );
    }
    this.channelColors[aChannelIdx] = aChannelColor;
  }

  /**
   * @param aChannelHeight
   *          the channelHeight to set
   */
  public final void setChannelHeight( final int aChannelHeight )
  {
    this.channelHeight = aChannelHeight;
  }

  /**
   * @param aColorScheme
   */
  public final void setColorScheme( final ColorScheme aColorScheme )
  {
    if ( aColorScheme == null )
    {
      throw new IllegalArgumentException( "Color scheme cannot be null!" );
    }
    this.colorScheme = aColorScheme;

    if ( ColorScheme.CUSTOM != aColorScheme )
    {
      setDefaultColorScheme();
    }
  }

  /**
   * Sets the color target.
   * 
   * @param aColorTarget
   *          the color target to set, cannot be <code>null</code>.
   */
  public final void setColorTarget( final ColorTarget aColorTarget )
  {
    this.colorTarget = aColorTarget;
  }

  /**
   * @param aCursorColors
   *          the cursorColors to set
   */
  public final void setCursorColor( final int aCursorIdx, final Color aCursorColor )
  {
    if ( aCursorColor == null )
    {
      throw new IllegalArgumentException( "Cursor color cannot be null!" );
    }
    this.cursorColors[aCursorIdx] = aCursorColor;
  }

  /**
   * Sets the signal edge slope.
   * 
   * @param aEdgeSlope
   *          the edge slope to set, cannot be <code>null</code>.
   */
  public void setEdgeSlope( final EdgeSlope aEdgeSlope )
  {
    if ( aEdgeSlope == null )
    {
      throw new IllegalArgumentException( "Edge slope cannot be null!" );
    }
    this.edgeSlope = aEdgeSlope;
  }

  /**
   * @param aGridColor
   *          the gridColor to set
   */
  public final void setGridColor( final Color aGridColor )
  {
    if ( aGridColor == null )
    {
      throw new IllegalArgumentException( "Grid color cannot be null!" );
    }
    this.gridColor = aGridColor;
  }

  /**
   * @param aGroupBackgroundColor
   *          the groupBackgroundColor to set
   */
  public final void setGroupBackgroundColor( final Color aGroupBackgroundColor )
  {
    if ( aGroupBackgroundColor == null )
    {
      throw new IllegalArgumentException( "Groupbyte background color cannot be null!" );
    }
    this.groupBackgroundColor = aGroupBackgroundColor;
  }

  /**
   * @param aGroupByteColor
   *          the groupByteColor to set
   */
  public final void setGroupByteColor( final Color aGroupByteColor )
  {
    if ( aGroupByteColor == null )
    {
      throw new IllegalArgumentException( "Groupbyte color cannot be null!" );
    }
    this.groupByteColor = aGroupByteColor;
  }

  /**
   * @param aLabelColor
   *          the labelColor to set
   */
  public final void setLabelColor( final Color aLabelColor )
  {
    if ( aLabelColor == null )
    {
      throw new IllegalArgumentException( "Label color cannot be null!" );
    }
    this.labelColor = aLabelColor;
  }

  /**
   * @param aScopeColor
   *          the scopeColor to set
   */
  public final void setScopeColor( final Color aScopeColor )
  {
    if ( aScopeColor == null )
    {
      throw new IllegalArgumentException( "Scope color cannot be null!" );
    }
    this.scopeColor = aScopeColor;
  }

  /**
   * @param aScopeHeight
   *          the scopeHeight to set
   */
  public final void setScopeHeight( final int aScopeHeight )
  {
    this.scopeHeight = aScopeHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowByte(int,
   *      boolean)
   */
  public final void setShowByte( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DiagramSettings.DISPLAY_BYTE;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DiagramSettings.DISPLAY_BYTE;
    }
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowChannels(int,
   *      boolean)
   */
  public final void setShowChannels( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DiagramSettings.DISPLAY_CHANNELS;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DiagramSettings.DISPLAY_CHANNELS;
    }
  }

  /**
   * @param aShowCursorTiming
   *          <code>true</code> if cursor timings should be shown,
   *          <code>false</code> otherwise.
   */
  public void setShowCursorTiming( final boolean aShowCursorTiming )
  {
    this.showCursorTiming = aShowCursorTiming;
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.settings.DiagramSettings#setShowScope(int,
   *      boolean)
   */
  public final void setShowScope( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DiagramSettings.DISPLAY_SCOPE;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DiagramSettings.DISPLAY_SCOPE;
    }
  }

  /**
   * Sets the signal alignment.
   * 
   * @param aSignalAlignment
   *          a signal alignment, never <code>null</code>.
   */
  public final void setSignalAlignment( final SignalAlignment aSignalAlignment )
  {
    if ( aSignalAlignment == null )
    {
      throw new IllegalArgumentException( "Signal alignment cannot be null!" );
    }
    this.signalAlignment = aSignalAlignment;
  }

  /**
   * @param aSignalColor
   */
  public void setSignalColor( final Color aSignalColor )
  {
    this.signalColor = aSignalColor;
  }

  /**
   * @param aSignalHeight
   *          the signalHeight to set
   */
  public final void setSignalHeight( final int aSignalHeight )
  {
    this.signalHeight = aSignalHeight;
  }

  /**
   * @param aTextColor
   *          the textColor to set
   */
  public final void setTextColor( final Color aTextColor )
  {
    if ( aTextColor == null )
    {
      throw new IllegalArgumentException( "Text color cannot be null!" );
    }
    this.textColor = aTextColor;
  }

  /**
   * @param aTimeColor
   *          the timeColor to set
   */
  public final void setTimeColor( final Color aTimeColor )
  {
    if ( aTimeColor == null )
    {
      throw new IllegalArgumentException( "Time color cannot be null!" );
    }
    this.timeColor = aTimeColor;
  }

  /**
   * @param aTriggerColor
   *          the triggerColor to set
   */
  public final void setTriggerColor( final Color aTriggerColor )
  {
    if ( aTriggerColor == null )
    {
      throw new IllegalArgumentException( "Trigger color cannot be null!" );
    }
    this.triggerColor = aTriggerColor;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "channelHeight", this.channelHeight );
    aSettings.putInt( "signalHeight", this.signalHeight );
    aSettings.putInt( "scopeHeight", this.scopeHeight );

    aSettings.putBoolean( "showCursorTiming", this.showCursorTiming );

    aSettings.put( "colorTarget", this.colorTarget.name() );
    aSettings.put( "colorScheme", this.colorScheme.name() );
    aSettings.put( "edgeSlope", this.edgeSlope.name() );
  }

  /**
   * @param aI
   * @param aFreq1
   * @param aFreq2
   * @param aFreq3
   * @param aPhase1
   * @param aPhase2
   * @param aPhase3
   * @return
   */
  private Color makeColorGradient( final int aI, final double aFreq1, final double aFreq2, final double aFreq3,
      final double aPhase1, final double aPhase2, final double aPhase3 )
  {
    final int width = 127;
    final int center = 128;
    final int red = ( int )( Math.sin( aFreq1 * aI + aPhase1 ) * width + center );
    final int grn = ( int )( Math.sin( aFreq2 * aI + aPhase2 ) * width + center );
    final int blu = ( int )( Math.sin( aFreq3 * aI + aPhase3 ) * width + center );
    return new Color( red, grn, blu );
  }

  /**
   * @return
   */
  private void makeColorPalette( final Color[] aResult, final int aSteps )
  {
    final double freq = 2 * Math.PI / aSteps;
    for ( int i = 0; i < aResult.length; i++ )
    {
      // aResult[i] = makeColorGradient( i, freq, freq, freq, 2.7, 2.4, 4.6 );
      // aResult[i] = makeColorGradient( i, freq, freq, freq, 2.7, 7.4, 3.4 );
      aResult[i] = makeColorGradient( i, freq, freq, freq, 2.0, 4.0, 6.0 );
    }
  }

  /**
   * @return
   */
  private void makeMonochromaticColorPalette( final Color[] aResult, final Color aColor )
  {
    Arrays.fill( aResult, aColor );
  }

  /**
   * 
   */
  private void setDefaultColorScheme()
  {
    if ( ColorScheme.DARK.equals( this.colorScheme ) )
    {
      this.backgroundColor = new Color( 0x10, 0x10, 0x10 );
      this.gridColor = new Color( 0x30, 0x30, 0x30 );
      this.groupBackgroundColor = this.gridColor.brighter();
      this.labelColor = new Color( 0x82, 0x87, 0x8f );
      this.signalColor = new Color( 0xc9, 0xc9, 0xc9 );
      this.scopeColor = this.signalColor;
      this.groupByteColor = this.signalColor;
      this.textColor = Color.WHITE;
      this.timeColor = new Color( 0x82, 0x87, 0x8f );
      this.triggerColor = new Color( 0x82, 0x87, 0x8f );

      makeMonochromaticColorPalette( this.cursorColors, this.signalColor );
      makeColorPalette( this.channelColors, 8 );
    }
    else
    {
      this.backgroundColor = Color.WHITE;
      this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
      this.groupBackgroundColor = this.gridColor.darker();
      this.labelColor = new Color( 0x82, 0x87, 0x8f );
      this.signalColor = new Color( 0x30, 0x4b, 0x75 );
      this.scopeColor = this.signalColor;
      this.groupByteColor = this.signalColor;
      this.textColor = new Color( 0x25, 0x25, 0x25 );
      this.timeColor = new Color( 0x25, 0x25, 0x25 );
      this.triggerColor = new Color( 0x82, 0x87, 0x8f );

      makeColorPalette( this.cursorColors, CapturedData.MAX_CURSORS );
      makeMonochromaticColorPalette( this.channelColors, this.signalColor );
    }
  }
}
