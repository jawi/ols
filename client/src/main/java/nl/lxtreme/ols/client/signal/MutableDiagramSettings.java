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
package nl.lxtreme.ols.client.signal;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;


/**
 * @author jawi
 */
public class MutableDiagramSettings implements DiagramSettings
{
  // VARIABLES

  private int channelHeight;
  private int scopeHeight;
  private int signalHeight;

  private Color triggerColor;
  private Color gridColor;
  private Color backgroundColor;
  private Color groupBackgroundColor;
  private Color groupByteColor;
  private Color scopeColor;
  private Color textColor;
  private Color timeColor;
  private Color labelColor;

  private final Color[] cursorColors;
  private final Color[] channelColors;

  /**
   * Display settings for each group. Can be any combinations (OR-ed) of the
   * defined MODE_* values.
   */
  private final int[] groupSettings;

  // CONSTRUCTORS

  /**
   * Creates a new MutableDiagramSettings instance.
   */
  public MutableDiagramSettings()
  {
    // this.backgroundColor = new Color( 0x10, 0x10, 0x10 );
    // this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    // this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    // this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    // this.textColor = Color.WHITE;
    // this.timeColor = Color.WHITE;
    // this.labelColor = new Color( 0x82, 0x87, 0x8f );
    //
    // this.cursorColors = makeMonochromaticColorPalette(
    // DataContainer.MAX_CURSORS );
    // this.channelColors = makeColorPalette( DataContainer.MAX_CHANNELS, 8 );

    this.backgroundColor = Color.WHITE;
    this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    this.textColor = new Color( 0x25, 0x25, 0x25 );
    this.timeColor = new Color( 0x25, 0x25, 0x25 );
    this.labelColor = new Color( 0x82, 0x87, 0x8f );

    this.cursorColors = makeColorPalette( DataContainer.MAX_CURSORS, DataContainer.MAX_CURSORS );
    this.channelColors = makeMonochromaticColorPalette( DataContainer.MAX_CHANNELS );

    this.groupByteColor = Color.GRAY;
    this.scopeColor = Color.GRAY;

    this.groupSettings = new int[4];
    for ( int i = 0; i < this.groupSettings.length; i++ )
    {
      this.groupSettings[i] = DISPLAY_CHANNELS;
    }

    setChannelHeight( 30 );
    setScopeHeight( 133 );
    setSignalHeight( getChannelHeight() - 4 );
  }

  /**
   * Creates a new MutableDiagramSettings instance.
   * 
   * @param aDiagramSettings
   *          the diagram settings to use as default, cannot be
   *          <code>null</code>.
   */
  public MutableDiagramSettings( final DiagramSettings aDiagramSettings )
  {
    this();

    setBackgroundColor( aDiagramSettings.getBackgroundColor() );
    setTriggerColor( aDiagramSettings.getTriggerColor() );
    setGridColor( aDiagramSettings.getGridColor() );
    setGroupBackgroundColor( aDiagramSettings.getGroupBackgroundColor() );
    setTextColor( aDiagramSettings.getTextColor() );
    setTimeColor( aDiagramSettings.getTimeColor() );
    setLabelColor( aDiagramSettings.getLabelColor() );
    setGroupByteColor( aDiagramSettings.getGroupByteColor() );
    setScopeColor( aDiagramSettings.getScopeColor() );

    for ( int i = 0; i < DataContainer.MAX_CURSORS; i++ )
    {
      setCursorColor( i, aDiagramSettings.getCursorColor( i ) );
    }

    for ( int i = 0; i < DataContainer.MAX_CHANNELS; i++ )
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
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getBackgroundColor()
   */
  @Override
  public final Color getBackgroundColor()
  {
    return this.backgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getChannelColor(int)
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getChannelHeight()
   */
  @Override
  public final int getChannelHeight()
  {
    return this.channelHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getCursorColor(int)
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGridColor()
   */
  @Override
  public final Color getGridColor()
  {
    return this.gridColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGroupBackgroundColor()
   */
  @Override
  public final Color getGroupBackgroundColor()
  {
    return this.groupBackgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGroupByteColor()
   */
  @Override
  public final Color getGroupByteColor()
  {
    return this.groupByteColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getLabelColor()
   */
  @Override
  public final Color getLabelColor()
  {
    return this.labelColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getScopeColor()
   */
  @Override
  public final Color getScopeColor()
  {
    return this.scopeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getScopeHeight()
   */
  @Override
  public final int getScopeHeight()
  {
    return this.scopeHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getSignalHeight()
   */
  @Override
  public final int getSignalHeight()
  {
    return this.signalHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTextColor()
   */
  @Override
  public final Color getTextColor()
  {
    return this.textColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTimeColor()
   */
  @Override
  public final Color getTimeColor()
  {
    return this.timeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTriggerColor()
   */
  @Override
  public final Color getTriggerColor()
  {
    return this.triggerColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowByte(int)
   */
  public final boolean isShowByte( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_BYTE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowChannels(int)
   */
  public final boolean isShowChannels( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_CHANNELS ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowScope(int)
   */
  public final boolean isShowScope( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettings.DISPLAY_SCOPE ) > 0 );
  }

  /**
   * @param aBackgroundColor
   *          the backgroundColor to set
   */
  public final void setBackgroundColor( final Color aBackgroundColor )
  {
    this.backgroundColor = aBackgroundColor;
  }

  /**
   * @param aChannelColor
   *          the channelColors to set
   */
  public final void setChannelColor( final int aChannelIdx, final Color aChannelColor )
  {
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
   * @param aCursorColors
   *          the cursorColors to set
   */
  public final void setCursorColor( final int aCursorIdx, final Color aCursorColor )
  {
    this.cursorColors[aCursorIdx] = aCursorColor;
  }

  /**
   * @param aGridColor
   *          the gridColor to set
   */
  public final void setGridColor( final Color aGridColor )
  {
    this.gridColor = aGridColor;
  }

  /**
   * @param aGroupBackgroundColor
   *          the groupBackgroundColor to set
   */
  public final void setGroupBackgroundColor( final Color aGroupBackgroundColor )
  {
    this.groupBackgroundColor = aGroupBackgroundColor;
  }

  /**
   * @param aGroupByteColor
   *          the groupByteColor to set
   */
  public final void setGroupByteColor( final Color aGroupByteColor )
  {
    this.groupByteColor = aGroupByteColor;
  }

  /**
   * @param aLabelColor
   *          the labelColor to set
   */
  public final void setLabelColor( final Color aLabelColor )
  {
    this.labelColor = aLabelColor;
  }

  /**
   * @param aScopeColor
   *          the scopeColor to set
   */
  public final void setScopeColor( final Color aScopeColor )
  {
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowByte(int, boolean)
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowChannels(int,
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowScope(int,
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
    this.textColor = aTextColor;
  }

  /**
   * @param aTimeColor
   *          the timeColor to set
   */
  public final void setTimeColor( final Color aTimeColor )
  {
    this.timeColor = aTimeColor;
  }

  /**
   * @param aTriggerColor
   *          the triggerColor to set
   */
  public final void setTriggerColor( final Color aTriggerColor )
  {
    this.triggerColor = aTriggerColor;
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
  private Color[] makeColorPalette( final int aLength, final int aSteps )
  {
    final Color[] result = new Color[aLength];
    final double freq = 2 * Math.PI / aSteps;
    for ( int i = 0; i < result.length; i++ )
    {
      // result[i] = makeColorGradient( i, freq, freq, freq, 2.7, 2.4, 4.6 );
      result[i] = makeColorGradient( i, freq, freq, freq, 2.7, 7.4, 3.4 );
    }
    return result;
  }

  /**
   * @return
   */
  private Color[] makeMonochromaticColorPalette( final int aLength )
  {
    final Color[] result = new Color[aLength];
    Arrays.fill( result, new Color( 0x30, 0x4b, 0x75 ) );
    return result;
  }
}
