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
 * 
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.diagram.settings;


import static org.junit.Assert.*;

import java.awt.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorScheme;
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorTarget;
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.EdgeSlope;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;

import org.junit.*;


/**
 * Test cases for {@link MutableDiagramSettings}.
 */
public class MutableDiagramSettingsTest
{
  // METHODS

  /**
   * Test method for
   * {@link MutableDiagramSettings#MutableDiagramSettings(nl.lxtreme.ols.client.diagram.settings.DiagramSettings)}
   * .
   */
  @Test
  public void testCopyConstructorOk()
  {
    MutableDiagramSettings originalInstance = new MutableDiagramSettings();

    MutableDiagramSettings newInstance = new MutableDiagramSettings( originalInstance );
    assertNotNull( newInstance );
  }

  /**
   * Test method for {@link MutableDiagramSettings#MutableDiagramSettings()} .
   */
  @Test
  public void testDefaultConstructorOk()
  {
    MutableDiagramSettings newInstance = new MutableDiagramSettings();
    assertNotNull( newInstance );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getBackgroundColor()} .
   */
  @Test
  public void testGetBackgroundColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setBackgroundColor( Color.BLACK );
    assertEquals( Color.BLACK, settings.getBackgroundColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getChannelColor(int)} .
   */
  @Test
  public void testGetChannelColor()
  {
    final Color[] colors = { Color.BLACK, Color.BLUE, Color.WHITE };

    final MutableDiagramSettings settings = new MutableDiagramSettings();
    for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
    {
      final Color color = colors[i % colors.length];

      settings.setChannelColor( i, color );
      assertEquals( color, settings.getChannelColor( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#getChannelHeight()} .
   */
  @Test
  public void testGetChannelHeight()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setChannelHeight( 10 );

    assertEquals( 10, settings.getChannelHeight() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getColorScheme()} .
   */
  @Test
  public void testGetColorScheme()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setColorScheme( ColorScheme.CUSTOM );

    assertEquals( ColorScheme.CUSTOM, settings.getColorScheme() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getColorTarget()} .
   */
  @Test
  public void testGetColorTarget()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setColorTarget( ColorTarget.LABELS );

    assertEquals( ColorTarget.LABELS, settings.getColorTarget() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getCursorColor(int)} .
   */
  @Test
  public void testGetCursorColor()
  {
    final Color[] colors = { Color.BLACK, Color.BLUE, Color.WHITE };

    final MutableDiagramSettings settings = new MutableDiagramSettings();

    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {

      final Color color = colors[i % colors.length];

      settings.setCursorColor( i, color );
      assertEquals( color, settings.getCursorColor( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#getBackgroundColor()} .
   */
  @Test
  public void testGetDefaultBackgroundColor()
  {
    assertNotNull( new MutableDiagramSettings().getBackgroundColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getChannelColor(int)} .
   */
  @Test
  public void testGetDefaultChannelColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
    {
      assertNotNull( settings.getChannelColor( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#getChannelHeight()} .
   */
  @Test
  public void testGetDefaultChannelHeight()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    assertTrue( settings.getChannelHeight() > 0 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getColorScheme()} .
   */
  @Test
  public void testGetDefaultColorScheme()
  {
    assertNotNull( new MutableDiagramSettings().getColorScheme() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getColorTarget()} .
   */
  @Test
  public void testGetDefaultColorTarget()
  {
    assertNotNull( new MutableDiagramSettings().getColorTarget() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getCursorColor(int)} .
   */
  @Test
  public void testGetDefaultCursorColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      assertNotNull( settings.getCursorColor( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#getEdgeSlope()} .
   */
  @Test
  public void testGetDefaultEdgeSlope()
  {
    assertNotNull( new MutableDiagramSettings().getEdgeSlope() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGridColor()} .
   */
  @Test
  public void testGetDefaultGridColor()
  {
    assertNotNull( new MutableDiagramSettings().getGridColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGroupBackgroundColor()} .
   */
  @Test
  public void testGetDefaultGroupBackgroundColor()
  {
    assertNotNull( new MutableDiagramSettings().getGroupBackgroundColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGroupByteColor()} .
   */
  @Test
  public void testGetDefaultGroupByteColor()
  {
    assertNotNull( new MutableDiagramSettings().getGroupByteColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getLabelColor()} .
   */
  @Test
  public void testGetDefaultLabelColor()
  {
    assertNotNull( new MutableDiagramSettings().getLabelColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getScopeColor()} .
   */
  @Test
  public void testGetDefaultScopeColor()
  {
    assertNotNull( new MutableDiagramSettings().getScopeColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getScopeHeight()} .
   */
  @Test
  public void testGetDefaultScopeHeight()
  {
    assertTrue( new MutableDiagramSettings().getScopeHeight() > 0 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalAlignment()} .
   */
  @Test
  public void testGetDefaultSignalAlignment()
  {
    assertNotNull( new MutableDiagramSettings().getSignalAlignment() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalColor()} .
   */
  @Test
  public void testGetDefaultSignalColor()
  {
    assertNotNull( new MutableDiagramSettings().getSignalColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalHeight()} .
   */
  @Test
  public void testGetDefaultSignalHeight()
  {
    assertTrue( new MutableDiagramSettings().getSignalHeight() > 0 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTextColor()} .
   */
  @Test
  public void testGetDefaultTextColor()
  {
    assertNotNull( new MutableDiagramSettings().getTextColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTimeColor()} .
   */
  @Test
  public void testGetDefaultTimeColor()
  {
    assertNotNull( new MutableDiagramSettings().getTimeColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTriggerColor()} .
   */
  @Test
  public void testGetDefaultTriggerColor()
  {
    assertNotNull( new MutableDiagramSettings().getTriggerColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getEdgeSlope()} .
   */
  @Test
  public void testGetEdgeSlope()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setEdgeSlope( EdgeSlope.PERPENDICULAR );

    assertEquals( EdgeSlope.PERPENDICULAR, settings.getEdgeSlope() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGridColor()} .
   */
  @Test
  public void testGetGridColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setGridColor( Color.BLACK );

    assertEquals( Color.BLACK, settings.getGridColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGroupBackgroundColor()} .
   */
  @Test
  public void testGetGroupBackgroundColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setGroupBackgroundColor( Color.BLACK );

    assertEquals( Color.BLACK, settings.getGroupBackgroundColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getGroupByteColor()} .
   */
  @Test
  public void testGetGroupByteColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setGroupByteColor( Color.WHITE );

    assertEquals( Color.WHITE, settings.getGroupByteColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getLabelColor()} .
   */
  @Test
  public void testGetLabelColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setLabelColor( Color.WHITE );

    assertEquals( Color.WHITE, settings.getLabelColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getScopeColor()} .
   */
  @Test
  public void testGetScopeColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setScopeColor( Color.BLUE );

    assertEquals( Color.BLUE, settings.getScopeColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getScopeHeight()} .
   */
  @Test
  public void testGetScopeHeight()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setScopeHeight( 13 );

    assertEquals( 13, settings.getScopeHeight() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalAlignment()} .
   */
  @Test
  public void testGetSignalAlignment()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setSignalAlignment( SignalAlignment.BOTTOM );

    assertEquals( SignalAlignment.BOTTOM, settings.getSignalAlignment() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalColor()} .
   */
  @Test
  public void testGetSignalColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setSignalColor( Color.YELLOW );

    assertEquals( Color.YELLOW, settings.getSignalColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getSignalHeight()} .
   */
  @Test
  public void testGetSignalHeight()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setSignalHeight( 14 );

    assertEquals( 14, settings.getSignalHeight() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTextColor()} .
   */
  @Test
  public void testGetTextColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setTextColor( Color.RED );

    assertEquals( Color.RED, settings.getTextColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTimeColor()} .
   */
  @Test
  public void testGetTimeColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setTimeColor( Color.WHITE );

    assertEquals( Color.WHITE, settings.getTimeColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#getTriggerColor()} .
   */
  @Test
  public void testGetTriggerColor()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();
    settings.setTriggerColor( Color.BLACK );

    assertEquals( Color.BLACK, settings.getTriggerColor() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#isShowByte(int)} .
   */
  @Test
  public void testIsShowByte()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();

    for ( int i = 0; i < Ols.MAX_BLOCKS; i++ )
    {
      settings.setShowByte( i, true );
      assertTrue( settings.isShowByte( i ) );

      settings.setShowByte( i, false );
      assertFalse( settings.isShowByte( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#isShowChannels(int)} .
   */
  @Test
  public void testIsShowChannels()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();

    for ( int i = 0; i < Ols.MAX_BLOCKS; i++ )
    {
      settings.setShowChannels( i, true );
      assertTrue( settings.isShowChannels( i ) );

      settings.setShowChannels( i, false );
      assertFalse( settings.isShowChannels( i ) );
    }
  }

  /**
   * Test method for {@link MutableDiagramSettings#isShowCursorTiming()} .
   */
  @Test
  public void testIsShowCursorTiming()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();

    settings.setShowCursorTiming( false );
    assertFalse( settings.isShowCursorTiming() );

    settings.setShowCursorTiming( true );
    assertTrue( settings.isShowCursorTiming() );
  }

  /**
   * Test method for {@link MutableDiagramSettings#isShowScope(int)} .
   */
  @Test
  public void testIsShowScope()
  {
    final MutableDiagramSettings settings = new MutableDiagramSettings();

    for ( int i = 0; i < Ols.MAX_BLOCKS; i++ )
    {
      settings.setShowScope( i, true );
      assertTrue( settings.isShowScope( i ) );

      settings.setShowScope( i, false );
      assertFalse( settings.isShowScope( i ) );
    }
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#MutableDiagramSettings(nl.lxtreme.ols.client.diagram.settings.DiagramSettings)}
   * .
   */
  @Test
  public void testNullDataSettingsCopyConstructorOk()
  {
    MutableDiagramSettings newInstance = new MutableDiagramSettings( ( DiagramSettings )null );
    assertNotNull( newInstance );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#MutableDiagramSettings(UserSettings)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testNullUserSettingCopyConstructorFail()
  {
    new MutableDiagramSettings( ( UserSettings )null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setCursorColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetCursorColorWithNegativeIndexFail()
  {
    new MutableDiagramSettings().setCursorColor( -1, Color.BLACK );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setCursorColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetCursorColorWithTooLargeIndexFail()
  {
    new MutableDiagramSettings().setCursorColor( Ols.MAX_CURSORS, Color.BLACK );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setChannelHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNegativeChannelHeightFail()
  {
    new MutableDiagramSettings().setChannelHeight( -1 );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setChannelColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNegativeIndexOfChannelColorFail()
  {
    new MutableDiagramSettings().setChannelColor( -1, Color.BLACK );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setScopeHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNegativeScopeHeightFail()
  {
    new MutableDiagramSettings().setScopeHeight( -1 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setSignalHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNegativeSignalHeightFail()
  {
    new MutableDiagramSettings().setSignalHeight( -1 );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setBackgroundColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullBackgroundColorFail()
  {
    new MutableDiagramSettings().setBackgroundColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setChannelColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullChannelColorFail()
  {
    new MutableDiagramSettings().setChannelColor( 0, null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setColorScheme(nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorScheme)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullColorSchemeFail()
  {
    new MutableDiagramSettings().setColorScheme( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setColorTarget(nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorTarget)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullColorTargetFail()
  {
    new MutableDiagramSettings().setColorTarget( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setCursorColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullCursorColorFail()
  {
    new MutableDiagramSettings().setCursorColor( 1, null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setEdgeSlope(nl.lxtreme.ols.client.diagram.settings.DiagramSettings.EdgeSlope)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullEdgeSlopeFail()
  {
    new MutableDiagramSettings().setEdgeSlope( null );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setGridColor(java.awt.Color)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullGridColorFail()
  {
    new MutableDiagramSettings().setGridColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setGroupBackgroundColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullGroupBackgroundColorFail()
  {
    new MutableDiagramSettings().setGroupBackgroundColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setGroupByteColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullGroupByteColorFail()
  {
    new MutableDiagramSettings().setGroupByteColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setLabelColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullLabelColorFail()
  {
    new MutableDiagramSettings().setLabelColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setScopeColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullScopeColorFail()
  {
    new MutableDiagramSettings().setScopeColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setSignalAlignment(nl.lxtreme.ols.client.diagram.settings.DiagramSettings.SignalAlignment)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullSignalAlignmentFail()
  {
    new MutableDiagramSettings().setSignalAlignment( null );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setTextColor(java.awt.Color)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullTextColorFail()
  {
    new MutableDiagramSettings().setTextColor( null );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setTimeColor(java.awt.Color)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullTimeColorFail()
  {
    new MutableDiagramSettings().setTimeColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setTriggerColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNullTriggerColorFail()
  {
    new MutableDiagramSettings().setTriggerColor( null );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setShowByte(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowByteForNegativeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowByte( -1, false );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setShowByte(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowByteForTooLargeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowByte( Ols.MAX_BLOCKS, false );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setShowChannels(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowChannelsForNegativeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowChannels( -1, false );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setShowChannels(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowChannelsForTooLargeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowChannels( Ols.MAX_BLOCKS, false );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setShowScope(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowScopeForNegativeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowScope( -1, false );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setShowScope(int, boolean)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetShowScopeForTooLargeGroupIndexFail()
  {
    new MutableDiagramSettings().setShowScope( Ols.MAX_BLOCKS, false );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setSignalColor(java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetSignalColor()
  {
    new MutableDiagramSettings().setSignalColor( null );
  }

  /**
   * Test method for
   * {@link MutableDiagramSettings#setChannelColor(int, java.awt.Color)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetTooLargeIndexOfChannelColorFail()
  {
    new MutableDiagramSettings().setChannelColor( Ols.MAX_CHANNELS, Color.BLACK );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setChannelHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroChannelHeightFail()
  {
    new MutableDiagramSettings().setChannelHeight( 0 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setScopeHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroScopeHeightFail()
  {
    new MutableDiagramSettings().setScopeHeight( 0 );
  }

  /**
   * Test method for {@link MutableDiagramSettings#setSignalHeight(int)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroSignalHeightFail()
  {
    new MutableDiagramSettings().setSignalHeight( 0 );
  }
}
