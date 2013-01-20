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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package org.sump.device.logicsniffer;


import java.util.*;
import java.util.concurrent.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.protocol.*;


/**
 * Test cases for {@link LogicSnifferAcquisitionTask}.
 */
@RunWith( Parameterized.class )
public class LogicSnifferEnabledGroupTest
{
  // VARIABLES

  private VirtualLogicSnifferDevice device;
  private LogicSnifferConfig config;

  private final int enabledGroupMask;
  private final boolean enableDdrMode;
  private final boolean[] expectedDisabledGroups;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDdrEnabledGroupTest instance.
   */
  public LogicSnifferEnabledGroupTest( final int aEnabledGroupMask, final boolean aEnableDdrMode,
      final boolean aEnableGroup1, final boolean aEnableGroup2, final boolean aEnableGroup3, final boolean aEnableGroup4 )
  {
    this.enabledGroupMask = aEnabledGroupMask;
    this.enableDdrMode = aEnableDdrMode;
    this.expectedDisabledGroups = new boolean[] { !aEnableGroup1, !aEnableGroup2, !aEnableGroup3, !aEnableGroup4 };
  }

  // METHODS

  /**
   * @return a collection of test data.
   */
  @Parameters
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // channel mask, ddr mode?, group 1/2/3/4 enabled
            { 0x00000000, false, false, false, false, false }, // 0
            { 0x000000FF, false, true, false, false, false }, // 1
            { 0x0000FF00, false, false, true, false, false }, // 2
            { 0x0000FFFF, false, true, true, false, false }, // 3
            { 0x00FF0000, false, false, false, true, false }, // 4
            { 0x00FF00FF, false, true, false, true, false }, // 5
            { 0x00FFFF00, false, false, true, true, false }, // 6
            { 0x00FFFFFF, false, true, true, true, false }, // 7
            { 0xFF000000, false, false, false, false, true }, // 8
            { 0xFF0000FF, false, true, false, false, true }, // 9
            { 0xFF00FF00, false, false, true, false, true }, // 10
            { 0xFF00FFFF, false, true, true, false, true }, // 11
            { 0xFFFF0000, false, false, false, true, true }, // 12
            { 0xFFFF00FF, false, true, false, true, true }, // 13
            { 0xFFFFFF00, false, false, true, true, true }, // 14
            { 0xFFFFFFFF, false, true, true, true, true }, // 15

            { 0x00000000, true, false, false, false, false }, // 16
            { 0x000000FF, true, true, false, true, false }, // 17
            { 0x0000FF00, true, false, true, false, true }, // 18
            { 0x0000FFFF, true, true, true, true, true }, // 19
            { 0x00FF0000, true, false, false, false, false }, // 20
            { 0x00FF00FF, true, true, false, true, false }, // 21
            { 0x00FFFF00, true, false, true, false, true }, // 22
            { 0x00FFFFFF, true, true, true, true, true }, // 23
            { 0xFF000000, true, false, false, false, false }, // 24
            { 0xFF0000FF, true, true, false, true, false }, // 25
            { 0xFF00FF00, true, false, true, false, true }, // 26
            { 0xFF00FFFF, true, true, true, true, true }, // 27
            { 0xFFFF0000, true, false, false, false, false }, // 28
            { 0xFFFF00FF, true, true, false, true, false }, // 29
            { 0xFFFFFF00, true, false, true, false, true }, // 30
            { 0xFFFFFFFF, true, true, true, true, true }, // 31
        } );
  }

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception
  {
    this.config = new LogicSnifferConfig();
    this.device = new VirtualLogicSnifferDevice( this.config );

    final DeviceProfile deviceProfile = this.device.addDeviceProfile( "VirtualLS", "\"Virtual LogicSniffer\"" );
    this.config.setDeviceProfile( deviceProfile );

    this.config.setAltNumberSchemeEnabled( false ); // don't care
    this.config.setClockSource( CaptureClockSource.INTERNAL ); // don't care
    this.config.setFilterEnabled( false ); // don't care
    this.config.setTestModeEnabled( false ); // don't care
    this.config.setRatio( 0.5 );
    this.config.setRleEnabled( true );
    this.config.setSampleCount( 4096 );
    this.config.setTriggerEnabled( false );
    this.config.setSampleRate( this.enableDdrMode ? 200000000 : 100000000 );
    this.config.setEnabledChannels( this.enabledGroupMask );

    this.device.open();
    this.device.configureAndArmDevice();

    TimeUnit.MILLISECONDS.sleep( 50L );
  }

  /**
   * @throws Exception
   */
  @After
  public void tearDown() throws Exception
  {
    this.device.close();
  }

  /**
   * Test method for GitHub issue #57.
   */
  @Test
  public void testEnableChannelGroupsWithDdrOk() throws Exception
  {
    // channel groups 0 & 1 are leading; 2 & 3 are following...
    this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP1_DISABLED, this.expectedDisabledGroups[0] );
    this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP2_DISABLED, this.expectedDisabledGroups[1] );
    this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP3_DISABLED, this.expectedDisabledGroups[2] );
    this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP4_DISABLED, this.expectedDisabledGroups[3] );
  }
}
