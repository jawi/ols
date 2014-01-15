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
package nl.lxtreme.ols.device.sump;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.device.sump.profile.*;
import nl.lxtreme.ols.device.sump.profile.DeviceProfile.CaptureClockSource;
import nl.lxtreme.ols.device.sump.protocol.*;

import org.junit.*;
import org.junit.rules.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * Test cases for {@link LogicSnifferAcquisitionTask}.
 */
@RunWith( Parameterized.class )
public class LogicSnifferEnabledGroupTest
{
  // VARIABLES

  private VirtualLogicSnifferDevice device;
  private SumpConfig config;

  private final int enabledGroupMask;
  private final boolean enableDdrMode;
  private final boolean[] expectedDisabledGroups;
  private final Class<? extends Exception> expectedException;
  @Rule
  public ExpectedException exception = ExpectedException.none();

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDdrEnabledGroupTest instance.
   */
  public LogicSnifferEnabledGroupTest( int aEnabledGroupMask, boolean aEnableDdrMode, boolean aEnableGroup1,
      boolean aEnableGroup2, boolean aEnableGroup3, boolean aEnableGroup4, Class<? extends Exception> exception )
  {
    this.enabledGroupMask = aEnabledGroupMask;
    this.enableDdrMode = aEnableDdrMode;
    this.expectedDisabledGroups = new boolean[] { !aEnableGroup1, !aEnableGroup2, !aEnableGroup3, !aEnableGroup4 };
    this.expectedException = exception;
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
            { 0x00000000, false, false, false, false, false, IllegalArgumentException.class }, // 0
            { 0x000000FF, false, true, false, false, false, null }, // 1
            { 0x0000FF00, false, false, true, false, false, null }, // 2
            { 0x0000FFFF, false, true, true, false, false, null }, // 3
            { 0x00FF0000, false, false, false, true, false, null }, // 4
            { 0x00FF00FF, false, true, false, true, false, null }, // 5
            { 0x00FFFF00, false, false, true, true, false, null }, // 6
            { 0x00FFFFFF, false, true, true, true, false, null }, // 7
            { 0xFF000000, false, false, false, false, true, null }, // 8
            { 0xFF0000FF, false, true, false, false, true, null }, // 9
            { 0xFF00FF00, false, false, true, false, true, null }, // 10
            { 0xFF00FFFF, false, true, true, false, true, null }, // 11
            { 0xFFFF0000, false, false, false, true, true, null }, // 12
            { 0xFFFF00FF, false, true, false, true, true, null }, // 13
            { 0xFFFFFF00, false, false, true, true, true, null }, // 14
            { 0xFFFFFFFF, false, true, true, true, true, null }, // 15

            { 0x00000000, true, false, false, false, false, IllegalArgumentException.class }, // 16
            { 0x000000FF, true, true, false, true, false, null }, // 17
            { 0x0000FF00, true, false, true, false, true, null }, // 18
            { 0x0000FFFF, true, true, true, true, true, null }, // 19
            { 0x00FF0000, true, false, false, false, false, IllegalArgumentException.class }, // 20
            { 0x00FF00FF, true, true, false, true, false, null }, // 21
            { 0x00FFFF00, true, false, true, false, true, null }, // 22
            { 0x00FFFFFF, true, true, true, true, true, null }, // 23
            { 0xFF000000, true, false, false, false, false, IllegalArgumentException.class }, // 24
            { 0xFF0000FF, true, true, false, true, false, null }, // 25
            { 0xFF00FF00, true, false, true, false, true, null }, // 26
            { 0xFF00FFFF, true, true, true, true, true, null }, // 27
            { 0xFFFF0000, true, false, false, false, false, IllegalArgumentException.class }, // 28
            { 0xFFFF00FF, true, true, false, true, false, null }, // 29
            { 0xFFFFFF00, true, false, true, false, true, null }, // 30
            { 0xFFFFFFFF, true, true, true, true, true, null }, // 31
        } );
  }

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception
  {
    final DeviceProfile deviceProfile = VirtualLogicSnifferDevice.createDeviceProfile( "VirtualLS",
        "\"Virtual LogicSniffer\"", true );

    SumpConfigBuilder builder = new SumpConfigBuilder( deviceProfile );
    builder.setAltNumberSchemeEnabled( false ); // don't care
    builder.setClockSource( CaptureClockSource.INTERNAL ); // don't care
    builder.setFilterEnabled( false ); // don't care
    builder.setTestModeEnabled( false ); // don't care
    builder.setRatio( 0.5 );
    builder.setRleEnabled( true );
    builder.setSampleCount( 4096 );
    builder.setTriggerEnabled( false );
    builder.setSampleRate( this.enableDdrMode ? 200000000 : 100000000 );
    builder.setEnabledChannels( this.enabledGroupMask );

    if ( this.expectedException != null )
    {
      this.exception.expect( this.expectedException );
    }

    this.config = builder.build();

    this.device = new VirtualLogicSnifferDevice( this.config );

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
    if ( this.device != null )
    {
      this.device.close();
    }
  }

  /**
   * Test method for GitHub issue #57.
   */
  @Test( timeout = 2000 )
  public void testEnableChannelGroupsWithDdrOk() throws Exception
  {
    if ( this.expectedException == null )
    {
      // channel groups 0 & 1 are leading; 2 & 3 are following...
      this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP1_DISABLED, this.expectedDisabledGroups[0] );
      this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP2_DISABLED, this.expectedDisabledGroups[1] );
      this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP3_DISABLED, this.expectedDisabledGroups[2] );
      this.device.assertFlagState( SumpCommandWriter.FLAG_GROUP4_DISABLED, this.expectedDisabledGroups[3] );
    }
  }
}
