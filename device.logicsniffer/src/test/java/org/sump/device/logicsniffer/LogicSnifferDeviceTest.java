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
import java.util.logging.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;


/**
 * Test cases for {@link LogicSnifferDevice}.
 */
@RunWith( Parameterized.class )
public class LogicSnifferDeviceTest
{
  // CONSTRUCTORS

  private static final Logger LOG = Logger.getAnonymousLogger();

  // VARIABLES

  private VirtualLogicSnifferDevice device;

  private final int delayCounter;
  private final double ratio;
  private final int readCounter;
  private final int sampleRate;
  private final boolean triggerEnabled;

  // CONSTANTS

  /**
   * Creates a new LogicSnifferDeviceTest instance.
   */
  public LogicSnifferDeviceTest( final int aSampleRate, final boolean aTriggerEnabled, final double aRatio,
      final int aReadCounter, final int aDelayCounter )
  {
    this.delayCounter = aDelayCounter;
    this.ratio = aRatio;
    this.sampleRate = aSampleRate;
    this.triggerEnabled = aTriggerEnabled;
    this.readCounter = aReadCounter;
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
        // { sample rate, trigger?, ratio, read counter, delay counter }
            { 1000000, false, 1.0, 1024, 1024 }, //
            { 1000000, true, 0.5, 1024, 512 }, //
            { 2000000, true, 0.25, 1024, 256 }, //
            { 100000000, true, 0.25, 1024, 256 }, //
            { 200000000, true, 0.25, 1024, 256 }, //
        } );
  }

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception
  {
    LogicSnifferConfig config = new LogicSnifferConfig();
    this.device = new VirtualLogicSnifferDevice( config );

    final DeviceProfile deviceProfile = this.device.addDeviceProfile( "VirtualLS", "\"Virtual LogicSniffer\"" );
    config.setDeviceProfile( deviceProfile );

    config.setAltNumberSchemeEnabled( false );
    config.setBaudrate( 9600 );
    config.setClockSource( CaptureClockSource.INTERNAL );
    config.setEnabledChannels( 0xFFFF );
    config.setFilterEnabled( true );
    config.setPortName( "/dev/virtual" );
    config.setRatio( this.ratio );
    config.setRleEnabled( false );
    config.setSampleCount( this.readCounter );
    config.setSampleRate( this.sampleRate );
    config.setTestModeEnabled( false );
    config.setTriggerEnabled( this.triggerEnabled );

    LOG.log( Level.INFO, "Sample rate: {0}Hz; Read counter: {1}; Delay counter: {2}; Ratio: {3}; Trigger: {4}",
        new Object[] { this.sampleRate, this.readCounter, this.delayCounter, this.ratio, this.triggerEnabled } );
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
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void testVerifyDividerAndCounters() throws Exception
  {
    this.device.doInBackground();

    this.device.assertSampleRate( this.sampleRate );
    this.device.assertReadAndDelayCount( this.readCounter, this.delayCounter );

    final boolean ddrMode = this.sampleRate > LogicSnifferDevice.CLOCK;
    this.device.assertFlagState( LogicSnifferDevice.FLAG_DEMUX, ddrMode );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_EXTERNAL, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_EXTERNAL_TEST_MODE, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_FILTER, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_GROUP1_DISABLED, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_GROUP2_DISABLED, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_GROUP3_DISABLED, !ddrMode );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_GROUP4_DISABLED, !ddrMode );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_INTERNAL_TEST_MODE, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_INVERTED, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_NUMBER_SCHEME, false );
    this.device.assertFlagState( LogicSnifferDevice.FLAG_RLE, false );
  }
}
