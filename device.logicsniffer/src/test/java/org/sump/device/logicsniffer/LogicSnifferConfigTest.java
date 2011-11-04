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
package org.sump.device.logicsniffer;


import static org.junit.Assert.*;

import org.junit.*;
import org.sump.device.logicsniffer.profile.*;


/**
 * Test cases for {@link LogicSnifferConfig}.
 */
public class LogicSnifferConfigTest
{
  // VARIABLES

  private LogicSnifferConfig config;
  private DeviceProfile profile;

  // METHODS

  /**
   * Sets up the test case.
   */
  @Before
  public void setUp() throws Exception
  {
    this.config = new LogicSnifferConfig();

    VirtualLogicSnifferDevice device = new VirtualLogicSnifferDevice( this.config );

    this.profile = device.addDeviceProfile( "MOCK", "MockedDevice" );
    this.config.setDeviceProfile( this.profile );

    device.close();
  }

  /**
   * Test method for {@link LogicSnifferConfig#getChannelCount()}.
   */
  @Test
  public void testGetChannelCount()
  {
    this.config.setSampleRate( 1 );
    assertEquals( 32, this.config.getChannelCount() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK );
    assertEquals( 32, this.config.getChannelCount() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );
    assertEquals( 16, this.config.getChannelCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getClockspeed()}.
   */
  @Test
  public void testGetClockspeed()
  {
    assertEquals( LogicSnifferAcquisitionTask.CLOCK, this.config.getClockspeed() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getDeviceProfile()}.
   */
  @Test
  public void testGetDeviceProfile()
  {
    assertEquals( this.profile, this.config.getDeviceProfile() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getDivider()}.
   */
  @Test
  public void testGetDivider()
  {
    this.config.setSampleRate( Integer.MAX_VALUE );
    assertEquals( 0, this.config.getDivider() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK );
    assertEquals( 0, this.config.getDivider() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK >> 1 );
    assertEquals( 1, this.config.getDivider() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK >> 2 );
    assertEquals( 3, this.config.getDivider() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getEnabledGroupCount()}.
   */
  @Test
  public void testGetEnabledGroupCountWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    this.config.setEnabledChannels( 0 );
    assertEquals( 0, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x000000FF );
    assertEquals( 1, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x0000FFFF );
    assertEquals( 2, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x00FFFFFF );
    assertEquals( 2, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertEquals( 2, this.config.getEnabledGroupCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getEnabledGroupCount()}.
   */
  @Test
  public void testGetEnabledGroupCountWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    this.config.setEnabledChannels( 0 );
    assertEquals( 0, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x000000FF );
    assertEquals( 1, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x0000FFFF );
    assertEquals( 2, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0x00FFFFFF );
    assertEquals( 3, this.config.getEnabledGroupCount() );

    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertEquals( 4, this.config.getEnabledGroupCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getGroupCount()}.
   */
  @Test
  public void testGetGroupCountWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    assertEquals( 2, this.config.getGroupCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getGroupCount()}.
   */
  @Test
  public void testGetGroupCountWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    assertEquals( 4, this.config.getGroupCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getRLEDataWidth()}.
   */
  @Test
  public void testGetRLEDataWidthWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    this.config.setEnabledChannels( 0 );
    assertEquals( 0, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x000000FF );
    assertEquals( 8, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x0000FFFF );
    assertEquals( 16, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x00FFFFFF );
    assertEquals( 16, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertEquals( 16, this.config.getRLEDataWidth() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getRLEDataWidth()}.
   */
  @Test
  public void testGetRLEDataWidthWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    this.config.setEnabledChannels( 0 );
    assertEquals( 0, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x000000FF );
    assertEquals( 8, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x0000FFFF );
    assertEquals( 16, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0x00FFFFFF );
    assertEquals( 24, this.config.getRLEDataWidth() );

    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertEquals( 32, this.config.getRLEDataWidth() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getSampleCount()}.
   */
  @Test
  public void testGetSampleCountWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    this.config.setSampleCount( 7 );
    assertEquals( 0, this.config.getSampleCount() );

    this.config.setSampleCount( 8 );
    assertEquals( 8, this.config.getSampleCount() );

    this.config.setSampleCount( 0xffff8 );
    assertEquals( 0xffff8, this.config.getSampleCount() );

    this.config.setSampleCount( 0xffff9 );
    assertEquals( 0xffff8, this.config.getSampleCount() );

    this.config.setSampleCount( Integer.MAX_VALUE );
    assertEquals( 0xffff8, this.config.getSampleCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getSampleCount()}.
   */
  @Test
  public void testGetSampleCountWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    this.config.setSampleCount( 0x03 );
    assertEquals( 0, this.config.getSampleCount() );

    this.config.setSampleCount( 0x04 );
    assertEquals( 0x04, this.config.getSampleCount() );

    this.config.setSampleCount( 0xffffc );
    assertEquals( 0xffffc, this.config.getSampleCount() );

    this.config.setSampleCount( 0xffffd );
    assertEquals( 0xffffc, this.config.getSampleCount() );

    this.config.setSampleCount( Integer.MAX_VALUE );
    assertEquals( 0xffffc, this.config.getSampleCount() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getSampleRate()}.
   */
  @Test
  public void testGetSampleRate()
  {
    this.config.setSampleRate( 1 );
    assertEquals( 1, this.config.getSampleRate() );

    this.config.setSampleRate( 0xFFFFFFE );
    assertEquals( 0xFFFFFFE, this.config.getSampleRate() );

    this.config.setSampleRate( 0xFFFFFFF );
    assertEquals( 0xFFFFFFF, this.config.getSampleRate() );

    this.config.setSampleRate( Integer.MAX_VALUE );
    assertEquals( 0xFFFFFFF, this.config.getSampleRate() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#getStopCounter()}.
   */
  @Test
  public void testGetStopCounter()
  {
    this.config.setSampleCount( 1000 );

    this.config.setRatio( 1.0 );
    assertEquals( 1000, this.config.getStopCounter() );

    this.config.setRatio( 0.5 );
    assertEquals( 500, this.config.getStopCounter() );

    this.config.setRatio( 0.1 );
    assertEquals( 100, this.config.getStopCounter() );

    this.config.setRatio( 0.0 );
    assertEquals( 0, this.config.getStopCounter() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#isDoubleDataRateEnabled()}.
   */
  @Test
  public void testIsDoubleDataRateEnabled()
  {
    this.config.setSampleRate( 1 );
    assertFalse( this.config.isDoubleDataRateEnabled() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK );
    assertFalse( this.config.isDoubleDataRateEnabled() );

    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );
    assertTrue( this.config.isDoubleDataRateEnabled() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#isFilterAvailable()}.
   */
  @Test
  public void testIsFilterAvailableWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    assertFalse( this.config.isFilterAvailable() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#isFilterAvailable()}.
   */
  @Test
  public void testIsFilterAvailableWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    assertTrue( this.config.isFilterAvailable() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#isGroupEnabled(int)}.
   */
  @Test
  public void testIsGroupEnabledWithDDR()
  {
    // With DDR...
    this.config.setSampleRate( LogicSnifferAcquisitionTask.CLOCK + 1 );

    // One channel group...
    this.config.setEnabledChannels( 0x000000FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x0000FF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FF0000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF000000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Two channel groups...
    this.config.setEnabledChannels( 0x0000FFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FFFF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFF0000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF00FF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FF00FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF0000FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Three channel groups...
    this.config.setEnabledChannels( 0x00FFFFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFFFF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFF00FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF00FFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Four channel groups...
    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );
  }

  /**
   * Test method for {@link LogicSnifferConfig#isGroupEnabled(int)}.
   */
  @Test
  public void testIsGroupEnabledWithoutDDR()
  {
    // Without DDR...
    this.config.setSampleRate( 1 );

    // One channel group...
    this.config.setEnabledChannels( 0x000000FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x0000FF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FF0000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF000000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Two channel groups...
    this.config.setEnabledChannels( 0x0000FFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FFFF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFF0000 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF00FF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0x00FF00FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF0000FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Three channel groups...
    this.config.setEnabledChannels( 0x00FFFFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertFalse( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFFFF00 );
    assertFalse( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFFFF00FF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertFalse( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    this.config.setEnabledChannels( 0xFF00FFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertFalse( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );

    // Four channel groups...
    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertTrue( this.config.isGroupEnabled( 0 ) );
    assertTrue( this.config.isGroupEnabled( 1 ) );
    assertTrue( this.config.isGroupEnabled( 2 ) );
    assertTrue( this.config.isGroupEnabled( 3 ) );
  }

  /**
   * Test method for {@link LogicSnifferConfig#setEnabledChannels(int)}.
   */
  @Test
  public void testSetEnabledChannels()
  {
    this.config.setEnabledChannels( 0x00 );
    assertEquals( 0, this.config.getEnabledChannelsMask() );

    this.config.setEnabledChannels( 0xFFFFFFFF );
    assertEquals( 0xFFFFFFFF, this.config.getEnabledChannelsMask() );

    this.config.setEnabledChannels( -1 );
    assertEquals( 0xFFFFFFFF, this.config.getEnabledChannelsMask() );
  }

  /**
   * Test method for {@link LogicSnifferConfig#setRatio(double)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetNegativeRatioFail()
  {
    this.config.setRatio( -0.1 ); // should fail!
  }

  /**
   * Test method for {@link LogicSnifferConfig#setRatio(double)}.
   */
  @Test
  public void testSetRatioOk()
  {
    this.config.setRatio( 0.0 );
    assertEquals( 0.0, this.config.getRatio(), 1.0e-16 );

    this.config.setRatio( 1.0 );
    assertEquals( 1.0, this.config.getRatio(), 1.0e-16 );
  }

  /**
   * Test method for {@link LogicSnifferConfig#setRatio(double)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetTooGreatRatioFail()
  {
    this.config.setRatio( 1.1 ); // should fail!
  }

  /**
   * Test method for {@link LogicSnifferConfig#setSampleCount(int)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroSampleCountFail()
  {
    this.config.setSampleCount( 0 ); // should fail!
  }

  /**
   * Test method for {@link LogicSnifferConfig#setSampleRate(int)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroSampleRateFail()
  {
    this.config.setSampleRate( 0 ); // should fail!
  }

  /**
   * Test method for {@link LogicSnifferConfig#setSampleRate(int)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetZeroSampleRateFails()
  {
    this.config.setSampleRate( 0 ); // should fail!
  }

}
