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
package org.sump.device.logicsniffer.profile;


import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;


/**
 * Test cases for {@link DeviceProfile}.
 */
public class DeviceProfileTest
{
  // VARIABLES

  private DeviceProfile profile;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp()
  {
    final Properties props = getMockedProperties();
    this.profile = new DeviceProfile();
    this.profile.setProperties( props );
  }

  /**
   * Test method for {@link DeviceProfile#clone()}.
   */
  @Test
  public void testCloneOk()
  {
    final DeviceProfile profile1 = new DeviceProfile();
    profile1.setProperties( getMockedProperties() );
    final DeviceProfile profile2 = profile1.clone();

    assertNotSame( profile1, profile2 );
    assertEquals( profile1, profile2 );
    assertEquals( profile1.getProperties(), profile2.getProperties() );
  }

  /**
   * Test method for {@link DeviceProfile#clone()}.
   */
  @Test
  public void testEmptyCloneOk()
  {
    final DeviceProfile profile1 = new DeviceProfile();
    final DeviceProfile profile2 = profile1.clone();

    assertNotSame( profile1, profile2 );
    assertEquals( profile1, profile2 );
    assertEquals( profile1.getProperties(), profile2.getProperties() );
  }

  /**
   * Test method for {@link DeviceProfile#getCaptureClock()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetCaptureClockFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CAPTURECLOCK, "test" );
    this.profile.getCaptureClock();
  }

  /**
   * Test method for {@link DeviceProfile#getCaptureClock()}.
   */
  @Test
  public void testGetCaptureClockOk()
  {
    assertArrayEquals( new CaptureClockSource[] { CaptureClockSource.INTERNAL }, this.profile.getCaptureClock() );
  }

  /**
   * Test method for {@link DeviceProfile#getCaptureClock()}.
   */
  @Test( expected = IllegalArgumentException.class )
  @Ignore( "does not work yet as expected." )
  public void testIsCaptureSizeBoundToEnabledChannelsFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CAPTURESIZE_BOUND, "test" );
    this.profile.isCaptureSizeBoundToEnabledChannels();
  }

  /**
   * Test method for {@link DeviceProfile#isCaptureSizeBoundToEnabledChannels()}
   * .
   */
  @Test
  public void testIsCaptureSizeBoundToEnabledChannelsOk()
  {
    assertEquals( false, this.profile.isCaptureSizeBoundToEnabledChannels() );
  }

  /**
   * Test method for {@link DeviceProfile#setProperties(java.util.Dictionary)} .
   */
  @Test
  public void testSetCompletePropertiesOk()
  {
    final Properties props = getMockedProperties();
    new DeviceProfile().setProperties( props );
  }

  /**
   * Test method for {@link DeviceProfile#setProperties(java.util.Dictionary)} .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testSetIncompletePropertiesFail()
  {
    final Properties props = getMockedProperties();
    props.remove( DeviceProfile.DEVICE_CAPTURECLOCK );
    new DeviceProfile().setProperties( props );
  }

  /**
   * @return
   */
  private Properties getMockedProperties()
  {
    Properties properties = new Properties();
    properties.put( DeviceProfile.DEVICE_CAPTURECLOCK, "INTERNAL" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZE_BOUND, "false" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZES, "1,2,3,4" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_COUNT, "4" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_GROUPS, "1" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_NUMBERING_SCHEMES, "DEFAULT" );
    properties.put( DeviceProfile.DEVICE_CLOCKSPEED, "1000000" );
    properties.put( DeviceProfile.DEVICE_DESCRIPTION, "Mocked Device Profile" );
    properties.put( DeviceProfile.DEVICE_FEATURE_NOISEFILTER, "false" );
    properties.put( DeviceProfile.DEVICE_FEATURE_RLE, "false" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TEST_MODE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TRIGGERS, "false" );
    properties.put( DeviceProfile.DEVICE_INTERFACE, "SERIAL" );
    properties.put( DeviceProfile.DEVICE_METADATA_KEYS, "mock" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "0" );
    properties.put( DeviceProfile.DEVICE_SAMPLE_REVERSE_ORDER, "false" );
    properties.put( DeviceProfile.DEVICE_SAMPLERATES, "1000000" );
    properties.put( DeviceProfile.DEVICE_SUPPORTS_DDR, "false" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_COMPLEX, "false" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_STAGES, "0" );
    properties.put( DeviceProfile.DEVICE_TYPE, "MOCK" );
    return properties;
  }

  /**
   * @param aKey
   * @param aValue
   */
  private void mutateProperty( final String aKey, final String aValue )
  {
    final Properties mockedProperties = getMockedProperties();
    mockedProperties.put( aKey, aValue );
    this.profile.setProperties( mockedProperties );
  }
}
