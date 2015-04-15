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
package org.sump.device.logicsniffer.profile;


import static org.junit.Assert.*;

import java.io.*;
import java.util.*;

import org.junit.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.profile.DeviceProfile.DeviceInterface;
import org.sump.device.logicsniffer.profile.DeviceProfile.NumberingScheme;


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
   * Test method for {@link DeviceProfile#getCaptureSizes()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetCaptureSizesFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CAPTURESIZES, "test" );
    this.profile.getCaptureSizes();
  }

  /**
   * Test method for {@link DeviceProfile#getCaptureSizes()}.
   */
  @Test
  @SuppressWarnings( "boxing" )
  public void testGetCaptureSizesOk()
  {
    assertArrayEquals( new Integer[] { 24576, 12288, 6144, 4096, 3072, 2048 }, this.profile.getCaptureSizes() );
  }

  /**
   * Test method for {@link DeviceProfile#getChannelCount()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetChannelCountFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CHANNEL_COUNT, "test" );
    this.profile.getChannelCount();
  }

  /**
   * Test method for {@link DeviceProfile#getCaptureSizes()}.
   */
  @Test
  public void testGetChannelCountOk()
  {
    assertEquals( 4, this.profile.getChannelCount() );
  }

  /**
   * Test method for {@link DeviceProfile#getChannelGroupCount()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetChannelGroupCountFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CHANNEL_GROUPS, "test" );
    this.profile.getChannelGroupCount();
  }

  /**
   * Test method for {@link DeviceProfile#getChannelGroupCount()}.
   */
  @Test
  public void testGetChannelGroupCountOk()
  {
    assertEquals( 1, this.profile.getChannelGroupCount() );
  }

  /**
   * Test method for {@link DeviceProfile#getChannelNumberingSchemes()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetChannelNumberingSchemesFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CHANNEL_NUMBERING_SCHEMES, "test" );
    this.profile.getChannelNumberingSchemes();
  }

  /**
   * Test method for {@link DeviceProfile#getChannelNumberingSchemes()}.
   */
  @Test
  public void testGetChannelNumberingSchemesOk()
  {
    assertArrayEquals( new NumberingScheme[] { NumberingScheme.DEFAULT }, this.profile.getChannelNumberingSchemes() );
  }

  /**
   * Test method for {@link DeviceProfile#getClockspeed()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetClockspeedFail()
  {
    mutateProperty( DeviceProfile.DEVICE_CLOCKSPEED, "test" );
    this.profile.getClockspeed();
  }

  /**
   * Test method for {@link DeviceProfile#getClockspeed()}.
   */
  @Test
  public void testGetClockspeedOk()
  {
    assertEquals( 1000000, this.profile.getClockspeed() );
  }

  /**
   * Test method for {@link DeviceProfile#getDescription()}.
   */
  @Test
  public void testGetDescriptionOk()
  {
    assertNotNull( this.profile.getDescription() );
  }

  /**
   * Test method for {@link DeviceProfile#getDeviceMetadataKeys()}.
   */
  @Test
  public void testGetDeviceMetadataKeysOk()
  {
    assertArrayEquals( new String[] { "a", "b", "a b c" }, this.profile.getDeviceMetadataKeys() );
  }

  /**
   * Test method for {@link DeviceProfile#getInterface()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetIntegerFail()
  {
    mutateProperty( DeviceProfile.DEVICE_INTERFACE, "test" );
    this.profile.getInterface();
  }

  /**
   * Test method for {@link DeviceProfile#getInterface()}.
   */
  @Test
  public void testGetInterfaceOk()
  {
    assertEquals( DeviceInterface.SERIAL, this.profile.getInterface() );
  }

  /**
   * Test method for {@link DeviceProfile#getMaximumCaptureSizeFor(int)}.
   */
  @Test
  public void testGetMaximumCaptureSizeForOk()
  {
    assertEquals( 24576, this.profile.getMaximumCaptureSizeFor( 1 ) );
    assertEquals( 12288, this.profile.getMaximumCaptureSizeFor( 2 ) );
    assertEquals( 6144, this.profile.getMaximumCaptureSizeFor( 3 ) );
    assertEquals( 6144, this.profile.getMaximumCaptureSizeFor( 4 ) );
    assertEquals( 4096, this.profile.getMaximumCaptureSizeFor( 5 ) );
    assertEquals( 4096, this.profile.getMaximumCaptureSizeFor( 6 ) );
    assertEquals( 3072, this.profile.getMaximumCaptureSizeFor( 7 ) );
  }

  /**
   * Test method for {@link DeviceProfile#getOpenPortDelay()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetOpenPortDelayFail()
  {
    mutateProperty( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "test" );
    this.profile.getOpenPortDelay();
  }

  /**
   * Test method for {@link DeviceProfile#getOpenPortDelay()}.
   */
  @Test
  public void testGetOpenPortDelayOk()
  {
    assertEquals( 10, this.profile.getOpenPortDelay() );
  }

  /**
   * Test method for {@link DeviceProfile#getSampleRates()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetSampleRatesFail()
  {
    mutateProperty( DeviceProfile.DEVICE_SAMPLERATES, "test" );
    this.profile.getSampleRates();
  }

  /**
   * Test method for {@link DeviceProfile#getSampleRates()}.
   */
  @Test
  @SuppressWarnings( "boxing" )
  public void testGetSampleRatesOk()
  {
    assertArrayEquals( new Integer[] { 7, 6, 5 }, this.profile.getSampleRates() );
  }

  /**
   * Test method for {@link DeviceProfile#getTriggerStages()}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testGetTriggerStagesFail()
  {
    mutateProperty( DeviceProfile.DEVICE_TRIGGER_STAGES, "test" );
    this.profile.getTriggerStages();
  }

  /**
   * Test method for {@link DeviceProfile#getTriggerStages()}.
   */
  @Test
  public void testGetTriggerStagesOk()
  {
    assertEquals( 0, this.profile.getTriggerStages() );
  }

  /**
   * Test method for {@link DeviceProfile#getType()}.
   */
  @Test
  public void testGetTypeOk()
  {
    assertNotNull( this.profile.getType() );
  }

  /**
   * Test method for {@link DeviceProfile#isCaptureSizeBoundToEnabledChannels()}
   * .
   */
  @Test
  @SuppressWarnings( "boxing" )
  public void testIsCaptureSizeBoundToEnabledChannelsOk()
  {
    assertEquals( true, this.profile.isCaptureSizeBoundToEnabledChannels() );
  }

  /**
   * Test method for {@link DeviceProfile#isComplexTriggersSupported()}.
   */
  @Test
  public void testIsComplexTriggerSupportedOk()
  {
    assertTrue( this.profile.isComplexTriggersSupported() );
  }

  /**
   * Test method for {@link DeviceProfile#isDoubleDataRateSupported()}.
   */
  @Test
  public void testIsDoubleDataRateSupportedOk()
  {
    assertTrue( this.profile.isDoubleDataRateSupported() );
  }

  /**
   * Test method for {@link DeviceProfile#isNoiseFilterSupported()}.
   */
  @Test
  public void testIsNoiseFilterSupportedOk()
  {
    assertFalse( this.profile.isNoiseFilterSupported() );
  }

  /**
   * Test method for {@link DeviceProfile#isOpenPortDtr()}.
   */
  @Test
  public void testIsOpenPortDtrOk()
  {
    assertTrue( this.profile.isOpenPortDtr() );
  }

  /**
   * Test method for {@link DeviceProfile#isRleSupported()}.
   */
  @Test
  public void testIsRleSupportedOk()
  {
    assertFalse( this.profile.isRleSupported() );
  }

  /**
   * Test method for {@link DeviceProfile#isSamplesInReverseOrder()}.
   */
  @Test
  public void testIsSamplesInReverseOrderOk()
  {
    assertFalse( this.profile.isSamplesInReverseOrder() );
  }

  /**
   * Test method for {@link DeviceProfile#isTestModeSupported()}.
   */
  @Test
  public void testIsTestModeSupportedOk()
  {
    assertTrue( this.profile.isTestModeSupported() );
  }

  /**
   * Test method for {@link DeviceProfile#isTriggerSupported()}.
   */
  @Test
  public void testIsTriggerSupportedOk()
  {
    assertFalse( this.profile.isTriggerSupported() );
  }

  /**
   * Tests whether Unicode escapes are interpreted correctly.
   *
   * @see http
   *      ://download.oracle.com/javase/6/docs/api/java/util/Properties.html#
   *      load(java.io.Reader)
   */
  @Test
  public void testReadUnicodeEscapedValueOk() throws IOException
  {
    final StringWriter writer = new StringWriter( 1024 );

    final Properties props = getMockedProperties();
    props.store( writer, null );

    final String tmp = writer.toString().replaceAll( "Device", "\u2039\u00a2" );
    final StringReader reader = new StringReader( tmp );

    final Properties readProps = new Properties();
    readProps.load( reader );

    DeviceProfile profile = new DeviceProfile();
    profile.setProperties( readProps );

    assertEquals( "Mocked \u2039\u00a2 Profile", profile.getDescription() );
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
   * Tests whether Unicode escapes are interpreted correctly.
   *
   * @see http
   *      ://download.oracle.com/javase/6/docs/api/java/util/Properties.html#
   *      load(java.io.Reader)
   */
  @Test
  public void testUnicodeEscapedValueOk()
  {
    mutateProperty( DeviceProfile.DEVICE_DESCRIPTION, "Test \u2039\u00a2 description" );
    assertEquals( "Test \u2039\u00a2 description", this.profile.getDescription() );
  }

  /**
   * @return
   */
  private Properties getMockedProperties()
  {
    Properties properties = new Properties();
    properties.put( DeviceProfile.DEVICE_CAPTURECLOCK, "INTERNAL" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZE_BOUND, "true" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZES, "2048, 3072, 4096, 6144, 12288, 24576" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_COUNT, "4" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_GROUPS, "1" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_NUMBERING_SCHEMES, "DEFAULT" );
    properties.put( DeviceProfile.DEVICE_CLOCKSPEED, "1000000" );
    properties.put( DeviceProfile.DEVICE_DIVIDER_CLOCKSPEED, "1000000" );
    properties.put( DeviceProfile.DEVICE_DESCRIPTION, "Mocked Device Profile" );
    properties.put( DeviceProfile.DEVICE_FEATURE_NOISEFILTER, "false" );
    properties.put( DeviceProfile.DEVICE_FEATURE_RLE, "false" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TEST_MODE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TRIGGERS, "false" );
    properties.put( DeviceProfile.DEVICE_INTERFACE, "SERIAL" );
    properties.put( DeviceProfile.DEVICE_METADATA_KEYS, "a,b,\"a b c\"" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "10" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DTR, "true" );
    properties.put( DeviceProfile.DEVICE_RECEIVE_TIMEOUT, "12" );
    properties.put( DeviceProfile.DEVICE_SAMPLE_REVERSE_ORDER, "false" );
    properties.put( DeviceProfile.DEVICE_SAMPLERATES, "5,6,7" );
    properties.put( DeviceProfile.DEVICE_SUPPORTS_DDR, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_COMPLEX, "true" );
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
