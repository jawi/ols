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
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package org.sump.device.logicsniffer.profile;


import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;
import org.osgi.service.cm.*;


/**
 * Test cases for {@link DeviceProfileManager}.
 */
public class DeviceProfileManagerTest
{
  // METHODS

  /**
   * Test method for {@link DeviceProfileManager#findProfile(String)}.
   */
  @Test
  public void testFindProfile() throws ConfigurationException
  {
    DeviceProfile profile = null;

    DeviceProfileManager manager = new DeviceProfileManager();
    manager.updated( "1", getMockedProperties( "SUMP", "\"Sump\"" ) );
    manager.updated( "2", getMockedProperties( "OLS", "\"*\", \"Open Logic Sniffer v1.01\"" ) );
    manager.updated( "3", getMockedProperties( "BP", "\"BPv3\"" ) );
    manager.updated( "4", getMockedProperties( "SHRIMP", "\"Shrimp1.0\"" ) );

    profile = manager.findProfile( "Shrimp1.0" );
    assertNotNull( "Shrimp", profile );
    assertEquals( "SHRIMP", profile.getType() );

    profile = manager.findProfile( "Open Logic Sniffer v1.01" );
    assertNotNull( "OLS", profile );
    assertEquals( "OLS", profile.getType() );

    profile = manager.findProfile( "Sump" );
    assertNotNull( "Sump", profile );
    assertEquals( "SUMP", profile.getType() );

    profile = manager.findProfile( "BPv3" );
    assertNotNull( "BP", profile );
    assertEquals( "BP", profile.getType() );

    profile = manager.findProfile( "My Unnamed Device" );
    assertNotNull( "Wildcard", profile );
    assertEquals( "OLS", profile.getType() );
  }

  /**
   * Test method for {@link DeviceProfileManager#getProfile(String)}.
   */
  @Test
  public void testGetProfile() throws ConfigurationException
  {
    DeviceProfileManager manager = new DeviceProfileManager();
    manager.updated( "1", getMockedProperties( "foo", "bar" ) );

    assertNotNull( manager.getProfile( "foo" ) );
    assertNull( manager.getProfile( "FOO" ) );
  }

  /**
   * @return
   */
  private Properties getMockedProperties( final String aType, final String aMetadataKeys )
  {
    Properties properties = new Properties();
    properties.put( DeviceProfile.DEVICE_CAPTURECLOCK, "INTERNAL" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZE_BOUND, "false" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZES, "1,2,3,4" );
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
    properties.put( DeviceProfile.DEVICE_METADATA_KEYS, aMetadataKeys );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "10" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DTR, "true" );
    properties.put( DeviceProfile.DEVICE_RECEIVE_TIMEOUT, "12" );
    properties.put( DeviceProfile.DEVICE_SAMPLE_REVERSE_ORDER, "false" );
    properties.put( DeviceProfile.DEVICE_SAMPLERATES, "5,6,7" );
    properties.put( DeviceProfile.DEVICE_SUPPORTS_DDR, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_COMPLEX, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_STAGES, "0" );
    properties.put( DeviceProfile.DEVICE_TYPE, aType );
    return properties;
  }

}
