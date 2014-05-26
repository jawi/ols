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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump;


import static nl.lxtreme.ols.device.sump.profile.Constants.*;

import java.util.*;

import nl.lxtreme.ols.device.sump.profile.*;


/**
 * 
 */
public class TestUtils
{
  // METHODS

  public static DeviceProfile createDeviceProfile( final String aType, final String aMetadataKeys,
      final boolean aLastSampleFirst )
  {
    Map<String, String> properties = new HashMap<String, String>();
    properties.put( DEVICE_CAPTURECLOCK, "INTERNAL" );
    properties.put( DEVICE_CAPTURESIZE_BOUND, "false" );
    properties.put( DEVICE_CAPTURESIZES, "4096,2048,1024,512,256,128,64,32,16" );
    properties.put( DEVICE_CHANNEL_COUNT, "32" );
    properties.put( DEVICE_CHANNEL_GROUPS, "4" );
    properties.put( DEVICE_CHANNEL_NUMBERING_SCHEMES, "DEFAULT" );
    properties.put( DEVICE_CLOCKSPEED, "100000000" );
    properties.put( DEVICE_DIVIDER_CLOCKSPEED, "100000000" );
    properties.put( DEVICE_DESCRIPTION, aType.concat( " Device Profile" ) );
    properties.put( DEVICE_FEATURE_NOISEFILTER, "true" );
    properties.put( DEVICE_FEATURE_RLE, "true" );
    properties.put( DEVICE_FEATURE_TEST_MODE, "true" );
    properties.put( DEVICE_FEATURE_TRIGGERS, "true" );
    properties.put( DEVICE_FEATURE_COMBINED_READDELAY_COUNT, "true" );
    properties.put( DEVICE_INTERFACE, "SERIAL" );
    properties.put( DEVICE_METADATA_KEYS, aMetadataKeys );
    properties.put( DEVICE_OPEN_PORT_DELAY, "0" );
    properties.put( DEVICE_OPEN_PORT_DTR, "false" );
    properties.put( DEVICE_RECEIVE_TIMEOUT, "12" );
    properties.put( DEVICE_LAST_SAMPLE_FIRST, Boolean.toString( aLastSampleFirst ) );
    properties.put( DEVICE_SAMPLERATES, "1000000" );
    properties.put( DEVICE_SUPPORTS_DDR, "true" );
    properties.put( DEVICE_TRIGGER_HP165XX, "false" );
    properties.put( DEVICE_TRIGGER_COMPLEX, "true" );
    properties.put( DEVICE_TRIGGER_STAGES, "4" );
    properties.put( DEVICE_TYPE, aType );

    return new DeviceProfile( properties );
  }
}
