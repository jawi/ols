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
package org.sump.device.logicsniffer.profile;


import java.util.*;
import java.util.logging.*;


/**
 * @author jawi
 */
public final class DeviceProfile
{
  // CONSTANTS

  /** The short (single word) type of the device described in this profile */
  public static final String DEVICE_TYPE = "device.type";
  /** A longer description of the device */
  public static final String DEVICE_DESCRIPTION = "device.description";
  /** The device interface, currently SERIAL only */
  public static final String DEVICE_INTERFACE = "device.interface";
  /** The device's native clockspeed, in Hertz. */
  public static final String DEVICE_CLOCKSPEED = "device.clockspeed";
  /**
   * Whether or not double-data-rate is supported by the device (also known as
   * the "demux"-mode).
   */
  public static final String DEVICE_SUPPORTS_DDR = "device.supports_ddr";
  /** Supported sample rates in Hertz, separated by comma's */
  public static final String DEVICE_SAMPLERATES = "device.samplerates";
  /** What capture clocks are supported */
  public static final String DEVICE_CAPTURECLOCK = "device.captureclock";
  /** The supported capture sizes, in bytes */
  public static final String DEVICE_CAPTURESIZES = "device.capturesizes";
  /** Whether or not the noise filter is supported */
  public static final String DEVICE_FEATURE_NOISEFILTER = "device.feature.noisefilter";
  /** Whether or not Run-Length encoding is supported */
  public static final String DEVICE_FEATURE_RLE = "device.feature.rle";
  /** Whether or not triggers are supported */
  public static final String DEVICE_FEATURE_TRIGGERS = "device.feature.triggers";
  /** The number of trigger stages */
  public static final String DEVICE_TRIGGER_STAGES = "device.trigger.stages";
  /** Whether or not "complex" triggers are supported */
  public static final String DEVICE_TRIGGER_COMPLEX = "device.trigger.complex";
  /** The total number of channels usable for capturing */
  public static final String DEVICE_CHANNEL_COUNT = "device.channel.count";
  /**
   * The number of channels groups, together with the channel count determines
   * the channels per group
   */
  public static final String DEVICE_CHANNEL_GROUPS = "device.channel.groups";
  /** Whether the capture size is limited by the enabled channel groups */
  public static final String DEVICE_CAPTURESIZE_BOUND = "device.capturesize.bound";

  /** All the profile keys that are supported. */
  private static final List<String> KNOWN_KEYS = Arrays.asList( new String[] { DEVICE_TYPE, DEVICE_DESCRIPTION,
      DEVICE_INTERFACE, DEVICE_CLOCKSPEED, DEVICE_SUPPORTS_DDR, DEVICE_SAMPLERATES, DEVICE_CAPTURECLOCK,
      DEVICE_CAPTURESIZES, DEVICE_FEATURE_NOISEFILTER, DEVICE_FEATURE_RLE, DEVICE_FEATURE_TRIGGERS,
      DEVICE_TRIGGER_STAGES, DEVICE_TRIGGER_COMPLEX, DEVICE_CHANNEL_COUNT, DEVICE_CHANNEL_GROUPS,
      DEVICE_CAPTURESIZE_BOUND } );

  private static final Logger LOG = Logger.getLogger( DeviceProfile.class.getName() );

  // VARIABLES

  private final Map<String, Object> properties;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfile.
   */
  public DeviceProfile()
  {
    this.properties = new HashMap<String, Object>();
  }

  // METHODS

  /**
   * Returns the description of the device this profile denotes.
   * 
   * @return a device description, never <code>null</code>.
   */
  public String getDescription()
  {
    final Object result = this.properties.get( DEVICE_DESCRIPTION );
    return result == null ? "" : ( String )result;
  }

  /**
   * Returns the device type this profile denotes.
   * 
   * @return a device type name, never <code>null</code>.
   */
  public String getType()
  {
    final Object result = this.properties.get( DEVICE_TYPE );
    return result == null ? "<unknown>" : ( String )result;
  }

  /**
   * @param aProperties
   *          the updated properties.
   */
  @SuppressWarnings( "rawtypes" )
  final void setProperties( final Dictionary aProperties )
  {
    final Map<String, Object> newProps = new HashMap<String, Object>();

    Enumeration keys = aProperties.keys();
    while ( keys.hasMoreElements() )
    {
      final String key = ( String )keys.nextElement();
      if ( !KNOWN_KEYS.contains( key ) )
      {
        LOG.log( Level.WARNING, "Unknown/unsupported profile key: " + key );
        continue;
      }

      final Object value = aProperties.get( key );
      newProps.put( key, value );
    }

    synchronized ( this.properties )
    {
      this.properties.clear();
      this.properties.putAll( newProps );
    }
  }
}
