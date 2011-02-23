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

import nl.lxtreme.ols.util.*;


/**
 * Provides a device profile.
 */
public final class DeviceProfile implements Cloneable
{
  // INNER TYPES

  /**
   * The various capture clock sources.
   */
  public static enum CaptureClockSource
  {
    INTERNAL, EXTERNAL_FALLING, EXTERNAL_RISING;
  }

  /**
   * The various interfaces of the device.
   */
  public static enum DeviceInterface
  {
    SERIAL, USB;
  }

  /**
   * The various numbering schemes.
   */
  public static enum NumberingScheme
  {
    DEFAULT, INSIDE, OUTSIDE;
  }

  /**
   * The various types of triggers.
   */
  public static enum TriggerType
  {
    SIMPLE, COMPLEX;
  }

  /**
   * Provides a numeric comparator, sorts in decrementing order.
   */
  private static class NumericComparator implements Comparator<Number>
  {
    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare( final Number aO1, final Number aO2 )
    {
      if ( aO1 instanceof Integer )
      {
        final int value1 = aO1.intValue();
        final int value2 = aO2.intValue();
        return ( value2 - value1 );
      }
      else if ( aO1 instanceof Long )
      {
        final long value1 = aO1.longValue();
        final long value2 = aO2.longValue();
        return ( int )( value2 - value1 );
      }

      final double value1 = aO1.doubleValue();
      final double value2 = aO2.doubleValue();
      return ( int )( value2 - value1 );
    }
  }

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
  /** Whether or not a testing mode is supported. */
  public static final String DEVICE_FEATURE_TEST_MODE = "device.feature.testmode";
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
  /** What channel numbering schemes are supported by the device. */
  public static final String DEVICE_CHANNEL_NUMBERING_SCHEMES = "device.channel.numberingschemes";
  /**
   * Is a delay after opening the port and device detection needed? (0 = no
   * delay, >0 = delay in milliseconds)
   */
  public static final String DEVICE_OPEN_PORT_DELAY = "device.open.portdelay";
  /**
   * Which metadata keys correspond to this device profile? Value is a
   * comma-separated list of (double quoted) names.
   */
  public static final String DEVICE_METADATA_KEYS = "device.metadata.keys";
  /**
   * In which order are samples sent back from the device? If <code>true</code>
   * then last sample first, if <code>false</code> then first sample first.
   */
  public static final String DEVICE_SAMPLE_REVERSE_ORDER = "device.samples.reverseOrder";

  /** All the profile keys that are supported. */
  private static final List<String> KNOWN_KEYS = Arrays.asList( new String[] { DEVICE_TYPE, DEVICE_DESCRIPTION,
      DEVICE_INTERFACE, DEVICE_CLOCKSPEED, DEVICE_SUPPORTS_DDR, DEVICE_SAMPLERATES, DEVICE_CAPTURECLOCK,
      DEVICE_CAPTURESIZES, DEVICE_FEATURE_NOISEFILTER, DEVICE_FEATURE_RLE, DEVICE_FEATURE_TEST_MODE,
      DEVICE_FEATURE_TRIGGERS, DEVICE_TRIGGER_STAGES, DEVICE_TRIGGER_COMPLEX, DEVICE_CHANNEL_COUNT,
      DEVICE_CHANNEL_GROUPS, DEVICE_CAPTURESIZE_BOUND, DEVICE_CHANNEL_NUMBERING_SCHEMES, DEVICE_OPEN_PORT_DELAY,
      DEVICE_METADATA_KEYS, DEVICE_SAMPLE_REVERSE_ORDER } );
  private static final List<String> IGNORED_KEYS = Arrays.asList( new String[] { "felix.fileinstall.filename",
      "service.pid", "service.factoryPid" } );

  private static final Logger LOG = Logger.getLogger( DeviceProfile.class.getName() );

  // VARIABLES

  private final Map<String, String> properties;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfile.
   */
  public DeviceProfile()
  {
    this.properties = new HashMap<String, String>();
  }

  // METHODS

  /**
   * Returns a deep copy of this device profile, including all properties.
   * 
   * @return a deep copy of this device profile, never <code>null</code>.
   * @see java.lang.Object#clone()
   */
  @Override
  public DeviceProfile clone()
  {
    try
    {
      DeviceProfile clone = ( DeviceProfile )super.clone();
      clone.properties.putAll( this.properties );
      return clone;
    }
    catch ( CloneNotSupportedException exception )
    {
      throw new IllegalStateException( exception );
    }
  }

  /**
   * Returns the capture clock sources supported by the device.
   * 
   * @return an array of capture clock sources, never <code>null</code>.
   */
  public CaptureClockSource[] getCaptureClock()
  {
    final String rawValue = this.properties.get( DEVICE_CAPTURECLOCK );
    final String[] values = rawValue.split( ",\\s*" );
    final CaptureClockSource[] result = new CaptureClockSource[values.length];
    for ( int i = 0; i < values.length; i++ )
    {
      result[i] = CaptureClockSource.valueOf( values[i].trim() );
    }
    return result;
  }

  /**
   * Returns all supported capture sizes.
   * 
   * @return an array of capture sizes, in bytes, never <code>null</code>.
   */
  public Integer[] getCaptureSizes()
  {
    final String rawValue = this.properties.get( DEVICE_CAPTURESIZES );
    final String[] values = rawValue.split( ",\\s*" );
    final List<Integer> result = new ArrayList<Integer>();
    for ( String value : values )
    {
      result.add( Integer.valueOf( value.trim() ) );
    }
    Collections.sort( result, new NumericComparator() );
    return result.toArray( new Integer[result.size()] );
  }

  /**
   * Returns the total number of capture channels.
   * 
   * @return a capture channel count, greater than 0.
   */
  public int getChannelCount()
  {
    final String value = this.properties.get( DEVICE_CHANNEL_COUNT );
    return Integer.parseInt( value );
  }

  /**
   * Returns the total number of channel groups.
   * 
   * @return a channel group count, greater than 0.
   */
  public int getChannelGroupCount()
  {
    final String value = this.properties.get( DEVICE_CHANNEL_GROUPS );
    return Integer.parseInt( value );
  }

  /**
   * Returns all supported channel numbering schemes.
   * 
   * @return an array of numbering schemes, never <code>null</code>.
   */
  public NumberingScheme[] getChannelNumberingSchemes()
  {
    final String rawValue = this.properties.get( DEVICE_CHANNEL_NUMBERING_SCHEMES );
    final String[] values = rawValue.split( ",\\s*" );
    final NumberingScheme[] result = new NumberingScheme[values.length];
    for ( int i = 0; i < result.length; i++ )
    {
      result[i] = NumberingScheme.valueOf( values[i].trim() );
    }
    return result;
  }

  /**
   * Returns the (maximum) capture clock of the device.
   * 
   * @return a capture clock, in Hertz.
   */
  public int getClockspeed()
  {
    final String value = this.properties.get( DEVICE_CLOCKSPEED );
    return Integer.parseInt( value );
  }

  /**
   * Returns the description of the device this profile denotes.
   * 
   * @return a device description, never <code>null</code>.
   */
  public String getDescription()
  {
    final String result = this.properties.get( DEVICE_DESCRIPTION );
    return result == null ? "" : ( String )result;
  }

  /**
   * Returns the metadata keys that allow identification of this device profile.
   * <p>
   * Note: if the returned array contains an empty string value (not
   * <code>null</code>, but <code>""</code>!), it means that this profile can be
   * used for <em>all</em> devices.
   * </p>
   * 
   * @return an array of metadata keys this profile supports, never
   *         <code>null</code>.
   */
  public String[] getDeviceMetadataKeys()
  {
    final String rawValue = this.properties.get( DEVICE_METADATA_KEYS );
    return StringUtils.tokenizeQuotedStrings( rawValue, ", " );
  }

  /**
   * Returns the interface over which the device communicates.
   * 
   * @return the device interface, never <code>null</code>.
   */
  public DeviceInterface getInterface()
  {
    final String value = this.properties.get( DEVICE_INTERFACE );
    return DeviceInterface.valueOf( value );
  }

  /**
   * Returns the delay between opening the port to the device and starting the
   * device detection cycle.
   * 
   * @return a delay, in milliseconds, >= 0.
   */
  public int getOpenPortDelay()
  {
    final String value = this.properties.get( DEVICE_OPEN_PORT_DELAY );
    return Integer.parseInt( value );
  }

  /**
   * Returns all supported sample rates.
   * 
   * @return an array of sample rates, in Hertz, never <code>null</code>.
   */
  public Integer[] getSampleRates()
  {
    final String rawValue = this.properties.get( DEVICE_SAMPLERATES );
    final String[] values = rawValue.split( ",\\s*" );
    final List<Integer> result = new ArrayList<Integer>();
    for ( String value : values )
    {
      result.add( Integer.valueOf( value.trim() ) );
    }
    Collections.sort( result, new NumericComparator() );

    return result.toArray( new Integer[result.size()] );
  }

  /**
   * Returns the total number of trigger stages (in the complex trigger mode).
   * 
   * @return a trigger stage count, greater than 0.
   */
  public int getTriggerStages()
  {
    final String value = this.properties.get( DEVICE_TRIGGER_STAGES );
    return Integer.parseInt( value );
  }

  /**
   * Returns the device type this profile denotes.
   * 
   * @return a device type name, never <code>null</code>.
   */
  public String getType()
  {
    final String result = this.properties.get( DEVICE_TYPE );
    return result == null ? "<unknown>" : result;
  }

  /**
   * Returns whether or not the capture size is bound to the number of channels.
   * 
   * @return <code>true</code> if the capture size is bound to the number of
   *         channels, <code>false</code> otherwise.
   */
  public boolean isCaptureSizeBoundToEnabledChannels()
  {
    final String value = this.properties.get( DEVICE_CAPTURESIZE_BOUND );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether or not the device supports "complex" triggers.
   * 
   * @return <code>true</code> if complex triggers are supported by the device,
   *         <code>false</code> otherwise.
   */
  public boolean isComplexTriggersSupported()
  {
    final String value = this.properties.get( DEVICE_TRIGGER_COMPLEX );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether or not the device supports "double-data rate" sampling,
   * also known as "demux"-sampling.
   * 
   * @return <code>true</code> if DDR is supported by the device,
   *         <code>false</code> otherwise.
   */
  public boolean isDoubleDataRateSupported()
  {
    final String value = this.properties.get( DEVICE_SUPPORTS_DDR );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether or not the device supports a noise filter.
   * 
   * @return <code>true</code> if a noise filter is present in the device,
   *         <code>false</code> otherwise.
   */
  public boolean isNoiseFilterSupported()
  {
    final String value = this.properties.get( DEVICE_FEATURE_NOISEFILTER );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether or not the device supports RLE (Run-Length Encoding).
   * 
   * @return <code>true</code> if a RLE encoder is present in the device,
   *         <code>false</code> otherwise.
   */
  public boolean isRleSupported()
  {
    final String value = this.properties.get( DEVICE_FEATURE_RLE );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether the device send its samples in "reverse" order.
   * 
   * @return <code>true</code> if samples are send in reverse order (= last
   *         sample first), <code>false</code> otherwise.
   */
  public boolean isSamplesInReverseOrder()
  {
    final String rawValue = this.properties.get( DEVICE_SAMPLE_REVERSE_ORDER );
    return Boolean.parseBoolean( rawValue );
  }

  /**
   * Returns whether or not the device supports a testing mode.
   * 
   * @return <code>true</code> if testing mode is supported by the device,
   *         <code>false</code> otherwise.
   */
  public boolean isTestModeSupported()
  {
    final String value = this.properties.get( DEVICE_FEATURE_TEST_MODE );
    return Boolean.parseBoolean( value );
  }

  /**
   * Returns whether or not the device supports triggers.
   * 
   * @return <code>true</code> if the device supports triggers,
   *         <code>false</code> otherwise.
   */
  public boolean isTriggerSupported()
  {
    final String value = this.properties.get( DEVICE_FEATURE_RLE );
    return Boolean.parseBoolean( value );
  }

  /**
   * @param aProperties
   *          the updated properties.
   */
  @SuppressWarnings( "rawtypes" )
  final void setProperties( final Dictionary aProperties )
  {
    final Map<String, String> newProps = new HashMap<String, String>();

    Enumeration keys = aProperties.keys();
    while ( keys.hasMoreElements() )
    {
      final String key = ( String )keys.nextElement();
      if ( !KNOWN_KEYS.contains( key ) && !IGNORED_KEYS.contains( key ) )
      {
        LOG.log( Level.WARNING, "Unknown/unsupported profile key: " + key );
        continue;
      }

      final String value = ( String )aProperties.get( key );
      newProps.put( key, value );
    }

    // Verify whether all known keys are defined...
    final List<String> checkedKeys = new ArrayList<String>( KNOWN_KEYS );
    checkedKeys.removeAll( newProps.keySet() );
    if ( !checkedKeys.isEmpty() )
    {
      throw new IllegalArgumentException( "Profile settings not complete! Missing keys are: " + checkedKeys.toString() );
    }

    synchronized ( this.properties )
    {
      this.properties.clear();
      this.properties.putAll( newProps );

      LOG.log( Level.INFO, "New device profile settings applied for {1} ({0}) ...", //
          new Object[] { getType(), getDescription() } );
    }
  }
}
