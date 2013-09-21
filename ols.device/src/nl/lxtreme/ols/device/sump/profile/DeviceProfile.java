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
package nl.lxtreme.ols.device.sump.profile;


import static nl.lxtreme.ols.device.sump.profile.Constants.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.util.*;


/**
 * Provides a device profile.
 */
public final class DeviceProfile implements Cloneable, Comparable<DeviceProfile>
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
    SERIAL, NETWORK, USB;
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

  // CONSTANTS

  /** Service PID of this device profile. */
  private static final String FELIX_SERVICE_PID = "service.pid";
  /** Factory Service PID of this device profile. */
  private static final String FELIX_SERVICE_FACTORY_PID = "service.factoryPid";

  /** All the profile keys that are supported. */
  private static final List<String> KNOWN_KEYS = Arrays.asList( new String[] { DEVICE_TYPE, DEVICE_DESCRIPTION,
      DEVICE_INTERFACE, DEVICE_CLOCKSPEED, DEVICE_SUPPORTS_DDR, DEVICE_SAMPLERATES, DEVICE_CAPTURECLOCK,
      DEVICE_CAPTURESIZES, DEVICE_FEATURE_NOISEFILTER, DEVICE_FEATURE_RLE, DEVICE_FEATURE_TEST_MODE,
      DEVICE_FEATURE_TRIGGERS, DEVICE_TRIGGER_STAGES, DEVICE_TRIGGER_COMPLEX, DEVICE_CHANNEL_COUNT,
      DEVICE_CHANNEL_GROUPS, DEVICE_CAPTURESIZE_BOUND, DEVICE_CHANNEL_NUMBERING_SCHEMES, DEVICE_OPEN_PORT_DELAY,
      DEVICE_METADATA_KEYS, DEVICE_SAMPLE_REVERSE_ORDER, DEVICE_OPEN_PORT_DTR, DEVICE_RECEIVE_TIMEOUT,
      DEVICE_DIVIDER_CLOCKSPEED } );
  private static final List<String> IGNORED_KEYS = Arrays.asList( new String[] { FELIX_SERVICE_PID,
      FELIX_SERVICE_FACTORY_PID } );

  private static final Logger LOG = Logger.getLogger( DeviceProfile.class.getName() );

  // VARIABLES

  private final ConcurrentMap<String, String> properties;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfile.
   */
  public DeviceProfile()
  {
    this.properties = new ConcurrentHashMap<String, String>();
  }

  // METHODS

  /**
   * @param aFilename
   * @return
   */
  static final File createFile( final String aFilename )
  {
    if ( aFilename == null )
    {
      throw new IllegalArgumentException( "Filename cannot be null!" );
    }
    return new File( aFilename.replaceAll( "^file:", "" ) );
  }

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
   * {@inheritDoc}
   */
  @Override
  public int compareTo( DeviceProfile aProfile )
  {
    // Issue #123: allow device profiles to be sorted alphabetically...
    int result = getDescription().compareTo( aProfile.getDescription() );
    if ( result == 0 )
    {
      result = getType().compareTo( aProfile.getType() );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof DeviceProfile ) )
    {
      return false;
    }

    final DeviceProfile other = ( DeviceProfile )aObject;
    return this.properties.equals( other.properties );
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
    Collections.sort( result, NumberUtils.<Integer> createNumberComparator( false /* aSortAscending */) );
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
   * Returns the clockspeed used in the divider calculation.
   * 
   * @return a clockspeed, in Hertz (Hz), defaults to 100MHz.
   */
  public int getDividerClockspeed()
  {
    final String value = this.properties.get( DEVICE_DIVIDER_CLOCKSPEED );
    return Integer.parseInt( value );
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
   * Returns the maximum capture size for the given number of <em>enabled</em>
   * channel groups.
   * <p>
   * If the maximum capture size is bound to the number of enabled
   * channel(group)s, this method will divide the maximum possible capture size
   * by the given group count, otherwise the maximum capture size will be
   * returned.
   * </p>
   * 
   * @param aChannelGroups
   *          the number of channel groups that should be enabled, > 0 && <
   *          channel group count.
   * @return a maximum capture size, in bytes, or -1 if no maximum could be
   *         determined, or the given parameter was <tt>0</tt>.
   * @see #isCaptureSizeBoundToEnabledChannels()
   * @see #getChannelGroupCount()
   */
  public int getMaximumCaptureSizeFor( final int aChannelGroups )
  {
    final Integer[] sizes = getCaptureSizes();
    if ( ( sizes == null ) || ( sizes.length == 0 ) || ( aChannelGroups == 0 ) )
    {
      return -1;
    }

    final int maxSize = sizes[0].intValue();
    if ( isCaptureSizeBoundToEnabledChannels() )
    {
      int indication = maxSize / aChannelGroups;

      // Issue #58: Search the best matching value...
      Integer result = null;
      for ( int i = sizes.length - 1; i >= 0; i-- )
      {
        if ( sizes[i].compareTo( Integer.valueOf( indication ) ) <= 0 )
        {
          result = sizes[i];
        }
      }

      return ( result == null ) ? indication : result.intValue();
    }

    return maxSize;
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
   * Returns the (optional) receive timeout.
   * <p>
   * WARNING: if no receive timeout is used, the communication essentially
   * results in a non-blocking I/O operation which can not be cancelled!
   * </p>
   * 
   * @return the receive timeout, in ms, or <code>null</code> when no receive
   *         timeout should be used.
   */
  public Integer getReceiveTimeout()
  {
    final String value = this.properties.get( DEVICE_RECEIVE_TIMEOUT );
    if ( value == null )
    {
      return null;
    }
    int timeout = Integer.parseInt( value );
    return ( timeout <= 0 ) ? null : Integer.valueOf( timeout );
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
    final SortedSet<Integer> result = new TreeSet<Integer>(
        NumberUtils.<Integer> createNumberComparator( false /* aSortAscending */) );
    for ( String value : values )
    {
      result.add( Integer.valueOf( value.trim() ) );
    }

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
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.properties == null ) ? 0 : this.properties.hashCode() );
    return result;
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
   * Returns whether upon opening the DTR line needs to be high (=
   * <code>true</code>) or low (= <code>false</code>).
   * <p>
   * This method has no meaning if the used interface is <em>not</em>
   * {@link DeviceInterface#SERIAL}.
   * </p>
   * 
   * @return <code>true</code> if the DTR line needs to be set upon opening the
   *         serial port, <code>false</code> if the DTR line needs to be reset
   *         upon opening the serial port.
   */
  public boolean isOpenPortDtr()
  {
    final String value = this.properties.get( DEVICE_OPEN_PORT_DTR );
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
    final String value = this.properties.get( DEVICE_FEATURE_TRIGGERS );
    return Boolean.parseBoolean( value );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return getType();
  }

  /**
   * @return the properties of this device profile, never <code>null</code>.
   */
  final Dictionary<String, String> getProperties()
  {
    return new Hashtable<String, String>( this.properties );
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

      final String value = aProperties.get( key ).toString();
      newProps.put( key, value.trim() );
    }

    // Verify whether all known keys are defined...
    final List<String> checkedKeys = new ArrayList<String>( KNOWN_KEYS );
    checkedKeys.removeAll( newProps.keySet() );
    if ( !checkedKeys.isEmpty() )
    {
      throw new IllegalArgumentException( "Profile settings not complete! Missing keys are: " + checkedKeys.toString() );
    }

    this.properties.putAll( newProps );

    LOG.log( Level.INFO, "New device profile settings applied for {1} ({0}) ...", //
        new Object[] { getType(), getDescription() } );
  }
}
