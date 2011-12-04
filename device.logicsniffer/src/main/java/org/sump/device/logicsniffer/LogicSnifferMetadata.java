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
package org.sump.device.logicsniffer;


import java.util.*;

import nl.lxtreme.ols.api.devices.*;

import org.sump.device.logicsniffer.profile.*;


/**
 * Provides an implementation of {@link DeviceMetadata}.
 */
public final class LogicSnifferMetadata implements DeviceMetadata
{
  // VARIABLES

  private final Map<Integer, Object> values;
  private DeviceProfile deviceProfile;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferMetadata instance.
   */
  public LogicSnifferMetadata()
  {
    super();

    this.values = new HashMap<Integer, Object>();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getAncillaryVersion()
   */
  @Override
  public String getAncillaryVersion()
  {
    return ( String )getValue( KEY_ANCILLARY_VERSION );
  }

  /**
   * Returns the current value of deviceProfile.
   * 
   * @return the deviceProfile
   */
  public DeviceProfile getDeviceProfile()
  {
    return this.deviceProfile;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getDynamicMemoryDepth()
   */
  @Override
  public Integer getDynamicMemoryDepth()
  {
    return ( Integer )getValue( KEY_DYNAMIC_MEMORY_DEPTH );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getFpgaVersion()
   */
  @Override
  public String getFpgaVersion()
  {
    return ( String )getValue( KEY_FPGA_VERSION );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getMaxSampleRate()
   */
  @Override
  public Integer getMaxSampleRate()
  {
    return ( Integer )getValue( KEY_MAX_SAMPLE_RATE );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getName()
   */
  @Override
  public String getName()
  {
    return ( String )getValue( KEY_DEVICE_NAME );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getProbeCount()
   */
  @Override
  public Integer getProbeCount()
  {
    Object result = getValue( KEY_PROBE_COUNT_LONG );
    if ( result == null )
    {
      result = getValue( KEY_PROBE_COUNT_SHORT );
    }
    return ( Integer )result;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getProbeCount()
   */
  public int getProbeCount( final int aDefaultProbeCount )
  {
    Integer result = getProbeCount();
    if ( result == null )
    {
      return aDefaultProbeCount;
    }
    return result.intValue();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getProtocolVersion()
   */
  @Override
  public Integer getProtocolVersion()
  {
    Object result = getValue( KEY_PROTOCOL_VERSION_LONG );
    if ( result == null )
    {
      result = getValue( KEY_PROTOCOL_VERSION_SHORT );
    }
    return ( Integer )result;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getSampleMemoryDepth()
   */
  @Override
  public Integer getSampleMemoryDepth()
  {
    return ( Integer )getValue( KEY_SAMPLE_MEMORY_DEPTH );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceMetadata#getSampleMemoryDepth()
   */
  public int getSampleMemoryDepth( final int aDefaultSize )
  {
    Integer result = getSampleMemoryDepth();
    if ( result == null )
    {
      return aDefaultSize;
    }
    return result.intValue();
  }

  /**
   * @see java.lang.Iterable#iterator()
   */
  @Override
  public Iterator<Object> iterator()
  {
    return this.values.values().iterator();
  }

  /**
   * Puts a key/value metadata pair.
   * 
   * @param aKey
   *          the (raw) key value as returned by the OLS;
   * @param aValue
   *          the (String, Byte or Integer) value that should be stored.
   * @throws IllegalStateException
   *           in case the given key was already registered before.
   */
  public void put( final int aKey, final Object aValue ) throws IllegalStateException
  {
    final Integer key = Integer.valueOf( aKey );
    if ( this.values.containsKey( key ) )
    {
      throw new IllegalStateException( "Duplicate key: 0x" + Integer.toHexString( aKey ) + "!" );
    }
    this.values.put( key, aValue );
  }

  /**
   * Sets deviceProfile to the given value.
   * 
   * @param aDeviceProfile
   *          the deviceProfile to set.
   */
  public void setDeviceProfile( final DeviceProfile aDeviceProfile )
  {
    this.deviceProfile = aDeviceProfile;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    final StringBuilder sb = new StringBuilder();
    sb.append( "Device name              => " ).append( getValue( KEY_DEVICE_NAME ) ).append( '\n' );
    sb.append( "FPGA version             => " ).append( getValue( KEY_FPGA_VERSION ) ).append( '\n' );
    sb.append( "Ancillary version        => " ).append( getValue( KEY_ANCILLARY_VERSION ) ).append( '\n' );
    sb.append( "Protocol version (long)  => " ).append( getValue( KEY_PROTOCOL_VERSION_LONG ) ).append( '\n' );
    sb.append( "Protocol version (short) => " ).append( getValue( KEY_PROTOCOL_VERSION_SHORT ) ).append( '\n' );
    sb.append( "Probe count (long)       => " ).append( getValue( KEY_PROBE_COUNT_LONG ) ).append( '\n' );
    sb.append( "Probe count (short)      => " ).append( getValue( KEY_PROBE_COUNT_SHORT ) ).append( '\n' );
    sb.append( "Sample memory depth (b)  => " ).append( getValue( KEY_SAMPLE_MEMORY_DEPTH ) ).append( '\n' );
    sb.append( "Dynamic memory depth (b) => " ).append( getValue( KEY_DYNAMIC_MEMORY_DEPTH ) ).append( '\n' );
    sb.append( "Max. sample rate (Hz)    => " ).append( getValue( KEY_MAX_SAMPLE_RATE ) ).append( '\n' );
    return sb.toString();
  }

  /**
   * Returns the value for the given key.
   * 
   * @param aKey
   *          the key whose value to return.
   * @return the value corresponding to the given key, can be <code>null</code>.
   */
  private Object getValue( final int aKey )
  {
    return this.values.get( Integer.valueOf( aKey ) );
  }
}
