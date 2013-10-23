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
package nl.lxtreme.ols.device.sump;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;

import java.util.*;


/**
 * Provides some (optional) metadata of a device, as might be used by devices
 * made by Dangerous Prototypes and others.
 * <p>
 * This interface provides direct access to the individual metadata values. In
 * addition, it can be iterated to access all "known" metadata values.
 * </p>
 */
public class DeviceMetadata
{
  // VARIABLES

  private final Map<Integer, Object> metadata = new HashMap<Integer, Object>();

  // METHODS

  /**
   * @return this metadata as {@link Dictionary}, never <code>null</code>.
   */
  public Dictionary<String, Object> asDictionary()
  {
    Dictionary<String, Object> result = new Hashtable<String, Object>();
    putOptional( result, LOOKUP_ANCILLARY_VERSION, getAncillaryVersion() );
    putOptional( result, LOOKUP_DEVICE_NAME, getName() );
    putOptional( result, LOOKUP_DYNAMIC_MEMORY_DEPTH, getDynamicMemoryDepth() );
    putOptional( result, LOOKUP_FPGA_VERSION, getFpgaVersion() );
    putOptional( result, LOOKUP_MAX_SAMPLE_RATE, getMaxSampleRate() );
    putOptional( result, LOOKUP_PROBE_COUNT, getProbeCount() );
    putOptional( result, LOOKUP_PROTOCOL_VERSION, getProtocolVersion() );
    putOptional( result, LOOKUP_SAMPLE_MEMORY_DEPTH, getSampleMemoryDepth() );
    return result;
  }

  /**
   * Returns the ancillary (e.g. PIC controller) version.
   * 
   * @return a version string, like "1.23", can be <code>null</code> in case no
   *         such version was available/known.
   */
  public String getAncillaryVersion()
  {
    return ( String )get( KEY_ANCILLARY_VERSION );
  }

  /**
   * Returns the maximum depth of the dynamic memory part.
   * 
   * @return a memory depth, in bytes, can be <code>null</code> in case this
   *         depth is unknown.
   */
  public Integer getDynamicMemoryDepth()
  {
    return ( Integer )get( KEY_DYNAMIC_MEMORY_DEPTH );
  }

  /**
   * Returns the FPGA version.
   * 
   * @return a version string, like "1.12", can be <code>null</code> in case no
   *         such version was available/known.
   */
  public String getFpgaVersion()
  {
    return ( String )get( KEY_FPGA_VERSION );
  }

  /**
   * Returns the maximum sample rate.
   * 
   * @return a sample rate, in Hz, can be <code>null</code> in case the maximum
   *         sample rate is unknown.
   */
  public Integer getMaxSampleRate()
  {
    return ( Integer )get( KEY_MAX_SAMPLE_RATE );
  }

  /**
   * Returns the device name.
   * 
   * @return a device name, can be <code>null</code> in case no device name is
   *         available/known.
   */
  public String getName()
  {
    return ( String )get( KEY_DEVICE_NAME );
  }

  /**
   * Returns the maximum number of probes supported by the device.
   * 
   * @return a probe count, >= 0 && < 256, can be <code>null</code> in case no
   *         probe count is available/known.
   */
  public Integer getProbeCount()
  {
    Integer result = ( Integer )get( KEY_PROBE_COUNT_SHORT );
    if ( result == null )
    {
      result = ( Integer )get( KEY_PROBE_COUNT_LONG );
    }
    return result;
  }

  /**
   * Returns the device protocol version.
   * 
   * @return a protocol version, or <code>null</code> in case no protocol
   *         version is available/known.
   */
  public Integer getProtocolVersion()
  {
    Integer result = ( Integer )get( KEY_PROTOCOL_VERSION_SHORT );
    if ( result == null )
    {
      result = ( Integer )get( KEY_PROTOCOL_VERSION_LONG );
    }
    return result;
  }

  /**
   * Returns the maximum sample memory.
   * 
   * @return the sample memory depth, in bytes, can be <code>null</code> in case
   *         no sample memory depth is available/known.
   */
  public Integer getSampleMemoryDepth()
  {
    return ( Integer )get( KEY_SAMPLE_MEMORY_DEPTH );
  }

  /**
   * Convenience method to put a numeric value.
   * 
   * @param aKey
   * @param aValue
   * @return
   */
  public Object put( int aKey, Object aValue )
  {
    return this.metadata.put( Integer.valueOf( aKey ), aValue );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    final StringBuilder sb = new StringBuilder();
    sb.append( "Device name              => " ).append( get( KEY_DEVICE_NAME ) ).append( '\n' );
    sb.append( "FPGA version             => " ).append( get( KEY_FPGA_VERSION ) ).append( '\n' );
    sb.append( "Ancillary version        => " ).append( get( KEY_ANCILLARY_VERSION ) ).append( '\n' );
    sb.append( "Protocol version (long)  => " ).append( get( KEY_PROTOCOL_VERSION_LONG ) ).append( '\n' );
    sb.append( "Protocol version (short) => " ).append( get( KEY_PROTOCOL_VERSION_SHORT ) ).append( '\n' );
    sb.append( "Probe count (long)       => " ).append( get( KEY_PROBE_COUNT_LONG ) ).append( '\n' );
    sb.append( "Probe count (short)      => " ).append( get( KEY_PROBE_COUNT_SHORT ) ).append( '\n' );
    sb.append( "Sample memory depth (b)  => " ).append( get( KEY_SAMPLE_MEMORY_DEPTH ) ).append( '\n' );
    sb.append( "Dynamic memory depth (b) => " ).append( get( KEY_DYNAMIC_MEMORY_DEPTH ) ).append( '\n' );
    sb.append( "Max. sample rate (Hz)    => " ).append( get( KEY_MAX_SAMPLE_RATE ) ).append( '\n' );
    return sb.toString();
  }

  /**
   * Convenience method to get a value by its numeric value.
   * 
   * @param aKey
   * @return
   */
  private Object get( int aKey )
  {
    return this.metadata.get( Integer.valueOf( aKey ) );
  }

  private void putOptional( Dictionary<String, Object> aDictionary, String aKey, Object aValue )
  {
    if ( aValue != null )
    {
      aDictionary.put( aKey, aValue );
    }
  }
}
