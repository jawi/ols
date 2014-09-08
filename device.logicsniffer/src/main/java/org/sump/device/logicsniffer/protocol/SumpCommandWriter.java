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
package org.sump.device.logicsniffer.protocol;


import java.io.*;
import java.util.logging.*;

import nl.lxtreme.ols.util.*;

import org.sump.device.logicsniffer.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.*;


/**
 * Wrapper to write SUMP-specific commands to a normal {@link DataOutputStream}.
 */
public class SumpCommandWriter implements SumpProtocolConstants, Closeable
{
  // CONSTANTS

  /** set trigger mask */
  private static final int SETTRIGMASK = 0xc0;
  /** set trigger value */
  private static final int SETTRIGVAL = 0xc1;
  /** set trigger configuration */
  private static final int SETTRIGCFG = 0xc2;
  /** set clock divider */
  private static final int SETDIVIDER = 0x80;
  /** set sample counters */
  private static final int SETSIZE = 0x81;
  /** set flags */
  private static final int SETFLAGS = 0x82;

  /** demultiplex */
  public static final int FLAG_DEMUX = 0x00000001;
  /** noise filter */
  public static final int FLAG_FILTER = 0x00000002;
  /** channel group 1 enabled? */
  public static final int FLAG_GROUP1_DISABLED = 0x00000004;
  /** channel group 2 enabled? */
  public static final int FLAG_GROUP2_DISABLED = 0x00000008;
  /** channel group 3 enabled? */
  public static final int FLAG_GROUP3_DISABLED = 0x00000010;
  /** channel group 4 enabled? */
  public static final int FLAG_GROUP4_DISABLED = 0x00000020;
  /** external trigger? */
  public static final int FLAG_EXTERNAL = 0x00000040;
  /** inverted */
  public static final int FLAG_INVERTED = 0x00000080;
  /** run length encoding */
  public static final int FLAG_RLE = 0x00000100;
  public static final int FLAG_RLE_MODE_0 = 0x00000000;
  public static final int FLAG_RLE_MODE_1 = 0x00004000;
  public static final int FLAG_RLE_MODE_2 = 0x00008000;
  public static final int FLAG_RLE_MODE_3 = 0x0000C000;
  /** Number Scheme */
  public static final int FLAG_NUMBER_SCHEME = 0x00000200;
  /** Testing mode (external, bits 16:32) */
  public static final int FLAG_EXTERNAL_TEST_MODE = 0x00000400;
  /** Testing mode (internal) */
  public static final int FLAG_INTERNAL_TEST_MODE = 0x00000800;

  private static final Logger LOG = Logger.getLogger( SumpCommandWriter.class.getName() );

  // VARIABLES

  protected final LogicSnifferConfig config;
  private final DataOutputStream outputStream;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpCommandWriter} instance.
   * 
   * @param aConfiguration
   *          the configuration to use, cannot be <code>null</code>;
   * @param aOutputStream
   *          the {@link DataOutputStream} to wrap, cannot be <code>null</code>.
   */
  public SumpCommandWriter( final LogicSnifferConfig aConfiguration, final DataOutputStream aOutputStream )
  {
    this.config = aConfiguration;
    this.outputStream = aOutputStream;
  }

  // METHODS

  /**
   * Determines which RLE-mode to use. There are up to four different RLE-modes
   * present in 'dogsbody' Verilog firmware.
   * <p>
   * The RLE-Encoding modes are:
   * </p>
   * <ol start="0">
   * <li>Issue {values} & {RLE-count} as pairs. Count inclusive of value
   * (<strike>backwards compatible</strike>);</li>
   * <li>Issue {values} & {RLE-count} as pairs. Count is <em>exclusive</em> of
   * value. Compatible with all clients;</li>
   * <li>Periodic. {values} reissued approximately every 256 {RLE-count} fields;
   * </li>
   * <li>Unlimited. {values} can be followed by unlimited numbers of
   * {RLE-counts}.</li>
   * </ol>
   * 
   * @return a RLE-mode, defaults to 1.
   */
  private static int determineRleMode()
  {
    return NumberUtils.smartParseInt( System.getProperty( "nl.lxtreme.ols.rle.mode" ), 1 );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    this.outputStream.close();
  }

  /**
   * @param aConfiguration
   * @throws IOException
   */
  public void writeCmdFinishNow() throws IOException
  {
    final boolean isRleEnabled = this.config.isRleEnabled();
    if ( isRleEnabled )
    {
      LOG.info( "Prematurely finishing RLE-enabled capture ..." );
      sendCommand( CMD_RLE_FINISH_NOW );
    }
    else
    {
      LOG.info( "Prematurely finishing normal capture ..." );
      writeCmdReset();
    }
  }

  /**
   * @throws IOException
   */
  public void writeCmdGetId() throws IOException
  {
    sendCommand( CMD_ID );
  }

  /**
   * Writes the 'get metadata' command to the device.
   */
  public void writeCmdGetMetadata() throws IOException
  {
    sendCommand( CMD_METADATA );
  }

  /**
   * Resets the OLS device by sending 5 consecutive 'reset' commands.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  public void writeCmdReset() throws IOException
  {
    for ( int i = 0; i < 5; i++ )
    {
      sendCommand( CMD_RESET );
    }
  }

  /**
   * @throws IOException
   */
  public void writeCmdRun() throws IOException
  {
    sendCommand( CMD_RUN );
  }

  /**
   * @param aConfiguration
   * @throws IOException
   */
  public int writeDeviceConfiguration() throws IOException
  {
    int trigcount;

    // set the sampling frequency...
    sendCommand( SETDIVIDER, this.config.getDivider() );

    final int stopCounter = configureTriggers();
    final int readCounter = this.config.getReadCounter();

    final int size;
    if ( this.config.isDoubleDataRateEnabled() )
    {
      // 0x7fff8 = 511Kb = the maximum size supported by the original SUMP
      // device when using the demultiplexer...
      final int maxSize = 0x7fff8;
      size = ( ( stopCounter & maxSize ) << 13 ) | ( ( ( readCounter & maxSize ) >> 3 ) - 1 );
      // A better approximation of "(readCounter - stopCounter) - 2". This also
      // solves issue #31...
      trigcount = ( ( ( size & 0xffff ) << 3 ) - ( ( ( size >> 16 ) & 0xffff ) << 3 ) );
    }
    else
    {
      // 0x3fffc = 255Kb = the maximum size supported by the original SUMP
      // device...
      final int maxSize = 0x3fffc;
      size = ( ( stopCounter & maxSize ) << 14 ) | ( ( ( readCounter & maxSize ) >> 2 ) - 1 );
      // A better approximation of "(readCounter - stopCounter) - 2". This also
      // solves issue #31...
      trigcount = ( ( ( size & 0xffff ) << 2 ) - ( ( ( size >> 16 ) & 0xffff ) << 2 ) );
    }

    // set the capture size...
    sendCommand( SETSIZE, size );

    int flags = 0;
    if ( this.config.isExternalClock() )
    {
      flags |= FLAG_EXTERNAL;
      if ( CaptureClockSource.EXTERNAL_FALLING == this.config.getClockSource() )
      {
        flags |= FLAG_INVERTED;
      }
    }

    // determine which channel groups are to be disabled...
    int enabledChannelGroups = 0;
    for ( int i = 0; i < this.config.getGroupCount(); i++ )
    {
      if ( this.config.isGroupEnabled( i ) )
      {
        enabledChannelGroups |= ( 1 << i );
      }
    }

    if ( this.config.isDoubleDataRateEnabled() )
    {
      // when DDR is selected, the groups selected in the upper two channel
      // groups must be the same as those selected in the lower two groups
      enabledChannelGroups |= ( ( enabledChannelGroups & 0x03 ) << 2 ) & 0x0c;

      flags |= FLAG_DEMUX;
      // if the demux bit is set, the filter flag *must* be cleared...
      flags &= ~FLAG_FILTER;
    }

    flags |= ~( enabledChannelGroups << 2 ) & 0x3c;

    if ( this.config.isFilterEnabled() && this.config.isFilterAvailable() )
    {
      flags |= FLAG_FILTER;
      // if the filter bit is set, the demux flag *must* be cleared...
      flags &= ~FLAG_DEMUX;
    }

    if ( this.config.isRleEnabled() )
    {
      flags |= FLAG_RLE;

      // Ian 'dogsbody''s Verilog understands four different RLE-modes...
      final int rleMode = determineRleMode();
      switch ( rleMode )
      {
        case 3:
          flags |= FLAG_RLE_MODE_3;
          break;
        case 2:
          flags |= FLAG_RLE_MODE_2;
          break;
        case 0:
          flags |= FLAG_RLE_MODE_0;
          break;
        default:
          flags |= FLAG_RLE_MODE_1;
          break;
      }
    }

    if ( this.config.isAltNumberSchemeEnabled() )
    {
      flags |= FLAG_NUMBER_SCHEME;
    }

    if ( this.config.isTestModeEnabled() )
    {
      flags |= FLAG_EXTERNAL_TEST_MODE;
    }

    LOG.log( Level.FINE, "Flags: 0b{0}", Integer.toBinaryString( flags ) );

    // finally set the device flags...
    sendCommand( SETFLAGS, flags );

    return trigcount;
  }

  /**
   * Sends a short command to the given stream. This method is intended to be
   * used for short commands, but can also be called with long command opcodes
   * if the data portion is to be set to 0.
   * 
   * @param aOpcode
   *          one byte operation code
   * @throws IOException
   *           if writing to stream fails
   */
  protected final void sendCommand( final int aOpcode ) throws IOException
  {
    if ( LOG.isLoggable( Level.ALL ) || LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, String.format( "Sending short command: 0x%02x", Integer.valueOf( aOpcode & 0xFF ) ) );
    }

    this.outputStream.writeByte( aOpcode );
    this.outputStream.flush();
  }

  /**
   * Sends a long command to the given stream.
   * 
   * @param aOpcode
   *          one byte operation code
   * @param aData
   *          four byte data portion
   * @throws IOException
   *           if writing to stream fails
   */
  protected final void sendCommand( final int aOpcode, final int aData ) throws IOException
  {
    final byte[] raw = new byte[5];
    int mask = 0xff;
    int shift = 0;

    raw[0] = ( byte )aOpcode;
    for ( int i = 1; i < 5; i++ )
    {
      raw[i] = ( byte )( ( aData & mask ) >> shift );
      mask = mask << 8;
      shift += 8;
    }

    if ( LOG.isLoggable( Level.ALL ) || LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, String.format( "Sending long command: 0x%02x with data 0x%08x", //
          Integer.valueOf( aOpcode & 0xFF ), Integer.valueOf( aData ) ) );
    }

    this.outputStream.write( raw );
    this.outputStream.flush();
  }

  /**
   * Sends the trigger mask, value and configuration to the OLS device.
   * 
   * @return the stop counter that is used for the trigger configuration.
   * @throws IOException
   *           in case of I/O problems.
   */
  private int configureTriggers() throws IOException
  {
    final int effectiveStopCounter;
    if ( this.config.isTriggerEnabled() )
    {
      for ( int i = 0; i < this.config.getMaxTriggerStages(); i++ )
      {
        final int indexMask = 4 * i;
        sendCommand( SETTRIGMASK | indexMask, this.config.getTriggerMask( i ) );
        sendCommand( SETTRIGVAL | indexMask, this.config.getTriggerValue( i ) );
        sendCommand( SETTRIGCFG | indexMask, this.config.getTriggerConfig( i ) );
      }
      effectiveStopCounter = this.config.getStopCounter();
    }
    else
    {
      sendCommand( SETTRIGMASK, 0 );
      sendCommand( SETTRIGVAL, 0 );
      sendCommand( SETTRIGCFG, LogicSnifferConfig.TRIGGER_CAPTURE );
      effectiveStopCounter = this.config.getReadCounter();
    }

    return effectiveStopCounter;
  }
}
