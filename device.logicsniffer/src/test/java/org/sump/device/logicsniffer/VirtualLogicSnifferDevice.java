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
package org.sump.device.logicsniffer;


import static org.junit.Assert.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.util.*;

import org.sump.device.logicsniffer.profile.*;


/**
 * Provides a "virtual" LogicSniffer device.
 */
public class VirtualLogicSnifferDevice extends LogicSnifferDevice
{
  // INNER TYPES

  /**
   * IOHelper provides an asynchronous reader for streams...
   */
  final class IOHelper extends Thread
  {
    // VARIABLES

    private final InputStream is;
    private final OutputStream os;
    private volatile boolean running;

    private int readCount;
    private int delayCount;
    private int sampleWidth;

    // CONSTRUCTORS

    /**
     * Creates a new StreamReader instance.
     */
    public IOHelper( final InputStream aIS, final OutputStream aOS )
    {
      super( "IOHelper" );
      this.is = aIS;
      this.os = aOS;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void run()
    {
      this.running = true;

      byte[] parameters = new byte[4];
      int command;
      int parameterValue;

      do
      {
        try
        {
          parameterValue = 0;

          if ( this.is.available() < 1 )
          {
            continue;
          }

          // First byte denotes the command...
          command = this.is.read();

          // Short or long command?
          if ( ( command & 0x80 ) != 0 )
          {
            this.is.read( parameters );
            // Construct a 32-bit integer back from the parameters
            parameterValue = ( ( ( parameters[3] & 0xFF ) << 24 ) | ( ( parameters[2] & 0xFF ) << 16 )
                | ( ( parameters[1] & 0xFF ) << 8 ) | ( parameters[0] & 0xFF ) );
          }

          switch ( command & 0xFF )
          {
            case 0x00:
              // Ignore; got a reset...
              break;
            case 0x01:
              // Run/arm the trigger...
              respondWithSampleData();
              break;
            case 0x02:
              // Ask for device ID...
              respondWithDeviceID();
              break;
            case 0x03:
              // Selftest...
              respondWithSelftestResult();
              break;
            case 0x04:
              // Send device metadata...
              respondWithDeviceMetadata();
              break;
            case 0x05:
              // Finish now... XXX
              LOG.info( "Received RLE FINISH NOW request..." );
              break;
            case 0xC0:
            case 0xC4:
            case 0xC8:
            case 0xCC:
              // Set Trigger Masks, trigger 0, 1, 2 & 3...
              LOG.info( "Received SET TRIGGER MASK..." );
              break;
            case 0xC1:
            case 0xC5:
            case 0xC9:
            case 0xCD:
              // Set Trigger Values, trigger 0, 1, 2 & 3...
              LOG.info( "Received SET TRIGGER VALUE..." );
              break;
            case 0xC2:
            case 0xC6:
            case 0xCA:
            case 0xCE:
              // Set Trigger Configuration, trigger 0, 1, 2 & 3...
              LOG.info( "Received SET TRIGGER CONFIGURATION..." );
              break;
            case 0x80:
              // Set divider...
              setDivider( parameterValue );
              break;
            case 0x81:
              // Set Read & Delay Count...
              this.readCount = ( ( parameterValue & 0xFFFF ) + 1 ) << 2;
              this.delayCount = ( ( parameterValue >> 16 ) & 0xFFFF ) << 2;
              setReadAndDelay( this.readCount, this.delayCount );
              break;
            case 0x82:
              // Set Flags...
              this.sampleWidth = 4;
              if ( ( parameterValue & LogicSnifferDevice.FLAG_GROUP1_DISABLED ) != 0 )
              {
                this.sampleWidth--;
              }
              if ( ( parameterValue & LogicSnifferDevice.FLAG_GROUP2_DISABLED ) != 0 )
              {
                this.sampleWidth--;
              }
              if ( ( parameterValue & LogicSnifferDevice.FLAG_GROUP3_DISABLED ) != 0 )
              {
                this.sampleWidth--;
              }
              if ( ( parameterValue & LogicSnifferDevice.FLAG_GROUP4_DISABLED ) != 0 )
              {
                this.sampleWidth--;
              }
              setFlags( parameterValue );
              break;
            default:
              LOG.log( Level.INFO, "Unknown/unimplemented command: 0x{0}!", Integer.toHexString( command ) );
              break;
          }
        }
        catch ( IOException exception )
        {
          this.running = false;
          throw new RuntimeException( exception );
        }
      }
      while ( this.running );
    }

    /**
     * Stops this runnable.
     */
    public void terminate()
    {
      this.running = false;
    }

    /**
     * @throws IOException
     */
    final void respondWithDeviceID() throws IOException
    {
      // Respond with '1ALS'...
      this.os.write( new byte[] { 0x31, 0x41, 0x4c, 0x53 } );
      this.os.flush();
    }

    /**
     * @throws IOException
     */
    final void respondWithDeviceMetadata() throws IOException
    {
      String name;
      byte[] buffer;

      //
      // 0x01: Device name...
      name = "Virtual LogicSniffer";
      buffer = new byte[name.length() + 2];
      System.arraycopy( name.getBytes(), 0, buffer, 1, name.length() );
      buffer[0] = 0x01;
      buffer[buffer.length - 1] = 0x00;
      this.os.write( buffer );
      // 0x02: FPGA version...
      name = "Virtual FPGA";
      buffer = new byte[name.length() + 2];
      System.arraycopy( name.getBytes(), 0, buffer, 1, name.length() );
      buffer[0] = 0x02;
      buffer[buffer.length - 1] = 0x00;
      this.os.write( buffer );
      // 0x03: Ancillary version...
      name = "Virtual PIC";
      buffer = new byte[name.length() + 2];
      System.arraycopy( name.getBytes(), 0, buffer, 1, name.length() );
      buffer[0] = 0x03;
      buffer[buffer.length - 1] = 0x00;
      this.os.write( buffer );
      //
      // 0x20: number of usable probes...
      this.os.write( new byte[] { 0x20, 0x00, 0x00, 0x00, 0x08 } );
      // 0x21: Amount of sample memory available (bytes)
      this.os.write( new byte[] { 0x21, 0x00, 0x00, 0x10, 0x00 } );
      // 0x22: Amount of dynamic memory available (bytes)
      this.os.write( new byte[] { 0x22, 0x00, 0x00, 0x00, 0x00 } );
      // 0x23: Maximum sample rate (hz)
      this.os.write( new byte[] { 0x23, 0x0B, ( byte )0xEB, ( byte )0xC2, 0x00 } );
      // 0x24: Protocol version (see below)
      this.os.write( new byte[] { 0x24, 0x00, 0x00, 0x00, 0x01 } );
      //
      // 0x40: Number of usable probes (short)
      this.os.write( new byte[] { 0x40, 0x08 } );
      // 0x41: Protocol version (short)
      this.os.write( new byte[] { 0x41, 0x01 } );
      //
      // 0x00: END-OF-DESCRIPTOR
      this.os.write( 0x00 );
      this.os.flush();
    }

    /**
     * @throws IOException
     */
    final void respondWithSampleData() throws IOException
    {
      final byte[] sample = new byte[this.sampleWidth];

      int sampleCount = this.readCount;
      LOG.info( "Responding with " + sampleCount + " samples of " + this.sampleWidth + " bytes wide!" );

      while ( sampleCount-- > 0 )
      {
        this.os.write( sample );
        this.os.flush();
      }

      LOG.info( "Responded with all samples!" );
    }

    /**
     * @throws IOException
     */
    final void respondWithSelftestResult() throws IOException
    {
      // TODO
      this.os.flush();
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getAnonymousLogger();

  // VARIABLES

  private final OutputStream outputStream;
  private final InputStream inputStream;
  private final IOHelper streamReader;
  private final DeviceProfileManager manager;

  private int dividerValue;
  private int delayCount;
  private int readCount;
  private int flags;

  // CONSTRUCTORS

  /**
   * Creates a new VirtualLogicSnifferDevice instance.
   */
  public VirtualLogicSnifferDevice( final LogicSnifferConfig aConfig ) throws IOException
  {
    super( aConfig );

    this.manager = new DeviceProfileManager();

    PipedInputStream pipeIn = new PipedInputStream();
    PipedOutputStream pipeOut = new PipedOutputStream();

    this.outputStream = new PipedOutputStream( pipeIn );
    this.inputStream = new PipedInputStream( pipeOut );

    this.streamReader = new IOHelper( pipeIn, pipeOut );
    this.streamReader.start();
  }

  // METHODS

  /**
   * @return
   */
  public DeviceProfile addDeviceProfile( final String aType, final String aMetadataKeys )
      throws org.osgi.service.cm.ConfigurationException
  {
    Properties properties = new Properties();
    properties.put( DeviceProfile.DEVICE_CAPTURECLOCK, "INTERNAL" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZE_BOUND, "false" );
    properties.put( DeviceProfile.DEVICE_CAPTURESIZES, "4096,2048,1024,512,256,128,64,32,16" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_COUNT, "32" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_GROUPS, "4" );
    properties.put( DeviceProfile.DEVICE_CHANNEL_NUMBERING_SCHEMES, "DEFAULT" );
    properties.put( DeviceProfile.DEVICE_CLOCKSPEED, "100000000" );
    properties.put( DeviceProfile.DEVICE_DESCRIPTION, aType.concat( " Device Profile" ) );
    properties.put( DeviceProfile.DEVICE_FEATURE_NOISEFILTER, "false" );
    properties.put( DeviceProfile.DEVICE_FEATURE_RLE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TEST_MODE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TRIGGERS, "true" );
    properties.put( DeviceProfile.DEVICE_INTERFACE, "SERIAL" );
    properties.put( DeviceProfile.DEVICE_METADATA_KEYS, aMetadataKeys );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "0" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DTR, "false" );
    properties.put( DeviceProfile.DEVICE_SAMPLE_REVERSE_ORDER, "false" );
    properties.put( DeviceProfile.DEVICE_SAMPLERATES, "1000000" );
    properties.put( DeviceProfile.DEVICE_SUPPORTS_DDR, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_COMPLEX, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_STAGES, "4" );
    properties.put( DeviceProfile.DEVICE_TYPE, aType );
    // Update the properties of a 'virtual' PID...
    this.manager.updated( "PID-" + aType, properties );

    return this.manager.getProfile( aType );
  }

  /**
   * @param aFlagMask
   * @param aExpectedState
   */
  public void assertFlagState( final int aFlagMask, final boolean aExpectedState )
  {
    final boolean state = ( this.flags & aFlagMask ) != 0;
    assertEquals( "Flag (" + Integer.toHexString( aFlagMask ) + ") not as expected!", aExpectedState, state );
  }

  /**
   * @param aExpectedSampleRate
   */
  public void assertReadAndDelayCount( final int aExpectedReadCount, final int aExpectedDelayCount )
  {
    final LogicSnifferConfig config = getConfig();

    int expectedReadCount = aExpectedReadCount;
    int expectedDelayCount = aExpectedDelayCount;

    if ( config.isDoubleDataRateEnabled() )
    {
      expectedReadCount >>= 1;
      expectedDelayCount >>= 1;
    }

    assertEquals( "Read count not as expected!", expectedReadCount, this.readCount );
    assertEquals( "Delay count not as expected!", expectedDelayCount, this.delayCount );
  }

  /**
   * @param aExpectedSampleRate
   */
  public void assertSampleRate( final int aExpectedSampleRate )
  {
    final LogicSnifferConfig config = getConfig();

    int clock = config.getClockspeed();
    if ( config.isDoubleDataRateEnabled() )
    {
      clock *= 2;
    }
    final int sampleRate = ( clock / ( this.dividerValue + 1 ) );
    assertEquals( "Sample rate not as expected!", aExpectedSampleRate, sampleRate );
  }

  /**
   * Closes this virtual device.
   */
  public void close()
  {
    this.streamReader.terminate();
    while ( this.streamReader.isAlive() )
    {
      try
      {
        this.streamReader.join();
      }
      catch ( InterruptedException exception )
      {
        this.streamReader.interrupt();
      }
    }
  }

  /**
   * Sets the divider as written to the device.
   * 
   * @param aDividerValue
   *          the divider value.
   */
  final void setDivider( final int aDividerValue )
  {
    this.dividerValue = aDividerValue;
  }

  /**
   * Sets the flags value as written to the device.
   * 
   * @param aFlags
   *          the flags.
   */
  final void setFlags( final int aFlags )
  {
    this.flags = aFlags;
  }

  /**
   * Sets the read- and delay-count as written to the device.
   * 
   * @param aReadCount
   *          the read counter value;
   * @param aDelayCount
   *          the delay counter value.
   */
  final void setReadAndDelay( final int aReadCount, final int aDelayCount )
  {
    this.readCount = aReadCount;
    this.delayCount = aDelayCount;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected StreamConnection getConnection( final String aPortName, final int aPortRate, final boolean aDtrValue )
      throws IOException
  {
    final StreamConnection connection = new StreamConnection()
    {
      // VARIABLES

      private final InputStream is = VirtualLogicSnifferDevice.this.inputStream;
      private final OutputStream os = VirtualLogicSnifferDevice.this.outputStream;

      // METHODS

      @Override
      public void close() throws IOException
      {
        HostUtils.closeResource( this.is );
        HostUtils.closeResource( this.os );
      }

      @Override
      public DataInputStream openDataInputStream() throws IOException
      {
        return new DataInputStream( openInputStream() );
      }

      @Override
      public DataOutputStream openDataOutputStream() throws IOException
      {
        return new DataOutputStream( openOutputStream() );
      }

      @Override
      public InputStream openInputStream() throws IOException
      {
        return this.is;
      }

      @Override
      public OutputStream openOutputStream() throws IOException
      {
        return this.os;
      }
    };

    return connection;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected DeviceProfileManager getDeviceProfileManager()
  {
    return this.manager;
  }
}
