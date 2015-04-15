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
package org.sump.device.logicsniffer;


import static org.junit.Assert.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.protocol.*;


/**
 * Provides a "virtual" LogicSniffer device.
 */
public class VirtualLogicSnifferDevice extends LogicSnifferAcquisitionTask
{
  // INNER TYPES

  /**
   * Provides samples to the {@link IOHelper}.
   */
  public static interface SampleProvider
  {
    // METHODS

    /**
     * Writes the samples.
     *
     * @param aOs
     * @param aSampleWidth
     * @param aSampleCount
     * @param aRleMode
     * @param aDdrMode
     */
    void write( OutputStream aOs, int aSampleWidth, int aSampleCount, boolean aRleMode, boolean aDdrMode )
        throws IOException;
  }

  /**
   * IOHelper provides an asynchronous reader for streams...
   */
  final class IOHelper extends Thread
  {
    // VARIABLES

    private final InputStream is;
    private final OutputStream os;
    private final SampleProvider sampleProvider;

    private volatile boolean running;
    private volatile int sizeValue;
    private volatile int sampleWidth;
    private volatile int enabledGroups;
    private volatile boolean ddrMode;
    private volatile boolean rleMode;

    // CONSTRUCTORS

    /**
     * Creates a new StreamReader instance.
     */
    public IOHelper( final InputStream aIS, final OutputStream aOS, final SampleProvider aSampleProvider )
    {
      super( "IOHelper" );
      this.is = aIS;
      this.os = aOS;
      this.sampleProvider = aSampleProvider;
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
              // LOG.info( "Received RLE FINISH NOW request..." );
              break;
            case 0xC0:
            case 0xC4:
            case 0xC8:
            case 0xCC:
              // Set Trigger Masks, trigger 0, 1, 2 & 3...
              // LOG.info( "Received SET TRIGGER MASK..." );
              break;
            case 0xC1:
            case 0xC5:
            case 0xC9:
            case 0xCD:
              // Set Trigger Values, trigger 0, 1, 2 & 3...
              // LOG.info( "Received SET TRIGGER VALUE..." );
              break;
            case 0xC2:
            case 0xC6:
            case 0xCA:
            case 0xCE:
              // Set Trigger Configuration, trigger 0, 1, 2 & 3...
              // LOG.info( "Received SET TRIGGER CONFIGURATION..." );
              break;
            case 0x80:
              // Set divider...
              setDivider( parameterValue );
              break;
            case 0x81:
              // Set Read & Delay Count...
              this.sizeValue = parameterValue;
              break;
            case 0x82:
              // Set Flags...
              this.ddrMode = ( ( parameterValue & SumpCommandWriter.FLAG_DEMUX ) != 0 );
              this.rleMode = ( ( parameterValue & SumpCommandWriter.FLAG_RLE ) != 0 );
              this.enabledGroups = ( ( parameterValue & 0x3C ) >> 2 );
              this.sampleWidth = Ols.MAX_BLOCKS - Integer.bitCount( this.enabledGroups );
              if ( this.ddrMode )
              {
                this.sampleWidth >>= 1;
              }
              setFlags( parameterValue );
              break;
            default:
              LOG.log( Level.INFO, "Unknown/unimplemented command: 0x{0}!", Integer.toHexString( command ) );
              break;
          }
        }
        catch ( Exception exception )
        {
          this.running = false;
          throw new RuntimeException( exception );
        }
      }
      while ( this.running && !isInterrupted() );
    }

    /**
     * Stops this runnable.
     */
    public synchronized void terminate()
    {
      this.running = false;
      interrupt();
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
      int readCount, delayCount;
      if ( this.ddrMode )
      {
        readCount = ( ( this.sizeValue & 0xFFFF ) + 1 ) << 3;
        delayCount = ( ( this.sizeValue >> 16 ) & 0xFFFF ) << 3;
      }
      else
      {
        readCount = ( ( this.sizeValue & 0xFFFF ) + 1 ) << 2;
        delayCount = ( ( this.sizeValue >> 16 ) & 0xFFFF ) << 2;
      }
      setReadAndDelay( readCount, delayCount );

      this.sampleProvider.write( this.os, this.sampleWidth, readCount, this.rleMode, this.ddrMode );
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

  /**
   * An {@link AcquisitionProgressListener} that does nothing.
   */
  static final class NullAcquisitionProgressListener implements AcquisitionProgressListener
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public void acquisitionInProgress( final int aPercentage )
    {
      // No-op
    }
  }

  /**
   * Default implementation of {@link SampleProvider}.
   */
  static final class SimpleSampleProvider implements SampleProvider
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public void write( final OutputStream aOs, final int aSampleWidth, final int aSampleCount, final boolean aRleMode,
        final boolean aDdrMode ) throws IOException
    {
      final byte[] sample = new byte[aSampleWidth];
      final byte value = ( byte )( aRleMode ? 0x7F : 0xFF );
      Arrays.fill( sample, value );

      for ( int i = 0; i < aSampleCount; i++ )
      {
        aOs.write( sample );
      }
      aOs.flush();
    }
  }

  // CONSTANTS

  static final Logger LOG = Logger.getAnonymousLogger();

  // VARIABLES

  private final OutputStream outputStream;
  private final InputStream inputStream;
  private final IOHelper streamReader;

  private volatile int dividerValue;
  private volatile int delayCount;
  private volatile int readCount;
  private volatile int flags;

  // CONSTRUCTORS

  /**
   * Creates a new VirtualLogicSnifferDevice instance.
   */
  public VirtualLogicSnifferDevice( final LogicSnifferConfig aConfig ) throws IOException
  {
    this( aConfig, new SimpleSampleProvider() );
  }

  /**
   * Creates a new VirtualLogicSnifferDevice instance.
   */
  public VirtualLogicSnifferDevice( final LogicSnifferConfig aConfig, final SampleProvider aSampleProvider )
      throws IOException
  {
    super( aConfig, null /* aConnection */, new DeviceProfileManager(), new NullAcquisitionProgressListener() );

    // Quite a lot of data can be pumped from this device, so we need some room
    // for it to store it all...
    final int pipeSize = 512 * 1024;

    PipedInputStream pipeIn = new PipedInputStream( pipeSize );
    PipedOutputStream pipeOut = new PipedOutputStream();

    this.outputStream = new PipedOutputStream( pipeIn );
    this.inputStream = new PipedInputStream( pipeOut, pipeSize );

    this.streamReader = new IOHelper( pipeIn, pipeOut, aSampleProvider );
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
    properties.put( DeviceProfile.DEVICE_DIVIDER_CLOCKSPEED, "100000000" );
    properties.put( DeviceProfile.DEVICE_DESCRIPTION, aType.concat( " Device Profile" ) );
    properties.put( DeviceProfile.DEVICE_FEATURE_NOISEFILTER, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_RLE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TEST_MODE, "true" );
    properties.put( DeviceProfile.DEVICE_FEATURE_TRIGGERS, "true" );
    properties.put( DeviceProfile.DEVICE_INTERFACE, "SERIAL" );
    properties.put( DeviceProfile.DEVICE_METADATA_KEYS, aMetadataKeys );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DELAY, "0" );
    properties.put( DeviceProfile.DEVICE_OPEN_PORT_DTR, "false" );
    properties.put( DeviceProfile.DEVICE_RECEIVE_TIMEOUT, "12" );
    properties.put( DeviceProfile.DEVICE_SAMPLE_REVERSE_ORDER, "true" );
    properties.put( DeviceProfile.DEVICE_SAMPLERATES, "1000000" );
    properties.put( DeviceProfile.DEVICE_SUPPORTS_DDR, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_COMPLEX, "true" );
    properties.put( DeviceProfile.DEVICE_TRIGGER_STAGES, "4" );
    properties.put( DeviceProfile.DEVICE_TYPE, aType );
    // Update the properties of a 'virtual' PID...
    getDeviceProfileManager().updated( "PID-" + aType, properties );

    return getDeviceProfileManager().getProfile( aType );
  }

  /**
   * @param aExpectedCount
   * @param aExpectedValue
   * @throws IOException
   */
  public void assertConstantDataStream( final AcquisitionResult aResult, final int aExpectedValue,
      final long aExpectedLength ) throws IOException
  {
    final int[] expectedValues = new int[2];
    Arrays.fill( expectedValues, aExpectedValue );

    final int[] actualValues = aResult.getValues();
    Assert.assertArrayEquals( "Sample values not as expected?!", expectedValues, actualValues );

    final long[] expectedTimestamps = new long[] { 0L, aExpectedLength };
    // Arrays.fill( expectedTimestamps, 0L );

    final long[] actualTimestamps = aResult.getTimestamps();
    Assert.assertArrayEquals( "Timestamps not as expected?!", expectedTimestamps, actualTimestamps );

    assertEquals( "Absolute length not equal?!", aExpectedLength, aResult.getAbsoluteLength() );
  }

  /**
   * @param aFlagMask
   * @param aExpectedState
   */
  public void assertFlagState( final int aFlagMask, final boolean aExpectedState )
  {
    final boolean state = ( this.flags & aFlagMask ) != 0;
    assertEquals( "Flag (" + Integer.toHexString( aFlagMask ) + ") not as expected!",
        Boolean.valueOf( aExpectedState ), Boolean.valueOf( state ) );
  }

  /**
   * @param aExpectedSampleRate
   */
  public void assertReadAndDelayCount( final int aExpectedReadCount, final int aExpectedDelayCount )
  {
    assertEquals( "Read count not as expected!", aExpectedReadCount, this.readCount );
    assertEquals( "Delay count not as expected!", aExpectedDelayCount, this.delayCount );
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
  @Override
  public synchronized void close()
  {
    this.streamReader.terminate();

    do
    {
      try
      {
        this.streamReader.join();
      }
      catch ( InterruptedException exception )
      {
        Thread.currentThread().interrupt();
      }
    }
    while ( this.streamReader.isAlive() );

    // super.close();
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
  protected StreamConnection getStreamConnection()
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
}
