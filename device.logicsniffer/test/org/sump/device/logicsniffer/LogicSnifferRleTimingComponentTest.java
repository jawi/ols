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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.osgi.service.cm.*;
import org.sump.device.logicsniffer.VirtualLogicSnifferDevice.SampleProvider;
import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;


/**
 * Some RLE-timing test cases for {@link LogicSnifferAcquisitionTask}.
 */
@RunWith( Parameterized.class )
public class LogicSnifferRleTimingComponentTest
{
  // INNER TYPES

  /**
   * 
   */
  final class RleSampleProvider implements SampleProvider
  {
    // VARIABLES

    private final int highTime;
    private final int lowTime;
    private final int sampleHighValue;
    private final int sampleLowValue;
    private final int packedHighValue;
    private final int packedLowValue;

    // CONSTRUCTORS

    /**
     * Creates a new LogicSnifferRleComponentTest.RleSampleProvider instance.
     */
    public RleSampleProvider( final int aWidth, final double aRatio, final int aMask )
    {
      final int pulseWidth = ( 1 << ( aWidth - 1 ) ) - 1;
      this.highTime = ( int )( aRatio * pulseWidth );
      this.lowTime = pulseWidth - this.highTime;

      final long baseValue = 0x55555555;
      this.sampleHighValue = ( int )( baseValue & aMask );
      this.sampleLowValue = ( ( this.sampleHighValue >> 1 ) & aMask );

      this.packedHighValue = NumberUtils.packBytes( this.sampleHighValue );
      this.packedLowValue = NumberUtils.packBytes( this.sampleLowValue );
    }

    // METHODS

    /**
     * Returns the time the RLE-encoded pulse is "high".
     * 
     * @return the high time, > 0.
     */
    public int getHighTime()
    {
      return this.highTime;
    }

    /**
     * Returns the value of when the sample is supposed to be "high".
     * 
     * @return the high value.
     */
    public int getHighValue()
    {
      return this.sampleHighValue;
    }

    /**
     * Returns the time the RLE-encoded pulse is "low".
     * 
     * @return the low time, > 0.
     */
    public int getLowTime()
    {
      return this.lowTime;
    }

    /**
     * Returns the value of when the sample is supposed to be "low".
     * 
     * @return the low value.
     */
    public int getLowValue()
    {
      return this.sampleLowValue;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void write( final OutputStream aOs, final int aSampleWidth, final int aSampleCount,
        final boolean aRleMode, final boolean aDdrMode ) throws IOException
    {
      byte[] buf = new byte[aSampleWidth];
      assertTrue( aSampleWidth > 0 );

      // write three leading zero's...
      if ( LogicSnifferRleTimingComponentTest.this.startTime > 0 )
      {
        int startTime = LogicSnifferRleTimingComponentTest.this.startTime >> 1;
        aOs.write( createSample( buf, aSampleWidth, this.packedLowValue ) );
        aOs.write( createRleCount( buf, aSampleWidth, startTime - 1 ) );
        aOs.write( createRleCount( buf, aSampleWidth, startTime - 1 ) );
        aOs.write( createSample( buf, aSampleWidth, this.packedLowValue ) );
      }
      else
      {
        aOs.write( createSample( buf, aSampleWidth, this.packedLowValue ) );
      }

      for ( int i = 0; i < ( aSampleCount / 2 ); i++ )
      {
        final boolean sampleLevel = ( ( i % 2 ) == 0 );

        int sampleValue = sampleLevel ? this.packedHighValue : this.packedLowValue;
        aOs.write( createSample( buf, aSampleWidth, sampleValue ) );

        int countValue = ( sampleLevel ? this.highTime : this.lowTime ) - 1;
        aOs.write( createRleCount( buf, aSampleWidth, countValue ) );
      }
      //
      aOs.flush();
    }

    private byte[] createRleCount( final byte[] aBuffer, final int aSampleWidth, int aCount )
    {
      for ( int i = 0; i < aBuffer.length; i++ )
      {
        aBuffer[i] = ( byte )( aCount & 0xFF );
        aCount >>>= 8;
      }
      aBuffer[aBuffer.length - 1] |= 0x80;
      return aBuffer;
    }

    private byte[] createSample( final byte[] aBuffer, final int aSampleWidth, int aValue )
    {
      if ( aValue == 0 )
      {
        Arrays.fill( aBuffer, ( byte )0 );
      }
      else
      {
        for ( int i = 0; i < aBuffer.length; i++ )
        {
          aBuffer[i] = ( byte )( aValue & 0xFF );
          aValue >>= 8;
        }
      }
      return aBuffer;
    }
  }

  // CONSTANTS

  static final double PWM_RATIO = 0.26;

  // VARIABLES

  private final RleSampleProvider provider;
  private final int enabledChannelMask;
  private final boolean ddrMode;
  private final int startTime;
  private final int sampleCount;

  private VirtualLogicSnifferDevice device;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferRleComponentTest instance.
   */
  public LogicSnifferRleTimingComponentTest( final int aWidth, final int aChannelMask, final boolean aDdrMode,
      final int aStartTime )
  {
    this.enabledChannelMask = aChannelMask;
    this.ddrMode = aDdrMode;
    this.startTime = aStartTime;
    this.sampleCount = 4096;

    this.provider = new RleSampleProvider( aWidth, PWM_RATIO, aChannelMask );
  }

  // METHODS

  /**
   * @return a collection of test data.
   */
  @Parameters
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // width, channel mask, ddr?, start time
            { 8, 0x000000FF, false, 0 }, // 0
            { 16, 0x0000FFFF, false, 1000 }, // 1
            { 24, 0x00FFFFFF, false, 74000 }, // 2
            { 32, 0xFFFFFFFF, false, 17000000 }, // 3
        } );
  }

  /**
   * @throws IOException
   * @throws ConfigurationException
   */
  @Before
  public void setupDevice() throws IOException, ConfigurationException
  {
    final LogicSnifferConfig config = new LogicSnifferConfig();
    this.device = new VirtualLogicSnifferDevice( config, this.provider );

    final DeviceProfile deviceProfile = this.device.addDeviceProfile( "VirtualLS", "\"Virtual LogicSniffer\"" );
    config.setDeviceProfile( deviceProfile );

    config.setAltNumberSchemeEnabled( false ); // don't care
    config.setClockSource( CaptureClockSource.INTERNAL ); // don't care
    config.setFilterEnabled( false ); // don't care
    config.setTestModeEnabled( false ); // don't care
    config.setEnabledChannels( this.enabledChannelMask );
    config.setRatio( 0.5 );
    config.setRleEnabled( true );
    config.setSampleCount( this.sampleCount );
    config.setSampleRate( this.ddrMode ? 200000000 : 100000000 );
    config.setTriggerEnabled( false );
  }

  /**
   * @throws Exception
   */
  @After
  public void tearDown() throws Exception
  {
    this.device.close();
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferAcquisitionTask#doInBackground()}
   * .
   */
  @Test( /* timeout = 10000 */)
  public void testRleOk() throws Exception
  {
    final AcquisitionResult result = this.device.call();

    verifyDecodedRleData( result );
  }

  /**
   * @param aValues
   * @param aTimestamps
   */
  private void verifyDecodedRleData( final AcquisitionResult aResult )
  {
    assertNotNull( aResult );

    final int[] values = aResult.getValues();
    final long[] timestamps = aResult.getTimestamps();

    assertEquals( values.length, timestamps.length );

    final int highTime = this.provider.getHighTime();
    final int lowTime = this.provider.getLowTime();

    long expectedTimeStamp = 0;

    for ( int i = 0; i < ( this.sampleCount / 2 ) - 1; i++ )
    {
      final boolean sampleLevel = ( ( ( i + 0 ) % 2 ) != 0 );

      assertEquals( "timestamp value(" + i + "): ", expectedTimeStamp, timestamps[i] );

      if ( i == 0 )
      {
        expectedTimeStamp += ( this.startTime == 0 ) ? 1 : this.startTime;
      }
      else
      {
        expectedTimeStamp += ( sampleLevel ) ? highTime : lowTime;
      }
    }
  }
}
