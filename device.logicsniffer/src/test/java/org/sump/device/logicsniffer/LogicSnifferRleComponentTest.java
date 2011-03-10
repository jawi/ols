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

import nl.lxtreme.ols.api.data.*;

import org.junit.*;
import org.osgi.service.cm.*;
import org.sump.device.logicsniffer.VirtualLogicSnifferDevice.SampleProvider;
import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;


/**
 * RLE-specific test cases for {@link LogicSnifferDevice}.
 */
@Ignore
public class LogicSnifferRleComponentTest
{
  // INNER TYPES

  static final class RleSampleProvider implements SampleProvider
  {
    // VARIABLES

    private final int highCount;
    private final int lowCount;

    // CONSTRUCTORS

    /**
     * Creates a new LogicSnifferRleComponentTest.RleSampleProvider instance.
     */
    public RleSampleProvider( final int aWidth, final double aRatio )
    {
      final int temp = ( 1 << ( aWidth - 1 ) ) - 1;
      this.highCount = ( int )( aRatio * temp );
      this.lowCount = temp - this.highCount;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void write( final OutputStream aOs, final int aSampleWidth, final int aSampleCount,
        final boolean aRleMode, final boolean aDdrMode ) throws IOException
    {
      byte[] buf = new byte[aSampleWidth];

      // write three leading zero's...
      aOs.write( createSample( buf, aSampleWidth, 0 ) );
      aOs.write( createRleCount( buf, aSampleWidth, 2 ) );

      for ( int i = 0; i < aSampleCount - 4; i++ )
      {
        final boolean sampleLevel = ( ( i % 2 ) == 0 );

        int sampleValue = sampleLevel ? 1 : 0;
        int countValue = sampleLevel ? this.highCount - 1 : this.lowCount - 1;

        try
        {
          aOs.write( createSample( buf, aSampleWidth, sampleValue ) );
          aOs.write( createRleCount( buf, aSampleWidth, countValue ) );
        }
        catch ( IOException exception )
        {
          System.out
              .println( "!!! ONLY WRITTEN " + ( aSampleCount - i ) + " SAMPLES! (" + exception.getMessage() + ")" );
          throw exception;
        }
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

  // METHODS

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test08bitRleWithDdrOk() throws Exception
  {
    fail();
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test08bitRleWithoutDdrOk() throws Exception
  {
    VirtualLogicSnifferDevice device = setupDevice( 8, false /* aDdr */, 0xFF );

    AcquisitionResult result = device.doInBackground();
    assertNotNull( result );

    final int[] values = result.getValues();
    final long[] timestamps = result.getTimestamps();

    assertEquals( values.length, timestamps.length );

    final int temp = ( 1 << 7 ) - 1;
    final int highCount = ( int )( 0.74 * temp );
    final int lowCount = temp - highCount;

    int expectedTimeStamp = 0, expectedSampleValue;
    for ( int i = 0; i < values.length; i++ )
    {
      expectedSampleValue = ( i % 2 );
      assertEquals( expectedSampleValue, values[i] );
      assertEquals( expectedTimeStamp, timestamps[i] );

      if ( i == 0 )
      {
        expectedTimeStamp += 3;
      }
      else
      {
        if ( expectedSampleValue == 1 )
        {
          expectedTimeStamp += highCount;
        }
        else
        {
          expectedTimeStamp += lowCount;
        }
      }
    }
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test16bitRleWithDdrOk() throws Exception
  {
    fail();
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test16bitRleWithoutDdrOk() throws Exception
  {
    VirtualLogicSnifferDevice device = setupDevice( 16, false /* aDdr */, 0xFFFF );

    AcquisitionResult result = device.doInBackground();
    assertNotNull( result );
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test24bitRleWithoutDdrOk() throws Exception
  {
    fail();
  }

  /**
   * Test method for
   * {@link org.sump.device.logicsniffer.LogicSnifferDevice#doInBackground()}.
   */
  @Test
  public void test32bitRleWithoutDdrOk() throws Exception
  {
    fail();
  }

  /**
   * @param aSampleRate
   * @param aTriggerEnabled
   * @param aRatio
   * @param aReadCounter
   * @param aDelayCounter
   * @param aEnabledChannelsMask
   * @return
   */
  private VirtualLogicSnifferDevice setupDevice( final int aSampleWidth, final boolean aDdr,
      final int aEnabledChannelsMask ) throws IOException, ConfigurationException
  {
    final LogicSnifferConfig config = new LogicSnifferConfig();

    VirtualLogicSnifferDevice device = new VirtualLogicSnifferDevice( config,
        new RleSampleProvider( aSampleWidth, 0.74 ) );

    final DeviceProfile deviceProfile = device.addDeviceProfile( "VirtualLS", "\"Virtual LogicSniffer\"" );
    config.setDeviceProfile( deviceProfile );

    config.setAltNumberSchemeEnabled( false ); // don't care
    config.setBaudrate( 9600 ); // don't care
    config.setPortName( "/dev/virtual" ); // don't care
    config.setClockSource( CaptureClockSource.INTERNAL ); // don't care
    config.setFilterEnabled( true ); // don't care
    config.setTestModeEnabled( false ); // don't care
    config.setEnabledChannels( aEnabledChannelsMask );
    config.setRatio( 0.5 );
    config.setRleEnabled( true );
    config.setSampleCount( 512 );
    config.setSampleRate( aDdr ? 200000000 : 100000000 );
    config.setTriggerEnabled( true );

    return device;
  }
}
