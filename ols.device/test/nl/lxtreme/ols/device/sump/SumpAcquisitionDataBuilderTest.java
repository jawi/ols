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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;
import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.sump.config.*;

import org.junit.*;


/**
 * Test cases for {@link SumpAcquisitionDataBuilder}.
 */
public class SumpAcquisitionDataBuilderTest
{
  // CONSTANTS

  private static final int NORMAL_MODE = 0x000;
  private static final int DDR_MODE = 0x001;
  private static final int RLE_MODE = 0x100;

  // METHODS

  private static void assertTimestamps( AcquisitionData data, long... aExpected )
  {
    long[] timestamps = data.getTimestamps();

    assertEquals( "Timestamp count not", aExpected.length, timestamps.length );
    for ( int i = 0; i < aExpected.length; i++ )
    {
      assertEquals( "Timestamp @ " + i, aExpected[i], timestamps[i] );
    }
  }

  private static void assertValues( AcquisitionData data, int... aExpected )
  {
    int[] values = data.getValues();

    assertEquals( "Value count not", aExpected.length, values.length );
    for ( int i = 0; i < aExpected.length; i++ )
    {
      assertEquals( "Value @ " + i, aExpected[i], values[i] );
    }
  }

  /**
   * Tests the building of 8-bit non-DDR mode input data into a correct
   * {@link AcquisitionDataBuilder}.
   */
  @Test
  public void testBuild08bitDataOk() throws Exception
  {
    // enable group 0...
    SumpConfig config = createSumpConfig( NORMAL_MODE, 0x000000FF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12, 0x34, 0x56, 0x78 );
    assertTimestamps( data, 0, 1, 2, 3 );
  }

  /**
   * Tests the building of 8-bit RLE-encoded non-DDR mode input data into a
   * correct {@link AcquisitionDataBuilder}.
   */
  @Test
  public void testBuild08bitRleDataOk() throws Exception
  {
    // enable group 0...
    SumpConfig config = createSumpConfig( RLE_MODE, 0x000000FF );

    byte[] input = { 0x12, ( byte )0x8a, 0x34, ( byte )0x8a, 0x56, ( byte )0x8a, 0x78, ( byte )0x8a };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12, 0x34, 0x56, 0x78, 0x78 );
    assertTimestamps( data, 0, 10, 20, 30, 39 );
  }

  /**
   * Tests the normalization of 16-bit non-DDR mode input data.
   */
  @Test
  public void testBuild16bitDataOk() throws Exception
  {
    // enable groups 0 & 1...
    SumpConfig config = createSumpConfig( NORMAL_MODE, 0x0000FFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x1234, 0x5678 );
    assertTimestamps( data, 0, 1 );
  }

  /**
   * Tests the normalization of 16-bit DDR-mode input data.
   */
  @Test
  public void testBuild16bitDdrDataOk() throws Exception
  {
    // enable groups 0 & 2; in case of DDR, channel groups 2 & 3 should always
    // be equal to channel groups 0 & 1!
    SumpConfig config = createSumpConfig( DDR_MODE, 0x00FF00FF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12, 0x34, 0x56, 0x78 );
    assertTimestamps( data, 0, 1, 2, 3 );
  }

  /**
   * Tests the normalization of 16-bit RLE encoded non-DDR mode input data.
   */
  @Test
  public void testBuild16bitRleDataOk() throws Exception
  {
    // enable groups 0 & 1...
    SumpConfig config = createSumpConfig( RLE_MODE, 0x0000FFFF );

    byte[] input = { 0x12, 0x34, ( byte )0x80, 0x0a, 0x56, 0x78, ( byte )0x80, 0x0a };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x1234, 0x5678, 0x5678 );
    assertTimestamps( data, 0, 10, 19 );
  }

  /**
   * Tests the normalization of 16-bit RLE encoded DDR-mode input data.
   */
  @Test
  public void testBuild16bitRleDdrDataOk() throws Exception
  {
    // enable groups 0 & 2; in case of DDR, channel groups 2 & 3 should always
    // be equal to channel groups 0 & 1!
    SumpConfig config = createSumpConfig( DDR_MODE | RLE_MODE, 0x00FF00FF );

    byte[] input = { 0x12, 0x34, ( byte )0x80, 0x03, 0x56, 0x78, ( byte )0x80, 0x03 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78, 0x56, 0x78, 0x56, 0x78 );
    assertTimestamps( data, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
  }

  /**
   * Tests the normalization of 24-bit non-DDR mode input data.
   */
  @Test
  public void testBuild24bitDataOk() throws Exception
  {
    SumpConfig config = createSumpConfig( NORMAL_MODE, 0x00FFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x56, 0x34, 0x12 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x123456, 0x563412 );
    assertTimestamps( data, 0, 1 );
  }

  /**
   * Tests the normalization of 24-bit RLE-encoded non-DDR mode input data.
   */
  @Test
  public void testBuild24bitRleDataOk() throws Exception
  {
    SumpConfig config = createSumpConfig( RLE_MODE, 0xFF00FFFF );

    byte[] input = { 0x12, 0x34, 0x56, ( byte )0x80, 0x00, 0x0a, 0x56, 0x34, 0x12, ( byte )0x80, 0x00, 0x0a };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12003456, 0x56003412, 0x56003412 );
    assertTimestamps( data, 0, 10, 19 );
  }

  /**
   * Tests the normalization of 32-bit non-DDR mode input data.
   */
  @Test
  public void testBuild32bitDataOk() throws Exception
  {
    // enable all groups
    SumpConfig config = createSumpConfig( NORMAL_MODE, 0xFFFFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78, 0x78, 0x56, 0x34, 0x12 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12345678, 0x78563412 );
    assertTimestamps( data, 0, 1 );
  }

  /**
   * Tests the normalization of 32-bit DDR-mode input data.
   */
  @Test
  public void testBuild32bitDdrDataOk() throws Exception
  {
    // enable all groups; in case of DDR, channel groups 2 & 3 should always be
    // equal to channel groups 0 & 1!
    SumpConfig config = createSumpConfig( DDR_MODE, 0xFFFFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78, 0x78, 0x56, 0x34, 0x12 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x1234, 0x5678, 0x7856, 0x3412 );
    assertTimestamps( data, 0, 1, 2, 3 );
  }

  /**
   * Tests the normalization of 32-bit RLE encoded non-DDR mode input data with
   * very large RLE counts.
   */
  @Test
  public void testBuild32bitHugeRleDataOk() throws Exception
  {
    // enable all groups
    SumpConfig config = createSumpConfig( RLE_MODE, 0xFFFFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78, ( byte )0xc0, 0x00, 0x00, 0x00, //
        0x78, 0x56, 0x34, 0x12, ( byte )0xc0, 0x00, 0x00, 0x00 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12345678, 0x78563412, 0x78563412 );
    assertTimestamps( data, 0, 1073741824L, 2147483647L );
  }

  /**
   * Tests the normalization of 32-bit RLE encoded non-DDR mode input data.
   */
  @Test
  public void testBuild32bitRleDataOk() throws Exception
  {
    // enable all groups
    SumpConfig config = createSumpConfig( RLE_MODE, 0xFFFFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78, ( byte )0x80, 0x00, 0x00, 0x0a, //
        0x78, 0x56, 0x34, 0x12, ( byte )0x80, 0x00, 0x00, 0x0a };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12345678, 0x78563412, 0x78563412 );
    assertTimestamps( data, 0, 10, 19 );
  }

  /**
   * Tests the normalization of 32-bit RLE encoded DDR-mode input data.
   */
  @Test
  public void testBuild32bitRleDdrDataOk() throws Exception
  {
    // enable all groups; in case of DDR, channel groups 2 & 3 should always be
    // equal to channel groups 0 & 1!
    SumpConfig config = createSumpConfig( DDR_MODE | RLE_MODE, 0xFFFFFFFF );

    byte[] input = { 0x12, 0x34, 0x56, 0x78, ( byte )0x80, 0x00, 0x00, 0x03, 0x78, 0x56, 0x34, 0x12, ( byte )0x80,
        0x00, 0x00, 0x03 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x1234, 0x5678, 0x1234, 0x5678, 0x1234, 0x5678, 0x7856, 0x3412, 0x7856, 0x3412, 0x7856, 0x3412 );
    assertTimestamps( data, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
  }

  /**
   * Tests that the progress listener is called at most 100 times for large
   * input data sets.
   */
  @Test
  public void testProgressListenerOk() throws Exception
  {
    SumpConfig config = createSumpConfig( NORMAL_MODE, 0x000000FF );
    // actual data itself is not relevant here...
    byte[] input = new byte[4096];

    AcquisitionProgressListener progressListener = createProgressListener();

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    builder.build( input, input.length, progressListener );

    verify( progressListener, atMost( 100 ) ).acquisitionInProgress( anyInt() );
  }

  /**
   * Tests that if a RLE-count is found without a preceeding sample value, that
   * this count is ignored.
   */
  @Test
  public void testRleCountWithoutSampleIsIgnoredOk() throws Exception
  {
    // enable groups 0 & 2...
    SumpConfig config = createSumpConfig( RLE_MODE, 0xFF0000FF );

    byte[] input = { ( byte )0x80, 0x02, 0x00, 0x12, ( byte )0x80, 0x03, 0x00, 0x34, ( byte )0x80, 0x04, 0x00, 0x56,
        ( byte )0x80, 0x05, 0x00, 0x78, ( byte )0x80, 0x06 };

    SumpAcquisitionDataBuilder builder = new SumpAcquisitionDataBuilder( config );
    AcquisitionData data = builder.build( input, input.length, createProgressListener() );

    assertValues( data, 0x12, 0x34, 0x56, 0x78, 0x78 );
    assertTimestamps( data, 0, 3, 7, 12, 17 );
  }

  private AcquisitionProgressListener createProgressListener()
  {
    return mock( AcquisitionProgressListener.class );
  }

  private SumpConfig createSumpConfig( int mode, int enabledGroupMask )
  {
    int groupCount = 4;

    Map<String, Serializable> config = new HashMap<String, Serializable>();
    config.put( KEY_SAMPLE_RATE, 1 ); // Hz
    config.put( KEY_LAST_SAMPLE_SENT_FIRST, Boolean.FALSE );
    config.put( KEY_ENABLED_CHANNELS, enabledGroupMask );
    config.put( KEY_GROUP_COUNT, groupCount );

    int flags = 0;
    for ( int i = 0; i < groupCount; i++ )
    {
      flags >>>= 1;
      if ( ( enabledGroupMask & 0xFF ) != 0 )
      {
        flags |= 8;
      }
      enabledGroupMask >>= 8;
    }
    flags = mode | ( ( ~flags & 0xF ) << 2 );

    config.put( KEY_FLAGS, flags );
    return new SumpConfig( config );
  }
}
