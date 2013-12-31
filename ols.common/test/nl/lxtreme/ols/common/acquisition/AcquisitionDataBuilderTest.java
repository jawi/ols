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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import static org.junit.Assert.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.AcquisitionDataBuilder.*;

import org.junit.*;


/**
 * Test cases for {@link AcquisitionDataBuilder}.
 */
public class AcquisitionDataBuilderTest
{
  // METHODS

  private static void assertChannelGroups( ChannelGroup[] aChannelGroups, int[][] aIndices )
  {
    int idx = 0;
    for ( ChannelGroup channelGroup : aChannelGroups )
    {
      assertEquals( idx, channelGroup.getIndex() );
      int[] groupIndices = aIndices[idx++];

      int chIdx = 0;
      for ( Channel ch : channelGroup.getChannels() )
      {
        assertEquals( groupIndices[chIdx++], ch.getIndex() );
      }
    }
  }

  private static void assertChannels( Channel[] aChannels )
  {
    int idx = 0;
    for ( Channel channel : aChannels )
    {
      assertEquals( idx++, channel.getIndex() );
    }
  }

  private static void assertChannels( Channel[] aChannels, int... aIndices )
  {
    int idx = 0;
    for ( Channel channel : aChannels )
    {
      assertEquals( aIndices[idx++], channel.getIndex() );
    }
  }

  private static void assertValues( int[] aValues, int... aExpected )
  {
    assertEquals( aExpected.length, aValues.length );
    for ( int i = 0; i < aExpected.length; i++ )
    {
      assertEquals( "Value @ " + i, aExpected[i], aValues[i] );
    }
  }

  private static void assertTimestamps( long[] aTimestamps, long... aExpected )
  {
    assertEquals( aExpected.length, aTimestamps.length );
    for ( int i = 0; i < aExpected.length; i++ )
    {
      assertEquals( "Timestamp @ " + i, aExpected[i], aTimestamps[i] );
    }
  }

  /**
   * Tests that we cannot add a channel to an unknown group.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAddChannelToUnknownGroupFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.addChannelToGroup( 0, 1 );
  }

  /**
   * Tests that we cannot create duplicate groups.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAddDuplicateChannelGroupFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.addChannelGroup( 0, "a" );
    builder.addChannelGroup( 0, "a" ); // should fail...
  }

  /**
   * Tests that we cannot add a channel twice to a group.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAddDuplicateChannelToOtherGroupFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.addChannelGroup( 0, "a" );
    builder.addChannelGroup( 1, "b" );
    builder.addChannelToGroup( 0, 0 );
    builder.addChannelToGroup( 0, 1 ); // should fail...
  }

  /**
   * Tests that we cannot add a channel twice to a group.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAddDuplicateChannelToSameGroupFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.addChannelGroup( 0, "a" );
    builder.addChannelToGroup( 0, 0 );
    builder.addChannelToGroup( 0, 0 ); // should fail...
  }

  /**
   * Tests that we can add samples starting at timestamp 0L.
   */
  @Test
  public void testAddSamplesStartingAtTimeZeroOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.addSample( 0L, 1 );
    builder.addSample( 1L, 1 );
    builder.addSample( 2L, 1 );
    builder.addSample( 3L, 1 );

    AcquisitionData data = builder.build();
    assertValues( data.getValues(), 1, 1 );
    assertTimestamps( data.getTimestamps(), 0L, 3L );
    assertEquals( 3L, data.getAbsoluteLength() );
  }

  /**
   * Tests that we can add samples starting at timestamp 0L.
   */
  @Test
  public void testAddSamplesWithDuplicatesOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.addSample( 0L, 1 );
    builder.addSample( 1L, 1 );
    builder.addSample( 2L, 1 );
    builder.addSample( 3L, 0 );
    builder.addSample( 4L, 1 );
    builder.addSample( 5L, 1 );
    builder.addSample( 6L, 1 );
    builder.addSample( 7L, 1 );
    builder.addSample( 8L, 0 );

    AcquisitionData data = builder.build();
    assertValues( data.getValues(), 1, 0, 1, 0 );
    assertTimestamps( data.getTimestamps(), 0L, 3L, 7L, 8L );
    assertEquals( 8L, data.getAbsoluteLength() );
  }

  /**
   * Tests that we can add samples starting at timestamp 1L.
   */
  @Test
  public void testAddSamplesStartingAtTimeOneOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.addSample( 1L, 1 );
    builder.addSample( 2L, 1 );
    builder.addSample( 3L, 1 );
    builder.addSample( 4L, 1 );

    AcquisitionData data = builder.build();
    assertEquals( 4L, data.getAbsoluteLength() );
    assertValues( data.getValues(), 1, 1 );
    assertTimestamps( data.getTimestamps(), 1L, 4L );
  }

  /**
   * Tests that the builder correctly tracks the "last" timestamp.
   */
  @Test
  public void testAddSamplesCorrectlyPicksUpLastTimestamp()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.addSample( 1, 1 );
    builder.addSample( 2, 0 );
    builder.addSample( 3, 1 );

    AcquisitionData data = builder.build();
    assertEquals( 3L, data.getAbsoluteLength() );
    assertValues( data.getValues(), 1, 0, 1 );
    assertTimestamps( data.getTimestamps(), 1, 2, 3 );
  }

  /**
   * Tests that the builder correctly tracks the "last" timestamp, even when
   * samples are added in reverse order.
   */
  @Test
  public void testAddSamplesInReverseCorrectlyPicksUpLastTimestamp()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.addSample( 3, 1 );
    builder.addSample( 2, 0 );
    builder.addSample( 1, 1 );

    AcquisitionData data = builder.build();
    assertEquals( 3L, data.getAbsoluteLength() );
    assertValues( data.getValues(), 1, 0, 1 );
    assertTimestamps( data.getTimestamps(), 1, 2, 3 );
  }

  /**
   * Tests that we cannot add a sample with a negative timestamp.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAddSampleWithNegativeTimestampFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.addSample( -1L, 1 ); // should fail...
  }

  /**
   * Tests creating of {@link AcquisitionData} with 16 channels.
   */
  @Test
  public void testApplyTemplateOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.setSampleRate( 1 );
    builder.addChannelGroup( 0, "Group A" );
    builder.addChannelToGroup( 3, 0 );
    builder.addChannelToGroup( 1, 0 );
    builder.addChannelToGroup( 0, 0 );
    builder.addChannelToGroup( 2, 0 );
    builder.addChannelGroup( 1, "Group B" );
    builder.addChannelToGroup( 4, 1 );
    builder.addChannelToGroup( 6, 1 );
    builder.addChannelToGroup( 5, 1 );
    builder.addChannelToGroup( 7, 1 );

    AcquisitionData data = builder.build();
    assertNotNull( data );

    Channel[] channels = data.getChannels();
    assertEquals( 8, channels.length );
    assertChannels( channels );

    ChannelGroup[] channelGroups = data.getChannelGroups();
    assertChannelGroups( channelGroups, new int[][] { { 3, 1, 0, 2 }, { 4, 6, 5, 7 } } );
    assertEquals( 2, channelGroups.length );

    builder = new AcquisitionDataBuilder();
    builder.applyTemplate( data, IncludeSamples.NO, IncludeAnnotations.NO );
    // cut out the MSB completely...
    builder.setChannelCount( 7 );
    builder.setEnabledChannelMask( 0x7F );

    AcquisitionData otherData = builder.build();
    assertNotNull( otherData );

    channels = otherData.getChannels();
    assertEquals( 7, channels.length );
    assertChannels( channels );

    channelGroups = otherData.getChannelGroups();
    assertChannelGroups( channelGroups, new int[][] { { 3, 1, 0, 2 }, { 4, 6, 5 } } );
    assertEquals( 2, channelGroups.length );
  }

  /**
   * Tests creating of {@link AcquisitionData} with 16 channels.
   */
  @Test
  public void testBuild16BitsDataWithChannelGroupsOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 16 );
    builder.setSampleRate( 1 );
    builder.addChannelGroup( 0, "Group A" );
    builder.addChannelToGroup( 3, 0 );
    builder.addChannelToGroup( 1, 0 );
    builder.addChannelToGroup( 0, 0 );
    builder.addChannelToGroup( 2, 0 );
    builder.addChannelGroup( 1, "Group B" );
    builder.addChannelToGroup( 4, 1 );
    builder.addChannelToGroup( 6, 1 );
    builder.addChannelToGroup( 5, 1 );
    builder.addChannelToGroup( 7, 1 );

    AcquisitionData data = builder.build();
    assertNotNull( data );

    Channel[] channels = data.getChannels();
    assertEquals( 16, channels.length );
    assertChannels( channels );

    ChannelGroup[] channelGroups = data.getChannelGroups();
    assertChannelGroups( channelGroups,
        new int[][] { { 3, 1, 0, 2 }, { 4, 6, 5, 7 }, { 8, 9, 10, 11, 12, 13, 14, 15 } } );
    assertEquals( 3, channelGroups.length );
  }

  /**
   * Tests creating of {@link AcquisitionData} with only a single channel.
   */
  @Test
  public void testBuild1BitDataWithoutChannelGroupsOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setAbsoluteLength( 10 );
    builder.setChannelCount( 1 );
    builder.setSampleRate( 1 );
    builder.setChannelLabel( 0, "aaa" );
    builder.setCursorLabel( 1, "bbb" );

    AcquisitionData data = builder.build();
    assertNotNull( data );

    Cursor[] cursors = data.getCursors();
    assertEquals( OlsConstants.MAX_CURSORS, cursors.length );
    assertEquals( "bbb", cursors[1].getLabel() );

    Channel[] channels = data.getChannels();
    assertEquals( 1, channels.length );
    assertChannels( channels );
    assertEquals( "aaa", channels[0].getLabel() );

    ChannelGroup[] channelGroups = data.getChannelGroups();
    assertEquals( 1, channelGroups.length );
    assertChannelGroups( channelGroups, new int[][] { { 0 } } );
  }

  /**
   * Tests creating of {@link AcquisitionData} with 32 channels.
   */
  @Test
  public void testBuild32BitsDataWithoutChannelGroupsOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 32 );
    builder.setSampleRate( 1 );

    AcquisitionData data = builder.build();
    assertNotNull( data );

    Channel[] channels = data.getChannels();
    assertEquals( 32, channels.length );
    assertChannels( channels );

    ChannelGroup[] channelGroups = data.getChannelGroups();
    assertEquals( 4, channelGroups.length );
  }

  /**
   * Tests creating of {@link AcquisitionData} with 8 channels.
   */
  @Test
  public void testBuild8BitsDataWithoutChannelGroupsOk()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 8 );
    builder.setEnabledChannelMask( 0xFF00 );
    builder.setSampleRate( 1 );

    AcquisitionData data = builder.build();
    assertNotNull( data );

    Channel[] channels = data.getChannels();
    assertEquals( 8, channels.length );
    assertChannels( channels, 8, 9, 10, 11, 12, 13, 14, 15 );

    ChannelGroup[] channelGroups = data.getChannelGroups();
    assertEquals( 1, channelGroups.length );
    assertChannelGroups( channelGroups, new int[][] { { 8, 9, 10, 11, 12, 13, 14, 15 } } );
  }

  /**
   * Tests that we must supply a valid channel mask.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testBuildWithInvalidChannelMaskFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 1 );
    builder.setEnabledChannelMask( 0 );

    builder.build(); // should fail.
  }

  /**
   * Tests that we must supply a channel count.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testBuildWithoutChannelCountFail()
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    builder.build(); // should fail.
  }
}
