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
package nl.lxtreme.ols.common.acquisition;


import static org.junit.Assert.*;

import org.junit.*;


/**
 * Test cases for {@link ChannelGroup}.
 */
public class ChannelGroupTest
{
  // METHODS

  /**
   * Tests that {@link ChannelGroup#getValue(int)} uses the logical channel
   * order, that is, the order in which the channels are currently defined.
   */
  @Test
  public void testGetValueUsesLogicalOrder()
  {
    AcquisitionDataBuilder builder;
    AcquisitionData data;
    ChannelGroup channelGroup;

    builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 4 );
    builder.add( builder.createChannelGroup().setIndex( 0 ).setChannelCount( 4 ).addChannel( 0, 1, 2, 3 ) );
    data = builder.build();

    channelGroup = data.getChannelGroups()[0];
    assertEquals( 0x01, channelGroup.getValue( 0x01 ) );
    assertEquals( 0x02, channelGroup.getValue( 0x02 ) );
    assertEquals( 0x04, channelGroup.getValue( 0x04 ) );
    assertEquals( 0x0C, channelGroup.getValue( 0x0C ) );
    assertEquals( 0x0F, channelGroup.getValue( 0x0F ) );
    assertEquals( 0x00, channelGroup.getValue( 0x10 ) );
    assertEquals( 0x0F, channelGroup.getValue( 0xFF ) );

    builder = new AcquisitionDataBuilder();
    builder.setChannelCount( 4 );
    builder.add( builder.createChannelGroup().setIndex( 0 ).setChannelCount( 4 ).addChannel( 3, 2, 1, 0 ) );
    data = builder.build();

    channelGroup = data.getChannelGroups()[0];
    assertEquals( 0x08, channelGroup.getValue( 0x01 ) );
    assertEquals( 0x04, channelGroup.getValue( 0x02 ) );
    assertEquals( 0x02, channelGroup.getValue( 0x04 ) );
    assertEquals( 0x03, channelGroup.getValue( 0x0C ) );
    assertEquals( 0x0F, channelGroup.getValue( 0x0F ) );
    assertEquals( 0x00, channelGroup.getValue( 0x10 ) );
    assertEquals( 0x0F, channelGroup.getValue( 0xFF ) );
  }
}
