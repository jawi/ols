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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import junit.framework.*;


/**
 * Test cases for {@link AcquisitionDataBuilder}.
 */
public class AcquisitionDataBuilderTest extends TestCase
{
  // VARIABLES

  private AcquisitionDataBuilder builder;

  // METHODS

  /**
   * Test that building without an enabled channel mask fails.
   */
  public void testBuildWithInvalidChannelMaskFail()
  {
    this.builder.setChannelCount( 1 );
    this.builder.setEnabledChannelMask( 2 );
    this.builder.setSampleRate( 1 );
    this.builder.setTriggerPosition( 1L );

    try
    {
      this.builder.build();
      fail( "Expected IllegalArgumentException!" );
    }
    catch ( IllegalArgumentException exception )
    {
      // Ok; expected...
    }
  }

  /**
   * Test that building without any data fails.
   */
  public void testBuildWithoutAnyDataFail()
  {
    try
    {
      this.builder.build();
      fail( "Expected IllegalArgumentException!" );
    }
    catch ( IllegalArgumentException exception )
    {
      // Ok; expected...
    }
  }

  /**
   * Test that building without a channel count fails.
   */
  public void testBuildWithoutChannelCountFail()
  {
    this.builder.setEnabledChannelMask( 1 );
    this.builder.setSampleRate( 1 );
    this.builder.setTriggerPosition( 1L );

    try
    {
      this.builder.build();
      fail( "Expected IllegalArgumentException!" );
    }
    catch ( IllegalArgumentException exception )
    {
      // Ok; expected...
    }
  }

  /**
   * Test that building without sample data succeeds.
   */
  public void testBuildWithoutSampleDataOk()
  {
    this.builder.setChannelCount( 1 );
    this.builder.setSampleRate( 2 );
    this.builder.setTriggerPosition( 3L );

    AcquisitionData data = this.builder.build();
    assertNotNull( data );

    assertEquals( 1L, data.getAbsoluteLength() );
    assertEquals( 3L, data.getTriggerPosition() );
    assertEquals( 2, data.getSampleRate() );
    assertEquals( 1, data.getChannelCount() );

    assertEquals( 1, data.getValues().length );
    assertEquals( 1, data.getTimestamps().length );
  }

  /**
   * Test that building with sample data succeeds.
   */
  public void testBuildWithSampleDataOk()
  {
    this.builder.setChannelCount( 1 );
    this.builder.setSampleRate( 1 );
    this.builder.setTriggerPosition( 1L );
    this.builder.addSample( 0L, 0 );
    this.builder.addSample( 1L, 1 );
    this.builder.addSample( 2L, 2 );

    AcquisitionData data = this.builder.build();
    assertNotNull( data );

    assertEquals( 3L, data.getAbsoluteLength() );
    assertEquals( 1L, data.getTriggerPosition() );
    assertEquals( 1, data.getSampleRate() );
    assertEquals( 1, data.getChannelCount() );

    assertEquals( 3, data.getValues().length );
    assertEquals( 3, data.getTimestamps().length );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void setUp() throws Exception
  {
    this.builder = new AcquisitionDataBuilder();
  }
}
