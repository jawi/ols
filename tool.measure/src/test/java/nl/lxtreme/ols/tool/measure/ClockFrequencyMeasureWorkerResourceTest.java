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
package nl.lxtreme.ols.tool.measure;


import static nl.lxtreme.ols.test.UnitTestUtils.*;
import static org.junit.Assert.*;

import java.net.*;
import java.util.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.tool.measure.ClockFrequencyMeasureWorker.ClockStats;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * Provides test cases for {@link ClockFrequencyMeasureWorker} based on actual
 * capture results.
 */
@RunWith( Parameterized.class )
public class ClockFrequencyMeasureWorkerResourceTest
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClockFrequencyMeasureWorkerResourceTest.class.getName() );

  // VARIABLES

  private final String resourceName;
  private final long expectedFrequency;

  private ClockStats result;
  private long sampleRate;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureWorkerResourceTest instance.
   */
  public ClockFrequencyMeasureWorkerResourceTest( final String aResourceName, final long aExpectedFrequency )
  {
    this.resourceName = aResourceName;
    this.expectedFrequency = aExpectedFrequency;
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
        // { resource name, expected frequency (Hz) }
            { "1.8432MHz_1.852MHz.ols", 1843200 }, // 0
            { "4.0MHz_4.0MHz.ols", 4000000 }, // 1
            { "4.7174MHz_4.762MHz.ols", 4717400 }, // 2
            { "4.9152MHz_5.0MHz.ols", 4915200 }, // 3
            { "5.12MHz_5.0MHz.ols", 5120000 }, // 4
            { "7.234MHz_7.143MHz.ols", 7234000 }, // 5
            { "7.5MHz_7.143MHz.ols", 7500000 }, // 6
            { "10.0MHz_10.0MHz.ols", 10000000 }, // 7
            { "14.31818MHz_14.286MHz.ols", 14318180 }, // 8
            { "16.257MHz_16.667MHz.ols", 16257000 }, // 9
            { "17.836MHz_16.667MHz.ols", 17836000 }, // 10
            { "18.0MHz_16.667MHz.ols", 18000000 }, // 11
            { "20.0MHz_20.0MHz.ols", 20000000 }, // 12
            { "24.0MHz_20.0MHz.ols", 24000000 }, // 13
            { "32.0MHz_33.333MHz.ols", 32000000 }, // 14
        } );
  }

  /**
   * Sets up this test case.
   */
  @Before
  public void setUp() throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), this.resourceName );
    DataContainer container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    this.sampleRate = container.getSampleRate();

    ClockFrequencyMeasureWorker worker = new ClockFrequencyMeasureWorker( container, toolContext, 0 );

    this.result = worker.doInBackground();
    assertNotNull( this.result );

    LOG.info( "Set up complete!\n" + this.result );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureWorker#doInBackground()}.
   */
  @Test
  public void testExpectedFrequencyWithinBoundaries() throws Exception
  {
    final double lowerBoundFrequency = this.result.getLowerBoundFrequency();
    final double upperBoundFrequency = this.result.getUpperBoundFrequency();

    assertRange( "Expected frequency not within boundaries!", //
        lowerBoundFrequency, upperBoundFrequency, this.expectedFrequency );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureWorker#doInBackground()}.
   */
  @Test
  public void testLowerFrequencyBoundOk() throws Exception
  {
    final double lowerBound = calculateBoundry( 0.6 );
    final double upperBound = calculateBoundry( 0.5 );

    final double lowerBoundFrequency = this.result.getLowerBoundFrequency();

    assertRange( lowerBound, upperBound, lowerBoundFrequency );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureWorker#doInBackground()}.
   */
  @Test
  public void testUpperFrequencyBoundOk() throws Exception
  {
    final double lowerBound = calculateBoundry( -0.5 );
    final double upperBound = calculateBoundry( -0.6 );

    final double upperBoundFrequency = this.result.getUpperBoundFrequency();

    assertRange( lowerBound, upperBound, upperBoundFrequency );
  }

  /**
   * @param aOffset
   * @return
   */
  private double calculateBoundry( final double aOffset )
  {
    double sr = this.sampleRate;
    double ef = this.expectedFrequency;
    return sr / ( Math.rint( sr / ef ) + aOffset );
  }
}
