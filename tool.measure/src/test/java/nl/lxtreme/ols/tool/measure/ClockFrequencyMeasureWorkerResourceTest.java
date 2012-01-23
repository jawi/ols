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


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.tool.measure.ClockFrequencyMeasureTask.ClockStats;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * Provides test cases for {@link ClockFrequencyMeasureTask} based on actual
 * capture results.
 */
@RunWith( Parameterized.class )
public class ClockFrequencyMeasureWorkerResourceTest
{
  // VARIABLES

  private final String resourceName;
  private final double expectedFrequency;

  private ClockStats result;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureWorkerResourceTest instance.
   */
  public ClockFrequencyMeasureWorkerResourceTest( final String aResourceName, final double aExpectedFrequency )
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
            { "1.8432MHz_1.852MHz.ols", 1844838 }, // 0
            { "4.0MHz_4.0MHz.ols", 4001140 }, // 1
            { "4.7174MHz_4.762MHz.ols", 4719112 }, // 2
            { "4.9152MHz_5.0MHz.ols", 4916165 }, // 3
            { "5.12MHz_5.0MHz.ols", 5120273 }, // 4
            { "7.234MHz_7.143MHz.ols", 7233767 }, // 5
            { "7.5MHz_7.143MHz.ols", 7502175 }, // 6
            { "10.0MHz_10.0MHz.ols", 10002035 }, // 7
            { "14.31818MHz_14.286MHz.ols", 14318500 }, // 8
            { "16.257MHz_16.667MHz.ols", 16257681 }, // 9
            { "17.836MHz_16.667MHz.ols", 17837071 }, // 10
            { "18.0MHz_16.667MHz.ols", 18000597 }, // 11
            { "20.0MHz_20.0MHz.ols", 19999499 }, // 12
            { "24.0MHz_20.0MHz.ols", 24000977 }, // 13
            { "32.0MHz_33.333MHz.ols", 32001302 }, // 14
        } );
  }

  /**
   * Sets up this test case.
   */
  @Before
  public void setUp() throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), this.resourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ClockFrequencyMeasureTask worker = new ClockFrequencyMeasureTask( toolContext );
    worker.setChannel( 0 );

    this.result = worker.call();
    assertNotNull( this.result );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureTask#doInBackground()}.
   */
  @Test
  public void testExpectedFrequencyOk() throws Exception
  {
    final double freq = this.result.getFrequency();

    assertEquals( "Expected frequency not within boundaries!", //
        this.expectedFrequency, freq, 1.0 );
  }
}
