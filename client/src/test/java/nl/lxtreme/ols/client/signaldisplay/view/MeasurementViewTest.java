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
package nl.lxtreme.ols.client.signaldisplay.view;


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.signaldisplay.view.MeasurementView.PulseCountInfo;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * Provides some test cases for {@link MeasurementView.SignalMeasurer}.
 */
@RunWith( Parameterized.class )
public class MeasurementViewTest
{
  // VARIABLES

  private final String resourceName;
  private final double expectedFrequency;
  private final double realFrequency;

  private volatile PulseCountInfo result;

  // CONSTRUCTORS

  /**
   * Creates a new {@link MeasurementViewTest} instance.
   */
  public MeasurementViewTest( final String aResourceName, final double aExpectedFrequency, final double aRealFrequency )
  {
    this.resourceName = aResourceName;
    this.expectedFrequency = aExpectedFrequency;
    this.realFrequency = aRealFrequency;
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
        { "1.8432MHz_1.852MHz.ols", 1844838, 1843200 }, // 0
        { "4.0MHz_4.0MHz.ols", 4001139, 4000000 }, // 1
        { "4.7174MHz_4.762MHz.ols", 4719111, 4717400 }, // 2
        { "4.9152MHz_5.0MHz.ols", 4916164, 4915200 }, // 3
        { "5.12MHz_5.0MHz.ols", 5120273, 5120000 }, // 4
        { "7.234MHz_7.143MHz.ols", 7233767, 7234000 }, // 5
        { "7.5MHz_7.143MHz.ols", 7502175, 7500000 }, // 6
        { "10.0MHz_10.0MHz.ols", 10002034, 10000000 }, // 7
        { "14.31818MHz_14.286MHz.ols", 14318500, 14318180 }, // 8
        { "16.257MHz_16.667MHz.ols", 16257681, 16257000 }, // 9
        { "17.836MHz_16.667MHz.ols", 17837070, 17836000 }, // 10
        { "18.0MHz_16.667MHz.ols", 18000597, 18000000 }, // 11
        { "20.0MHz_20.0MHz.ols", 19999498, 20000000 }, // 12
        { "24.0MHz_20.0MHz.ols", 24000976, 24000000 }, // 13
        { "32.0MHz_33.333MHz.ols", 32001302, 32000000 }, // 14
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

    int channel = 0;

    MeasurementView.SignalMeasurer worker = new MeasurementView.SignalMeasurer( container, channel, 0L, toolContext
        .getData().getAbsoluteLength() );

    this.result = worker.run();
    assertNotNull( this.result );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureTask#doInBackground()}.
   */
  @Test
  @SuppressWarnings( "boxing" )
  public void testExpectedFrequencyOk() throws Exception
  {
    final Double freq = this.result.getFrequency();
    assertNotNull( freq );

    assertEquals( "Expected frequency not within boundaries!", this.expectedFrequency, freq.doubleValue(), 1.0 );

    double error = Math.abs( 1.0 - ( freq.doubleValue() / this.realFrequency ) ) * 100.0;

    System.out.printf( "Expected = %.3f, \tGot = %.3f,\tReal = %.3f\tRelError = %f%%\n", this.expectedFrequency, freq,
        this.realFrequency, error );

  }
}
