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

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.client.signaldisplay.view.MeasurementView.PulseCountInfo;
import nl.lxtreme.ols.common.acquisition.*;

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

  private volatile PulseCountInfo result;

  // CONSTRUCTORS

  /**
   * Creates a new {@link MeasurementViewTest} instance.
   */
  public MeasurementViewTest( final String aResourceName, final double aExpectedFrequency )
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
            { "1.8432MHz_1.852MHz.ols", 1884352 }, // 0
            { "4.0MHz_4.0MHz.ols", 4014227 }, // 1
            { "4.7174MHz_4.762MHz.ols", 4735256 }, // 2
            { "4.9152MHz_5.0MHz.ols", 4925683 }, // 3
            { "5.12MHz_5.0MHz.ols", 5121079 }, // 4
            { "7.234MHz_7.143MHz.ols", 7232669 }, // 5
            { "7.5MHz_7.143MHz.ols", 7506775 }, // 6
            { "10.0MHz_10.0MHz.ols", 10004066 }, // 7
            { "14.31818MHz_14.286MHz.ols", 14318967 }, // 8
            { "16.257MHz_16.667MHz.ols", 16258414 }, // 9
            { "17.836MHz_16.667MHz.ols", 17838770 }, // 10
            { "18.0MHz_16.667MHz.ols", 18002034 }, // 11
            { "20.0MHz_20.0MHz.ols", 20000978 }, // 12
            { "24.0MHz_20.0MHz.ols", 24004068 }, // 13
            { "32.0MHz_33.333MHz.ols", 32000508 }, // 14
        } );
  }

  /**
   * Sets up this test case.
   */
  @Before
  public void setUp() throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), this.resourceName );
    AcquisitionResult container = OlsDataHelper.read( new InputStreamReader( resource.openStream() ) );

    int start = 0;
    int end = container.getValues().length;
    int channel = 0;

    MeasurementView.SignalMeasurer worker = new MeasurementView.SignalMeasurer( container, channel, start, end );

    this.result = worker.run();
    assertNotNull( this.result );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureTask#doInBackground()}.
   */
  @Test
  public void testExpectedFrequencyOk() throws Exception
  {
    final Double freq = this.result.getFrequency();
    assertNotNull( freq );

    assertEquals( "Expected frequency not within boundaries!", this.expectedFrequency, freq.doubleValue(), 1.0 );
  }
}
