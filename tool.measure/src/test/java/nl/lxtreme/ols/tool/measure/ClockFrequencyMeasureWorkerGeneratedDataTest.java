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


import static nl.lxtreme.ols.test.data.DataTestUtils.*;
import static org.junit.Assert.*;

import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.data.DataTestUtils.TestDataProvider;
import nl.lxtreme.ols.tool.measure.ClockFrequencyMeasureTask.ClockStats;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * Provides test cases for the {@link ClockFrequencyMeasureTask} based on
 * generated data. TODO jawi: fix these tests!
 */
@RunWith( Parameterized.class )
@Ignore
public class ClockFrequencyMeasureWorkerGeneratedDataTest
{
  // INNER TYPES

  /**
   * Provides some default test data.
   */
  static class JitteredTestDataProvider implements TestDataProvider
  {
    // VARIABLES

    private final double jitterPercentage;

    // CONSTRUCTORS

    /**
     * Creates a new ClockFrequencyMeasureWorkerTest.JitteredTestDataProvider
     * instance.
     */
    public JitteredTestDataProvider( final double aJitterPercentage )
    {
      this.jitterPercentage = aJitterPercentage;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void fillData( final int[] aValues, final long[] aTimestamps, final int aDataSize )
    {
      Random miscRnd = new Random();
      Random valueRnd = new Random();
      Random timestampRnd = new Random();

      int value = 0xAA;
      long timestamp = 0;
      for ( int i = 0; i < aDataSize; i++ )
      {
        int sampleValue = value;

        int interval = TIME_INTERVAL;
        if ( miscRnd.nextDouble() < this.jitterPercentage )
        {
          final double nextGaussianTimeInterval = timestampRnd.nextGaussian();
          final int jitterValue = ( int )( ( 0.5 * TIME_INTERVAL ) * nextGaussianTimeInterval );
          interval += jitterValue;
        }
        timestamp += interval;

        if ( miscRnd.nextDouble() < this.jitterPercentage )
        {
          int mask = 1 << valueRnd.nextInt( 8 );
          if ( ( sampleValue & mask ) != 0 )
          {
            sampleValue &= ~mask;
          }
          else
          {
            sampleValue |= mask;
          }
        }

        aValues[i] = sampleValue;
        aTimestamps[i] = timestamp;

        value = ( value == 0xAA ) ? 0x55 : 0xAA;
      }
    }
  }

  // CONSTANTS

  private static final int TIME_INTERVAL = 10;
  private static final int OVERSAMPLING_FACTOR = 2 * TIME_INTERVAL;

  // VARIABLES

  private final int dataSize;
  private final int sampleRate;
  private final TestDataProvider dataProvider;
  private final double allowedDutyCycleTolerance;
  private final double allowedFrequencyTolerance;

  private ClockFrequencyMeasureTask worker;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureWorkerTest instance.
   */
  public ClockFrequencyMeasureWorkerGeneratedDataTest( final int aDataSize, final int aSampleRate,
      final double aJitterFactor, final double aDutyCycleTolerance, final double aFrequencyTolerance )
  {
    this.dataSize = aDataSize;
    this.sampleRate = aSampleRate;
    this.dataProvider = new JitteredTestDataProvider( aJitterFactor );
    this.allowedDutyCycleTolerance = aDutyCycleTolerance;
    this.allowedFrequencyTolerance = aFrequencyTolerance;
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
        // { datasize, sample rate }
            { 256 /* samples */, 1 /* Hz */, 0.1, 0.0001, 0.0001 }, // 0
            { 256 /* samples */, 10 /* Hz */, 0.1, 0.0001, 0.0001 }, // 1
            { 256 /* samples */, 100 /* Hz */, 0.1, 0.0001, 0.0001 }, // 2
            { 256 /* samples */, 1000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 3
            { 256 /* samples */, 10000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 4
            { 256 /* samples */, 100000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 5
            { 256 /* samples */, 1000000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 6
            { 256 /* samples */, 10000000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 7
            { 256 /* samples */, 100000000 /* Hz */, 0.1, 0.0001, 0.0001 }, // 8

            { 256 /* samples */, 5 /* Hz */, 0.2, 0.0001, 0.0001 }, // 9
            { 256 /* samples */, 50 /* Hz */, 0.2, 0.0001, 0.0001 }, // 10
            { 256 /* samples */, 500 /* Hz */, 0.2, 0.0001, 0.0001 }, // 11
            { 256 /* samples */, 5000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 12
            { 256 /* samples */, 50000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 13
            { 256 /* samples */, 500000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 14
            { 256 /* samples */, 5000000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 15
            { 256 /* samples */, 50000000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 16
            { 256 /* samples */, 100000000 /* Hz */, 0.2, 0.0001, 0.0001 }, // 17

            { 256 /* samples */, 3 /* Hz */, 0.15, 0.0001, 0.0001 }, // 18
            { 256 /* samples */, 33 /* Hz */, 0.15, 0.0001, 0.0001 }, // 19
            { 256 /* samples */, 333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 20
            { 256 /* samples */, 3333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 21
            { 256 /* samples */, 33333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 22
            { 256 /* samples */, 333333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 23
            { 256 /* samples */, 3333333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 24
            { 256 /* samples */, 33333333 /* Hz */, 0.15, 0.0001, 0.0001 }, // 25
            { 256 /* samples */, 100000003 /* Hz */, 0.15, 0.0001, 0.0001 }, // 26

            { 64 /* samples */, 3 /* Hz */, 0.05, 0.0001, 0.0001 }, // 27
            { 64 /* samples */, 33 /* Hz */, 0.05, 0.0001, 0.0001 }, // 28
            { 64 /* samples */, 111 /* Hz */, 0.05, 0.0001, 0.0001 }, // 29
            { 64 /* samples */, 333 /* Hz */, 0.05, 0.0001, 0.0001 }, // 30
        } );
  }

  /**
   * @throws Exception
   */
  @Before
  public void setUp() throws Exception
  {
    DataContainer container = createMockDataContainer( this.dataSize, 2, OVERSAMPLING_FACTOR * this.sampleRate,
        this.dataProvider );
    ToolContext toolContext = createToolContext( container );

    this.worker = new ClockFrequencyMeasureTask( toolContext );
    this.worker.setChannel( 0 );
  }

  /**
   * Test method for {@link ClockFrequencyMeasureTask#doInBackground()}.
   */
  @Test
  public void testDoInBackground() throws Exception
  {
    ClockStats result = this.worker.call();

    assertNotNull( result );
    assertEquals( 0.5, result.getDutyCycle(), this.allowedDutyCycleTolerance );
    assertEquals( this.sampleRate, result.getFrequency(), this.allowedFrequencyTolerance );
  }
}
