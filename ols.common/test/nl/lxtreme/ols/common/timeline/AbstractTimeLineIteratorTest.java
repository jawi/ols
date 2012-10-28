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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.timeline;


import java.util.*;

import junit.framework.*;


/**
 * Test cases for {@link SampleTimeLineIterator}.
 */
public class AbstractTimeLineIteratorTest extends TestCase
{
  // METHODS

  /**
   * Tests that {@link AbstractTimeLineIterator#peekValue()} points to before
   * the first value after a new iterator has been created.
   */
  public void testAfterCreationPointsToFirstValue()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L );
    assertEquals( 0L, iter.peekValue() );
  }

  /**
   * Tests that {@link AbstractTimeLineIterator#peekValue()} points to the last
   * value after an iterator has been fully iterated.
   */
  public void testAfterFullIterationPointsToLastValue()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L );

    iter.next();
    iter.next();
    iter.next();
    iter.next();

    try
    {
      iter.next();
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 4, iter.peekValue() );
  }

  /**
   * Tests that creating an iterator where the ending timestamp is before the
   * starting timestamp fails.
   */
  public void testCreateInvalidIteratorFail()
  {
    try
    {
      create( 2, 1, new long[] { 1, 2, 3, 4 } );
      fail( "Expected an exception!" );
    }
    catch ( IllegalArgumentException exception )
    {
      // Ok; expected
    }
  }

  /**
   * Tests that {@link AbstractTimeLineIterator#hasNext()} stops when the last
   * timestamp has been reached.
   */
  public void testHasNextStopsAtEndOk()
  {
    AbstractTimeLineIterator iter = create( 1L );

    assertTrue( iter.hasNext() );
    assertEquals( 1, iter.next() );

    assertFalse( iter.hasNext() );
  }

  /**
   * Tests that we cannot iterate over the timeline with a negative delta right
   * from the start.
   */
  public void testInitialIterateBackwordWithDeltaOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L, 5L );

    assertEquals( 0, iter.peekValue() );
    try
    {
      iter.next( -1 );
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 0, iter.peekValue() );
  }

  /**
   * Tests that we cannot iterate over the timeline with a positive delta that
   * is beyond the last value right from the start.
   */
  public void testInitialIterateForwardWithDeltaOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L, 5L );

    assertEquals( 0, iter.peekValue() );
    try
    {
      iter.next( 6 );
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 0, iter.peekValue() );
  }

  /**
   * Tests that we can iterate backward over the timeline with a negative delta.
   */
  public void testIterateBackwordWithDeltaOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L, 5L );

    assertEquals( 0, iter.peekValue() );
    iter.next( 5 );

    assertEquals( 5, iter.peekValue() );
    iter.next( -2 );

    assertEquals( 3, iter.peekValue() );
    iter.next( -2 );

    assertEquals( 1, iter.peekValue() );

    try
    {
      iter.next( -2 );
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 1, iter.peekValue() );
  }

  /**
   * Tests that we can iterate forward over the timeline with a positive delta.
   */
  public void testIterateForwardWithDeltaOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L, 5L );

    assertEquals( 0, iter.peekValue() );
    iter.next( 2 );

    assertEquals( 2, iter.peekValue() );
    iter.next( 2 );

    assertEquals( 4, iter.peekValue() );

    try
    {
      iter.next( 2 );
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 4, iter.peekValue() );
  }

  /**
   * Tests that creating an iterator with a starting timestamp beyond the first
   * value and an ending timestamp before the last value yields the correct
   * subset.
   */
  public void testIterateSubSetOk()
  {
    AbstractTimeLineIterator iter = create( 1, 2, new long[] { 1, 2, 3, 4 } );

    assertEquals( 1, iter.peekValue() );
    iter.next();

    assertEquals( 2, iter.peekValue() );
    iter.next();

    assertEquals( 3, iter.peekValue() );

    try
    {
      iter.next();
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }

    assertEquals( 3, iter.peekValue() );
  }

  /**
   * Tests that {@link AbstractTimeLineIterator#next()} throws an exception when
   * called after the last element.
   */
  public void testNextThrowsExceptionAfterEndOk()
  {
    AbstractTimeLineIterator iter = create( 1L );
    iter.next();

    try
    {
      iter.next();
      fail( "Expected an exception!" );
    }
    catch ( NoSuchElementException exception )
    {
      // Ok; expected...
    }
  }

  /**
   * Tests that calling reset after having marked a position works as expected.
   */
  public void testResetToInitialMarkedPositionOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L );

    iter.mark();

    iter.next(); // 1
    iter.next(); // 2
    iter.next(); // 3
    iter.next(); // 4

    iter.reset();

    assertEquals( 0, iter.peekValue() );
  }

  /**
   * Tests that calling reset after having marked a position works as expected.
   */
  public void testResetToMarkedPositionOk()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L );

    iter.next(); // 1
    iter.next(); // 2

    iter.mark();

    iter.next(); // 3
    iter.next(); // 4

    iter.reset();

    assertEquals( 2, iter.peekValue() );
  }

  /**
   * Tests that calling reset without having marked a position fails with an
   * exception.
   */
  public void testResetWithoutMarkedPositionFail()
  {
    AbstractTimeLineIterator iter = create( 1L, 2L, 3L, 4L );

    iter.next(); // 1
    iter.next(); // 2
    iter.next(); // 3
    iter.next(); // 4

    try
    {
      iter.reset(); // fail!
      fail( "Expected an exception!" );
    }
    catch ( IllegalStateException exception )
    {
      // Ok; expected...
    }

    assertEquals( 4, iter.peekValue() );
  }

  private AbstractTimeLineIterator create( final int startTime, final int endTime, final long... timestamps )
  {
    return new AbstractTimeLineIterator( timestamps[startTime], timestamps[endTime] )
    {
      @Override
      public int peekValue( final long timeValue )
      {
        return ( int )timeValue;
      }
    };
  }

  private AbstractTimeLineIterator create( final long... timestamps )
  {
    return create( 0, timestamps.length - 1, timestamps );
  }
}
