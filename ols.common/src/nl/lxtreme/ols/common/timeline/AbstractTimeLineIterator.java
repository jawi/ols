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
import java.util.concurrent.atomic.*;


/**
 * Provides a {@link AbstractTimeLineIterator} iterator, which allows you to
 * iterate between a given start- and ending time stamp in single or arbitrary
 * increments.
 * <p>
 * This class supports a single marker on the timeline, that can be set using
 * {@link #mark()}. One can return to this position later on by calling
 * {@link #reset()}.
 * </p>
 * <p>
 * This class is thread-safe.
 * </p>
 */
public abstract class AbstractTimeLineIterator
{
  // VARIABLES

  private final AtomicLong time;

  protected final long startTime;
  protected final long endTime;

  private volatile long mark;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AbstractTimeLineIterator} instance.
   * 
   * @param aStartTime
   *          the start timestamp, >= 0L;
   * @param aEndTime
   *          the end timestamp, >= 0L.
   */
  protected AbstractTimeLineIterator( final long aStartTime, final long aEndTime )
  {
    if ( aStartTime > aEndTime )
    {
      throw new IllegalArgumentException( "StartTime cannot be beyond EndTime!" );
    }

    this.startTime = aStartTime;
    this.endTime = aEndTime;

    // set the time postion to before the starting time...
    this.time = new AtomicLong( aStartTime - 1L );
    // set the initial marker before that...
    this.mark = aStartTime - 2L;
  }

  // METHODS

  /**
   * Returns the current position of the timeline.
   * 
   * @return the current timestamp, can be
   *         <em>before<em> the start value, but never beyond the end value.
   */
  public long getTimestamp()
  {
    return this.time.get();
  }

  /**
   * Returns whether or not we can safely call {@link #next()} without going out
   * of bounds.
   * 
   * @return <code>true</code> if {@link #next()} can be called safely,
   *         <code>false</code> if the end of the timeline has been reached.
   * @see Iterator#hasNext()
   */
  public boolean hasNext()
  {
    return this.time.get() < this.endTime;
  }

  /**
   * Marks the current time. Subsequent calls to {@link #reset()} will attempt
   * to reposition the timeline to this position.
   * 
   * @return the marked time position.
   */
  public long mark()
  {
    return this.mark = this.time.get();
  }

  /**
   * Increments the timeline with a single step, and returns the value at the
   * <em>new</em> position.
   * 
   * @return the value at the next timeline position.
   * @throws NoSuchElementException
   *           in case we are outside the timeline boundaries.
   * @see Iterator#next()
   */
  public int next()
  {
    return next( 1L );
  }

  /**
   * Increments the timeline with a given step, and returns the value at the
   * <em>new</em> position.
   * 
   * @param aDelta
   *          the step increment to add to the timeline, can also be negative.
   * @return the value at the next timeline position.
   * @throws NoSuchElementException
   *           in case we are outside the timeline boundaries.
   * @see Iterator#next()
   */
  public int next( final long aDelta )
  {
    long time = this.time.addAndGet( aDelta );
    if ( time < this.startTime )
    {
      // We've walked beyond the first value; increment it to let peekValue
      // yield the last value...
      while ( !this.time.compareAndSet( time, time + Math.abs( aDelta ) ) )
      {
        time = this.time.get();
      }
      throw new NoSuchElementException();
    }
    if ( time > this.endTime )
    {
      // We've walked beyond the last value; decrement it to let peekValue
      // yield the last value...
      while ( !this.time.compareAndSet( time, time - Math.abs( aDelta ) ) )
      {
        time = this.time.get();
      }
      throw new NoSuchElementException();
    }
    return peekValue( time );
  }

  /**
   * Returns the sample value for the <em>current</em> time stamp.
   * <p>
   * This method is the same as calling
   * <code>peekValue( getTimestamp() );</code>.
   * </p>
   * 
   * @return the data value at the current time stamp.
   */
  public int peekValue()
  {
    return peekValue( this.time.get() );
  }

  /**
   * Returns the sample value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value at the given time value.
   */
  public abstract int peekValue( final long aTimeValue );

  /**
   * Resets the position of this timeline to the position marked earlier.
   * <p>
   * Multiple calls to this method are allowed to return to the same position on
   * over and over again.
   * </p>
   * 
   * @throws IllegalStateException
   *           in case no mark was set.
   */
  public void reset()
  {
    long currentMark = this.mark;
    if ( currentMark < ( this.startTime - 1L ) )
    {
      throw new IllegalStateException( "No mark is set!" );
    }
    long oldTime = this.time.get();
    while ( !this.time.compareAndSet( oldTime, currentMark ) )
    {
      oldTime = this.time.get();
    }
  }
}
