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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay;


import java.awt.*;

import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 * Provides a small DTO for keeping signal hover information together.
 */
public final class MeasurementInfo
{
  // VARIABLES

  private final int channelIdx;
  private final String channelLabel;
  private final double refTime;
  private final Long startTimestamp;
  private final Long transitionTimestamp;
  private final Long endTimestamp;
  private final Rectangle rectangle;
  private final Double highTime;
  private final Double totalTime;
  private final Integer midSamplePos;
  private final boolean hasTimingData;

  // CONSTRUCTORS

  /**
   * Creates a new SignalHoverInfo instance.
   * 
   * @param aElement
   *          the signal element for which this object is created;
   * @param aRefTime
   *          the time stamp of this hover, based on the mouse position.
   */
  public MeasurementInfo( final SignalElement aElement, final double aRefTime )
  {
    this.channelIdx = aElement.getChannel().getIndex();
    this.channelLabel = aElement.getLabel();
    this.rectangle = new Rectangle();
    this.startTimestamp = null;
    this.transitionTimestamp = null;
    this.endTimestamp = null;
    this.refTime = aRefTime;
    this.totalTime = null;
    this.highTime = null;
    this.midSamplePos = null;
    this.hasTimingData = false;
  }

  /**
   * Creates a new {@link MeasurementInfo} instance for the given signal element
   * and without timing data.
   * 
   * @param aElement
   *          the signal element for which this object is created;
   * @param aStartState
   *          the starting state # of the measurement;
   * @param aTransitionState
   *          the state # at with the signal transitions;
   * @param aEndState
   *          the ending state # of the measurement;
   * @param aRefState
   *          the reference state of the measurement;
   * @param aZoomFactor
   *          the zoom factor;
   * @param aSampleRate
   *          the sample rate.
   */
  public MeasurementInfo( final SignalElement aElement, final long aStartState, final long aTransitionState,
      final long aEndState, final double aRefState, final double aZoomFactor, final double aSampleRate )
  {
    this.channelIdx = aElement.getChannel().getIndex();
    this.channelLabel = aElement.getLabel();

    this.startTimestamp = Long.valueOf( aStartState );
    this.endTimestamp = Long.valueOf( aEndState );
    this.transitionTimestamp = Long.valueOf( aTransitionState );

    this.refTime = aRefState;

    this.midSamplePos = Integer.valueOf( ( int )( aTransitionState * aZoomFactor ) );

    this.highTime = null;
    this.totalTime = null;

    this.rectangle = new Rectangle();
    this.rectangle.x = ( int )( aZoomFactor * aStartState );
    this.rectangle.width = ( int )( aZoomFactor * ( aEndState - aStartState ) );
    this.rectangle.y = aElement.getYposition() + aElement.getOffset();
    this.rectangle.height = aElement.getSignalHeight();

    this.hasTimingData = false;
  }

  /**
   * Creates a new {@link MeasurementInfo} instance for the given signal element
   * and with timing data.
   * 
   * @param aElement
   *          the signal element for which this object is created;
   * @param aStartTime
   *          the starting timestamp of the measurement;
   * @param aTransitionTime
   *          the timestamp at with the signal transitions;
   * @param aEndTime
   *          the ending timestamp of the measurement;
   * @param aHighTime
   *          the time the signal is high in the measurement;
   * @param aRefTime
   *          the reference time of the measurement;
   * @param aZoomFactor
   *          the zoom factor;
   * @param aSampleRate
   *          the sample rate.
   */
  public MeasurementInfo( final SignalElement aElement, final long aStartTime, final long aTransitionTime,
      final long aEndTime, final long aHighTime, final double aRefTime, final double aZoomFactor,
      final double aSampleRate )
  {
    this.channelIdx = aElement.getChannel().getIndex();
    this.channelLabel = aElement.getLabel();

    this.startTimestamp = Long.valueOf( aStartTime );
    this.endTimestamp = Long.valueOf( aEndTime );
    this.transitionTimestamp = Long.valueOf( aTransitionTime );

    this.refTime = aRefTime;

    this.midSamplePos = Integer.valueOf( ( int )( aTransitionTime * aZoomFactor ) );

    this.highTime = Double.valueOf( aHighTime / aSampleRate );
    this.totalTime = Double.valueOf( ( aEndTime - aStartTime ) / aSampleRate );

    this.rectangle = new Rectangle();
    this.rectangle.x = ( int )( aZoomFactor * aStartTime );
    this.rectangle.width = ( int )( aZoomFactor * ( aEndTime - aStartTime ) );
    this.rectangle.y = aElement.getYposition() + aElement.getOffset();
    this.rectangle.height = aElement.getSignalHeight();

    this.hasTimingData = true;
  }

  // METHODS

  /**
   * Returns whether or not the given {@link MeasurementInfo} is defined, that
   * is, whether it is non-<code>null</code> and not empty.
   * 
   * @param aMeasurementInfo
   *          the signal hover info to test, may be <code>null</code>.
   * @return <code>true</code> if the given signal hover information is defined,
   *         <code>false</code> otherwise.
   */
  public static boolean isDefined( final MeasurementInfo aMeasurementInfo )
  {
    return ( aMeasurementInfo != null ) && !aMeasurementInfo.isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || ( getClass() != aObject.getClass() ) )
    {
      return false;
    }

    final MeasurementInfo other = ( MeasurementInfo )aObject;
    if ( this.channelIdx != other.channelIdx )
    {
      return false;
    }

    if ( this.endTimestamp == null )
    {
      if ( other.endTimestamp != null )
      {
        return false;
      }
    }
    else if ( !this.endTimestamp.equals( other.endTimestamp ) )
    {
      return false;
    }

    if ( this.midSamplePos == null )
    {
      if ( other.midSamplePos != null )
      {
        return false;
      }
    }
    else if ( !this.midSamplePos.equals( other.midSamplePos ) )
    {
      return false;
    }

    if ( Double.doubleToLongBits( this.refTime ) != Double.doubleToLongBits( other.refTime ) )
    {
      return false;
    }

    if ( this.startTimestamp == null )
    {
      if ( other.startTimestamp != null )
      {
        return false;
      }
    }
    else if ( !this.startTimestamp.equals( other.startTimestamp ) )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the channel index.
   * 
   * @return a channel index, >= 0, never <code>null</code>.
   */
  public int getChannelIndex()
  {
    return this.channelIdx;
  }

  /**
   * Returns the current value of channelLabel.
   * 
   * @return the channelLabel
   */
  public String getChannelLabel()
  {
    return this.channelLabel;
  }

  /**
   * Returns the duty cycle, or the ratio in which the signal is high to the
   * total time of the signal.
   * 
   * @return a duty cycle, as percentage.
   */
  @SuppressWarnings( "boxing" )
  public Double getDutyCycle()
  {
    if ( ( this.highTime == null ) || ( this.totalTime == null ) )
    {
      return null;
    }
    return ( ( 100.0 * this.highTime ) / this.totalTime );
  }

  /**
   * Returns the current value of endTimestamp.
   * 
   * @return the endTimestamp
   */
  public Long getEndTimestamp()
  {
    return this.endTimestamp;
  }

  /**
   * Returns the time the signal is in a non-zero (or high) state.
   * 
   * @return a pulse high time, in seconds.
   */
  public Double getHighTime()
  {
    return this.highTime;
  }

  /**
   * Returns the time the signal is in a zero (or low) state.
   * 
   * @return a pulse low time, in seconds.
   */
  @SuppressWarnings( "boxing" )
  public Double getLowTime()
  {
    if ( ( this.totalTime == null ) || ( this.highTime == null ) )
    {
      return null;
    }
    return this.totalTime - this.highTime;
  }

  /**
   * Returns the current value of middleXpos.
   * 
   * @return the middleXpos
   */
  public Integer getMidSamplePos()
  {
    return this.midSamplePos;
  }

  /**
   * Returns the hover rectangle.
   * 
   * @return the rectangle, never <code>null</code>.
   */
  public Rectangle getRectangle()
  {
    return this.rectangle;
  }

  /**
   * Returns the time value where the mouse cursor is.
   * 
   * @return a reference time value, in seconds.
   */
  public double getReferenceTime()
  {
    return this.refTime;
  }

  /**
   * Returns the current value of startTimestamp.
   * 
   * @return the startTimestamp
   */
  public Long getStartTimestamp()
  {
    return this.startTimestamp;
  }

  /**
   * Returns the width of the total pulse.
   * 
   * @return a total pulse width, in seconds.
   */
  public Double getTotalTime()
  {
    return this.totalTime;
  }

  /**
   * Returns the timestamp in the measurement window at which the signal
   * transitions.
   * 
   * @return the transition timestamp, can be <code>null</code> if no timing or
   *         state data is present.
   */
  public Long getTransitionTimestamp()
  {
    return this.transitionTimestamp;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + this.channelIdx;
    result = ( prime * result ) + ( ( this.endTimestamp == null ) ? 0 : this.endTimestamp.hashCode() );
    result = ( prime * result ) + ( ( this.midSamplePos == null ) ? 0 : this.midSamplePos.hashCode() );
    long temp = Double.doubleToLongBits( this.refTime );
    result = ( prime * result ) + ( int )( temp ^ ( temp >>> 32 ) );
    result = ( prime * result ) + ( ( this.startTimestamp == null ) ? 0 : this.startTimestamp.hashCode() );
    return result;
  }

  /**
   * Returns whether or not there is timing data available.
   * 
   * @return <code>true</code> if the timing data (low, high, total time) is
   *         present, <code>false</code> otherwise.
   */
  public boolean hasTimingData()
  {
    return this.hasTimingData;
  }

  /**
   * Returns whether or not this measurement information is empty, meaning that
   * it doesn't cover any part of a signal.
   * 
   * @return <code>true</code> if <b>no</b> part of a signal is covered,
   *         <code>false</code> otherwise.
   */
  public boolean isEmpty()
  {
    return this.rectangle.isEmpty();
  }
}
