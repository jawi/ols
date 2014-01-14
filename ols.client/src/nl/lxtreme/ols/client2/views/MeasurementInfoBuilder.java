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
package nl.lxtreme.ols.client2.views;


import java.awt.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a builder for creating new instances of {@link MeasurementInfo}.
 */
public class MeasurementInfoBuilder
{
  // INNER TYPES

  /**
   * Provides the actual measurement information.
   */
  public static final class MeasurementInfo
  {
    // VARIABLES

    private Channel channel;
    private double refTime;
    private Long startTimestamp;
    private Long transitionTimestamp;
    private Long endTimestamp;
    private Rectangle rectangle;
    private Double highTime;
    private Double totalTime;
    private Integer midSamplePos;
    private boolean hasTimingData;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MeasurementInfo} instance.
     */
    private MeasurementInfo()
    {
      this.rectangle = new Rectangle();
    }

    // METHODS

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
      if ( !this.channel.equals( other.channel ) )
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
     * Returns the channel.
     * 
     * @return a channel, never <code>null</code>.
     */
    public Channel getChannel()
    {
      return this.channel;
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
     * @return the transition timestamp, can be <code>null</code> if no timing
     *         or state data is present.
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
      result = ( prime * result ) + ( ( this.channel == null ) ? 0 : this.channel.hashCode() );
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
     * Returns whether or not this measurement information is empty, meaning
     * that it doesn't cover any part of a signal.
     * 
     * @return <code>true</code> if <b>no</b> part of a signal is covered,
     *         <code>false</code> otherwise.
     */
    public boolean isEmpty()
    {
      return this.rectangle.isEmpty();
    }
  }

  // VARIABLES

  private final AcquisitionData data;
  private final double zoomFactor;

  private Channel channel;
  private double refTime;
  private long start;
  private long end;
  private long transition;
  private long high;
  private int yPosition; // in px.
  private int height; // in px.

  // CONSTRUCTORS

  /**
   * Creates a new {@link MeasurementInfoBuilder} instance.
   */
  public MeasurementInfoBuilder( AcquisitionData aData, double aZoomFactor )
  {
    this.data = aData;
    this.zoomFactor = aZoomFactor;
  }

  // METHODS

  /**
   * @return the actual {@link MeasurementInfo}, never <code>null</code>.
   */
  public MeasurementInfo build()
  {
    double sr = this.data.getSampleRate();

    MeasurementInfo result = new MeasurementInfo();
    result.channel = this.channel;
    result.refTime = this.refTime;
    result.hasTimingData = this.data.hasTimingData();
    result.startTimestamp = this.start;
    result.endTimestamp = this.end;
    result.transitionTimestamp = this.transition;
    result.midSamplePos = ( int )( this.transition * this.zoomFactor );
    if ( result.hasTimingData )
    {
      result.highTime = this.high / sr;
      result.totalTime = ( this.end - this.start ) / sr;
    }
    result.rectangle.x = ( int )( this.zoomFactor * this.start );
    result.rectangle.width = ( int )( this.zoomFactor * ( this.end - this.start ) );
    result.rectangle.y = this.yPosition;
    result.rectangle.height = this.height;

    return result;
  }

  public MeasurementInfoBuilder setChannel( Channel aChannel )
  {
    this.channel = aChannel;
    return this;
  }

  public MeasurementInfoBuilder setEndTime( long aEndTime )
  {
    this.end = aEndTime;
    return this;
  }

  public MeasurementInfoBuilder setHeight( int aHeight )
  {
    this.height = aHeight;
    return this;
  }

  public MeasurementInfoBuilder setHighTime( long aHighTime )
  {
    this.high = aHighTime;
    return this;
  }

  public MeasurementInfoBuilder setReferenceTime( double aReferenceTime )
  {
    this.refTime = aReferenceTime;
    return this;
  }

  public MeasurementInfoBuilder setStartTime( long aStartTime )
  {
    this.start = aStartTime;
    return this;
  }

  public MeasurementInfoBuilder setTransitionTime( long aTransitionTime )
  {
    this.transition = aTransitionTime;
    return this;
  }

  public MeasurementInfoBuilder setYposition( int aYpos )
  {
    this.yPosition = aYpos;
    return this;
  }
}
