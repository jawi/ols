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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.api.data;


/**
 * Provides a base data implementation.
 * 
 * @param <TYPE>
 *          the actual content type of this base data entity.
 */
public abstract class BaseData<TYPE extends Comparable<? super TYPE>> implements Comparable<TYPE>
{
  // VARIABLES

  private final int idx;
  private final int channelIdx;
  private final int startSampleIdx;
  private final int endSampleIdx;
  private final String eventName;

  // CONSTRUCTORS

  /**
   * Creates a new BaseData instance.
   */
  protected BaseData( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx )
  {
    this( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, null );
  }

  /**
   * Creates a new BaseData instance.
   */
  protected BaseData( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final String aEventName )
  {
    this.idx = aIdx;
    this.channelIdx = aChannelIdx;
    this.startSampleIdx = aStartSampleIdx;
    this.endSampleIdx = aEndSampleIdx;
    this.eventName = aEventName;
  }

  /**
   * Creates a new BaseData instance.
   */
  protected BaseData( final int aIdx, final int aChannelIdx, final int aSampleIdx, final String aEventName )
  {
    this( aIdx, aChannelIdx, aSampleIdx, aSampleIdx, aEventName );
  }

  // METHODS

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( final TYPE aComparable )
  {
    return ( this.idx - ( ( nl.lxtreme.ols.api.data.BaseData<?> )aComparable ).getIndex() );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof BaseData<?> ) )
    {
      return false;
    }

    final BaseData<?> other = ( nl.lxtreme.ols.api.data.BaseData<?> )aObject;
    if ( this.idx != other.idx )
    {
      return false;
    }
    if ( this.channelIdx != other.channelIdx )
    {
      return false;
    }
    if ( this.startSampleIdx != other.startSampleIdx )
    {
      return false;
    }
    if ( this.endSampleIdx != other.endSampleIdx )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the channel index this data belongs to.
   * 
   * @return a channel index, >= 0 && < 32.
   */
  public final int getChannelIdx()
  {
    return this.channelIdx;
  }

  /**
   * Returns the start sample (array) index on which this data/event ended.
   * 
   * @return an array index, >= 0.
   * @see #getStartSampleIndex()
   */
  public final int getEndSampleIndex()
  {
    return this.endSampleIdx;
  }

  /**
   * Returns the event name, in case this data represents an event.
   * 
   * @return the event name, can be <code>null</code>.
   * @see #isEvent()
   */
  public final String getEventName()
  {
    return this.eventName;
  }

  /**
   * Returns the index of this data event/value.
   * 
   * @return the index, zero-based.
   */
  public final int getIndex()
  {
    return this.idx;
  }

  /**
   * Returns the start sample (array) index on which this data/event started.
   * 
   * @return an array index, >= 0.
   * @see #getEndSampleIndex()
   */
  public final int getStartSampleIndex()
  {
    return this.startSampleIdx;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + this.idx;
    result = ( prime * result ) + this.startSampleIdx;
    result = ( prime * result ) + this.endSampleIdx;
    return result;
  }

  /**
   * Returns whether this data represents an event.
   * <p>
   * By default, an event is characterized by having a defined event name.
   * </p>
   * 
   * @return <code>true</code> if this data represents an event,
   *         <code>false</code> otherwise.
   */
  public boolean isEvent()
  {
    return ( this.eventName != null ) && !this.eventName.trim().isEmpty();
  }
}
