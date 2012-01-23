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


import java.util.*;
import java.util.concurrent.*;


/**
 * Denotes a set of channel annotations for a single channel.
 */
@Deprecated
public class ChannelAnnotations
{
  // VARIABLES

  private final int channel;
  private final SortedSet<ChannelAnnotation> annotations;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelAnnotations instance.
   * 
   * @param aChannel
   *          the index of the sample channel, >= 0 && <
   *          {@value nl.lxtreme.ols.api.Ols#MAX_CHANNELS}.
   */
  public ChannelAnnotations( final int aChannel )
  {
    this.channel = aChannel;
    this.annotations = new ConcurrentSkipListSet<ChannelAnnotation>();
  }

  // METHODS

  /**
   * Adds a new annotation to this container.
   * 
   * @param aStartIdx
   *          the start sample index of the annotation to add;
   * @param aEndIdx
   *          the end sample index of the annotation to add;
   * @param aData
   *          the actual annotation data of the annotation to add.
   */
  public void addAnnotation( final long aStartTimestamp, final long aEndTimestamp, final Object aData )
  {
    this.annotations.add( new ChannelAnnotation( aStartTimestamp, aEndTimestamp, aData ) );
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
    if ( ( aObject == null ) || !( aObject instanceof ChannelAnnotations ) )
    {
      return false;
    }

    final ChannelAnnotations other = ( ChannelAnnotations )aObject;
    if ( this.channel != other.channel )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the annotation at the given time index.
   * 
   * @param aTimeIndex
   *          the index of the timestamps to return the annotation for, >= 0.
   * @return a channel annotation valid at the given time index, or
   *         <code>null</code> if no such annotation was present.
   */
  public ChannelAnnotation getAnnotation( final int aTimeIndex )
  {
    for ( ChannelAnnotation annotation : this.annotations )
    {
      if ( annotation.isInRange( aTimeIndex ) )
      {
        return annotation;
      }
    }
    return null;
  }

  /**
   * @return the annotations
   */
  public Collection<ChannelAnnotation> getAnnotations()
  {
    return this.annotations;
  }

  /**
   * Returns all annotations that are "visible" in the range of the given start
   * and end index.
   * 
   * @param aStartIdx
   *          the start index of the "visible" range, >= 0;
   * @param aEndIdx
   *          the end index of the "visible" range, >= 0.
   * @return an iterator of all "visible" channel annotations, never
   *         <code>null</code>.
   */
  public Iterator<ChannelAnnotation> getAnnotations( final int aStartIdx, final int aEndIdx )
  {
    // Try to find the exact starting & ending index in our own
    // administration...
    final List<ChannelAnnotation> result = new ArrayList<ChannelAnnotation>();
    for ( ChannelAnnotation ann : this.annotations )
    {
      if ( ann.isInRange( aStartIdx, aEndIdx ) )
      {
        result.add( ann );
      }
    }

    // Craft an iterator that walks between the determined boundries...
    return result.iterator();
  }

  /**
   * @return the channel
   */
  public int getChannel()
  {
    return this.channel;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + this.channel;
    return result;
  }
}
