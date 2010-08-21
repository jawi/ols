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
package nl.lxtreme.ols.api.data;


import java.util.*;


/**
 * @author jawi
 */
public class ChannelAnnotations
{
  // VARIABLES

  private final int channel;
  private final List<ChannelAnnotation> annotations;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelAnnotations instance.
   */
  public ChannelAnnotations( final int aChannel )
  {
    this.channel = aChannel;
    this.annotations = new ArrayList<ChannelAnnotation>();
  }

  // METHODS

  /**
   * @param aStartIdx
   * @param aEndIdx
   * @param aData
   */
  public void addAnnotation( final long aStartIdx, final long aEndIdx, final Object aData )
  {
    this.annotations.add( new ChannelAnnotation( aStartIdx, aEndIdx, aData ) );
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
   * @param aTimeIndex
   * @return
   */
  public ChannelAnnotation getAnnotation( final long aTimeIndex )
  {
    for ( ChannelAnnotation annotation : this.annotations )
    {
      if ( ( annotation.getStartIndex() <= aTimeIndex ) && ( annotation.getEndIndex() >= aTimeIndex ) )
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
   * @return the channel
   */
  public int getChannel()
  {
    return this.channel;
  }

  /**
   * @param aStartIdx
   * @param aEndIdx
   * @return
   */
  public Iterator<ChannelAnnotation> getIterator()
  {
    // Craft an iterator that walks between the determined boundries...
    return this.annotations.iterator();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.channel;
    return result;
  }
}
