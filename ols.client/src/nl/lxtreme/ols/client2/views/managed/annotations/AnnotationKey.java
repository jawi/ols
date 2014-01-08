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
package nl.lxtreme.ols.client2.views.managed.annotations;


import java.util.*;

import nl.lxtreme.ols.common.annotation.*;


/**
 * Provides a customer key for use in a {@link Map}.
 */
class AnnotationKey implements Comparable<AnnotationKey>
{
  // VARIABLES

  final long startTime;
  final long endTime;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationKey} instance.
   */
  public AnnotationKey( final DataAnnotation aAnnotation )
  {
    this( aAnnotation.getStartTimestamp(), aAnnotation.getEndTimestamp() );
  }

  /**
   * Creates a new {@link AnnotationKey} instance.
   */
  AnnotationKey( final long aStartTime, final long aEndTime )
  {
    this.startTime = aStartTime;
    this.endTime = aEndTime;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final AnnotationKey aOtherKey )
  {
    int result = ( int )( this.startTime - aOtherKey.startTime );
    if ( result == 0 )
    {
      result = ( int )( aOtherKey.endTime - this.endTime );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object obj )
  {
    if ( this == obj )
    {
      return true;
    }
    if ( ( obj == null ) || !( obj instanceof AnnotationKey ) )
    {
      return false;
    }

    final AnnotationKey other = ( AnnotationKey )obj;
    if ( ( this.endTime != other.endTime ) || ( this.startTime != other.startTime ) )
    {
      return false;
    }

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( int )( this.endTime ^ ( this.endTime >>> 32 ) );
    result = ( prime * result ) + ( int )( this.startTime ^ ( this.startTime >>> 32 ) );
    return result;
  }
}
