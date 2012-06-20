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
package nl.lxtreme.ols.tool.base.annotation;


import nl.lxtreme.ols.api.data.annotation.*;


/**
 * 
 */
public class SampleDataAnnotation implements DataAnnotation<String>
{
  // VARIABLES

  private final int channelIdx;
  private final long startTimestamp;
  private final long endTimestamp;
  private final String text;

  // CONSTRUCTORS

  /**
   * Creates a new DataAnnotation instance.
   */
  public SampleDataAnnotation( final int aChannelIdx, final long aStartTimestamp, final long aEndTimestamp,
      final String aText )
  {
    this.channelIdx = aChannelIdx;
    this.startTimestamp = aStartTimestamp;
    this.endTimestamp = aEndTimestamp;
    this.text = aText;
  }

  /**
   * Creates a new DataAnnotation instance.
   */
  public SampleDataAnnotation( final int aChannelIdx, final long aStartTimestamp, final String aText )
  {
    this( aChannelIdx, aStartTimestamp, aStartTimestamp + 1, aText );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Annotation<String> aOther )
  {
    int result = ( this.channelIdx - aOther.getChannel() );
    if ( result == 0 )
    {
      if ( aOther instanceof DataAnnotation )
      {
        DataAnnotation<?> dataAnnotation = ( DataAnnotation<?> )aOther;

        result = ( int )( this.startTimestamp - dataAnnotation.getStartTimestamp() );
        if ( result == 0 )
        {
          result = ( int )( this.endTimestamp - dataAnnotation.getEndTimestamp() );
          if ( result == 0 )
          {
            result = getAnnotation().compareTo( aOther.getAnnotation() );
          }
        }
      }
      else
      {
        result = getAnnotation().compareTo( aOther.getAnnotation() );
      }
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAnnotation()
  {
    return this.text;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getChannel()
  {
    return this.channelIdx;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public long getEndTimestamp()
  {
    return this.endTimestamp;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public long getStartTimestamp()
  {
    return this.startTimestamp;
  }
}
