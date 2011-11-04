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


import nl.lxtreme.ols.api.tools.*;


/**
 * 
 */
public class SampleDataAnnotation implements DataAnnotation<String>
{
  // VARIABLES

  private final int channelIdx;
  private final int startSampleIdx;
  private final int endSampleIdx;
  private final String text;

  // CONSTRUCTORS

  /**
   * Creates a new DataAnnotation instance.
   */
  public SampleDataAnnotation( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final String aText )
  {
    this.channelIdx = aChannelIdx;
    this.startSampleIdx = aStartSampleIdx;
    this.endSampleIdx = aEndSampleIdx;
    this.text = aText;
  }

  // METHODS

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
  public int getEndSampleIndex()
  {
    return this.endSampleIdx;
  }

  /**
   * {@inheritDoc}
   */
  public int getStartSampleIndex()
  {
    return this.startSampleIdx;
  }
}
