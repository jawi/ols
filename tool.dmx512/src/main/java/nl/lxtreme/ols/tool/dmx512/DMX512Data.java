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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.dmx512;


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;


/**
 * 
 */
public class DMX512Data extends BaseData<DMX512Data>
{
  // VARIABLES

  private final int data;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512Data} instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   */
  public DMX512Data( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final ErrorType aType )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aType.name() );
    this.data = -1;
  }

  /**
   * Creates a new {@link DMX512Data} instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   */
  public DMX512Data( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aData )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.data = aData;
  }

  /**
   * Creates a new {@link DMX512Data} instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   */
  public DMX512Data( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final String aEventName )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aEventName );
    this.data = -1;
  }

  // METHODS

  /**
   * Returns the current value of data.
   * 
   * @return the data
   */
  public int getData()
  {
    return this.data;
  }
}
