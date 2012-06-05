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


/**
 * 
 */
public class DMX512Data extends BaseData<DMX512Data>
{
  // CONSTANTS

  public static final int SYMBOL = 0;
  public static final int FRAME_ERROR = 1;
  public static final int PARITY_ERROR = 2;
  public static final int START_ERROR = 3;

  // VARIABLES

  private final int data;
  private final int type;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512Data} instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   */
  public DMX512Data( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aType )
  {
    this( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aType, -1 );
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
      final int aType, final int aData )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.type = aType;
    this.data = aData;
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

  /**
   * @return
   */
  public boolean isFrameError()
  {
    return FRAME_ERROR == this.type;
  }

  /**
   * @return
   */
  public boolean isParityError()
  {
    return PARITY_ERROR == this.type;
  }

  /**
   * @return
   */
  public boolean isStartError()
  {
    return START_ERROR == this.type;
  }

  /**
   * @return
   */
  public boolean isSymbol()
  {
    return SYMBOL == this.type;
  }
}
