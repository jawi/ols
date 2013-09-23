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
package nl.lxtreme.ols.tool.i2s;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * 
 */
public final class I2SDataSet extends BaseDataSet<I2SData>
{
  // CONSTANTS

  public static final String I2S_BUS_ERROR = "BUS-ERROR";

  // VARIABLES

  private int busErrors;
  private int decodedBytes;

  // CONSTRUCTORS

  /**
   * 
   */
  public I2SDataSet( final int aStartSampleIdx, final int aStopSampleIdx, final AcquisitionResult aData )
  {
    super( aStartSampleIdx, aStopSampleIdx, aData );

    this.busErrors = 0;
    this.decodedBytes = 0;
  }

  // METHODS

  /**
   * Returns the number of bus errors.
   * 
   * @return the number of bus errors, >= 0.
   */
  public int getBusErrorCount()
  {
    return this.busErrors;
  }

  /**
   * Returns the number of decoded bytes.
   * 
   * @return the number of decoded bytes, >= 0.
   */
  public int getDecodedByteCount()
  {
    return this.decodedBytes;
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final Channel aChannel, final long aWordValue )
  {
    final int idx = size();
    this.decodedBytes++;
    addData( new I2SData( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aChannel, aWordValue ) );
  }
}

/* EOF */
