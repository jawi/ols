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
package nl.lxtreme.ols.tool.i2c;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * 
 */
public final class I2CDataSet extends BaseDataSet<I2CData>
{
  // CONSTANTS

  public static final String I2C_ACK = "ACK";
  public static final String I2C_BUS_ERROR = "BUS-ERROR";
  public static final String I2C_NACK = "NACK";
  public static final String I2C_START = "START";
  public static final String I2C_STOP = "STOP";

  // VARIABLES

  private int busErrors;
  private int decodedBytes;

  // CONSTRUCTORS

  /**
   * 
   */
  public I2CDataSet( final int aStartSampleIdx, final int aStopSampleIdx, final AcquisitionResult aData )
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
   */
  public void reportACK( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new I2CData( idx, aChannelIdx, aSampleIdx, I2C_ACK ) );
  }

  /**
   * @param aTime
   */
  public void reportBusError( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    this.busErrors++;
    addData( new I2CData( idx, aChannelIdx, aSampleIdx, I2C_BUS_ERROR ) );
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aByteValue )
  {
    final int idx = size();
    this.decodedBytes++;
    addData( new I2CData( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aByteValue ) );
  }

  /**
   * @param aTime
   */
  public void reportNACK( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new I2CData( idx, aChannelIdx, aSampleIdx, I2C_NACK ) );
  }

  /**
   * @param aTime
   */
  public void reportStartCondition( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new I2CData( idx, aChannelIdx, aSampleIdx, I2C_START ) );
  }

  /**
   * @param aTime
   */
  public void reportStopCondition( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new I2CData( idx, aChannelIdx, aSampleIdx, I2C_STOP ) );
  }
}

/* EOF */
