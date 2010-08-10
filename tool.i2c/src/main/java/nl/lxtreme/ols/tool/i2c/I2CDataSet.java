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
package nl.lxtreme.ols.tool.i2c;


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;


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
  public I2CDataSet( final int aStartSampleIdx, final int aStopSampleIdx, final CapturedData aData )
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
  public void reportACK( final long aTime )
  {
    addData( new I2CData( aTime, I2C_ACK, indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   */
  public void reportBusError( final long aTime )
  {
    addData( new I2CData( aTime, I2C_BUS_ERROR, indexToTime( aTime ) ) );
    this.busErrors++;
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  public void reportData( final long aTime, final int aByteValue )
  {
    addData( new I2CData( aTime, aByteValue, indexToTime( aTime ) ) );
    this.decodedBytes++;
  }

  /**
   * @param aTime
   */
  public void reportNACK( final long aTime )
  {
    addData( new I2CData( aTime, I2C_NACK, indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   */
  public void reportStartCondition( final long aTime )
  {
    addData( new I2CData( aTime, I2C_START, indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   */
  public void reportStopCondition( final long aTime )
  {
    addData( new I2CData( aTime, I2C_STOP, indexToTime( aTime ) ) );
  }
}

/* EOF */
