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
package nl.lxtreme.ols.tool.onewire;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a 1-Wire data set.
 */
public class OneWireDataSet extends BaseDataSet<OneWireData>
{
  // CONSTANTS

  public static final String OW_RESET = "RESET";
  public static final String OW_BUS_ERROR = "BUS-ERROR";

  // VARIABLES

  private int busErrors;
  private int decodedBytes;

  // CONSTRUCTORS

  /**
   * @param aStartOfDecodeIdx
   * @param aEndOfDecodeIdx
   * @param aData
   */
  public OneWireDataSet( final int aStartOfDecodeIdx, final int aEndOfDecodeIdx, final AcquisitionResult aData )
  {
    super( aStartOfDecodeIdx, aEndOfDecodeIdx, aData );

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
   * Reports a bus error event.
   * 
   * @param aChannelIdx
   *          the channel index on which the bus error event occurred;
   * @param aSampleIdx
   *          the sample index on which the bus error event occurred.
   */
  public void reportBusError( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    this.busErrors++;
    addData( new OneWireData( idx, aChannelIdx, aSampleIdx, OW_BUS_ERROR, false /* aSlavePresent */) );
  }

  /**
   * Reports a data value.
   * 
   * @param aChannelIdx
   *          the channel index;
   * @param aStartSampleIdx
   *          the start sample index of the data;
   * @param aEndSampleIdx
   *          the end sample index of the data;
   * @param aByteValue
   *          the decoded data value.
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aByteValue )
  {
    final int idx = size();
    this.decodedBytes++;
    addData( new OneWireData( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aByteValue ) );
  }

  /**
   * Reports a reset event.
   * 
   * @param aChannelIdx
   *          the channel index on which the reset event occurred;
   * @param aSampleIdx
   *          the sample index on which the reset event occurred;
   * @param aEndSampleIdx
   *          the end sample index on which the reset event occurred;
   * @param aSlaveIsPresent
   *          <code>true</code> if a slave responsed to the master reset,
   *          <code>false</code> if no slave responded to the master reset.
   */
  public void reportReset( final int aChannelIdx, final int aSampleIdx, final int aEndSampleIdx,
      final boolean aSlaveIsPresent )
  {
    final int idx = size();
    addData( new OneWireData( idx, aChannelIdx, aSampleIdx, OW_RESET, aSlaveIsPresent ) );
  }
}
