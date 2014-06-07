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
package nl.lxtreme.ols.tool.can;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * Represents all decoded CAN-bus data.
 */
public class CanBusDataSet extends BaseDataSet<CanBusData>
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link CanBusDataSet} instance.
   * 
   * @param aStartOfDecodeIdx
   *          the sample index where the CAN-data decoding is started, &gt;= 0;
   * @param aEndOfDecodeIdx
   *          the sample index until which the CAN-data is decoded, &gt;=
   *          aStartOfDecodeIdx;
   * @param aData
   *          the acquisition data that is used as source for the decoding
   *          process.
   */
  public CanBusDataSet( int aStartOfDecodeIdx, int aEndOfDecodeIdx, AcquisitionData aData )
  {
    super( aStartOfDecodeIdx, aEndOfDecodeIdx, aData );
  }

}
