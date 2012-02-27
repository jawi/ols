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
package nl.lxtreme.ols.tool.jtag;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * @author jajans
 * @author Mario Schrenk
 */
public final class JTAGDataSet extends BaseDataSet<JTAGData>
{
  // CONSTANTS

  public static final String JTAG_TDI = "TDI";
  public static final String JTAG_TDO = "TDO";
  public static final String JTAG_TCK = "TCK";
  public static final String JTAG_TMS = "TMS";

  // CONSTRUCTORS

  /**
   * Creates a new JTAGDataSet instance.
   */
  public JTAGDataSet( final int aStartOfDecode, final int aEndOfDecode, final AcquisitionResult aData )
  {
    super( aStartOfDecode, aEndOfDecode, aData );
  }

  // METHODS

  /**
   * @param aTimeValue
   */
  public void reportJTAGState( final int aChannelIdx, final int aStartIdx, final int aEndIdx, final JTAGState aDataValue )
  {
    final int idx = size();
    addData( new JTAGData( idx, aChannelIdx, JTAG_TMS, aDataValue, aStartIdx, aEndIdx ) );
  }
}
