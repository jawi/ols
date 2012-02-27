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
package nl.lxtreme.ols.tool.spi;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * @author jajans
 */
public final class SPIDataSet extends BaseDataSet<SPIData>
{
  // CONSTANTS

  public static final String SPI_CS_LOW = "CS_LOW";
  public static final String SPI_CS_HIGH = "CS_HIGH";
  public static final String SPI_MOSI = "MOSI";
  public static final String SPI_MISO = "MISO";
  public static final String SPI_SCK = "SCK";
  public static final String SPI_CS = "/CS";

  // CONSTRUCTORS

  /**
   * Creates a new SPIDataSet instance.
   */
  public SPIDataSet( final int aStartOfDecode, final int aEndOfDecode, final AcquisitionResult aData )
  {
    super( aStartOfDecode, aEndOfDecode, aData );
  }

  // METHODS

  /**
   * @param aTimeValue
   */
  public void reportCSHigh( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new SPIData( idx, aChannelIdx, SPI_CS_HIGH, aSampleIdx ) );
  }

  /**
   * @param aTimeValue
   */
  public void reportCSLow( final int aChannelIdx, final int aSampleIdx )
  {
    final int idx = size();
    addData( new SPIData( idx, aChannelIdx, SPI_CS_LOW, aSampleIdx ) );
  }

  /**
   * @param aTimeValue
   */
  public void reportMisoData( final int aChannelIdx, final int aStartIdx, final int aEndIdx, final int aDataValue )
  {
    final int idx = size();
    addData( new SPIData( idx, aChannelIdx, SPI_MISO, aDataValue, aStartIdx, aEndIdx ) );
  }

  /**
   * @param aTimeValue
   */
  public void reportMosiData( final int aChannelIdx, final int aStartIdx, final int aEndIdx, final int aDataValue )
  {
    final int idx = size();
    addData( new SPIData( idx, aChannelIdx, SPI_MOSI, aDataValue, aStartIdx, aEndIdx ) );
  }
}
