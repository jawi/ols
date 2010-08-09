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
package nl.lxtreme.ols.tool.spi;


import java.util.*;


/**
 * @author jajans
 *
 */
public class SPIDataSet
{
  // CONSTANTS

  public static final String SPI_CS_LOW = "CSLOW";
  public static final String SPI_CS_HIGH = "CSHIGH";

  // VARIABLES

  private final int startOfDecode;
  private final int endOfDecode;
  private final List<SPIData> data;

  // CONSTRUCTORS

  /**
   * Creates a new SPIDataSet instance.
   */
  public SPIDataSet( final int aStartOfDecode, final int aEndOfDecode )
  {
    this.startOfDecode = aStartOfDecode;
    this.endOfDecode = aEndOfDecode;

    this.data = new ArrayList<SPIData>();
  }

  // METHODS

  /**
   * @return
   */
  public List<SPIData> getDecodedData()
  {
    return this.data;
  }

  /**
   * @return
   */
  public int getEndOfDecode()
  {
    return this.endOfDecode;
  }

  /**
   * @return
   */
  public int getStartOfDecode()
  {
    return this.startOfDecode;
  }

  /**
   * @return
   */
  public boolean isEmpty()
  {
    return this.data.isEmpty();
  }

  /**
   * @param aTimeValue
   */
  public void reportCSHigh( final long aTimeValue )
  {
    this.data.add( new SPIData( aTimeValue, SPI_CS_HIGH ) );
  }

  /**
   * @param aTimeValue
   */
  public void reportCSLow( final long aTimeValue )
  {
    this.data.add( new SPIData( aTimeValue, SPI_CS_LOW ) );
  }

  /**
   * @param aTimeValue
   */
  public void reportData( final long aTimeValue, final int aMiSoValue, final int aMoSiValue )
  {
    this.data.add( new SPIData( aTimeValue, aMoSiValue, aMiSoValue ) );
  }
}
