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


import nl.lxtreme.ols.api.data.BaseData;


/**
 * Class for JTAG dataset
 * 
 * @author Frank Kunz
 * @author J.W. Janssen
 * @author Mario Schrenk
 */
public final class JTAGData extends BaseData<JTAGData>
{
  // VARIABLES

  private final Object dataValue;
  private final String dataName;

  // CONSTRUCTORS

  /**
   * @param aTime
   * @param aMoSiValue
   * @param aMiSoValue
   */
  public JTAGData( final int aIdx, final int aChannelIdx, JTAGState aState,
      final int aStartSampleIdx, final int aEndSampleIdx )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, JTAGDataSet.JTAG_TMS );
    this.dataName = JTAGDataSet.JTAG_TMS;
    this.dataValue = aState;
  }

  /**
   * @param aTime
   * @param aMoSiValue
   * @param aMiSoValue
   */
  public JTAGData( final int aIdx, final int aChannelIdx, final String aDataName, final Object aDataValue,
      final int aStartSampleIdx, final int aEndSampleIdx )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.dataName = aDataName;
    this.dataValue = aDataValue;
  }

  // METHODS

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final JTAGData aComparable )
  {
    return ( getStartSampleIndex() - aComparable.getStartSampleIndex() );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( !super.equals( aObject ) || !( aObject instanceof JTAGData ) )
    {
      return false;
    }

    final JTAGData other = ( JTAGData )aObject;
    if ( this.dataValue != other.dataValue )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns whether this data is representing TDI-data, or TDO-data.
   * 
   * @return the data name, can be <code>null</code>.
   */
  public String getDataName()
  {
    return this.dataName;
  }

  /**
   * @return the TDO/TDI data value.
   */
  public final Object getDataValue()
  {
    return this.dataValue;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    if ( this.dataValue != null )
    {
      result = prime * result + this.dataValue.hashCode();
    }
    return result;
  }

  /**
   * @return
   */
  public boolean isData()
  {
    return ( this.dataName != null ) && !this.dataName.trim().isEmpty();
  }

  /**
   * @return
   */
  public final boolean isTdiData()
  {
    return JTAGDataSet.JTAG_TDI.equals( this.dataName );
  }

  /**
   * @return
   */
  public final boolean isTdoData()
  {
    return JTAGDataSet.JTAG_TDO.equals( this.dataName );
  }
}
