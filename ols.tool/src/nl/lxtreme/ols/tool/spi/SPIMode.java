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


import nl.lxtreme.ols.api.data.*;


/**
 * Mode Numbers
 * <p>
 * The combinations of polarity and phases are often referred to as modes which
 * are commonly numbered according to the following convention, with CPOL as the
 * high order bit and CPHA as the low order bit:
 * </p>
 * 
 * <pre>
 * Mode  CPOL  CPHA
 *   0     0     0
 *   1     0     1
 *   2     1     0
 *   3     1     1
 * </pre>
 */
public enum SPIMode
{
  /**
   * Tries to auto-detect the SPI-mode, by determining what the default state of
   * the clock signal is (either high or low), making it possible to determine
   * either mode 0 or 2.
   */
  AUTODETECT, //
  /**
   * CPOL = 0, CPHA = 0; data is to be captured on the clock's rising edge
   * (low->high transition) and data are propagated on a falling edge (high->low
   * clock transition).
   */
  MODE_0, //
  /**
   * CPOL = 0, CPHA = 1; data is to be captured on the clock's falling edge and
   * data are propagated on a rising edge.
   */
  MODE_1, //
  /**
   * CPOL = 1, CPHA = 0; data is to be captured on clock's falling edge and data
   * are propagated on a rising edge.
   */
  MODE_2, //
  /**
   * CPOL = 1, CPHA = 1; data is to be captured on clock's rising edge and data
   * are propagated on a falling edge.
   */
  MODE_3;

  // METHODS

  /**
   * Returns the CPHA value, which defines whether the data is to be sampled at
   * the odd or even clock edges.
   * 
   * @return '1' if the CPHA value is one (mode 1 and 3), '0' otherwise.
   */
  public int getCPHA()
  {
    return ( ( this == MODE_1 ) || ( this == MODE_3 ) ) ? 1 : 0;
  }

  /**
   * Returns the CPOL value, which defined the clock polarity (default 1 or
   * default 0).
   * 
   * @return '1' is the CPOL value is default high, '0' otherwise.
   */
  public int getCPOL()
  {
    return ( ( this == MODE_2 ) || ( this == MODE_3 ) ) ? 1 : 0;
  }

  /**
   * Returns the data change edge, on which the MISO/MOSI lines are allowed to
   * change.
   * 
   * @param aMode
   *          the SPI mode to return the data change edge for, cannot be
   *          <code>null</code>.
   * @return the data change edge.
   */
  public Edge getDataChangeEdge()
  {
    return getSampleEdge().invert();
  }

  /**
   * Returns the data sample edge, on which the MISO/MOSI lines are to be
   * sampled.
   * 
   * @param aMode
   *          the SPI mode to return the sample edge for, cannot be
   *          <code>null</code>.
   * @return the sample clock edge.
   */
  public Edge getSampleEdge()
  {
    return ( ( this == SPIMode.MODE_0 ) || ( this == SPIMode.MODE_3 ) ) ? Edge.RISING : Edge.FALLING;
  }
}
