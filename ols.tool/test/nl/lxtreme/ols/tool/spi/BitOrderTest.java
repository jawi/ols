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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.spi;


import static nl.lxtreme.ols.tool.spi.BitOrder.*;

import junit.framework.*;


/**
 * 
 */
public class BitOrderTest extends TestCase
{
  // METHODS

  /**
   * 
   */
  public void testConvertBitOrderOk()
  {
    assertEquals( 256, convertBitOrder( 1, 9, BitOrder.LSB_FIRST ) );
    assertEquals( 1, convertBitOrder( 256, 9, BitOrder.LSB_FIRST ) );
    assertEquals( 1, convertBitOrder( convertBitOrder( 1, 9, BitOrder.LSB_FIRST ), 9, BitOrder.LSB_FIRST ) );

    assertEquals( 1, convertBitOrder( 1, 9, BitOrder.MSB_FIRST ) );
    assertEquals( 128, convertBitOrder( 128, 9, BitOrder.MSB_FIRST ) );
    assertEquals( 1, convertBitOrder( convertBitOrder( 1, 9, BitOrder.MSB_FIRST ), 9, BitOrder.MSB_FIRST ) );

    assertEquals( 256, convertBitOrder( convertBitOrder( 1, 9, BitOrder.LSB_FIRST ), 9, BitOrder.MSB_FIRST ) );
  }

}
