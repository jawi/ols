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
package nl.lxtreme.ols.api.util;


import static org.junit.Assert.*;

import org.junit.*;


/**
 * Test cases for {@link SizeUnit}.
 */
public class SizeUnitTest
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.SizeUnit#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertEquals( "0.00B", SizeUnit.format( 0 ) );
    assertEquals( "10.00kB", SizeUnit.format( 10240 ) );
    assertEquals( "10.00MB", SizeUnit.format( 10240 * 1024 ) );
    assertEquals( "-10.00MB", SizeUnit.format( -10240 * 1024 ) );
  }
}
