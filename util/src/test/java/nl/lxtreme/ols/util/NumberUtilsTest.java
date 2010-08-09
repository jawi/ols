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
package nl.lxtreme.ols.util;


import static org.junit.Assert.*;
import nl.lxtreme.ols.util.NumberUtils.*;

import org.junit.*;


/**
 * @author jawi
 *
 */
public class NumberUtilsTest
{
  // METHODS

  /**
   * 
   */
  @Test
  public void testSmartParseIntBinaryUnitOk()
  {
    assertEquals( 4096, NumberUtils.smartParseInt( "4k" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4K" ) );
    assertEquals( 4096 * 1024, NumberUtils.smartParseInt( "4M" ) );
    assertEquals( 4096 * 1024, NumberUtils.smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntFail()
  {
    assertEquals( 0, NumberUtils.smartParseInt( "test" ) );
    assertEquals( 0, NumberUtils.smartParseInt( "" ) );
    assertEquals( 1, NumberUtils.smartParseInt( "", 1 ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntOk()
  {
    assertEquals( -1, NumberUtils.smartParseInt( "-1" ) );
    assertEquals( 1, NumberUtils.smartParseInt( "1" ) );
    assertEquals( 2, NumberUtils.smartParseInt( "2 " ) );
    assertEquals( 3, NumberUtils.smartParseInt( "3 4" ) );
    assertEquals( 4, NumberUtils.smartParseInt( "4,5" ) );
    assertEquals( 4, NumberUtils.smartParseInt( "4Hz" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4k" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4K" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4M" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntSIUnitOk()
  {
    assertEquals( 4000, NumberUtils.smartParseInt( "4k", UnitDefinition.SI ) );
    assertEquals( 4000, NumberUtils.smartParseInt( "4K", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  MB", UnitDefinition.SI ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntWithNullArgument()
  {
    assertEquals( 0, NumberUtils.smartParseInt( null ) );
    assertEquals( -1, NumberUtils.smartParseInt( null, -1 ) );
  }
}
