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
package nl.lxtreme.ols.util.swing;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import junit.framework.*;


/**
 * 
 */
public class SwingComponentUtilsTest extends TestCase
{
  // METHODS

  /**
   * 
   */
  public void testSmartParseIntBinaryUnitOk()
  {
    assertEquals( 4096, smartParseInt( "4k", 0 ) );
    assertEquals( 4096, smartParseInt( "4K", 0 ) );
    assertEquals( 4096 * 1024, smartParseInt( "4M", 0 ) );
    assertEquals( 4096 * 1024, smartParseInt( "4  M", 0 ) );
  }

  /**
   * 
   */
  public void testSmartParseIntFail()
  {
    assertEquals( 0, smartParseInt( "test", 0 ) );
    assertEquals( 0, smartParseInt( "", 0 ) );
    assertEquals( 1, smartParseInt( "", 1 ) );
  }

  /**
   * 
   */
  public void testSmartParseIntOk()
  {
    assertEquals( -1, smartParseInt( "-1", 0 ) );
    assertEquals( 1, smartParseInt( "1", 0 ) );
    assertEquals( 2, smartParseInt( "2 ", 0 ) );
    assertEquals( 3, smartParseInt( "3 4", 0 ) );
    assertEquals( 4, smartParseInt( "4,5", 0 ) );
    assertEquals( 4, smartParseInt( "4Hz", 0 ) );
    assertEquals( 4096, smartParseInt( "4k", 0 ) );
    assertEquals( 4096, smartParseInt( "4K", 0 ) );
    assertEquals( 4194304, smartParseInt( "4M", 0 ) );
    assertEquals( 4194304, smartParseInt( "4  M", 0 ) );
  }

  /**
   * 
   */
  public void testSmartParseIntSIUnitOk()
  {
    assertEquals( 4000, smartParseInt( "4k", UnitDefinition.SI, 0 ) );
    assertEquals( 4000, smartParseInt( "4K", UnitDefinition.SI, 0 ) );
    assertEquals( 4000000, smartParseInt( "4M", UnitDefinition.SI, 0 ) );
    assertEquals( 4000000, smartParseInt( "4  M", UnitDefinition.SI, 0 ) );
    assertEquals( 4000000, smartParseInt( "4  MB", UnitDefinition.SI, 0 ) );
  }

  /**
   * 
   */
  public void testSmartParseIntWithNullArgument()
  {
    assertEquals( 0, smartParseInt( null, 0 ) );
    assertEquals( -1, smartParseInt( null, -1 ) );
  }

}
