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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.api.util;


import static org.junit.Assert.*;


/**
 * Base class for testing {@link Unit}-related classes.
 */
public abstract class UnitTestBase
{
  // METHODS

  /**
   * @param expectedValue
   * @param expectedDisplayName
   * @param actual
   */
  protected void assertUnitEquals( String expectedValue, String expectedDisplayName, String actual )
  {
    String expected = Unit.format( expectedValue, expectedDisplayName );
    assertEquals( expected, actual );
  }
}
