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
package nl.lxtreme.ols.util.osgi;


import static org.junit.Assert.*;

import org.junit.*;


/**
 * Tests {@link ManifestHeader}.
 */
public class ManifestHeaderTest
{

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.osgi.ManifestHeader#splitValue()}.
   */
  @Test
  public void testSplitValue()
  {
    ManifestHeader header;

    header = new ManifestHeader( "test", null );
    assertArrayEquals( new String[] {}, header.splitValue() );

    header = new ManifestHeader( "test", "" );
    assertArrayEquals( new String[] {}, header.splitValue() );

    header = new ManifestHeader( "test", "value1" );
    assertArrayEquals( new String[] { "value1" }, header.splitValue() );

    header = new ManifestHeader( "test", "value1,value2,value3" );
    assertArrayEquals( new String[] { "value1", "value2", "value3" }, header.splitValue() );

    header = new ManifestHeader( "test", "value1, value2, value3" );
    assertArrayEquals( new String[] { "value1", "value2", "value3" }, header.splitValue() );

    header = new ManifestHeader( "test", "value1, value2, " );
    assertArrayEquals( new String[] { "value1", "value2" }, header.splitValue() );
  }
}
