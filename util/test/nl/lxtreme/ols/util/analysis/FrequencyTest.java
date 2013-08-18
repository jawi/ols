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
package nl.lxtreme.ols.util.analysis;


import static org.junit.Assert.*;
import org.junit.*;


/**
 *
 */
@SuppressWarnings( "boxing" )
public class FrequencyTest
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.analysis.Frequency#getCount(Comparable)}.
   */
  @Test
  public void testGetCount()
  {
    final Frequency<Integer> f = new Frequency<Integer>();

    for ( int i = 0; i <= 10; i++ )
    {
      for ( int j = 0; j < i; j++ )
      {
        f.addValue( Integer.valueOf( i ) );
      }
    }

    assertEquals( 0, f.getCount( 0 ) );
    assertEquals( 1, f.getCount( 1 ) );
    assertEquals( 2, f.getCount( 2 ) );
    assertEquals( 3, f.getCount( 3 ) );
    assertEquals( 4, f.getCount( 4 ) );
    assertEquals( 5, f.getCount( 5 ) );
    assertEquals( 6, f.getCount( 6 ) );
    assertEquals( 7, f.getCount( 7 ) );
    assertEquals( 8, f.getCount( 8 ) );
    assertEquals( 9, f.getCount( 9 ) );
    assertEquals( 10, f.getCount( 10 ) );
    assertEquals( 0, f.getCount( 11 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.analysis.Frequency#getHighestRanked()}.
   */
  @Test
  public void testGetHighestRanked()
  {
    final Frequency<Integer> f = new Frequency<Integer>();

    for ( int i = 0; i <= 10; i++ )
    {
      for ( int j = 0; j < i; j++ )
      {
        f.addValue( Integer.valueOf( i ) );
      }
    }

    assertEquals( Integer.valueOf( 10 ), f.getHighestRanked() );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.analysis.Frequency#getLowestRanked()}.
   */
  @Test
  public void testGetLowestRanked()
  {
    final Frequency<Integer> f = new Frequency<Integer>();

    for ( int i = 0; i <= 10; i++ )
    {
      for ( int j = 0; j < i; j++ )
      {
        f.addValue( Integer.valueOf( i ) );
      }
    }

    assertEquals( Integer.valueOf( 1 ), f.getLowestRanked() );
  }
}
