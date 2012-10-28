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
package org.sump.device.logicsniffer.protocol;


/**
 * 
 */
public class DemonCoreCommandWriterTest
{

  public void test()
  {
    // for ( int i = Integer.MIN_VALUE; i < Integer.MAX_VALUE; i++ )
    // {
    // assertEquals( "Failed for value: " + i, Integer.bitCount( i ), test( i )
    // );
    // }
  }

  /**
   * @param aMask
   * @return
   */
  final int test( final int aMask )
  {
    int bitcount = 0;
    int bitmask = 1;
    for ( int i = 0; i < 32; i++, bitmask <<= 1 )
    {
      if ( ( aMask & bitmask ) != 0 )
      {
        bitcount++;
      }
    }
    return bitcount;
  }

  /**
   * @throws java.lang.Exception
   */
  protected void setUp() throws Exception
  {
  }

}
