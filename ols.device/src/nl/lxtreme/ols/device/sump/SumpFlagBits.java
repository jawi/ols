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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump;


/**
 * Represents the various bits in the "flags" register used in the SUMP
 * protocol.
 */
public enum SumpFlagBits
{
  /** Demux Mode (DDR sample mode). */
  DEMUX_MODE( 0x0001 ),
  /** Enable/disable noise filter. */
  NOISE_FILTER( 0x0002 ),
  /** Disable channel group 1. */
  DISABLE_GROUP_1( 0x0004 ),
  /** Disable channel group 2. */
  DISABLE_GROUP_2( 0x0008 ),
  /** Disable channel group 3. */
  DISABLE_GROUP_3( 0x0010 ),
  /** Disable channel group 4. */
  DISABLE_GROUP_4( 0x0020 ),
  /** Disabled channel groups. */
  DISABLED_GROUPS( DISABLE_GROUP_1.mask | DISABLE_GROUP_2.mask | DISABLE_GROUP_3.mask | DISABLE_GROUP_4.mask ),
  /** External clock source. */
  EXTERNAL_CLOCK_SOURCE( 0x0040 ),
  /** Use inverted capture clock (capture on falling edge). */
  INVERT_CLOCK_SOURCE( 0x0080 ),
  /** Enable RLE compression. */
  ENABLE_RLE_MODE( 0x0100 ),
  /** Swap number scheme (swap upper/lower 16 bits). */
  SWAP_NUMBER_SCHEME( 0x0200 ),
  /** External test mode (output pattern on bits 31:16). */
  EXTERNAL_TEST_MODE( 0x0400 ),
  /** External test mode (output pattern on bits 31:16). */
  INTERNAL_TEST_MODE( 0x0800 ),
  /** Issue values & rle-count as pairs (backwards compatible). */
  RLE_MODE_1( 0x4000 ),
  /** Periodic mode. Values reissued approximately every 256 rle-count fields. */
  RLE_MODE_2( 0x8000 ),
  /** Unlimited mode. Values can be followed by unlimited number of rle-count's. */
  RLE_MODE_3( 0xC000 ),
  /** Extra RLE compression modes. */
  RLE_COMPRESSION_MODE( 0xC000 );

  private final int mask;

  private SumpFlagBits( int aMask )
  {
    this.mask = aMask;
  }

  public int apply( int aInput, boolean aEnable )
  {
    if ( aEnable )
    {
      return aInput | this.mask;
    }
    return aInput & ~this.mask;
  }

  /**
   * @return the bit-mask of this flag.
   */
  public int getMask()
  {
    return this.mask;
  }

  public boolean isSet( int aInput )
  {
    return ( aInput & this.mask ) != 0;
  }
}
