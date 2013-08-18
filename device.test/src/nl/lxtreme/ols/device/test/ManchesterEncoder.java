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
package nl.lxtreme.ols.device.test;


import java.util.*;


/**
 * Represents a Manchester line-encoder.
 */
final class ManchesterEncoder
{
  // CONSTANTS

  private static final int LINE_IDX = 0x02;

  private final List<Integer> data;
  private final int sampleRate;
  private final int clockSpeed;
  private final int bitBoundary;
  private final int setupPoint;
  private final int trigger;

  // CONSTRUCTORS

  /**
   * Creates a new ManchesterEncoder instance.
   */
  public ManchesterEncoder()
  {
    this.data = new ArrayList<Integer>();

    this.clockSpeed = 1020304; // 1.020304MHz
    this.sampleRate = 2750000; // 2.75MHz

    this.bitBoundary = ( int )( Math.rint( this.sampleRate / ( double )this.clockSpeed ) );
    this.setupPoint = this.bitBoundary / 2;

    // Write some initial stuff bits...
    for ( int i = 0; i < ( 5 * this.bitBoundary ); i++ )
    {
      this.data.add( Integer.valueOf( 1 << LINE_IDX ) );
    }

    this.trigger = this.data.size();

    // Synchronize byte...
    write( ( byte )0xaa );
  }

  // METHODS

  /**
   * @return the encoded data as array, never <code>null</code>.
   */
  public int[] getData()
  {
    final int size = this.data.size();
    final int[] result = new int[size];
    for ( int i = 0; i < size; i++ )
    {
      result[i] = this.data.get( i ).intValue();
    }
    return result;
  }

  /**
   * @return the sample rate, in Hertz.
   */
  public int getRate()
  {
    return this.sampleRate;
  }

  /**
   * @return the trigger position, as sample index.
   */
  public int getTrigger()
  {
    return this.trigger;
  }

  /**
   * @param aString
   *          the data to write, cannot be <code>null</code>.
   */
  public void writeData( final String aString )
  {
    final byte[] bytes = aString.getBytes();
    for ( byte b : bytes )
    {
      write( b );
    }

    // Some additional stuff bits...
    for ( int i = 0; i < ( 3 * this.bitBoundary ); i++ )
    {
      this.data.add( Integer.valueOf( 1 << LINE_IDX ) );
    }
    this.data.add( Integer.valueOf( 1 ) );
  }

  /**
   * @param aByte
   *          the byte to encode, cannot be <code>null</code>.
   */
  private void write( final byte aByte )
  {
    for ( int i = 7; i >= 0; i-- )
    {
      int b = ( aByte & ( 1 << i ) );
      writeBit( b );
    }
  }

  /**
   * @param aBitValue
   *          the bit value to encode, should be either 0 or 1.
   */
  private void writeBit( final int aBitValue )
  {
    if ( aBitValue != 0 )
    {
      // Logical one...
      writeManchesterOne();
    }
    else
    {
      // Logical zero...
      writeManchesterZero();
    }
  }

  /**
   * 1. Set the output signal low<br>
   * 2. Wait for mid-bit time (T)<br>
   * 3. Set the output signal high<br>
   * 4. Wait for mid-bit time (T)<br>
   */
  private void writeManchesterOne()
  {
    final Integer low = Integer.valueOf( 0 );
    final Integer high = Integer.valueOf( 1 << LINE_IDX );

    // 1 & 2...
    for ( int i = 0; i < this.setupPoint; i++ )
    {
      this.data.add( low );
    }
    // 3 & 4...
    for ( int i = 0; i < this.setupPoint; i++ )
    {
      this.data.add( high );
    }
  }

  /**
   * 1. Set the output signal high<br>
   * 2. Wait for mid-bit time (T)<br>
   * 3. Set the output signal low<br>
   * 4. Wait for mid-bit time (T)<br>
   */
  private void writeManchesterZero()
  {
    final Integer low = Integer.valueOf( 0 );
    final Integer high = Integer.valueOf( 1 << LINE_IDX );

    // 1 & 2...
    for ( int i = 0; i < this.setupPoint; i++ )
    {
      this.data.add( high );
    }
    // 3 & 4...
    for ( int i = 0; i < this.setupPoint; i++ )
    {
      this.data.add( low );
    }
  }
}
