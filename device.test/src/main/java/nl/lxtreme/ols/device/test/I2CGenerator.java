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
package nl.lxtreme.ols.device.test;


import java.util.*;


/**
 * 
 */
final class I2CGenerator
{
  // CONSTANTS

  private static final int SDA = 0x01;
  private static final int SCL = 0x02;

  private static final int ACK = 0x00;
  private static final int NACK = 0x01;

  // VARIABLES

  private final int sampleRate;
  private final int busSpeed;
  private final int tickSize;
  private final List<Integer> data;
  private int idx;
  private int trigger;

  // CONSTRUCTORS

  /**
   * Creates a new I2CGenerator instance.
   */
  public I2CGenerator()
  {
    this.data = new ArrayList<Integer>();

    this.busSpeed = 1000000; // 1 MHz
    this.sampleRate = 4000000; // 4 MHz

    this.tickSize = ( int )( Math.rint( this.sampleRate / ( double )this.busSpeed ) );
  }

  // METHODS

  /**
   * @return
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
   * @return
   */
  public int getRate()
  {
    return this.sampleRate;
  }

  /**
   * Returns the trigger offset of the generated signal.
   * 
   * @return a trigger offset.
   */
  public int getTrigger()
  {
    return this.trigger;
  }

  /**
   * Writes a given string as bit-stream to the given data array.
   * 
   * @param aData
   * @param aString
   * @return the I2C clock speed.
   */
  public void writeBitStream( final String aString )
  {
    this.idx = 0;

    writeStartBit();

    this.trigger = this.data.size();

    writeByte( ( byte )0xf6 ); // write to address 0x1ff (10-bit)
    writeBit( ACK ); // ack

    writeByte( ( byte )0xff );
    writeBit( ACK ); // ack

    for ( byte b : aString.getBytes() )
    {
      writeByte( b );
      // ack
      writeBit( Math.random() > 0.8 ? NACK : ACK );
    }
    writeStopBit();

    writeStartBit();

    writeByte( ( byte )0x55 ); // read from address 0x2a
    writeBit( ACK ); // ack

    writeByte( ( byte )'!' );
    // ack
    writeBit( Math.random() > 0.8 ? NACK : ACK );

    writeStopBit();

    int i = 20;
    while ( i-- >= 0 )
    {
      this.data.add( Integer.valueOf( 0 ) );
    }
  }

  /**
   * @return
   */
  private int getClock()
  {
    final double nextTickMultiple = this.tickSize * Math.floor( this.idx / ( double )this.tickSize );
    final double tickIdx = ( nextTickMultiple == 0.0 ) ? this.idx : Math.ceil( this.idx / nextTickMultiple );
    return ( ( tickIdx % 2 ) == 0 ) ? 1 : 0;
  }

  /**
   * @param aData
   * @param aBit
   */
  private void writeBit( final int aBit )
  {
    int val = this.data.get( this.idx ).intValue();

    // While SCL should be high, let it also keep SDA stable...
    while ( getClock() == 1 )
    {
      this.data.add( Integer.valueOf( val | SCL ) );
      this.idx++;
    }

    // Clear SCL...
    val &= ~SCL;
    this.data.remove( this.idx );
    this.data.add( Integer.valueOf( val ) );

    // One tick later, update SDA...
    if ( aBit == 0 )
    {
      val &= ~SDA;
    }
    else if ( aBit >= 1 )
    {
      val |= SDA;
    }
    this.data.add( Integer.valueOf( val ) );
    this.idx++;

    // As long as SCL remains low, keep SDA stable...
    while ( getClock() == 0 )
    {
      this.data.add( Integer.valueOf( val ) );
      this.idx++;
    }
  }

  /**
   * @param aData
   * @param aValue
   */
  private void writeByte( byte aValue )
  {
    int bit;

    for ( bit = 0; bit < 8; bit++ )
    {
      writeBit( aValue & 0x80 );
      aValue <<= 1;
    }
  }

  /**
   * Data transfer is initiated with the START bit when SDA is pulled low while
   * SCL stays high.
   */
  private void writeStartBit()
  {
    int i = this.tickSize - 1;
    while ( i-- > 0 )
    {
      this.data.add( Integer.valueOf( SDA | SCL ) );
    }
    this.data.add( Integer.valueOf( SCL ) );

    i = this.tickSize;
    while ( i-- > 0 )
    {
      this.data.add( Integer.valueOf( SCL ) );
    }

    this.idx = this.data.size() - 1;
  }

  /**
   * When the transfer is complete, a STOP bit (P) is sent by releasing the data
   * line to allow it to be pulled up while SCL is constantly high.
   */
  private void writeStopBit()
  {
    writeBit( -1 ); // stuff

    // SCL should become high now...
    this.data.add( Integer.valueOf( SCL ) );

    // One tick later also make SDA high...
    int i = this.tickSize - 1;
    while ( i-- > 0 )
    {
      this.data.add( Integer.valueOf( SDA | SCL ) );
    }

    this.idx = this.data.size() - 1;
  }

}

/* EOF */
