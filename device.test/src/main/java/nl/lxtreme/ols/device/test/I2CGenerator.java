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

  // VARIABLES

  private int              clockSpeed;
  private int              idx;
  private boolean          tick;

  // METHODS

  /**
   * Writes a given string as bit-stream to the given data array.
   * 
   * @param aData
   * @param aString
   * @return the I2C clock speed.
   */
  public int writeBitStream( final int[] aData, final String aString )
  {
    this.clockSpeed = 100000; // 100kHz

    Arrays.fill( aData, SCL );
    this.idx = 0;

    writeStartBit( aData );
    for ( byte b : aString.getBytes() )
    {
      writeByte( aData, b );
      // ack
      writeBit( aData, Math.random() > 0.8 ? 1 : 0 );
    }
    writeStopBit( aData );

    while ( this.idx < aData.length )
    {
      aData[this.idx++] |= SDA | SCL;
    }

    return this.clockSpeed;
  }

  /**
   * @return
   */
  private int getClock()
  {
    int result = this.tick ? 0 : 1;
    if ( this.idx % 4 == 0 )
    {
      this.tick = !this.tick;
    }
    return result;
  }

  /**
   * @param aData
   * @param aBit
   */
  private void writeBit( final int[] aData, final int aBit )
  {
    int val = aData[this.idx];
    while ( getClock() == 1 )
    {
      aData[this.idx] |= val | SCL;
      this.idx++;
    }

    aData[this.idx] &= ~SCL;
    if ( aBit == 0 )
    {
      aData[this.idx] &= ~SDA;
    }
    else
    {
      aData[this.idx] |= SDA;
    }
    val = aData[this.idx];
    this.idx++;

    while ( getClock() == 0 )
    {
      aData[this.idx] |= val;
      aData[this.idx] &= ~SCL;
      this.idx++;
    }

    while ( getClock() == 1 )
    {
      aData[this.idx] |= val | SCL;
      this.idx++;
    }
  }

  /**
   * @param aData
   * @param aValue
   */
  private void writeByte( final int[] aData, byte aValue )
  {
    int bit;

    for ( bit = 0; bit < 8; bit++ )
    {
      writeBit( aData, aValue & 0x80 );
      aValue <<= 1;
    }
  }

  /**
   * Data transfer is initiated with the START bit when SDA is pulled low while SCL stays high.
   * 
   * @param aData
   */
  private void writeStartBit( final int[] aData )
  {
    aData[this.idx++] |= SDA | SCL;
    aData[this.idx++] |= SDA | SCL;
    aData[this.idx++] |= SDA | SCL;
    aData[this.idx++] |= SCL;
    aData[this.idx++] |= SCL;
  }

  /**
   * When the transfer is complete, a STOP bit (P) is sent by releasing the data line to allow it to be pulled up while
   * SCL is constantly high.
   * 
   * @param aData
   */
  private void writeStopBit( final int[] aData )
  {
    int val = aData[this.idx];
    while ( getClock() == 0 )
    {
      aData[this.idx] |= val;
      aData[this.idx] &= ~SCL;
      this.idx++;
    }

    aData[this.idx++] |= SCL;
    aData[this.idx++] |= SCL;
    aData[this.idx++] |= SDA | SCL;
    aData[this.idx++] |= SDA | SCL;
    aData[this.idx++] |= SDA | SCL;
  }

}

/* EOF */
