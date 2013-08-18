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
 * Provides a 1-Wire protocol data generator.
 * <p>
 * This generator is based on application note 126
 * "1-Wire Communication Through Software" of Maxim (May 30, 2002).
 * </p>
 */
final class OneWireGenerator
{
  // CONSTANTS

  private static final int OW_LINE = 0x01;

  // VARIABLES

  private final int sampleRate;
  private final int busSpeed;
  private final int tickSize;
  private final List<Integer> data;
  private final int a, b, c, d, e, f, g, h, i, j;

  private int trigger;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireGenerator instance.
   * 
   * @param aStandard
   *          <code>true</code> to use the "standard" bus timing mode (max. 16.3
   *          KBit/s), <code>false</code> to use the "overdrive" bus timing mode
   *          (max. 163 KBit/s).
   */
  public OneWireGenerator( final boolean aStandard )
  {
    this.data = new ArrayList<Integer>();

    this.busSpeed = 1000000; // 1 MHz
    this.sampleRate = this.busSpeed * 8; // 8 MHz

    this.tickSize = ( int )( Math.rint( this.sampleRate / ( double )this.busSpeed ) );

    if ( aStandard )
    {
      this.a = ( int )( 6.0 * this.tickSize );
      this.b = ( int )( 64.0 * this.tickSize );
      this.c = ( int )( 60.0 * this.tickSize );
      this.d = ( int )( 10.0 * this.tickSize );
      this.e = ( int )( 9.0 * this.tickSize );
      this.f = ( int )( 55.0 * this.tickSize );
      this.g = ( int )( 0.0 * this.tickSize );
      this.h = ( int )( 480.0 * this.tickSize );
      this.i = ( int )( 70.0 * this.tickSize );
      this.j = ( int )( 410.0 * this.tickSize );
    }
    else
    {
      this.a = ( int )( 1.5 * this.tickSize );
      this.b = ( int )( 7.5 * this.tickSize );
      this.c = ( int )( 7.5 * this.tickSize );
      this.d = ( int )( 2.5 * this.tickSize );
      this.e = ( int )( 0.75 * this.tickSize );
      this.f = ( int )( 7.0 * this.tickSize );
      this.g = ( int )( 2.5 * this.tickSize );
      this.h = ( int )( 70.0 * this.tickSize );
      this.i = ( int )( 8.5 * this.tickSize );
      this.j = ( int )( 40.0 * this.tickSize );
    }
  }

  // METHODS

  /**
   * Returns the sample data with the encoded 1-wire data.
   * 
   * @return a array of sample data, never <code>null</code>.
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
   * Returns the sample rate of this data generator.
   * 
   * @return a sample rate, defaults to 4 MHz.
   */
  public int getRate()
  {
    return this.sampleRate;
  }

  /**
   * Returns the trigger offset.
   * 
   * @return the trigger offset, never <code>null</code>.
   */
  public int getTrigger()
  {
    return this.trigger;
  }

  /**
   * Writes a given string as 1-wire encoded bit-stream.
   * 
   * @param aString
   *          the data to write as bit stream, cannot be <code>null</code>.
   */
  public void writeBitStream( final String aString )
  {
    writeStuffData( 5, OW_LINE );

    writeReset();

    writeStuffData( 5, OW_LINE );

    for ( byte b : aString.getBytes() )
    {
      writeByte( b );
    }

    // Write some stuff data...
    writeStuffData( 5, OW_LINE );
  }

  /**
   * Sample read method.
   * 
   * @return the read 1-wire bit value.
   */
  @SuppressWarnings( "unused" )
  private int readBit()
  {
    int result;

    // drive 1-wire line low...
    this.data.add( Integer.valueOf( 0 ) );
    tickDelay( this.a );

    // drive 1-wire line high...
    this.data.add( Integer.valueOf( OW_LINE ) );
    tickDelay( this.e );

    // read 1-wire line (faked)...
    result = Math.random() > 0.5 ? 1 : 0;
    tickDelay( this.f );

    return result;
  }

  /**
   * "Delays" the signal for a certain amount of time by injecting
   * <tt>aValue</tt> data values.
   * 
   * @param aValue
   *          the value to delay for, >= 1.
   */
  private void tickDelay( final int aValue )
  {
    final Integer curValue = this.data.isEmpty() ? Integer.valueOf( 1 ) : this.data.get( this.data.size() - 1 );
    for ( int i = 1; i < aValue; i++ )
    {
      this.data.add( curValue );
    }
  }

  /**
   * Generates a 1-wire one-value.
   * 
   * @param aWriteOne
   *          <code>true</code> to write a one, <code>false</code> to write a
   *          zero.
   */
  private void writeBit( final boolean aWriteOne )
  {
    if ( aWriteOne )
    {
      // write 1...
      this.data.add( Integer.valueOf( 0 ) );
      tickDelay( this.a );

      this.data.add( Integer.valueOf( OW_LINE ) );
      tickDelay( this.b );
    }
    else
    {
      // write 0...
      this.data.add( Integer.valueOf( 0 ) );
      tickDelay( this.c );

      this.data.add( Integer.valueOf( OW_LINE ) );
      tickDelay( this.d );
    }
  }

  /**
   * Writes a given byte value as 1-wire encoded bit stream.
   * 
   * @param aValue
   *          the byte value to write.
   */
  private void writeByte( final byte aValue )
  {
    int value = aValue;
    for ( int loop = 0; loop < 8; loop++ )
    {
      writeBit( ( value & 0x01 ) == 0x01 );
      value >>= 1;
    }
  }

  /**
   * Generates a 1-wire reset.
   */
  private void writeReset()
  {
    tickDelay( this.g );
    // drive 1-wire line low...
    this.data.add( Integer.valueOf( 0 ) );

    tickDelay( this.h );
    // drive 1-wire line high...
    this.data.add( Integer.valueOf( OW_LINE ) );

    this.trigger = this.data.size();

    tickDelay( this.i - 1 );
    // drive 1-wire line low (signals device presence)...
    this.data.add( Integer.valueOf( Math.random() > 0.3 ? 0 : OW_LINE ) );

    tickDelay( this.j - 1 );
    // drive 1-wire line high...
    this.data.add( Integer.valueOf( OW_LINE ) );
    this.data.add( Integer.valueOf( OW_LINE ) );
  }

  /**
   * Writes some stuf data for a certain amount of data.
   * 
   * @param aDelay
   *          the delay;
   * @param aValue
   *          the value to stuff the data with.
   */
  private void writeStuffData( final int aDelay, final int aValue )
  {
    this.data.add( Integer.valueOf( aValue ) );
    tickDelay( aDelay );
  }
}
