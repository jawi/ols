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


import java.io.*;

import org.sump.device.logicsniffer.*;


/**
 * Provides an extension to the {@link SumpCommandWriter} that allows the
 * "Demon Core" to be configured as well.
 */
public class DemonCoreCommandWriter extends SumpCommandWriter
{
  // INNER TYPES

  /**
   * Denotes one of the two edge-triggers.
   */
  public static enum TriggerEdge
  {
    /** The first edge-trigger. */
    EDGE_1( 0x34 ), //
    /** The second edge-trigger. */
    EDGE_2( 0x35 );

    private final int lutChainAddress;

    /**
     * Creates a new TriggerEdge instance.
     * 
     * @param aLutChainAddress
     *          the LUT chain address to use.
     */
    private TriggerEdge( final int aLutChainAddress )
    {
      this.lutChainAddress = aLutChainAddress;
    }

    /**
     * Returns the current value of lutChainAddress.
     * 
     * @return the LUT chain address, never <code>null</code>.
     */
    public int getLutChainAddress()
    {
      return this.lutChainAddress;
    }
  }

  /**
   * Denotes one of the two range-triggers.
   */
  public static enum TriggerRange
  {
    /** The first range-trigger, lower boundary. */
    RANGE_1_LOWER( 0x30 ), //
    /** The first edge-trigger, upper boundary. */
    RANGE_1_UPPER( 0x31 ), //
    /** The second range-trigger, lower boundary. */
    RANGE_2_LOWER( 0x32 ), //
    /** The second edge-trigger, upper boundary. */
    RANGE_2_UPPER( 0x33 );

    private final int lutChainAddress;

    /**
     * Creates a new TriggerRange instance.
     * 
     * @param aLutChainAddress
     *          the LUT chain address to use.
     */
    private TriggerRange( final int aLutChainAddress )
    {
      this.lutChainAddress = aLutChainAddress;
    }

    /**
     * Returns the current value of lutChainAddress.
     * 
     * @return the LUT chain address, never <code>null</code>.
     */
    public int getLutChainAddress()
    {
      return this.lutChainAddress;
    }

    /**
     * Returns whether or not this range represents the lower boundary.
     * 
     * @return <code>true</code> if this range is the lower boundary,
     *         <code>false</code> otherwise.
     */
    public boolean isLowerRange()
    {
      return ( this == RANGE_1_LOWER ) || ( this == RANGE_2_LOWER );
    }
  }

  /**
   * Denotes one of the ten trigger terms.
   */
  public static enum TriggerTerm
  {
    /** The 1st term. */
    TERM_A( 0x20 ), //
    /** The 2nd term. */
    TERM_B( 0x21 ), //
    /** The 3rd term. */
    TERM_C( 0x22 ), //
    /** The 4th term. */
    TERM_D( 0x23 ), //
    /** The 5th term. */
    TERM_E( 0x24 ), //
    /** The 6th term. */
    TERM_F( 0x25 ), //
    /** The 7th term. */
    TERM_G( 0x26 ), //
    /** The 8th term. */
    TERM_H( 0x27 ), //
    /** The 9th term. */
    TERM_I( 0x28 ), //
    /** The 10th term. */
    TERM_J( 0x29 );

    private final int lutChainAddress;

    /**
     * Creates a new TriggerTerm instance.
     * 
     * @param aLutChainAddress
     *          the LUT chain address to use.
     */
    private TriggerTerm( final int aLutChainAddress )
    {
      this.lutChainAddress = aLutChainAddress;
    }

    /**
     * Returns the current value of lutChainAddress.
     * 
     * @return the LUT chain address, never <code>null</code>.
     */
    public int getLutChainAddress()
    {
      return this.lutChainAddress;
    }
  }

  /**
   * Denotes one of the two timer-triggers.
   */
  public static enum TriggerTimer
  {
    /** The first timer-trigger. */
    TIMER_1( 0x38 ), //
    /** The second timer-trigger. */
    TIMER_2( 0x3A );

    private final int lutChainAddress;

    /**
     * Creates a new TriggerTimer instance.
     * 
     * @param aLutChainAddress
     *          the LUT chain address to use.
     */
    private TriggerTimer( final int aLutChainAddress )
    {
      this.lutChainAddress = aLutChainAddress;
    }

    /**
     * Returns the current value of lutChainAddress.
     * 
     * @return the LUT chain address, never <code>null</code>.
     */
    public int getLutChainAddress()
    {
      return this.lutChainAddress;
    }
  }

  // CONSTANTS

  public static final int RANGE_XOR0 = 0xAAAA;
  public static final int RANGE_XOR1 = 0x5555;
  public static final int RANGE_NOP = 0xFFFF;

  public static final int EDGE_RISE0 = 0x0A0A;
  public static final int EDGE_RISE1 = 0x00CC;
  public static final int EDGE_FALL0 = 0x5050;
  public static final int EDGE_FALL1 = 0x3300;
  public static final int EDGE_BOTH0 = ( EDGE_RISE0 | EDGE_FALL0 );
  public static final int EDGE_BOTH1 = ( EDGE_RISE1 | EDGE_FALL1 );
  public static final int EDGE_NEITHER0 = ( ~EDGE_BOTH0 & 0xFFFF );
  public static final int EDGE_NEITHER1 = ( ~EDGE_BOTH1 & 0xFFFF );

  public static final int TRIGSTATE_STATENUM_MASK = 0xF;
  public static final int TRIGSTATE_OBTAIN_MASK = 0x000FFFFF;
  public static final int TRIGSTATE_ELSE_BITOFS = 20;
  public static final long TRIGSTATE_STOP_TIMER0 = 0x01000000L;
  public static final long TRIGSTATE_STOP_TIMER1 = 0x02000000L;
  public static final long TRIGSTATE_CLEAR_TIMER0 = 0x04000000L;
  public static final long TRIGSTATE_CLEAR_TIMER1 = 0x08000000L;
  public static final long TRIGSTATE_START_TIMER0 = 0x10000000L;
  public static final long TRIGSTATE_START_TIMER1 = 0x20000000L;
  public static final long TRIGSTATE_TRIGGER_FLAG = 0x40000000L;
  public static final long TRIGSTATE_LASTSTATE = 0x80000000L;

  public static final int OP_NOP = 0;
  public static final int OP_ANY = 1;
  public static final int OP_AND = 2;
  public static final int OP_NAND = 3;
  public static final int OP_OR = 4;
  public static final int OP_NOR = 5;
  public static final int OP_XOR = 6;
  public static final int OP_NXOR = 7;
  public static final int OP_A = 8;
  public static final int OP_B = 9;

  private static final int CMD_SELECT = 0x9E;
  private static final int CMD_CHAIN = 0x9F;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DemonCoreCommandWriter} instance.
   * 
   * @param aConfiguration
   *          the configuration to use;
   * @param aOutputStream
   *          the {@link DataOutputStream} to use.
   */
  public DemonCoreCommandWriter( final LogicSnifferConfig aConfiguration, final DataOutputStream aOutputStream )
  {
    super( aConfiguration, aOutputStream );
  }

  // METHODS

  /**
   * Setup LUT's for Edge Detectors.
   * 
   * @param aEdge
   *          selects which edge we want to select & write trigger data for;
   * @param aRisingEdge
   *          the bitmask denoting all channels on which a rising edge should be
   *          detected;
   * @param aFallingEdge
   *          the bitmask denoting all channels on which a falling edge should
   *          be detected;
   * @param aNeitherEdge
   *          ??? XXX
   * @throws IOException
   *           in case of I/O problems.
   */
  public void writeEdge( final TriggerEdge aEdge, final int aRisingEdge, final int aFallingEdge, final int aNeitherEdge )
      throws IOException
  {
    writeSelect( aEdge.getLutChainAddress() );

    int lutvalue = 0;

    long bitmask = 0x80000000L;
    for ( int i = 0; i < 16; i++ )
    {
      // Evaluate indata bit1...
      if ( ( aNeitherEdge != 0 ) && ( bitmask != 0 ) )
      {
        lutvalue |= EDGE_NEITHER1;
      }
      else
      {
        if ( ( aRisingEdge & bitmask ) != 0 )
        {
          lutvalue |= EDGE_RISE1;
        }
        if ( ( aFallingEdge & bitmask ) != 0 )
        {
          lutvalue |= EDGE_FALL1;
        }
      }
      bitmask >>>= 1;

      // Evaluate indata bit0...
      if ( ( aNeitherEdge != 0 ) && ( bitmask != 0 ) )
      {
        lutvalue |= EDGE_NEITHER0;
      }
      else
      {
        if ( ( aRisingEdge & bitmask ) != 0 )
        {
          lutvalue |= EDGE_RISE0;
        }
        if ( ( aFallingEdge & bitmask ) != 0 )
        {
          lutvalue |= EDGE_FALL0;
        }
      }
      bitmask >>>= 1;

      if ( ( i & 1 ) == 0 )
      {
        lutvalue <<= 16;
      }
      else
      {
        writeChain( lutvalue ); // write total of 256 bits
        lutvalue = 0;
      }
    }
  }

  /**
   * Setup LUT's for Range Detectors.
   * 
   * @param aRange
   *          the trigger range selector;
   * @param aTarget
   *          the target upper/lower limit value to set;
   * @param aMask
   *          the target mask to set (indicates which bits should participate in
   *          range compare).
   * @throws IOException
   *           in case of I/O problems.
   */
  public void writeRange( final TriggerRange aRange, final int aTarget, int aMask ) throws IOException
  {
    long value;
    int lutValue = 0;
    int i;

    writeSelect( aRange.getLutChainAddress() );

    // Count # of bits in mask...
    int bitcount = Integer.bitCount( aMask );

    // Prepare target value...
    if ( aRange.isLowerRange() )
    {
      // lower range value
      value = ~( aTarget - 1 ) & 0xFFFFFFFF;
    }
    else
    {
      // upper range target
      value = ~aTarget & 0xFFFFFFFF;
    }

    // Push MSB of target into bit 31...
    value <<= ( 32 - bitcount ) & 0xFFFFFFFF;

    // Generate & program LUT values. Total of 512 bits.
    for ( i = 0; i < 16; i++ )
    {
      if ( ( ( aMask >>> 31 ) & 1 ) == 0 )
      {
        lutValue = RANGE_NOP;
      }
      else
      {
        lutValue = ( ( ( value >>> 31 ) & 1 ) != 0 ) ? RANGE_XOR1 : RANGE_XOR0;
        value <<= 1;
      }
      aMask <<= 1;
      lutValue <<= 16;

      if ( ( ( aMask >>> 31 ) & 1 ) == 0 )
      {
        lutValue |= RANGE_NOP;
      }
      else
      {
        lutValue |= ( ( ( value >>> 31 ) & 1 ) != 0 ) ? RANGE_XOR1 : RANGE_XOR0;
        value <<= 1;
      }
      aMask <<= 1;

      writeChain( lutValue );
    }
  }

  /**
   * Setup LUT's for Trigger Term Inputs are 32-bit target & mask for comparing
   * against captured analyzer data. If a mask bit is set, the corresponding
   * target bit participates in the trigger.
   * 
   * @param aTerm
   *          the trigger term to write;
   * @param aTarget
   *          the trigger target value;
   * @param aMask
   *          the trigger mask value.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void writeTerm( final TriggerTerm aTerm, final int aTarget, final int aMask ) throws IOException
  {
    int bitmask = 1;
    int lutvalue0 = 0;
    int lutvalue1 = 0;
    int lutvalue2 = 0;
    int lutvalue3 = 0;

    for ( int i = 0; i < 16; i++ )
    {
      if ( ( ( i ^ ( aTarget & 0xF ) ) & ( aMask & 0xF ) ) == 0 )
      {
        lutvalue0 |= bitmask;
      }

      if ( ( ( i ^ ( ( aTarget >>> 4 ) & 0xF ) ) & ( ( aMask >>> 4 ) & 0xF ) ) == 0 )
      {
        lutvalue0 |= ( bitmask << 16 );
      }

      if ( ( ( i ^ ( ( aTarget >>> 8 ) & 0xF ) ) & ( ( aMask >>> 8 ) & 0xF ) ) == 0 )
      {
        lutvalue1 |= bitmask;
      }

      if ( ( ( i ^ ( ( aTarget >>> 12 ) & 0xF ) ) & ( ( aMask >>> 12 ) & 0xF ) ) == 0 )
      {
        lutvalue1 |= ( bitmask << 16 );
      }

      if ( ( ( i ^ ( ( aTarget >>> 16 ) & 0xF ) ) & ( ( aMask >>> 16 ) & 0xF ) ) == 0 )
      {
        lutvalue2 |= bitmask;
      }

      if ( ( ( i ^ ( ( aTarget >>> 20 ) & 0xF ) ) & ( ( aMask >>> 20 ) & 0xF ) ) == 0 )
      {
        lutvalue2 |= ( bitmask << 16 );
      }

      if ( ( ( i ^ ( ( aTarget >>> 24 ) & 0xF ) ) & ( ( aMask >>> 24 ) & 0xF ) ) == 0 )
      {
        lutvalue3 |= bitmask;
      }

      if ( ( ( i ^ ( ( aTarget >>> 28 ) & 0xF ) ) & ( ( aMask >>> 28 ) & 0xF ) ) == 0 )
      {
        lutvalue3 |= ( bitmask << 16 );
      }

      bitmask <<= 1;
    }

    // Write data into LUT serial chain. MSB must goes in first.
    // Total of 128 bits.
    writeSelect( aTerm.getLutChainAddress() );
    writeChain( lutvalue3 );
    writeChain( lutvalue2 );
    writeChain( lutvalue1 );
    writeChain( lutvalue0 );
  }

  /**
   * Setup trigger timers.
   * 
   * @param aTimer
   *          the trigger timer to set;
   * @param aValue
   *          the 36-bit timer value to set.
   */
  public void writeTriggerLimit( final TriggerTimer aTimer, final long aValue ) throws IOException
  {
    writeSelect( aTimer.getLutChainAddress() );
    writeChain( ( int )( aValue & 0xFFFFFFFF ) );
    writeSelect( aTimer.getLutChainAddress() + 1 );
    writeChain( ( int )( ( aValue >>> 32 ) & 0x0F ) );
  }

  /**
   * Setup trigger state. Specify trigger mode, timer controls, "else" state
   * number, and necessary "obtain" count (number of hits needed before full hit
   * observed).
   */
  public void writeTriggerState( final int aStateNumber, // 0 to 15
      final boolean aLastState, final boolean aSetTrigger, //
      final int aStartTimer, // bit0=timer1, bit1=timer2
      final int aStopTimer, // bit0=timer1, bit1=timer2
      final int aClearTimer, // bit0=timer1, bit1=timer2
      final int aElseState, // 0 to 15
      final int aOccurrenceCount ) throws IOException
  {
    writeSelect( aStateNumber & TRIGSTATE_STATENUM_MASK );

    int value = ( ( aElseState & TRIGSTATE_STATENUM_MASK ) << TRIGSTATE_ELSE_BITOFS )
        | ( aOccurrenceCount & TRIGSTATE_OBTAIN_MASK );
    if ( aLastState )
    {
      value |= TRIGSTATE_LASTSTATE;
    }
    if ( aSetTrigger )
    {
      value |= TRIGSTATE_TRIGGER_FLAG;
    }
    if ( ( aStartTimer & 1 ) != 0 )
    {
      value |= TRIGSTATE_START_TIMER0;
    }
    if ( ( aStartTimer & 2 ) != 0 )
    {
      value |= TRIGSTATE_START_TIMER1;
    }
    if ( ( aStopTimer & 1 ) != 0 )
    {
      value |= TRIGSTATE_STOP_TIMER0;
    }
    if ( ( aStopTimer & 2 ) != 0 )
    {
      value |= TRIGSTATE_STOP_TIMER1;
    }
    if ( ( aClearTimer & 1 ) != 0 )
    {
      value |= TRIGSTATE_CLEAR_TIMER0;
    }
    if ( ( aClearTimer & 2 ) != 0 )
    {
      value |= TRIGSTATE_CLEAR_TIMER1;
    }
    writeChain( value );
  }

  /**
   * Setup trigger state term combinational sum. Operations for all 11 fields
   * are merged & written. Uses table based lookups.
   */
  public void writeTriggerSum( final int aStateNumber, final int aStateTerm, final int op_ab, final int op_c_range1,
      final int op_d_edge1, final int op_e_timer1, final int op_fg, final int op_h_range2, final int op_i_edge2,
      final int op_j_timer2, final int op_mid1, final int op_mid2, final int op_final ) throws IOException
  {
    // { NOP ANY AND NAND OR NOR XOR NXOR A B }
    int pairvalue[] = { 0x0000, 0xFFFF, 0x8000, 0x7FFF, 0xF888, 0x0777, 0x7888, 0x8777, 0x8888, 0xF000 };
    int midvalue[] = { 0x0000, 0xFFFF, 0x8000, 0x7FFF, 0xFFFE, 0x0001, 0x0116, 0xFEE9, 0xEEEE, 0xFFF0 };
    int finalvalue[] = { 0x0000, 0xFFFF, 0x0008, 0x0007, 0x000E, 0x0001, 0x0006, 0x0009, 0x0002, 0x0004 };

    writeSelect( 0x40 + ( aStateNumber * 4 ) + aStateTerm );
    writeChain( finalvalue[op_final] );
    writeChain( ( midvalue[op_mid2] << 16 ) | midvalue[op_mid1] );
    writeChain( ( pairvalue[op_j_timer2] << 16 ) | pairvalue[op_i_edge2] );
    writeChain( ( pairvalue[op_h_range2] << 16 ) | pairvalue[op_fg] );
    writeChain( ( pairvalue[op_e_timer1] << 16 ) | pairvalue[op_d_edge1] );
    writeChain( ( pairvalue[op_c_range1] << 16 ) | pairvalue[op_ab] );
  }

  /**
   * @param aValue
   * @throws IOException
   */
  private void writeChain( final int aValue ) throws IOException
  {
    sendCommand( CMD_CHAIN, aValue );
  }

  /**
   * @param aValue
   * @throws IOException
   */
  private void writeSelect( final int aValue ) throws IOException
  {
    sendCommand( CMD_SELECT, aValue );
  }
}
