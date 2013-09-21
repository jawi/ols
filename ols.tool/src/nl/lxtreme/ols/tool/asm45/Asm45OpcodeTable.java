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
package nl.lxtreme.ols.tool.asm45;


/**
 * Class for opcode table
 * 
 * @author Ansgar Kueckes
 */
public class Asm45OpcodeTable
{
  // VARIABLES

  /** 1 for opcode, 0 for operand or variant bits */
  private final int mask;
  /** effective opcode data (with respect to mask) */
  private final int opcode;
  /** 3-character mnemonic */
  private final String mnemonic;
  /** addressing modes (see above) */
  private final int mode;
  /** minimum timing in system clocks (175 nanoseconds) */
  private final int timing;

  // CONSTRUCTORS

  /**
   * @param aMask
   * @param aOpcode
   * @param aMnemnonic
   * @param aMode
   * @param aTiming
   */
  public Asm45OpcodeTable( final int aMask, final int aOpcode, final String aMnemonic, final int aMode,
      final int aTiming )
  {
    this.mask = aMask;
    this.opcode = aOpcode;
    this.mnemonic = aMnemonic;
    this.mode = aMode;
    this.timing = aTiming;
  }

  // METHODS

  /**
   * @return mask
   */
  public int getMask()
  {
    return this.mask;
  }

  /**
   * @return mnemonic
   */
  public String getMnemonic()
  {
    return this.mnemonic;
  }

  /**
   * @return mode
   */
  public int getMode()
  {
    return this.mode;
  }

  /**
   * @return opcode
   */
  public int getOpcode()
  {
    return this.opcode;
  }

  /**
   * @return timing
   */
  public int getTiming()
  {
    return this.timing;
  }
}
