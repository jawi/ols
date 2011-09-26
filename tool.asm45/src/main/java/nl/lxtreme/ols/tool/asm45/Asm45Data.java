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


import nl.lxtreme.ols.api.data.*;


/**
 * Class for Asm45 dataset (stores Asm45 decoded data)
 * 
 * @author Ansgar Kueckes
 */
public final class Asm45Data extends BaseData<Asm45Data>
{
  // CONSTANTS

  public static final String TYPE_INSTRUCTION = "I";
  public static final String TYPE_DATA_WORD = "DW";
  public static final String TYPE_DATA_BYTE_LEFT = "DBL";
  public static final String TYPE_DATA_BYTE_RIGHT = "DBR";

  // VARIABLES

  /** # of clocks for event */
  private final int clocks;
  /** 6-bit memory block */
  private final int block;
  /**
   * 16-bit memory address within block (including MSB=upper/lower block
   * selection)
   */
  private final int address;
  /** 16-bit data */
  private final int value;
  /** true if external bus grant/DMA active */
  private final boolean busGrant;
  /**
   * one of "I"=instruction, "DW"=data word, "DBL"=data byte left, "DBR"=data
   * byte right
   */
  private final String type;
  /** decoded 9845 assembler instruction / data transfer as a string */
  private final String event;

  // CONSTRUCTORS

  /**
   * @param aClocks
   * @param aBlock
   * @param aAddress
   * @param aValue
   * @param aBusGrant
   * @param aType
   * @param aEvent
   */
  public Asm45Data( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aClocks, final int aBlock, final int aAddress, final int aValue, final boolean aBusGrant,
      final String aType, final String aEvent )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.clocks = aClocks;
    this.block = aBlock;
    this.address = aAddress;
    this.value = aValue;
    this.busGrant = aBusGrant;
    this.type = aType;
    this.event = aEvent;
  }

  /**
   * @param aEventName
   */
  public Asm45Data( final int aIdx, final int aChannelIdx, final int aSampleIdx, final String aEventName )
  {
    super( aIdx, aChannelIdx, aSampleIdx, aEventName );
    this.clocks = 0;
    this.block = 0;
    this.address = 0;
    this.value = 0;
    this.busGrant = false;
    this.type = "";
    this.event = "";
  }

  // METHODS

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( !super.equals( aObject ) || !( aObject instanceof Asm45Data ) )
    {
      return false;
    }

    final Asm45Data other = ( Asm45Data )aObject;
    if ( this.value != other.value )
    {
      return false;
    }

    return true;
  }

  /**
   * @return the address, never <code>null</code>.
   */
  public int getAddress()
  {
    return this.address;
  }

  /**
   * @return the block, never <code>null</code>.
   */
  public int getBlock()
  {
    return this.block;
  }

  /**
   * @return the bus grant, never <code>null</code>.
   */
  public boolean getBusGrant()
  {
    return this.busGrant;
  }

  /**
   * @return the clocks, never <code>null</code>.
   */
  public int getClocks()
  {
    return this.clocks;
  }

  /**
   * @return the event, never <code>null</code>.
   */
  public String getEvent()
  {
    return this.event;
  }

  /**
   * @return the value, never <code>null</code>.
   */
  public String getType()
  {
    return this.type;
  }

  /**
   * @return the value, never <code>null</code>.
   */
  public int getValue()
  {
    return this.value;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = ( prime * result ) + this.value;
    return result;
  }
}
