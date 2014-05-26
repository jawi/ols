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
 * Represents a "basic" trigger, as used in the original SUMP protocol.
 */
public class SumpBasicTrigger
{
  // CONSTANTS

  // trigger will start capture when fired
  public final static int TRIGGER_CAPTURE = 0x08000000;

  // VARIABLES

  private final int mask;
  private final int value;
  private final int config;

  /**
   * Creates a new {@link SumpBasicTrigger} instance that will always trigger a
   * capture.
   */
  public SumpBasicTrigger()
  {
    this( 0, 0, TRIGGER_CAPTURE );
  }

  /**
   * Creates a new {@link SumpBasicTrigger} instance.
   * 
   * @param aMask
   *          the mask of this trigger;
   * @param aValue
   *          the value of this trigger;
   * @param aConfig
   *          the configuration of this trigger.
   */
  public SumpBasicTrigger( int aMask, int aValue, int aConfig )
  {
    this.mask = aMask;
    this.value = aValue;
    this.config = aConfig;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( aObject == null || !( aObject instanceof SumpBasicTrigger ) )
    {
      return false;
    }

    SumpBasicTrigger other = ( SumpBasicTrigger )aObject;
    if ( this.config != other.config )
    {
      return false;
    }
    if ( this.mask != other.mask )
    {
      return false;
    }
    if ( this.value != other.value )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the current value of config.
   * 
   * @return the config
   */
  public int getConfig()
  {
    return this.config;
  }

  /**
   * Returns the current value of mask.
   * 
   * @return the mask
   */
  public int getMask()
  {
    return this.mask;
  }

  /**
   * Returns the current value of value.
   * 
   * @return the value
   */
  public int getValue()
  {
    return this.value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.config;
    result = prime * result + this.mask;
    result = prime * result + this.value;
    return result;
  }
}
