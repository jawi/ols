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
package nl.lxtreme.ols.device.sump.config;


import nl.lxtreme.ols.device.sump.*;


/**
 * Provides a builder for creating basic trigger(-stages).
 */
public class SumpBasicTriggerBuilder
{
  // CONSTANTS

  private static final int TRIGGER_CAPTURE = SumpBasicTrigger.TRIGGER_CAPTURE;
  // mask for delay value
  private static final int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private static final int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private static final int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private static final int TRIGGER_SERIAL = 0x04000000;

  // VARIABLES

  private boolean serialMode;
  private int serialChannel;
  private int stage;
  private int mask;
  private int value;
  private int level;
  private int delay;
  private boolean startCapture;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpBasicTriggerBuilder} instance.
   */
  public SumpBasicTriggerBuilder()
  {
    this.serialChannel = -1;
    this.serialMode = false;
    this.stage = -1;
    this.mask = 0;
    this.value = 0;
    this.level = 0;
    this.delay = 0;
    this.startCapture = true;
  }

  public SumpBasicTrigger build( int aTriggerStages, int aChannelCount, boolean aDemuxMode )
  {
    if ( this.stage < 0 || this.stage >= aTriggerStages )
    {
      throw new IllegalArgumentException( "Invalid trigger stage #" + this.stage + ", only " + aTriggerStages
          + " supported!" );
    }
    if ( this.level < 0 || this.level >= aTriggerStages )
    {
      throw new IllegalArgumentException( "Invalid trigger level #" + this.level + ", only " + aTriggerStages
          + " supported!" );
    }

    int config = 0;

    if ( this.serialMode )
    {
      // Serial trigger...
      if ( this.serialChannel < 0 || this.serialChannel >= aChannelCount )
      {
        throw new IllegalArgumentException( "No or invalid serial channel defined!" );
      }

      config = TRIGGER_SERIAL;
      config |= ( this.serialChannel << 20 ) & TRIGGER_CHANNELMASK;
    }

    config |= this.delay & TRIGGER_DELAYMASK;
    config |= ( this.level << 16 ) & TRIGGER_LEVELMASK;
    config |= ( this.startCapture ) ? TRIGGER_CAPTURE : 0;

    int mask = this.mask;
    int value = this.value;

    if ( aDemuxMode )
    {
      // Ensure that in demux/DDR mode, the channel groups are copied...
      mask = ( mask & 0xFFFF ) | ( ( mask & 0xFFFF ) << 16 );
      value = ( value & 0xFFFF ) | ( ( value & 0xFFFF ) << 16 );
    }

    return new SumpBasicTrigger( mask, value, config );
  }

  public SumpBasicTriggerBuilder setSerialMode( boolean aEnable )
  {
    this.serialMode = aEnable;
    return this;
  }

  public SumpBasicTriggerBuilder setSerialChannel( int aChannel )
  {
    this.serialChannel = aChannel;
    return this;
  }

  public SumpBasicTriggerBuilder setStage( int aStage )
  {
    this.stage = aStage;
    return this;
  }

  public SumpBasicTriggerBuilder setMask( int aMask )
  {
    this.mask = aMask;
    return this;
  }

  public SumpBasicTriggerBuilder setValue( int aValue )
  {
    this.value = aValue;
    return this;
  }

  public SumpBasicTriggerBuilder setLevel( int aLevel )
  {
    this.level = aLevel;
    return this;
  }

  public SumpBasicTriggerBuilder setDelay( int aDelay )
  {
    this.delay = aDelay;
    return this;
  }

  public SumpBasicTriggerBuilder setStartCapture( boolean aStartCapture )
  {
    this.startCapture = aStartCapture;
    return this;
  }
}
