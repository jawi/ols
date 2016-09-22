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
package nl.lxtreme.ols.tool.onewire;


/**
 * Helper class for the different timing values used in the standard and
 * overdrive mode.
 */
final class OneWireTiming
{
  // VARIABLES

  private final double aMin, aMax;
  private final double bMin, bMax;
  private final double cMin, cMax;
  private final double dMin, dMax;
  private final double eMax;
  private final double gMin;
  private final double hMin, hMax;
  private final double iMin, iMax;
  private final double jMin;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireTiming instance.
   * 
   * @param aMode
   *          the bus mode to determine the time values for, cannot be
   *          <code>null</code>.
   */
  public OneWireTiming( final OneWireBusMode aMode )
  {
    // Taken from: <http://www.maxim-ic.com/app-notes/index.mvp/id/126>
    if ( OneWireBusMode.STANDARD == aMode )
    {
      // Issue #76, take 3: make the minimum level for writing a 'one' even
      // shorter...
      this.aMin = 1;
      this.aMax = 15;
      this.bMin = 59;
	  this.bMax = 119;
      this.cMin = 60;
      this.cMax = 120;
      this.dMin = 5.3;
	  this.dMax = 60;
      this.eMax = 0;
      this.gMin = 0;
      this.hMin = 480;
      this.hMax = 640;
      this.iMin = 15;
      this.iMax = 70;
      this.jMin = 410;
    }
    else if ( OneWireBusMode.OVERDRIVE == aMode )
    {
      this.aMin = 1;
      this.aMax = 2;
      this.bMin = 8;
	  this.bMax = 15;
      this.cMin = 7;
      this.cMax = 14;
      this.dMin = 2.3;
	  this.dMax = 10; 
      this.eMax = 1.2;
      this.gMin = 2.5;
      this.hMin = 68;
      this.hMax = 80;
      this.iMin = 7;
      this.iMax = 9.3;
      this.jMin = 39.5;
    }
    else
    {
      throw new RuntimeException( "Unknown bus mode: " + aMode );
    }
  }

  // METHODS

  /**
   * Returns the minimal bit frame length.
   * 
   * @return a bit frame length, in microseconds.
   */
  public double getBitFrameLength()
  {
    return Math.max( this.aMin + this.bMax, this.cMin + this.dMax );
  }

  /**
   * Returns the minimal reset frame length.
   * 
   * @return a minimal reset frame length, in microseconds.
   */
  public double getResetFrameLength()
  {
    return ( this.gMin + this.hMin + this.iMin + this.jMin );
  }

  /**
   * Returns whether the given time value can be regarded as a set bit-value.
   * 
   * @param aTimeValue
   *          the time value to determine whether it is a bit-value.
   * @return <code>true</code> if the given time value represents a set
   *         bit-value (1), <code>false</code> otherwise.
   */
  public boolean isOne( final double aTimeValue )
  {
    // Issue #76: we should take the master sample time into account, otherwise
    // we're too relaxed for devices with strict timing...
    return ( aTimeValue >= this.aMin ) && ( aTimeValue <= ( this.aMax + this.eMax ) );
  }

  /**
   * Returns whether the given time value (representing the time between a
   * falling and rising edge) can be regarded as a reset value.
   * 
   * @param aTimeValue
   *          the time value to determine whether it is a reset.
   * @return <code>true</code> if the given time value represents a reset,
   *         <code>false</code> otherwise.
   */
  public boolean isReset( final double aTimeValue )
  {
    // Issue #128: master pulls line low for at least Hmin us, which is the
    // lower bound.After releasing it, it has to wait at least Imax + Jmin to
    // detect the presence of a slave, so the upper bound is defined as:
    // Hmax + Imax + Jmin...
    return ( aTimeValue >= this.hMin ) && ( aTimeValue <= ( this.hMax + this.iMax + this.jMin ) );
  }

  /**
   * Returns whether the given time value (representing the time between a
   * falling and rising edge) can be regarded as a "slave presence" pulse.
   * 
   * @param aTimeValue
   *          the time value to determine whether it is a "slave presence"
   *          pulse.
   * @return <code>true</code> if the given time value represents a slave
   *         presence pulse, <code>false</code> otherwise.
   */
  public boolean isSlavePresencePulse( final double aTimeValue )
  {
    // Issue #77: we're looking for a presence pulse that is at *most* iMax
    // uSecs, not between iMin and iMax uSecs...
    return ( aTimeValue >= 0.0 ) && ( aTimeValue <= this.iMax );
  }

  /**
   * Returns whether the given time value can be regarded as a unset bit-value.
   * 
   * @param aTimeValue
   *          the time value to determine whether it is a bit-value.
   * @return <code>true</code> if the given time value represents a unset
   *         bit-value (0), <code>false</code> otherwise.
   */
  public boolean isZero( final double aTimeValue )
  {
    // Issue #76: we should take the master sample time into account, otherwise
    // we're too relaxed for devices with strict timing...
    return ( aTimeValue >= ( this.aMax + this.eMax ) ) && ( aTimeValue <= this.cMax );
  }
}
