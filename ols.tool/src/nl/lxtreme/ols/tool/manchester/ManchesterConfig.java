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
package nl.lxtreme.ols.tool.manchester;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the Manchester decoder tool.
 */
@Meta.OCD( name = "Manchester decoder configuration" )
public interface ManchesterConfig
{
  // METHODS

  /**
   * Returns whether or not the clock should be automatically be recovered.
   * 
   * @return <code>true</code> if the clock should be recovered,
   *         <code>false</code> if the given clock should be used.
   */
  @Meta.AD( name = "Recover clock", description = "{listen=!clock}", deflt = "true" )
  boolean recoverClock();

  /**
   * Returns the clock speed of the Manchester-encoded data.
   * 
   * @return a clock speed, in Hertz, > 0.
   */
  @Meta.AD( name = "Clock speed", deflt = "" )
  int clock();

  /**
   * Returns the index of the channel on which the Manchester-encoded data is
   * present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "Data", deflt = "0", required = true )
  int dataIdx();

  /**
   * Returns the index of the channel on which the (recovered) clock is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "Clock", deflt = "1", required = true )
  int clockIdx();

}
