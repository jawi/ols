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
package nl.lxtreme.ols.tool.i2c;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the I2C decoder tool.
 */
@Meta.OCD( name = "I2C tool configuration" )
public interface I2CConfig
{
  // METHODS

  /**
   * Returns whether or not SDA/SCL lines should be detected.
   * 
   * @return <code>true</code> if SDA/SCL should be auto-detected,
   *         <code>false</code> if the channel indexes are fixed (lineA = SDA,
   *         lineB = SCL).
   */
  @Meta.AD( name = "Auto-detect SDA/SCL", deflt = "true" )
  boolean detectLines();

  /**
   * Returns the index of the channel on which the SDA/LineA data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "Line A (SDA)", deflt = "0", required = true )
  int lineAIdx();

  /**
   * Returns the index of the channel on which the SCL/LineB data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "Line B (SCL)", deflt = "1", required = true )
  int lineBIdx();

  /**
   * @return <code>true</code> if ACKs should be reported, <code>false</code>
   *         otherwise.
   */
  @Meta.AD( name = "Report ACKs", deflt = "true" )
  boolean reportAck();

  /**
   * @return <code>true</code> if NACKs should be reported, <code>false</code>
   *         otherwise.
   */
  @Meta.AD( name = "Report NACKs", deflt = "true" )
  boolean reportNack();

  /**
   * @return <code>true</code> if start conditions should be reported,
   *         <code>false</code> otherwise.
   */
  @Meta.AD( name = "Report START", deflt = "false" )
  boolean reportStart();

  /**
   * @return <code>true</code> if stop conditions should be reported,
   *         <code>false</code> otherwise.
   */
  @Meta.AD( name = "Report STOP", deflt = "false" )
  boolean reportStop();

}
