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
package nl.lxtreme.ols.tool.jtag;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the JTAG decoder tool.
 */
@Meta.OCD( name = "JTAG tool configuration" )
public interface JTAGConfig
{
  // METHODS

  /**
   * Returns the index of the channel on which the TCK data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "TCK", deflt = "0", required = true )
  int tckIdx();

  /**
   * Returns the index of the channel on which the TMS data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "TMS", deflt = "1", required = true )
  int tmsIdx();

  /**
   * Returns the index of the channel on which the TDI data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "TDI", deflt = "2", required = true )
  int tdiIdx();

  /**
   * Returns the index of the channel on which the TDO data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "TDO", deflt = "3", required = true )
  int tdoIdx();

}
