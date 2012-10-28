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
package nl.lxtreme.ols.tool.nrz;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the NRZ decoder tool.
 */
@Meta.OCD( name = "NRZ decoder configuration" )
public interface NRZConfig
{
  // METHODS

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
