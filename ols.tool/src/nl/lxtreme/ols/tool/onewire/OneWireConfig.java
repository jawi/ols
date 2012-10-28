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
package nl.lxtreme.ols.tool.onewire;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the 1-wire decoder tool.
 */
@Meta.OCD( name = "1-Wire tool configuration" )
public interface OneWireConfig
{
  // METHODS

  /**
   * Returns the index of the channel on which the 1-wire data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "1-Wire line", deflt = "0", required = true )
  int channelIdx();

  /**
   * Returns the bus mode denoting the various timing constraints.
   * 
   * @return the bus mode, never <code>null</code>.
   */
  @Meta.AD( name = "Bus mode", optionLabels = { "Standard", "Overdrive" }, deflt = "0" )
  OneWireBusMode busMode();
}
