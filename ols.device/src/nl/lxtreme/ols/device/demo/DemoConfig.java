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
package nl.lxtreme.ols.device.demo;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration for the demo device.
 */
@Meta.OCD( name = "Demo device" )
public interface DemoConfig
{
  // METHODS

  @Meta.AD( name = "Data function", deflt = "I2C sample", required = true, optionValues = { "Sawtooth", "All zeros",
      "Sine", "odd-even", "0x55-0xAA", "Random", "I2C sample", "1-Wire sample", "Manchester encoded", "0x80-0x00" } )
  String dataFunction();

  @Meta.AD( name = "Data length", deflt = "1024", required = true, optionValues = { "16", "256", "1024", "4096",
      "8192", "16384", "32768", "65536", "131072" } )
  int dataLength();

  @Meta.AD( name = "Channels", deflt = "8", required = true, optionValues = { "1", "4", "8", "16", "32" } )
  int channels();

}
