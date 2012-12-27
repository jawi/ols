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
package nl.lxtreme.ols.device.generic;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration for the generic device.
 */
@Meta.OCD( name = "Generic device" )
public interface GenericConfig
{
  // METHODS

  @Meta.AD( name = "Device path", required = true, description = "The path/URI to the device to read from." )
  String path();

  @Meta.AD( name = "Sample rate", required = true, min = "1", deflt = "1000000", description = "The sample rate in Hertz." )
  int sampleRate();

  @Meta.AD( name = "Sample depth", required = true, min = "1", deflt = "1024", description = "The number of samples to take." )
  int sampleDepth();

  @Meta.AD( name = "Sample width", required = true, min = "1", deflt = "1", description = "The number of bytes of each sample." )
  int sampleWidth();

  @Meta.AD( name = "Channel count", required = true, min = "1", deflt = "8", description = "The number of channels in each sample." )
  int channelCount();

}
