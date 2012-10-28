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
package nl.lxtreme.ols.tool.asm45;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the Asm45 decoder tool.
 */
@Meta.OCD( name = "1-Wire tool configuration" )
public interface Asm45Config
{
  // METHODS

  @Meta.AD( name = "IDA0/..IDA15/", description = "{readonly=true}", deflt = "0..15" )
  String idaIdxs();

  @Meta.AD( name = "BSC0/..BSC5/", description = "{readonly=true}", deflt = "16..21" )
  String bcsIdxs();

  @Meta.AD( name = "SMC/", deflt = "22", required = true )
  int smcIdx();

  @Meta.AD( name = "STM/", deflt = "23", required = true )
  int stmIdx();

  @Meta.AD( name = "EBG", deflt = "25", required = true )
  int ebgIdx();

  @Meta.AD( name = "BYTE", deflt = "26", required = true )
  int byteIdx();

  @Meta.AD( name = "BL", deflt = "27", required = true )
  int blIdx();

  @Meta.AD( name = "WRT/", deflt = "29", required = true )
  int wrtIdx();

  @Meta.AD( name = "SYNC", deflt = "30", required = true )
  int syncIdx();

  @Meta.AD( name = "Show instructions", deflt = "true" )
  boolean reportInst();

  @Meta.AD( name = "Show data transfers", deflt = "true" )
  boolean reportData();

  @Meta.AD( name = "Show bus grants", deflt = "false" )
  boolean reportBusGrants();
}
