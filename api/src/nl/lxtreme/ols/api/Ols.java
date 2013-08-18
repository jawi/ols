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
package nl.lxtreme.ols.api;


/**
 * Defines some bare constants, used throughout the rest of the application.
 */
public interface Ols
{
  // METHODS

  /** indicates that rate or trigger position are not available */
  public final static int NOT_AVAILABLE = -1;

  /** The maximum number of cursors that can be set. */
  public static final int MAX_CURSORS = 10;

  /** The maximum number of channels. */
  public static final int MAX_CHANNELS = 32;

  /** The number of channels per block. */
  public static final int CHANNELS_PER_BLOCK = 8;

  /** The maximum number of blocks. */
  public static final int MAX_BLOCKS = MAX_CHANNELS / CHANNELS_PER_BLOCK;

}
