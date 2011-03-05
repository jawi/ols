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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.api.tools;


/**
 * Denotes the context in which a tool is to be run.
 * <p>
 * For example, it provides the information about which range of samples should
 * be analysed.
 * </p>
 */
public interface ToolContext
{
  // METHODS

  /**
   * Returns the number of channels in the sample data.
   * 
   * @return the channel count, >= 0.
   */
  int getChannels();

  /**
   * Returns a bitmask of enabled channels in the sample data.
   * 
   * @return a bitmask of enabled channels, for example, 0xFF for the first 8
   *         channels.
   */
  int getEnabledChannels();

  /**
   * Returns the ending sample index on which the decoding should end.
   * 
   * @return a end sample index, >= 0.
   */
  int getEndSampleIndex();

  /**
   * Returns the length of the decoding area.
   * 
   * @return a decoding length, >= 0.
   */
  int getLength();

  /**
   * Returns the starting sample index on which the decoding should start.
   * 
   * @return a start sample index, >= 0.
   */
  int getStartSampleIndex();
}

/* EOF */
