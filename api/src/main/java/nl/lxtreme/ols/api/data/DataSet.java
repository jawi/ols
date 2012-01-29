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
package nl.lxtreme.ols.api.data;


import nl.lxtreme.ols.api.acquisition.*;


/**
 * Denotes a set of data, including the acquisition result, the channels and the
 * cursors.
 */
public interface DataSet
{
  // METHODS

  /**
   * Returns the captured data of this project.
   * 
   * @return a captured data, can be <code>null</code>.
   */
  public AcquisitionResult getCapturedData();

  /**
   * Returns a single channel.
   * 
   * @param aIndex
   *          the channel index, >= 0 && <
   *          {@link AcquisitionResult#getChannels()}.
   * @return an array of channels, never <code>null</code>.
   */
  public Channel getChannel( int aIndex );

  /**
   * Returns the channels of this project.
   * 
   * @return an array of channels, never <code>null</code>.
   */
  public Channel[] getChannels();

  /**
   * Returns a single cursor
   * 
   * @param aIndex
   *          the index of the cursor to retrieve, >= 0.
   */
  public Cursor getCursor( int aIndex );

  /**
   * Returns the available cursors of this project.
   * 
   * @return an array of cursors, never <code>null</code>.
   */
  public Cursor[] getCursors();

  /**
   * Returns whether or not cursors are enabled.
   * 
   * @return <code>true</code> if cursors are enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isCursorsEnabled();

  /**
   * Sets whether or not cursors are enabled.
   * 
   * @param aEnabled
   *          <code>true</code> if cursors are enabled, <code>false</code>
   *          otherwise.
   */
  public void setCursorsEnabled( final boolean aEnabled );
}
