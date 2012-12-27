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
package nl.lxtreme.ols.client.ui.tool.impl;


import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides a small helper class that provides information about the acquisition
 * data using reasonable defaults in case no acquisition data is
 * present/available.
 */
final class AcquisitionDataInfo
{
  // VARIABLES

  private final Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionDataInfo} instance.
   */
  public AcquisitionDataInfo( final Session aSession )
  {
    this.session = aSession;
  }

  // METHODS

  /**
   * Returns the number of channels.
   * 
   * @return the number of channels in the data, >= 0 && &lt;=
   *         {@link Ols#MAX_CHANNELS}.
   */
  public int getChannelCount()
  {
    return this.session.hasData() ? this.session.getAcquisitionData().getChannelCount() : 0;
  }

  /**
   * Returns the current cursor data.
   * 
   * @return an array with cursor data, never <code>null</code>.
   */
  public Cursor[] getCursors()
  {
    Cursor[] result;
    if ( this.session.hasData() )
    {
      result = this.session.getAcquisitionData().getCursors().clone();
    }
    else
    {
      result = new Cursor[0];
    }
    return result;
  }

  /**
   * Returns bit mask of the enabled channels.
   * 
   * @return the bit mask denoting which channels participate in the actual
   *         data.
   */
  public int getEnabledChannelMask()
  {
    return this.session.hasData() ? this.session.getAcquisitionData().getEnabledChannels() : 0;
  }

  /**
   * Returns whether or not there are defined cursors.
   * 
   * @return <code>true</code> if at least one cursor is defined,
   *         <code>false</code> otherwise.
   */
  public boolean hasDefinedCursors()
  {
    for ( Cursor cursor : getCursors() )
    {
      if ( cursor.isDefined() )
      {
        return true;
      }
    }
    return false;
  }
}
