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
package nl.lxtreme.ols.client.tool;


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

  private final int channelCount;
  private final int enabledChannelMask;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionDataInfo} instance.
   */
  public AcquisitionDataInfo()
  {
    this.channelCount = Ols.MAX_CHANNELS;
    this.enabledChannelMask = ( int )( ( 1L << Ols.MAX_CHANNELS ) - 1L );
  }

  /**
   * Creates a new {@link AcquisitionDataInfo} instance.
   */
  public AcquisitionDataInfo( final Session aSession )
  {
    int cc = Ols.MAX_CHANNELS;
    int ecm = ( int )( ( 1L << cc ) - 1L );

    if ( aSession.hasData() )
    {
      AcquisitionData data = aSession.getAcquisitionData();

      cc = data.getChannelCount();
      ecm = data.getEnabledChannels();
    }

    this.channelCount = cc;
    this.enabledChannelMask = ecm;
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
    return this.channelCount;
  }

  /**
   * Returns bit mask of the enabled channels.
   * 
   * @return the bit mask denoting which channels participate in the actual
   *         data.
   */
  public int getEnabledChannelMask()
  {
    return this.enabledChannelMask;
  }
}
