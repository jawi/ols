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
package nl.lxtreme.ols.common.session;


import nl.lxtreme.ols.common.acquisition.*;


/**
 * Represents a "session" that holds the users' current dataset(s) and other
 * session-related information.
 */
public interface Session
{
  // CONSTANTS

  /** The topic prefix */
  String TOPIC_PREFIX = Session.class.getPackage().getName().replaceAll( "\\.", "/" );

  /** Denotes the topic at which changes in */
  String TOPIC_ACQUISITION_DATA_CHANGED = TOPIC_PREFIX + "/dataChanged";

  /**
   * Denotes the event property that contains the changed acquisition data. The
   * value is an {@link AcquisitionData} value, which can be <code>null</code>.
   */
  String KEY_ACQUISITION_DATA = "acquisitionData";

  // METHODS

  /**
   * Returns the current acquisition data.
   * 
   * @return the current acquisition data, can only be <code>null</code> if no
   *         data is available.
   */
  AcquisitionData getAcquisitionData();

  /**
   * Returns whether or not there is data present.
   * 
   * @return <code>true</code> if there is data available, <code>false</code>
   *         otherwise.
   */
  boolean hasData();

  /**
   * Resets the current session, clearing all contained data.
   */
  void reset();

  /**
   * Sets the acquisition data to the one given.
   * 
   * @param aData
   *          the acquisition data to set, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given parameter was <code>null</code>.
   */
  void setAcquisitionData( AcquisitionData aData ) throws IllegalArgumentException;
}
