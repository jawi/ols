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
package nl.lxtreme.ols.tool.api;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides the context in which a tool is to be run.
 * <p>
 * For example, it provides the information about which range of samples should
 * be analysed.
 * </p>
 */
public interface ToolContext
{
  // METHODS

  /**
   * Adds a given annotation to this tool context, notifying listeners about
   * this event.
   * 
   * @param aAnnotation
   *          the annotation to add, cannot be <code>null</code>.
   */
  void addAnnotation( Annotation aAnnotation );

  /**
   * Clears all annotations for a channel.
   * 
   * @param aChannelIdxs
   *          the indexes of the channel to clear the annotations for, cannot be
   *          <code>null</code>. Invalid channel indexes are silently ignored.
   */
  void clearAnnotations( int... aChannelIdxs );

  /**
   * Returns the current acquisition data.
   * <p>
   * This method is a shortcut for <tt>getSession().getAcquisitionData()</tt>.
   * </p>
   * 
   * @return the acquisition data, can not be <code>null</code> (meaning that a
   *         tool context can only be created when there is valid acquisition
   *         data).
   * @see #getSession()
   */
  AcquisitionData getAcquisitionData();

  /**
   * Returns the number of channels in the sample data.
   * 
   * @return the channel count, >= 0.
   */
  int getChannelCount();

  /**
   * Returns a bitmask of enabled channels in the sample data.
   * 
   * @return a bitmask of enabled channels, for example, <tt>0xFF</tt> for the
   *         first 8 channels.
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
   * @return a tool progress listener, never <code>null</code>.
   */
  ToolProgressListener getProgressListener();

  /**
   * Provides access to the current session, to obtain the session-related
   * information.
   * 
   * @return the current session, never <code>null</code>.
   */
  Session getSession();

  /**
   * Returns the starting sample index on which the decoding should start.
   * 
   * @return a start sample index, >= 0.
   */
  int getStartSampleIndex();
}

/* EOF */
