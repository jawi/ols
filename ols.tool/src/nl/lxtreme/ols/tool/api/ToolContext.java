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
   * Returns the acquisition result for use in the tool.
   * 
   * @return the data to be analyzed, never <code>null</code>.
   */
  AcquisitionData getData();
}

/* EOF */
