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
import nl.lxtreme.ols.common.annotation.*;


/**
 * Represents a "session" that holds the users' current dataset(s) and other
 * session-related information.
 */
public interface Session
{
  // CONSTANTS

  /** The topic prefix */
  String TOPIC_PREFIX = Session.class.getPackage().getName().replaceAll( "\\.", "/" );

  /**
   * Denotes all topics used by this interface.
   */
  String TOPIC_ANY = TOPIC_PREFIX + "/*";

  /**
   * Denotes the topic at which changes in. The associated event contains a
   * single property, {@link #KEY_ACQUISITION_DATA}, which contains the
   * <em>new</em> acquisition data.
   */
  String TOPIC_ACQUISITION_DATA_CHANGED = TOPIC_PREFIX + "/dataChanged";

  /**
   * Denotes the topic to listen for when an annotation is added. The associated
   * event contains a single property, {@link #KEY_ANNOTATION}, which denotes
   * the added annotation.
   */
  String TOPIC_ANNOTATION_ADDED = TOPIC_PREFIX + "/annotation/added";

  /**
   * Denotes the topic to listen for when an annotation is cleared. The
   * associated event contains a single property, {@link #KEY_CHANNEL}, which if
   * non-null represents the channel on which the annotations should be cleared.
   * If null, all annotations on all channels should be cleared.
   */
  String TOPIC_ANNOTATION_CLEARED = TOPIC_PREFIX + "/annotation/cleared";

  /**
   * Denotes the event property that contains the changed acquisition data. The
   * value is an {@link AcquisitionData} value, which can be <code>null</code>.
   */
  String KEY_ACQUISITION_DATA = "acquisitionData";

  /**
   * Denotes the event property that contains the added annotation. The value is
   * a {@link Annotation} value, which is never <code>null</code>.
   */
  String KEY_ANNOTATION = "annotation";

  /**
   * Denotes the event property that represent the channel index (as
   * {@link Integer}) for which an annotation is added or for which the
   * annotations should be cleared. In the latter case, if the value of this
   * property is <code>null</code>, all annotations on all channels should be
   * cleared.
   */
  String KEY_CHANNEL = "channel";

  // METHODS

  /**
   * Returns the current acquisition data.
   * 
   * @return the current acquisition data, can only be <code>null</code> if no
   *         data is available.
   */
  AcquisitionData getAcquisitionData();

  /**
   * Returns the current annotation data.
   * 
   * @return the annotation data container, can only be <code>null</code> if no
   *         data is available.
   */
  AnnotationData getAnnotationData();

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
