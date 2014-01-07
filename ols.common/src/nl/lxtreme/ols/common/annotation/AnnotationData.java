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
package nl.lxtreme.ols.common.annotation;


import java.util.*;


/**
 * Provides a container for {@link Annotation}s.
 */
public interface AnnotationData
{
  // METHODS

  /**
   * Adds a given annotation to this container.
   * 
   * @param aAnnotation
   *          the annotation to add, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given annotation was <code>null</code>.
   */
  void add( Annotation aAnnotation );

  /**
   * Clears all annotations of all channels.
   */
  void clearAll();

  /**
   * Clears all annotations of the channel denoted by the given index.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, &gt;= 0
   *          && &lt; 32.
   * @throws IllegalArgumentException
   *           in case the given channel index was invalid.
   */
  void clear( int aChannelIdx );

  /**
   * Returns all annotations of all channels in a single set.
   * <p>
   * The returned set is sorted on the start timestamp of the annotation in
   * ascending order.
   * </p>
   * 
   * @return a sorted set of annotations, never <code>null</code>.
   */
  SortedSet<Annotation> getAnnotations();

  /**
   * Returns all annotations for a single channel.
   * <p>
   * The returned set is sorted on the start timestamp of the annotation in
   * ascending order.
   * </p>
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, &gt;= 0
   *          && &lt; 32.
   * @return a sorted set of annotations, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given channel index was invalid.
   */
  SortedSet<Annotation> getAnnotations( int aChannelIdx );

  /**
   * Returns all data annotations for a single channel that fall in a given time
   * interval.
   * <p>
   * The returned set is sorted on the start timestamp of the annotation in
   * ascending order.
   * </p>
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, &gt;= 0
   *          && &lt; 32;
   * @param aStartTime
   *          the starting timestamp;
   * @param aEndTime
   *          the ending timestamp.
   * @return a sorted set of annotations, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given channel index was invalid.
   */
  SortedSet<DataAnnotation> getAnnotations( int aChannelIdx, long aStartTime, long aEndTime );

  /**
   * Returns whether or not annotations are present for the channel with the
   * given index.
   * 
   * @param aType
   *          the type of annotations that should be present on the channel,
   *          cannot be <code>null</code>;
   * @param aChannelIdx
   *          the index of the channel to test, &gt;= 0 && &lt; 32.
   * @return <code>true</code> if there are annotations present for the
   *         requested channel, <code>false</code> if no annotations are
   *         present.
   */
  boolean hasAnnotations( Class<? extends Annotation> aType, int aChannelIdx );
}
