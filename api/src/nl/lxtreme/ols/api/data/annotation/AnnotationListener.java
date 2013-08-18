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
package nl.lxtreme.ols.api.data.annotation;


/**
 * Can be used to create a service that listens for the addition/removal of
 * annotation on channel data.
 */
public interface AnnotationListener
{
  // METHODS

  /**
   * Called when all annotations for all channels need to be cleared.
   * <p>
   * Called <em>before</em> any annotation is added.
   * </p>
   */
  void clearAnnotations();

  /**
   * Called when the annotations for a single channel need to be cleared.
   * <p>
   * Called <em>before</em> any annotation is added.
   * </p>
   * 
   * @param aChannelIdx
   *          the channel index to clear the annotation for, >= 0 && < 32.
   */
  void clearAnnotations( int aChannelIdx );

  /**
   * Called for each annotation.
   * 
   * @param aAnnotation
   *          the (new) annotation, cannot be <code>null</code>.
   */
  void onAnnotation( Annotation<?> aAnnotation );

}
