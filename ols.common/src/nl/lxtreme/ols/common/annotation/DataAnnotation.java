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
package nl.lxtreme.ols.common.annotation;


import java.util.*;


/**
 * Denotes an annotation for data, which normally covers a certain range of
 * samples.
 */
public interface DataAnnotation extends Annotation
{
  // METHODS

  /**
   * Returns the (optional) properties of this annotation, such as color,
   * additional type information and so on.
   * 
   * @return a map of additional properties, cannot be <code>null</code> but may
   *         be empty.
   */
  Map<String, Object> getProperties();

  /**
   * Returns the ending time stamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  long getEndTimestamp();

  /**
   * Returns the starting time stamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  long getStartTimestamp();

}
