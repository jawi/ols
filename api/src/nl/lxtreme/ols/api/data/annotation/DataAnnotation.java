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
 * Denotes an annotation for data, which normally covers a certain range of
 * samples.
 */
public interface DataAnnotation<ANNOTATION_TYPE> extends Annotation<ANNOTATION_TYPE>
{
  // METHODS

  /**
   * Returns the ending timecstamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  public long getEndTimestamp();

  /**
   * Returns the starting time stamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  public long getStartTimestamp();

}
