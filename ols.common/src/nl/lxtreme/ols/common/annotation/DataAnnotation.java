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
  // CONSTANTS

  /**
   * Denotes a property to use an alternative color for the annotation. The
   * value of this property is either a {@link java.awt.Color} object, or a
   * triple hex-string value (e.g. "#aabbcc").
   */
  String KEY_COLOR = "color";
  /**
   * Denotes a property that provides additional human-readable information
   * about the annotation. The value of this property should be a string.
   */
  String KEY_DESCRIPTION = "desc";
  /**
   * Denotes a property that describes what kind of annotation this is, e.g.,
   * whether it represents a decoded data symbol, an error, etc. The value of
   * this property should be one of {@link #TYPE_ERROR}, {@link #TYPE_EVENT} or
   * {@link #TYPE_SYMBOL}.
   */
  String KEY_TYPE = "type";

  /** Denotes this annotation as an error. */
  String TYPE_ERROR = "error";
  /** Denotes this annotation as a generic event. */
  String TYPE_EVENT = "event";
  /** Denotes this annotation as a symbol. */
  String TYPE_SYMBOL = "symbol";

  /** Include the description (if defined). */
  int OPTION_WITH_DESCRIPTION = 1;
  /** Include decimal representation of the data. */
  int OPTION_WITH_DEC_DATA = 2;
  /** Include binary representation of the data. */
  int OPTION_WITH_BIN_DATA = 4;
  /** Include octal representation of the data. */
  int OPTION_WITH_OCT_DATA = 8;
  /** Include hexadecimal representation of the data. */
  int OPTION_WITH_HEX_DATA = 16;
  /** Include a character representation of the data. */
  int OPTION_WITH_CHAR_DATA = 32;
  /** Default options. */
  int OPTION_DEFAULT = OPTION_WITH_DESCRIPTION | OPTION_WITH_CHAR_DATA;

  // METHODS

  /**
   * Returns the ending time stamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  long getEndTimestamp();

  /**
   * Returns the (optional) properties of this annotation, such as color,
   * additional type information and so on.
   * 
   * @return a map of additional properties, cannot be <code>null</code> but may
   *         be empty.
   */
  Map<String, Object> getProperties();

  /**
   * Returns the starting time stamp of this annotation.
   * 
   * @return a time stamp, >= 0.
   */
  long getStartTimestamp();

  /**
   * Returns a text representation of this annotation.
   * 
   * @param aOptions
   *          a bit mask representing the various options to include in the
   *          returned text, see {@link #OPTION_*} constants.
   * @return a text representation, never <code>null</code>.
   */
  String getText( int aOptions );
}
