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
package nl.lxtreme.ols.tool.serialdebug.terminal;


/**
 * Denotes a cursor, or the current write-position of a terminal.
 */
public interface ICursor
{
  // METHODS

  /**
   * Returns the blinking rate of this cursor.
   * 
   * @return the current blinking rate, in milliseconds.
   * @see #setBlinkRate(int)
   */
  int getBlinkRate();

  /**
   * Returns the X-position of the cursor.
   * 
   * @return a X-position, >= 0.
   */
  int getX();

  /**
   * Returns the Y-position of the cursor.
   * 
   * @return a Y-position, >= 0.
   */
  int getY();

  /**
   * Returns whether or not this cursor is visible on screen.
   * 
   * @return <code>true</code> if this cursor is currently visible,
   *         <code>false</code> otherwise.
   */
  boolean isVisible();

  /**
   * Sets the blinking rate of this cursor.
   * 
   * @param aRate
   *          a blinking rate, in milliseconds. A rate of 0 means no blinking.
   */
  void setBlinkRate( int aRate );

  /**
   * Sets the visibility of the cursor.
   * 
   * @param aVisible
   *          <code>true</code> to make the cursor visible, <code>false</code>
   *          to hide the cursor.
   */
  void setVisible( boolean aVisible );
}
