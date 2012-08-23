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


import javax.swing.text.*;


/**
 * Denotes a terminal, which is a text area of fixed dimensions (width and
 * height).
 */
public interface ITerminal
{
  // CONSTANTS

  /**
   * Denotes a property for the cursor position.
   */
  String PROPERTY_CURSOR = "cursor";

  // METHODS

  /**
   * Called to set the new terminal attributes.
   * 
   * @param aAttributes
   *          the attributes to set, can be <code>null</code>.
   */
  void changeAttribute( AttributeSet aAttributes );

  /**
   * Clears the current line.
   * 
   * @param aMode
   *          the clear modus: 0 = erase from cursor to right (default), 1 =
   *          erase from cursor to left, 2 = erase entire line.
   */
  void clearLine( int aMode );

  /**
   * Clears the screen.
   * 
   * @param aMode
   *          the clear modus: 0 = erase from cursor to below (default), 1 =
   *          erase from cursor to top, 2 = erase entire screen.
   */
  void clearScreen( int aMode );

  /**
   * Returns the contents of this terminal as styled document.
   * <p>
   * The returned document contains a property {@link #PROPERTY_CURSOR} (an
   * Integer value) that represents the cursor position in the document.
   * </p>
   * 
   * @return a styled document, never <code>null</code>.
   */
  StyledDocument getAsDocument();

  /**
   * Returns the cursor of this terminal, denoting the current write-position
   * is.
   * 
   * @return the current cursor, never <code>null</code>.
   */
  ICursor getCursor();

  /**
   * Returns the height of this terminal.
   * 
   * @return a height, in characters.
   */
  int getHeight();

  /**
   * Returns the width of this terminal.
   * 
   * @return a width, in characters.
   */
  int getWidth();

  /**
   * Moves the cursor to the given X,Y position.
   * 
   * @param aXpos
   *          the absolute X-position, zero-based (zero meaning start of current
   *          line). If -1, then the current X-position is unchanged.
   * @param aYpos
   *          the absolute Y-position, zero-based (zero meaning start of current
   *          screen). If -1, then the current Y-position is unchanged.
   */
  void moveCursorAbsolute( int aXpos, int aYpos );

  /**
   * Moves the cursor relatively to the given X,Y position.
   * 
   * @param aXpos
   *          the relative X-position to move. If > 0, then move to the right;
   *          if 0, then the X-position is unchanged; if < 0, then move to the
   *          left;
   * @param aYpos
   *          the relative Y-position to move. If > 0, then move to the bottom;
   *          if 0, then the Y-position is unchanged; if < 0, then move to the
   *          top.
   */
  void moveCursorRelative( int aXpos, int aYpos );

  /**
   * Scrolls all lines a given number down.
   * 
   * @param aLines
   *          the number of lines to scroll down, > 0.
   */
  void scrollDown( int aLines );

  /**
   * Scrolls all lines a given number up.
   * 
   * @param aLines
   *          the number of lines to scroll up, > 0.
   */
  void scrollUp( int aLines );

  /**
   * Displays the given character as literal text on the current cursor
   * position.
   * 
   * @param aChars
   *          the character sequence to display, cannot be <code>null</code>.
   * @return the index until which the given sequence is written.
   * @see #getCursor()
   */
  int writeText( CharSequence aChars );
}
