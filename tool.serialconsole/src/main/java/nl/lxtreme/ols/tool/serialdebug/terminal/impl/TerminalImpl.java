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
package nl.lxtreme.ols.tool.serialdebug.terminal.impl;


import java.awt.*;
import java.util.*;

import javax.swing.text.*;

import nl.lxtreme.ols.tool.serialdebug.terminal.*;


/**
 * Provides an implementation of {@link ITerminal}.
 */
public class TerminalImpl implements ITerminal
{
  // INNER TYPES

  /**
   * Denotes a cell of text in this terminal.
   */
  protected static class TextCell
  {
    // VARIABLES

    private final AttributeSet attributes;
    private final char text;

    // CONSTRUCTORS

    /**
     * Creates a new {@link TextCell} instance.
     */
    public TextCell()
    {
      this( ' ', null );
    }

    /**
     * Creates a new {@link TextCell} instance.
     */
    public TextCell( final AttributeSet aAttributes )
    {
      this( ' ', aAttributes );
    }

    /**
     * Creates a new {@link TextCell} instance.
     */
    public TextCell( final char aChar, final AttributeSet aAttributes )
    {
      this.text = aChar;
      this.attributes = aAttributes;
    }

    // METHODS

    /**
     * Returns the current value of attributes.
     * 
     * @return the attributes, can be <code>null</code>.
     */
    public AttributeSet getAttributes()
    {
      return this.attributes;
    }

    /**
     * Returns the current text of this cell.
     * 
     * @return the text in this cell, as character.
     */
    public char getText()
    {
      return this.text;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return "[" + this.text + "]";
    }
  }

  // VARIABLES

  private final CursorImpl cursor;

  private volatile AttributeSet attributes;
  private volatile TextCell[] buffer;

  private int width;
  private int height;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TerminalImpl} instance.
   * 
   * @param aWidth
   *          the width of this terminal, in characters;
   * @param aHeight
   *          the height of this terminal, in characters.
   */
  public TerminalImpl( final int aWidth, final int aHeight )
  {
    setDimensions( aWidth, aHeight );

    this.cursor = new CursorImpl();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void changeAttribute( final AttributeSet aAttributes )
  {
    this.attributes = aAttributes;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearLine( final int aMode )
  {
    final int xPos = this.cursor.getX();
    final int yPos = this.cursor.getY();

    int idx;

    switch ( aMode )
    {
      case 0:
        // erase from cursor to end of line...
        idx = getAbsoluteIndex( xPos, yPos );
        break;
      case 1:
        // erase from cursor to start of line...
        idx = getAbsoluteIndex( xPos, yPos );
        break;
      case 2:
        // erase entire line...
        idx = getAbsoluteIndex( 0, yPos );
        break;

      default:
        throw new IllegalArgumentException( "Invalid clear line mode!" );
    }

    clearLine( aMode, idx );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearScreen( final int aMode )
  {
    final int xPos = this.cursor.getX();
    final int yPos = this.cursor.getY();

    clearScreen( aMode, getAbsoluteIndex( xPos, yPos ) );
  }

  /**
   * {@inheritDoc}
   */
  public final StyledDocument getAsDocument()
  {
    DefaultStyledDocument result = new DefaultStyledDocument();

    try
    {
      int cursorIdx = getAbsoluteCursorIndex();
      Integer cursorPos = null;

      for ( int idx = getFirstAbsoluteIndex(), offset = 0; idx < getLastAbsoluteIndex(); idx++ )
      {
        if ( idx == cursorIdx )
        {
          cursorPos = Integer.valueOf( offset );
        }

        if ( ( idx > 0 ) && ( ( idx % getWidth() ) == 0 ) )
        {
          result.insertString( offset++, "\n", null );
        }

        TextCell cell = getCellAt( idx );
        if ( cell != null )
        {
          result.insertString( offset++, Character.toString( cell.getText() ), cell.getAttributes() );
        }
      }

      if ( cursorPos == null )
      {
        cursorPos = Integer.valueOf( result.getLength() );
      }

      result.putProperty( PROPERTY_CURSOR, cursorPos );
    }
    catch ( BadLocationException exception )
    {
      throw new RuntimeException( exception );
    }

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ICursor getCursor()
  {
    return this.cursor;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getHeight()
  {
    return this.height;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getWidth()
  {
    return this.width;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void moveCursorAbsolute( final int aXpos, final int aYpos )
  {
    int xPos = aXpos;
    if ( xPos < 0 )
    {
      xPos = this.cursor.getX();
    }
    if ( xPos >= this.width )
    {
      xPos = this.width - 1;
    }

    int yPos = aYpos;
    if ( yPos < 0 )
    {
      yPos = this.cursor.getY();
    }
    if ( yPos >= this.height )
    {
      yPos = this.height - 1;
    }

    this.cursor.setPosition( xPos, yPos );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void moveCursorRelative( final int aXpos, final int aYpos )
  {
    int xPos = Math.max( 0, this.cursor.getX() + aXpos );
    int yPos = Math.max( 0, this.cursor.getY() + aYpos );

    moveCursorAbsolute( xPos, yPos );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void scrollDown( final int aLines )
  {
    if ( aLines < 1 )
    {
      throw new IllegalArgumentException( "Invalid number of lines!" );
    }

    if ( aLines >= this.height )
    {
      // Simply clear the screen...
      clearScreen( 2 );
    }
    else
    {
      int startPos = this.width * aLines;
      System.arraycopy( this.buffer, 0, this.buffer, startPos, this.buffer.length - startPos );
      Arrays.fill( this.buffer, 0, startPos, null );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void scrollUp( final int aLines )
  {
    if ( aLines < 1 )
    {
      throw new IllegalArgumentException( "Invalid number of lines!" );
    }

    if ( aLines >= this.height )
    {
      // Simply clear the screen...
      clearScreen( 2 );
    }
    else
    {
      int startPos = this.width * aLines;
      System.arraycopy( this.buffer, startPos, this.buffer, 0, this.buffer.length - startPos - 1 );
      Arrays.fill( this.buffer, this.buffer.length - startPos, this.buffer.length, null );
    }
  }

  /**
   * Sets the dimensions of this terminal to the given width and height.
   * 
   * @param aWidth
   *          the new width of this terminal, > 0;
   * @param aHeight
   *          the new height of this terminal, > 0.
   */
  public void setDimensions( final int aWidth, final int aHeight )
  {
    if ( aWidth <= 0 )
    {
      throw new IllegalArgumentException( "Invalid width!" );
    }
    if ( aHeight <= 0 )
    {
      throw new IllegalArgumentException( "Invalid height!" );
    }

    if ( ( aWidth == this.width ) && ( aHeight == this.height ) )
    {
      // Nothing to do...
      return;
    }

    this.width = aWidth;
    this.height = aHeight;

    TextCell[] newBuffer = new TextCell[this.width * this.height];
    if ( this.buffer != null )
    {
      int srcPos = Math.max( 0, this.buffer.length - newBuffer.length );
      int destPos = Math.max( 0, newBuffer.length - this.buffer.length );
      int length = this.buffer.length - srcPos;

      System.arraycopy( this.buffer, srcPos, newBuffer, destPos, length );
    }
    this.buffer = newBuffer;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int writeText( final CharSequence aChars )
  {
    if ( aChars == null )
    {
      throw new IllegalArgumentException( "Chars cannot be null!" );
    }

    int idx = getAbsoluteCursorIndex();

    for ( int i = 0; i < aChars.length(); i++ )
    {
      char c = aChars.charAt( i );

      switch ( c )
      {
        case '\010':
          // Backspace
          idx = removeChar( --idx );
          break;

        case '\007':
          // Bell
          Toolkit.getDefaultToolkit().beep();
          break;

        case '\012':
          // Newline
          idx += this.width;
          break;

        case '\015':
          // Carriage return
          idx -= ( idx % this.width );
          break;

        default:
          idx = writeChar( idx, c ) + 1;
          break;
      }
    }

    updateCursorByAbsoluteIndex( idx );
    return aChars.length();
  }

  /**
   * Clears the line using the given absolute index as cursor position.
   * 
   * @param aMode
   *          the clear modus: 0 = erase from cursor to right (default), 1 =
   *          erase from cursor to left, 2 = erase entire line.
   * @param aAbsoluteIndex
   *          the absolute index of the cursor.
   */
  protected final void clearLine( final int aMode, final int aAbsoluteIndex )
  {
    int yPos = ( int )Math.floor( aAbsoluteIndex / this.width );
    int xPos = aAbsoluteIndex - ( yPos * this.width );

    int idx;
    int length;

    switch ( aMode )
    {
      case 0:
        // erase from cursor to end of line...
        idx = aAbsoluteIndex;
        length = this.width - xPos;
        break;
      case 1:
        // erase from cursor to start of line...
        idx = aAbsoluteIndex - xPos;
        length = xPos;
        break;
      case 2:
        // erase entire line...
        idx = aAbsoluteIndex;
        length = this.width;
        break;

      default:
        throw new IllegalArgumentException( "Invalid clear line mode!" );
    }

    for ( int i = 0; i < length; i++ )
    {
      removeChar( idx++ );
    }
  }

  /**
   * Clears the screen using the given absolute index as cursor position.
   * 
   * @param aMode
   *          the clear modus: 0 = erase from cursor to below (default), 1 =
   *          erase from cursor to top, 2 = erase entire screen.
   * @param aAbsoluteIndex
   *          the absolute index of the cursor.
   */
  protected final void clearScreen( final int aMode, final int aAbsoluteIndex )
  {
    switch ( aMode )
    {
      case 0:
        // erase from cursor to end of screen...
        int lastIdx = getLastAbsoluteIndex();
        for ( int i = aAbsoluteIndex; i < lastIdx; i++ )
        {
          removeChar( i );
        }
        break;
      case 1:
        // erase from cursor to start of screen...
        int firstIdx = getFirstAbsoluteIndex();
        for ( int i = firstIdx; i < aAbsoluteIndex; i++ )
        {
          removeChar( i );
        }
        break;
      case 2:
        // erase entire screen...
        Arrays.fill( this.buffer, getFirstAbsoluteIndex(), getLastAbsoluteIndex(), null );
        moveCursorAbsolute( 0, 0 );
        break;

      default:
        throw new IllegalArgumentException( "Invalid clear screen mode!" );
    }
  }

  /**
   * Returns the absolute index according to the current cursor position.
   * 
   * @return an absolute index of the cursor position, >= 0.
   */
  protected final int getAbsoluteCursorIndex()
  {
    return getAbsoluteIndex( this.cursor.getX(), this.cursor.getY() );
  }

  /**
   * Returns the absolute index according to the given X,Y-position.
   * 
   * @param aXpos
   *          the X-position;
   * @param aYpos
   *          the Y-position.
   * @return an absolute index of the cursor position, >= 0.
   */
  protected final int getAbsoluteIndex( final int aXpos, final int aYpos )
  {
    return ( aYpos * this.width ) + aXpos;
  }

  /**
   * Returns the current value of attributes.
   * 
   * @return the attributes, can be <code>null</code>.
   */
  protected final AttributeSet getAttributes()
  {
    return this.attributes;
  }

  /**
   * Returns the cell at the given absolute index.
   * 
   * @param aAbsoluteIndex
   *          the absolute of the cell to retrieve.
   * @return the text cell at the given index, can be <code>null</code> if no
   *         cell is defined.
   */
  protected final TextCell getCellAt( final int aAbsoluteIndex )
  {
    return this.buffer[aAbsoluteIndex];
  }

  /**
   * Returns the cell at the given X,Y-position.
   * 
   * @param aXpos
   *          the X-position of the cell to retrieve;
   * @param aYpos
   *          the Y-position of the cell to retrieve.
   * @return the text cell at the given X,Y-position, or <code>null</code> if
   *         there is no such cell.
   */
  protected final TextCell getCellAt( final int aXpos, final int aYpos )
  {
    return getCellAt( getAbsoluteIndex( aXpos, aYpos ) );
  }

  /**
   * @return the first absolute index of this screen, >= 0.
   */
  protected final int getFirstAbsoluteIndex()
  {
    return getAbsoluteIndex( 0, 0 );
  }

  /**
   * @return the last absolute index of this screen, >= 0.
   */
  protected final int getLastAbsoluteIndex()
  {
    return getAbsoluteIndex( this.width - 1, this.height - 1 );
  }

  /**
   * Removes the character at the absolute index.
   * 
   * @param aAbsoluteIndex
   *          the index on which to remove the character, >= 0.
   * @return the absolute index on which the character was removed.
   */
  protected final int removeChar( final int aAbsoluteIndex )
  {
    int idx = aAbsoluteIndex;
    int firstIdx = getFirstAbsoluteIndex();
    if ( idx < firstIdx )
    {
      return firstIdx;
    }

    TextCell cell = this.buffer[aAbsoluteIndex];
    if ( cell != null )
    {
      cell = new TextCell( cell.getAttributes() );
    }
    this.buffer[idx] = cell;
    return idx;
  }

  /**
   * Updates the cursor according to the given absolute index.
   * 
   * @param aIndex
   *          the absolute index to convert back to a X,Y-position.
   */
  protected final void updateCursorByAbsoluteIndex( final int aIndex )
  {
    int yPos = ( int )Math.floor( aIndex / this.width );
    int xPos = aIndex - ( yPos * this.width );

    this.cursor.setPosition( xPos, yPos );
  }

  /**
   * Writes a given character at the absolute index, scrolling the screen up if
   * beyond the last index is written.
   * 
   * @param aAbsoluteIndex
   *          the index on which to write the given char, >= 0;
   * @param aChar
   *          the character to write.
   * @return the absolute index on which the character was written.
   */
  protected final int writeChar( final int aAbsoluteIndex, final char aChar )
  {
    int idx = aAbsoluteIndex;
    int lastIdx = getLastAbsoluteIndex();
    if ( idx >= lastIdx )
    {
      scrollUp( 1 );
      idx -= this.width;
    }
    this.buffer[idx] = new TextCell( aChar, this.attributes );
    return idx;
  }
}
