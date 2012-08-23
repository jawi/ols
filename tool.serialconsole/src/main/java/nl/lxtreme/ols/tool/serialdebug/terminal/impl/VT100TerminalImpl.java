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

import nl.lxtreme.ols.tool.serialdebug.*;


/**
 * Represents a VT100 terminal implementation.
 */
public class VT100TerminalImpl extends TerminalImpl
{
  // CONSTANTS

  private static final Color[] XTERM_COLORS = { new Color( 0, 0, 0 ), // Black
      new Color( 205, 0, 0 ), // Red
      new Color( 0, 205, 0 ), // Green
      new Color( 205, 205, 0 ), // Yellow
      new Color( 0, 0, 238 ), // Blue
      new Color( 205, 0, 205 ), // Magenta
      new Color( 0, 205, 205 ), // Cyan
      new Color( 229, 229, 229 ), // White
  };

  // CONSTRUCTORS

  /**
   * Creates a new {@link VT100TerminalImpl} instance.
   * 
   * @param aWidth
   *          the width of this terminal, in characters;
   * @param aHeight
   *          the height of this terminal, in characters.
   */
  public VT100TerminalImpl( final int aWidth, final int aHeight )
  {
    super( aWidth, aHeight );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int writeText( final CharSequence aText )
  {
    boolean echo = true;
    boolean csiFound = false;
    boolean dec = false;
    Stack<Object> paramStack = new Stack<Object>();

    int lastPosition = 0;

    int idx = getAbsoluteCursorIndex();

    final int length = aText.length();
    for ( int i = 0; i < length; i++ )
    {
      int c = aText.charAt( i );
      int la = ( i < ( length - 1 ) ) ? aText.charAt( i + 1 ) : -1;

      if ( c == '\033' )
      {
        // Escape character...
        csiFound = false;
        dec = false;
        paramStack.clear();

        if ( la == '[' )
        {
          // CSI found...
          csiFound = true;
          i++;
        }
        else if ( la == '_' )
        {
          // APC found; ignore all text until <ESC>\ is found...
          echo = false;
          i++;
          lastPosition = i;
        }
        else if ( la == '\\' )
        {
          // ST found; re-enable echo...
          echo = true;
          i++;
          lastPosition = i;
        }
        else if ( ( la == 'D' ) || ( la == 'E' ) || ( la == 'H' ) || ( la == 'M' ) || ( la == 'N' ) || ( la == 'O' )
            || ( la == 'P' ) || ( la == 'V' ) || ( la == 'W' ) || ( la == 'X' ) || ( la == 'Z' ) //
            || ( la == ']' ) || ( la == '^' ) )
        {
          // simple C1 characters we simply ignore...
          i++;
          lastPosition = i;
        }
        else if ( ( la == ' ' ) || ( la == '#' ) || ( la == '%' ) || ( la == '(' ) || ( la == ')' ) || ( la == '*' )
            || ( la == '+' ) )
        {
          // VT100 commands, take one character as parameter which we'll
          // ignore...
          i += 2;
          lastPosition = i;
        }
        else if ( la >= 0 )
        {
          // Unknown C1...
          System.out.printf( "Unknown C1: %c %n", Integer.valueOf( la ) );
          i++;
          lastPosition = i;
        }
      }
      else if ( c == '\007' )
      {
        // Bell...
        Toolkit.getDefaultToolkit().beep();
      }
      else if ( c == '\010' )
      {
        // Backspace...
        idx--;
      }
      else if ( c == '\n' )
      {
        // Linefeed...
        idx += getWidth();
      }
      else if ( c == '\r' )
      {
        // Carriage return...
        idx -= ( idx % getWidth() );
      }
      else
      {
        // All other cases...
        if ( !csiFound )
        {
          if ( echo )
          {
            idx = writeChar( idx, ( char )c ) + 1;
          }
          lastPosition = i;
        }
        else
        {
          switch ( c )
          {
            case 'c':
            {
              // Send Device Attributes (Primary DA)
              int arg = getInteger( paramStack, 0 );
              if ( arg != 0 )
              {
                System.out.printf( "Unknown DA: %d %n", Integer.valueOf( arg ) );
              }
              else
              {
                // Send back that we're a VT220... XXX
                // aCallback.sendRaw( ( byte )'\033', ( byte )'[', ( byte )'6',
                // ( byte )'c' );
              }
              csiFound = false;
              lastPosition = i;
              break;
            }

            // case 'f' is handled with case 'H'!

            case 'h':
            {
              // Set mode / DEC Private Mode Set ...
              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'l':
            {
              // Reset mode / DEC Private Mode Reset...
              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'm':
            {
              // Turn on/off character attributes ...
              SimpleAttributeSet attrs = createSimpleAttributeSet();
              while ( !paramStack.isEmpty() )
              {
                handleGraphicsRendering( attrs, getInteger( paramStack, 0 ) );
              }
              changeAttribute( attrs );
              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'n':
            {
              //
              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'r':
            {
              // Set scrolling region (top; bottom)...
              int bottom = getInteger( paramStack, 1 );
              int top = getInteger( paramStack, 1 );

              System.out.printf( "TODO: set scrolling region (%d, %d)%n", top, bottom );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'A':
            {
              // Cursor Up N Times
              int n = getInteger( paramStack, 1 );

              idx = Math.max( getFirstAbsoluteIndex(), idx - ( n * getWidth() ) );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'B':
            {
              // Cursor Down N Times
              int n = getInteger( paramStack, 1 );

              idx = Math.min( getLastAbsoluteIndex(), idx + ( n * getWidth() ) );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'C':
            {
              // Cursor Forward N Times
              int n = getInteger( paramStack, 1 );

              idx = Math.min( getLastAbsoluteIndex(), idx + n );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'D':
            {
              // Cursor Backward N Times
              int n = getInteger( paramStack, 1 );

              idx = Math.max( getFirstAbsoluteIndex(), idx - n );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'E':
            {
              // Cursor Next Line N Times
              int n = getInteger( paramStack, 1 );

              System.out.printf( "Next line: %d%n", n );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'F':
            {
              // Cursor Previous Line N Times
              int n = getInteger( paramStack, 1 );

              System.out.printf( "Previous line: %d%n", n );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'G':
            {
              // Cursor Character Absolute [x] ...
              int x = getInteger( paramStack, 0 );

              idx = Math.max( getFirstAbsoluteIndex(), ( idx - ( idx % getWidth() ) ) + x );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'f':
            case 'H':
            {
              // Move cursor to x,y...
              int x = getInteger( paramStack, 1 );
              int y = getInteger( paramStack, 1 );

              idx = getAbsoluteIndex( x - 1, y - 1 );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'J':
            {
              // Clear screen...
              int mode = getInteger( paramStack, 0 );

              clearScreen( mode, idx );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case 'K':
            {
              // Clear line...
              int mode = getInteger( paramStack, 0 );

              clearLine( mode, idx );

              csiFound = false;
              lastPosition = i;
              break;
            }

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            {
              int value = c - '0';
              while ( Character.isDigit( la ) )
              {
                i++;
                c = aText.charAt( i );
                la = ( i < ( length - 1 ) ) ? aText.charAt( i + 1 ) : -1;

                value = ( value * 10 ) + ( c - '0' );
              }
              paramStack.push( Integer.valueOf( value ) );
              break;
            }

            case ';':
              // Param separator...
              break;

            case '?':
            {
              // DEC specific...
              dec = true;
              break;
            }

            default:
              // Unknown
              System.out.printf( "UNRECOGNIZED: csi" );
              if ( dec )
              {
                System.out.printf( " ?" );
              }
              while ( !paramStack.isEmpty() )
              {
                System.out.printf( " %s", paramStack.firstElement() );
                paramStack.pop();
              }
              System.out.printf( " %c %n", c );
              csiFound = false;
              lastPosition = i;
              break;
          }
        }
      }
    }

    updateCursorByAbsoluteIndex( idx );

    return lastPosition;
  }

  /**
   * Converts a given index to a color value.
   * 
   * @param aIndex
   *          the color index to return as color, >= 0 && <= 7.
   * @return a color value, never <code>null</code>.
   */
  private Color convertToColor( final int aIndex )
  {
    return XTERM_COLORS[aIndex % XTERM_COLORS.length];
  }

  /**
   * @return a new attribute set, never <code>null</code>.
   */
  private SimpleAttributeSet createSimpleAttributeSet()
  {
    SimpleAttributeSet attrs = new SimpleAttributeSet();
    StyleConstants.setFontFamily( attrs, "Monospaced" );
    StyleConstants.setFontSize( attrs, 14 );
    StyleConstants.setForeground( attrs, ConsolePane.PLAIN_TEXT_COLOR );
    StyleConstants.setBackground( attrs, ConsolePane.BACKGROUND_COLOR );
    AttributeSet existingAttributes = getAttributes();
    if ( existingAttributes != null )
    {
      attrs.addAttributes( existingAttributes );
    }
    return attrs;
  }

  /**
   * @param aParamStack
   * @param aDefault
   * @return
   */
  private int getInteger( final Stack<Object> aParamStack, final int aDefault )
  {
    int result = aDefault;
    if ( !aParamStack.isEmpty() )
    {
      Object param = aParamStack.pop();
      if ( param instanceof Integer )
      {
        result = ( ( Integer )param ).intValue();
      }
    }
    return result;
  }

  /**
   * @param aAttrs
   * @param aA
   *          the first parameter;
   * @param aB
   *          the second parameter;
   */
  private void handleGraphicsRendering( final SimpleAttributeSet aAttrs, final int aA )
  {
    if ( aA == 0 )
    {
      StyleConstants.setFontFamily( aAttrs, "Monospaced" );
      StyleConstants.setFontSize( aAttrs, 14 );
      StyleConstants.setForeground( aAttrs, ConsolePane.PLAIN_TEXT_COLOR );
      StyleConstants.setBackground( aAttrs, ConsolePane.BACKGROUND_COLOR );
      StyleConstants.setBold( aAttrs, false );
      StyleConstants.setUnderline( aAttrs, false );
    }
    else if ( aA == 1 )
    {
      StyleConstants.setBold( aAttrs, true );
    }
    else if ( aA == 4 )
    {
      StyleConstants.setUnderline( aAttrs, true );
    }
    else if ( aA == 7 )
    {
      Color fg = StyleConstants.getForeground( aAttrs );
      Color bg = StyleConstants.getBackground( aAttrs );
      StyleConstants.setForeground( aAttrs, bg );
      StyleConstants.setBackground( aAttrs, fg );
    }
    else if ( aA == 21 )
    {
      StyleConstants.setBold( aAttrs, false );
    }
    else if ( aA == 24 )
    {
      StyleConstants.setUnderline( aAttrs, false );
    }
    else if ( aA == 27 )
    {
      Color fg = StyleConstants.getForeground( aAttrs );
      Color bg = StyleConstants.getBackground( aAttrs );
      StyleConstants.setForeground( aAttrs, bg );
      StyleConstants.setBackground( aAttrs, fg );
    }
    else if ( ( aA >= 30 ) && ( aA <= 37 ) )
    {
      // Handle foreground color...
      Color fg = convertToColor( aA - 30 );
      StyleConstants.setForeground( aAttrs, fg );
    }
    else if ( aA == 39 )
    {
      // Default foreground color...
      StyleConstants.setForeground( aAttrs, ConsolePane.PLAIN_TEXT_COLOR );
    }
    else if ( ( aA >= 40 ) && ( aA <= 47 ) )
    {
      Color bg = convertToColor( aA - 40 );
      StyleConstants.setBackground( aAttrs, bg );
    }
    else if ( aA == 49 )
    {
      // Default background color...
      StyleConstants.setBackground( aAttrs, ConsolePane.BACKGROUND_COLOR );
    }
  }
}
