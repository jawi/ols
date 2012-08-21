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
package nl.lxtreme.ols.tool.serialdebug;


import java.io.*;
import java.util.*;


/**
 * Provides a simple interpreter for handling escapes in strings, such as "$00"
 * or "\00".
 */
final class StringInterpreter
{
  // VARIABLES

  private boolean appendNewLine;

  // METHODS

  /**
   * @param aText
   * @return
   */
  public byte[] interpret( final String aText )
  {
    List<Byte> result = new ArrayList<Byte>();

    if ( aText != null )
    {
      final byte[] bytes;
      try
      {
        bytes = aText.getBytes( "UTF-8" );
      }
      catch ( UnsupportedEncodingException exception )
      {
        throw new RuntimeException( "UTF-8 not supported?!" );
      }

      for ( int i = 0; i < bytes.length; i++ )
      {
        byte c = bytes[i];
        byte la = i < ( bytes.length - 1 ) ? bytes[i + 1] : -1;

        if ( c == '$' )
        {
          if ( la == '$' )
          {
            // Escaped string character...
            result.add( Byte.valueOf( ( byte )'$' ) );
            i++;
          }
          else if ( Character.isDigit( la ) )
          {
            // Escaped digit, pick up to 3 digits...
            int value = 0;
            int origI = i; // points to the '$'!

            do
            {
              value = ( value * 10 ) + ( la - '0' );
              i++;
              la = i < ( bytes.length - 1 ) ? bytes[i + 1] : -1;
            }
            while ( ( ( i - origI ) < 3 ) && Character.isDigit( la ) );

            if ( value > 255 )
            {
              for ( int j = origI; j <= i; j++ )
              {
                result.add( Byte.valueOf( bytes[j] ) );
              }
            }
            else
            {
              result.add( Byte.valueOf( ( byte )value ) );
            }
          }
          else
          {
            // Unknown...
            result.add( Byte.valueOf( c ) );
          }
        }
        else if ( c == '\\' )
        {
          if ( la == '\\' )
          {
            // Escaped backslash...
            result.add( Byte.valueOf( ( byte )'\\' ) );
            i++;
          }
          else if ( Character.isDigit( la ) )
          {
            // Escaped digit, pick up to 3 digits...
            int value = 0;
            int origI = i; // points to the '\\'!

            do
            {
              value = ( value * 10 ) + ( la - '0' );
              i++;
              la = i < ( bytes.length - 1 ) ? bytes[i + 1] : -1;
            }
            while ( ( ( i - origI ) < 3 ) && Character.isDigit( la ) );

            if ( value > 255 )
            {
              for ( int j = origI; j <= i; j++ )
              {
                result.add( Byte.valueOf( bytes[j] ) );
              }
            }
            else
            {
              result.add( Byte.valueOf( ( byte )value ) );
            }
          }
          else if ( Character.isLetter( la ) )
          {
            // Escaped letter...
            switch ( la )
            {
              case 'r':
                result.add( Byte.valueOf( ( byte )'\r' ) );
                i++;
                break;
              case 'n':
                result.add( Byte.valueOf( ( byte )'\n' ) );
                i++;
                break;
              case 'b':
                result.add( Byte.valueOf( ( byte )'\b' ) );
                i++;
                break;
              case 't':
                result.add( Byte.valueOf( ( byte )'\t' ) );
                i++;
                break;
              case 'f':
                result.add( Byte.valueOf( ( byte )'\f' ) );
                i++;
                break;
              default:
                result.add( Byte.valueOf( ( byte )'\\' ) );
                result.add( Byte.valueOf( la ) );
                i++;
                break;
            }
          }
          else
          {
            // Unknown...
            result.add( Byte.valueOf( c ) );
          }
        }
        else
        {
          // Non-escaped byte...
          result.add( Byte.valueOf( c ) );
        }
      }
    }

    if ( this.appendNewLine )
    {
      result.add( Byte.valueOf( ( byte )'\n' ) );
    }

    byte[] resultValue = new byte[result.size()];
    for ( int i = 0; i < resultValue.length; i++ )
    {
      resultValue[i] = result.get( i ).byteValue();
    }

    return resultValue;
  }

  /**
   * Returns the current value of appendNewLine.
   * 
   * @return the appendNewLine
   */
  public boolean isAppendNewLine()
  {
    return this.appendNewLine;
  }

  /**
   * Sets appendNewLine to the given value.
   * 
   * @param aAppendNewLine
   *          the appendNewLine to set.
   */
  public void setAppendNewLine( final boolean aAppendNewLine )
  {
    this.appendNewLine = aAppendNewLine;
  }
}
