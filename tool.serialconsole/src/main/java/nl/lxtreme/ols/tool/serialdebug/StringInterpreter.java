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


/**
 * Provides a simple interpreter for handling escapes in strings, such as "$00"
 * or "\00".
 */
final class StringInterpreter
{
  // VARIABLES

  private boolean appendNewLine;

  // METHODS

  public byte[] interpret( final String aText )
  {
    StringBuilder sb = new StringBuilder();

    if ( ( aText != null ) && !"".equals( aText.trim() ) )
    {
      char[] chars = aText.toCharArray();
      for ( int i = 0; i < chars.length; i++ )
      {
        int c = chars[i];
        int la = i < ( chars.length - 1 ) ? chars[i + 1] : -1;

        if ( c == '$' )
        {
          if ( la == '$' )
          {
            // Escaped string character...
            sb.append( '$' );
            i++;
          }
          else if ( Character.isDigit( la ) )
          {
            // Escaped digit, pick up to 3 digits...
            int value = ( la - '0' );
            i++;

            la = i < ( chars.length - 1 ) ? chars[i + 1] : -1;
            if ( Character.isDigit( la ) )
            {
              value = ( value * 10 ) + ( la - '0' );
              i++;

              la = i < ( chars.length - 1 ) ? chars[i + 1] : -1;
              if ( Character.isDigit( la ) )
              {
                value = ( value * 10 ) + ( la - '0' );
                i++;
              }
            }

            if ( value > 255 )
            {
              sb.append( "$" ).append( Integer.toString( value ) );
            }
            else
            {
              sb.append( ( char )value );
            }
          }
          else
          {
            // Unknown...
            sb.append( ( char )c );
          }
        }
        else if ( c == '\\' )
        {
          if ( la == '\\' )
          {
            // Escaped backslash...
            sb.append( "\\" );
            i++;
          }
          else if ( Character.isDigit( la ) )
          {
            // Escaped digit, pick up to 3 digits...
            int value = ( la - '0' );
            i++;

            la = i < ( chars.length - 1 ) ? chars[i + 1] : -1;
            if ( Character.isDigit( la ) )
            {
              value = ( value * 10 ) + ( la - '0' );
              i++;

              la = i < ( chars.length - 1 ) ? chars[i + 1] : -1;
              if ( Character.isDigit( la ) )
              {
                value = ( value * 10 ) + ( la - '0' );
                i++;
              }
            }

            if ( value > 255 )
            {
              sb.append( "\\" ).append( Integer.toString( value ) );
            }
            else
            {
              sb.append( ( char )value );
            }
          }
          else if ( Character.isLetter( la ) )
          {
            // Escaped letter...
            switch ( ( char )la )
            {
              case 'r':
                sb.append( "\r" );
                i++;
                break;
              case 'n':
                sb.append( "\n" );
                i++;
                break;
              case 'b':
                sb.append( "\b" );
                i++;
                break;
              case 't':
                sb.append( "\t" );
                i++;
                break;
              case 'f':
                sb.append( "\f" );
                i++;
                break;
              default:
                sb.append( "\\" ).append( ( char )la );
                i++;
                break;
            }
          }
          else
          {
            // Unknown...
            sb.append( ( char )c );
          }
        }
        else
        {
          sb.append( ( char )c );
        }
      }
    }

    if ( this.appendNewLine )
    {
      sb.append( "\n" );
    }

    return sb.toString().getBytes();
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
