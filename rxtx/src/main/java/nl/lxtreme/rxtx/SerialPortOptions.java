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
package nl.lxtreme.rxtx;


import gnu.io.*;

import java.util.regex.*;


/**
 * Provides a convenience class for accessing the serial port properties.
 */
final class SerialPortOptions
{
  // CONSTANTS

  private static final Pattern PATTERN = Pattern.compile(
      "serial:([^;]+)(?:;baudrate=(\\d+))?(?:;options=([5-8])\\s*([mnoe])\\s*(1|1.5|2)\\s*(off|xon_xoff|rts_cts)?)?",
      Pattern.CASE_INSENSITIVE );

  // VARIABLES

  private String portName;
  private int baudrate;
  private int databits;
  private int stopbits;
  private int parityMode;
  private int flowControl;

  // CONSTRUCTORS

  /**
   * Creates a new SerialPortOptions instance.
   * 
   * @param aURI
   *          the serial port URI, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given URI was <code>null</code>.
   */
  public SerialPortOptions( final String aURI ) throws IllegalArgumentException
  {
    if ( aURI == null )
    {
      throw new IllegalArgumentException( "URI cannot be null!" );
    }

    // Default to 9600 baud...
    this.baudrate = 9600;

    // Default to 8 databits, no parity and 1 stopbit...
    this.databits = SerialPort.DATABITS_8;
    this.parityMode = SerialPort.PARITY_NONE;
    this.stopbits = SerialPort.STOPBITS_1;

    this.flowControl = SerialPort.FLOWCONTROL_NONE;

    parseURI( aURI );
  }

  // METHODS

  /**
   * @return the baudrate
   */
  public int getBaudrate()
  {
    return this.baudrate;
  }

  /**
   * @return the databits
   */
  public int getDatabits()
  {
    return this.databits;
  }

  /**
   * @return the flowControl
   */
  public int getFlowControl()
  {
    return this.flowControl;
  }

  /**
   * @return the parityMode
   */
  public int getParityMode()
  {
    return this.parityMode;
  }

  /**
   * @return the portName
   */
  public String getPortName()
  {
    return this.portName;
  }

  /**
   * @return the stopbits
   */
  public int getStopbits()
  {
    return this.stopbits;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseBaudrate( final String aStr )
  {
    int result;
    try
    {
      result = Integer.valueOf( aStr );
    }
    catch ( NumberFormatException exception )
    {
      result = -1;
    }
    return result;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseDataBits( final String aStr )
  {
    if ( "5".equals( aStr ) )
    {
      return SerialPort.DATABITS_5;
    }
    else if ( "6".equals( aStr ) )
    {
      return SerialPort.DATABITS_6;
    }
    else if ( "7".equals( aStr ) )
    {
      return SerialPort.DATABITS_7;
    }
    else if ( "8".equals( aStr ) )
    {
      return SerialPort.DATABITS_8;
    }
    return -1;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseFlowControl( final String aStr )
  {
    if ( "off".equals( aStr ) )
    {
      return SerialPort.FLOWCONTROL_NONE;
    }
    else if ( "xon_xoff".equals( aStr ) )
    {
      return SerialPort.FLOWCONTROL_XONXOFF_IN;
    }
    else if ( "rts_cts".equals( aStr ) )
    {
      return SerialPort.FLOWCONTROL_RTSCTS_IN;
    }
    return -1;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseParityMode( final String aStr )
  {
    if ( "e".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_EVEN;
    }
    else if ( "m".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_MARK;
    }
    else if ( "n".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_NONE;
    }
    else if ( "o".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_ODD;
    }
    else if ( "s".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_SPACE;
    }
    return -1;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseStopBits( final String aStr )
  {
    if ( "1".equals( aStr ) )
    {
      return SerialPort.STOPBITS_1;
    }
    else if ( "1.5".equals( aStr ) )
    {
      return SerialPort.STOPBITS_1_5;
    }
    else if ( "2".equals( aStr ) )
    {
      return SerialPort.STOPBITS_2;
    }
    return -1;
  }

  /**
   * @param aURI
   *          the URI to parse, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given URI was invalid/not parsable.
   */
  private void parseURI( final String aURI ) throws IllegalArgumentException
  {
    final Matcher matcher = PATTERN.matcher( aURI );
    if ( !matcher.matches() )
    {
      throw new IllegalArgumentException( "Illegal URI!" );
    }

    this.portName = matcher.group( 1 );

    if ( matcher.groupCount() > 1 )
    {
      final int parsedBaudrate = parseBaudrate( matcher.group( 2 ) );
      if ( parsedBaudrate >= 0 )
      {
        this.baudrate = parsedBaudrate;
      }

      if ( matcher.groupCount() > 2 )
      {
        final int parsedDataBits = parseDataBits( matcher.group( 3 ) );
        if ( parsedDataBits >= 0 )
        {
          this.databits = parsedDataBits;
        }

        if ( matcher.groupCount() > 3 )
        {
          final int parsedParityMode = parseParityMode( matcher.group( 4 ) );
          if ( parsedParityMode >= 0 )
          {
            this.parityMode = parsedParityMode;
          }

          if ( matcher.groupCount() > 4 )
          {
            final int parsedStopBits = parseStopBits( matcher.group( 5 ) );
            if ( parsedStopBits >= 0 )
            {
              this.stopbits = parsedStopBits;
            }

            if ( matcher.groupCount() > 5 )
            {
              final int parsedFlowControl = parseFlowControl( matcher.group( 6 ) );
              if ( parsedFlowControl >= 0 )
              {
                this.flowControl = parsedFlowControl;
              }
            }
          }
        }
      }
    }
  }
}
