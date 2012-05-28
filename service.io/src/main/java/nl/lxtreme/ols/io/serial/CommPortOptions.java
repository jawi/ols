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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.io.serial;


import java.util.regex.*;

import purejavacomm.*;


/**
 * Provides a convenience class for accessing the serial port properties.
 * <p>
 * Based on documentation from <a href=
 * "http://java.sun.com/javame/reference/apis/jsr218/javax/microedition/io/CommConnection.html"
 * >http://java.sun.com/javame/reference/apis/jsr218/javax/microedition/io/
 * CommConnection.html</a>:
 * </p>
 * <table>
 * <tr>
 * <th>Parameter</th>
 * <th>Default</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>baudrate</td>
 * <td>platform dependent</td>
 * <td>The speed of the port.</td>
 * </tr>
 * <tr>
 * <td>bitsperchar</td>
 * <td>8</td>
 * <td>The number bits per character (7 or 8).</td>
 * </tr>
 * <tr>
 * <td>stopbits</td>
 * <td>1</td>
 * <td>The number of stop bits per char (1, 1.5 or 2).</td>
 * </tr>
 * <tr>
 * <td>parity</td>
 * <td>none</td>
 * <td>The parity can be "odd", "even", or "none".</td>
 * </tr>
 * <tr>
 * <td>blocking</td>
 * <td>on</td>
 * <td>If on, wait for a full buffer when reading.</td>
 * </tr>
 * <tr>
 * <td>autocts</td>
 * <td>on</td>
 * <td>If on, wait for the CTS line to be on before writing (not used in this
 * bundle).</td>
 * </tr>
 * <tr>
 * <td>autorts</td>
 * <td>on</td>
 * <td>If on, turn on the RTS line when the input buffer is not full. If off,
 * the RTS line is always on (not used in this bundle).</td>
 * </tr>
 * <tr>
 * <td>flowcontrol</td>
 * <td>off</td>
 * <td>The flow control to use, can be "off", "xon_xoff", "rts_cts".</td>
 * </tr>
 * <tr>
 * <td>dtr</td>
 * <td>off</td>
 * <td>If on, turn on the DTR line. If off (the default), the DTR is turned low.
 * </td>
 * </tr>
 * <tr>
 * <td>delay</td>
 * <td>0</td>
 * <td>(<b>OLS-specific addition!</b>) If &gt; 0, delays for a number of
 * milliseconds after opening the serial port. If 0 (the default), the port
 * opening is not delayed.</td>
 * </tr>
 * <tr>
 * <td>recv_timeout</td>
 * <td>100</td>
 * <td>(<b>OLS-specific addition!</b>) Allows the receive timeout to be
 * manipulated. Greater values give slower hardware more time to push out their
 * data.</td>
 * </tr>
 * </table>
 */
final class CommPortOptions
{
  // CONSTANTS

  private static final Pattern SCHEMA_REGEX = Pattern.compile( "^comm:([^;]+)(?:;([^\r\n]+))*$" );
  private static final Pattern OPTION_REGEX = Pattern
      .compile(
          "(baudrate|bitsperchar|stopbits|parity|blocking|autocts|autorts|flowcontrol|dtr|delay|recv_timeout)=([\\.\\d\\w_-]+)",
          Pattern.CASE_INSENSITIVE );

  // VARIABLES

  private String portName;
  private int baudrate;
  private int databits;
  private int stopbits;
  private int parityMode;
  private int flowControl;
  private boolean blocking;
  private boolean dtr;
  private int openDelay;
  private int recvTimeout;

  // CONSTRUCTORS

  /**
   * Creates a new SerialPortOptions instance.
   * 
   * @param aURI
   *          the serial port URI, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given URI was <code>null</code>.
   */
  public CommPortOptions( final String aURI ) throws IllegalArgumentException
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

    // Default to NO flow control...
    this.flowControl = SerialPort.FLOWCONTROL_NONE;

    // Default to blocking I/O...
    this.blocking = true;

    // Default to a low DTR signal...
    this.dtr = false;

    // Default to no open delay...
    this.openDelay = 0;

    // Default to a small delay of 100ms...
    this.recvTimeout = 100;

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
   * @return the delay after opening the port, in milliseconds.
   */
  public int getOpenDelay()
  {
    return this.openDelay;
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
   * @return the receive timeout, in milliseconds.
   */
  public int getReceiveTimeout()
  {
    return this.recvTimeout;
  }

  /**
   * @return the stopbits
   */
  public int getStopbits()
  {
    return this.stopbits;
  }

  /**
   * @return
   */
  public boolean isBlocking()
  {
    return this.blocking;
  }

  /**
   * @return the dtr
   */
  public boolean isDTR()
  {
    return this.dtr;
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
      result = Integer.parseInt( aStr );
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
  private int parseOpenDelay( final String aStr )
  {
    int result = 0;
    try
    {
      result = Integer.parseInt( aStr );
    }
    catch ( NumberFormatException exception )
    {
      // Ignore, fall back to default value of 0...
    }
    return result;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseParityMode( final String aStr )
  {
    if ( "even".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_EVEN;
    }
    else if ( "mark".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_MARK;
    }
    else if ( "none".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_NONE;
    }
    else if ( "odd".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_ODD;
    }
    else if ( "space".equalsIgnoreCase( aStr ) )
    {
      return SerialPort.PARITY_SPACE;
    }
    return -1;
  }

  /**
   * @param aStr
   * @return
   */
  private int parseRecvTimeout( final String aStr )
  {
    int result = 100;
    try
    {
      result = Integer.parseInt( aStr );
    }
    catch ( NumberFormatException exception )
    {
      // Ignore, fall back to default value of 0...
    }
    return result;
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
    if ( aURI == null )
    {
      throw new IllegalArgumentException( "URI cannot be null!" );
    }

    final Matcher schemaMatcher = SCHEMA_REGEX.matcher( aURI );
    if ( !schemaMatcher.matches() )
    {
      throw new IllegalArgumentException( "URI invalid!" );
    }

    // port name is mandatory...
    this.portName = schemaMatcher.group( 1 );

    String options = schemaMatcher.group( 2 );
    if ( options == null )
    {
      options = "";
    }

    final Matcher optionMatcher = OPTION_REGEX.matcher( options );
    while ( optionMatcher.find() )
    {
      final String key = optionMatcher.group( 1 ).toLowerCase();
      final String value = optionMatcher.group( 2 );

      if ( "baudrate".equals( key ) )
      {
        int parsedValue = parseBaudrate( value );
        if ( parsedValue >= 0 )
        {
          this.baudrate = parsedValue;
        }
      }
      else if ( "bitsperchar".equals( key ) )
      {
        int parsedValue = parseDataBits( value );
        if ( parsedValue >= 0 )
        {
          this.databits = parsedValue;
        }
      }
      else if ( "stopbits".equals( key ) )
      {
        int parsedValue = parseStopBits( value );
        if ( parsedValue >= 0 )
        {
          this.stopbits = parsedValue;
        }
      }
      else if ( "parity".equals( key ) )
      {
        int parsedValue = parseParityMode( value );
        if ( parsedValue >= 0 )
        {
          this.parityMode = parsedValue;
        }
      }
      else if ( "flowcontrol".equals( key ) )
      {
        int parsedValue = parseFlowControl( value );
        if ( parsedValue >= 0 )
        {
          this.flowControl = parsedValue;
        }
      }
      else if ( "blocking".equals( key ) )
      {
        this.blocking = "on".equalsIgnoreCase( value );
      }
      else if ( "autocts".equals( key ) )
      {
        final boolean parsedValue = "on".equalsIgnoreCase( value );
        if ( parsedValue )
        {
          this.flowControl = SerialPort.FLOWCONTROL_RTSCTS_IN;
        }
      }
      else if ( "autorts".equals( key ) )
      {
        final boolean parsedValue = "on".equalsIgnoreCase( value );
        if ( parsedValue )
        {
          this.flowControl = SerialPort.FLOWCONTROL_RTSCTS_OUT;
        }
      }
      else if ( "dtr".equals( key ) )
      {
        this.dtr = "on".equalsIgnoreCase( value );
      }
      else if ( "delay".equals( key ) )
      {
        this.openDelay = parseOpenDelay( value );
      }
      else if ( "recv_timeout".equals( key ) )
      {
        this.recvTimeout = parseRecvTimeout( value );
      }
    }
  }
}
