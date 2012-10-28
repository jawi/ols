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
package nl.lxtreme.ols.io.socket;


import java.util.regex.*;


/**
 * Provides the various options for connecting to a socket.
 */
final class SocketOptions
{
  // CONSTANTS

  private static final Pattern SCHEMA_REGEX = Pattern
      .compile( "^socket:(?://)?([^:;]*)(?::(\\d+))?(?:;([^\\r\\n]+))*$" );
  private static final Pattern OPTION_REGEX = Pattern.compile( "(timeout)=([\\.\\d\\w_-]+)", Pattern.CASE_INSENSITIVE );

  // VARIABLES

  private String address;
  private int port;
  private int timeout;

  // CONSTRUCTORS

  /**
   * Creates a new SocketOptions instance.
   * 
   * @param aURI
   *          the socket URI, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given URI was <code>null</code>.
   */
  public SocketOptions( final String aURI ) throws IllegalArgumentException
  {
    // Default connect to the local host...
    this.address = null;

    // Default to no timeout...
    this.timeout = 0;

    parseURI( aURI );
  }

  // METHODS

  /**
   * Returns the address/hostname of the host to connect to.
   * 
   * @return the address or hostname, never <code>null</code>.
   */
  public String getAddress()
  {
    return this.address;
  }

  /**
   * Returns the port number to connect to.
   * 
   * @return the port number, > 0 && < 65536.
   */
  public int getPort()
  {
    return this.port;
  }

  /**
   * Returns the timeout to use for opening the socket.
   * 
   * @return the timeout, in milliseconds.
   */
  public int getTimeout()
  {
    return this.timeout;
  }

  /**
   * @return <code>true</code> if a timeout is specified, <code>false</code>
   *         otherwise.
   */
  public boolean hasTimeout()
  {
    return this.timeout > 0;
  }

  /**
   * @return <code>true</code> if a remote socket is to be connected,
   *         <code>false</code> if a local socket is to be connected.
   */
  public boolean isRemoteSocket()
  {
    return ( this.address != null ) && !this.address.trim().isEmpty();
  }

  /**
   * @param aStr
   * @return
   */
  private int parseTimeout( final String aStr )
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

    // host/address is optional...
    final String addressValue = schemaMatcher.group( 1 );
    if ( ( addressValue != null ) && !addressValue.isEmpty() )
    {
      this.address = addressValue;
    }

    // port is optional as well...
    final String portValue = schemaMatcher.group( 2 );
    if ( ( portValue != null ) && !portValue.isEmpty() )
    {
      this.port = Integer.parseInt( portValue ) & 0xFFFF;
    }

    String options = schemaMatcher.group( 3 );
    if ( ( options == null ) || options.trim().isEmpty() )
    {
      // We're done...
      return;
    }

    final Matcher optionMatcher = OPTION_REGEX.matcher( options );
    while ( optionMatcher.find() )
    {
      final String key = optionMatcher.group( 1 ).toLowerCase();
      final String value = optionMatcher.group( 2 );

      if ( "timeout".equals( key ) )
      {
        int parsedValue = parseTimeout( value );
        if ( parsedValue >= 0 )
        {
          this.timeout = parsedValue;
        }
      }
    }
  }
}
