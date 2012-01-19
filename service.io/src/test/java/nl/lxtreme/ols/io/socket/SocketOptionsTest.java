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


import static org.junit.Assert.*;

import org.junit.*;


/**
 * Test cases for {@link SocketOptions}.
 */
public class SocketOptionsTest
{
  // METHODS

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testInvalidUriFail() throws Exception
  {
    new SocketOptions( "socket://localhost:abc;timeout=250" );
  }

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testNullUriFail() throws Exception
  {
    new SocketOptions( null );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseEmptyUriOk() throws Exception
  {
    new SocketOptions( "socket://" );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseFullUriOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://localhost:1234;timeout=250" );

    assertNotNull( options );
    assertEquals( "localhost", options.getAddress() );
    assertEquals( 1234, options.getPort() );
    assertEquals( 250, options.getTimeout() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseNonEmptyAddressAndTimeoutOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://127.0.0.1;timeout=234" );
    assertEquals( "127.0.0.1", options.getAddress() );
    assertEquals( 0, options.getPort() );
    assertEquals( 234, options.getTimeout() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseNonEmptyAddressOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://127.0.0.1" );
    assertEquals( "127.0.0.1", options.getAddress() );
    assertEquals( 0, options.getPort() );
    assertEquals( 0, options.getTimeout() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseNonEmptyPortAndTimeoutOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://:125;timeout=234" );
    assertNull( options.getAddress() );
    assertEquals( 125, options.getPort() );
    assertEquals( 234, options.getTimeout() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseNonEmptyPortOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://:125" );
    assertNull( options.getAddress() );
    assertEquals( 125, options.getPort() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseNonEmptyTimeoutOk() throws Exception
  {
    final SocketOptions options = new SocketOptions( "socket://;timeout=123" );
    assertNull( options.getAddress() );
    assertEquals( 0, options.getPort() );
    assertEquals( 123, options.getTimeout() );
  }

}
