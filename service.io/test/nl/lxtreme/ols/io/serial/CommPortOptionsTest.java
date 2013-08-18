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


import static org.junit.Assert.*;

import org.junit.*;

import purejavacomm.*;


/**
 * Tests for {@link CommPortOptions}.
 */
public class CommPortOptionsTest
{
  // METHODS

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testParseEmptyPortNameFail() throws Exception
  {
    new CommPortOptions( "comm:;baudrate=1" );
  }

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testParseEmptyUriFail() throws Exception
  {
    new CommPortOptions( "comm:" );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseFullUriOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions(
        "comm:COM13;baudrate=2400;bitsperchar=5;stopbits=1.5;parity=odd;flowcontrol=xon_xoff;dtr=on" );

    assertNotNull( options );
    assertEquals( "COM13", options.getPortName() );
    assertEquals( 2400, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_5, options.getDatabits() );
    assertEquals( SerialPort.PARITY_ODD, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1_5, options.getStopbits() );
    assertEquals( SerialPort.FLOWCONTROL_XONXOFF_IN, options.getFlowControl() );
    assertTrue( options.isDTR() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseInvalidOpenDelayOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;baudrate=115200;delay=xyz" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
    assertEquals( 0, options.getOpenDelay() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseInvalidReceiveTimeoutOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;baudrate=115200;recv_timeout=xyz" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
    assertEquals( 100, options.getReceiveTimeout() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseOpenDelayOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;baudrate=115200;delay=115" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
    assertEquals( 115, options.getOpenDelay() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParsePortNameAndOptionsOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;bitsperchar=5;stopbits=2;parity=mark" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 9600, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_5, options.getDatabits() );
    assertEquals( SerialPort.PARITY_MARK, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_2, options.getStopbits() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParsePortNameBaudrateOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;baudrate=115200" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
    assertEquals( 0, options.getOpenDelay() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParsePortNameOnlyOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 9600, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseValidReceiveTimeoutOk() throws Exception
  {
    final CommPortOptions options = new CommPortOptions( "comm:/dev/tty.usb0;baudrate=115200;recv_timeout=12" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
    assertEquals( 12, options.getReceiveTimeout() );
  }
}
