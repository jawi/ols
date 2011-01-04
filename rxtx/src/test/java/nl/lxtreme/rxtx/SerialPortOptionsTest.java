/**
 * 
 */
package nl.lxtreme.rxtx;


import static org.junit.Assert.*;
import gnu.io.*;

import org.junit.*;


/**
 * @author jawi
 */
public class SerialPortOptionsTest
{
  // METHODS

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testParseEmptyPortNameFail() throws Exception
  {
    new SerialPortOptions( "comm:;baudrate=1" );
  }

  /**
   * @throws Exception
   */
  @Test( expected = IllegalArgumentException.class )
  public void testParseEmptyUriFail() throws Exception
  {
    new SerialPortOptions( "comm:" );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParseFullUriOk() throws Exception
  {
    final SerialPortOptions options = new SerialPortOptions(
        "comm:COM13;baudrate=2400;bitsperchar=5;stopbits=1.5;parity=odd" );

    assertNotNull( options );
    assertEquals( "COM13", options.getPortName() );
    assertEquals( 2400, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_5, options.getDatabits() );
    assertEquals( SerialPort.PARITY_ODD, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1_5, options.getStopbits() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParsePortNameAndOptionsOk() throws Exception
  {
    final SerialPortOptions options = new SerialPortOptions( "comm:/dev/tty.usb0;bitsperchar=5;stopbits=2;parity=mark" );

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
    final SerialPortOptions options = new SerialPortOptions( "comm:/dev/tty.usb0;baudrate=115200" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 115200, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void testParsePortNameOnlyOk() throws Exception
  {
    final SerialPortOptions options = new SerialPortOptions( "comm:/dev/tty.usb0" );

    assertNotNull( options );
    assertEquals( "/dev/tty.usb0", options.getPortName() );
    assertEquals( 9600, options.getBaudrate() );
    assertEquals( SerialPort.DATABITS_8, options.getDatabits() );
    assertEquals( SerialPort.PARITY_NONE, options.getParityMode() );
    assertEquals( SerialPort.STOPBITS_1, options.getStopbits() );
  }
}
