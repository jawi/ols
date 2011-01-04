/*
 * @(#)CommConnection.java	1.16 02/09/06 @(#)
 *
 * Copyright (c) 2001-2002 Sun Microsystems, Inc.  All rights reserved.
 * PROPRIETARY/CONFIDENTIAL
 * Use is subject to license terms.
 */

package javax.microedition.io;


/**
 * This interface defines a logical serial port connection. A "logical" serial
 * port is defined as a logical connection through which bytes are transferring
 * serially. The logical serial port is defined within the underlying operating
 * system and may not necessarily correspond to a physical RS-232 serial port.
 * For instance, IrDA IRCOMM ports can commonly be configured as a logical
 * serial port within the operating system so that it can act as a "logical"
 * serial port.
 * <P>
 * A comm port is accessed using a Generic Connection Framework string with an
 * explicit port identifier and embedded configuration parameters, each
 * separated with a semi-colon (;).
 * </P>
 * <P>
 * Only one application may be connected to a particular serial port at a given
 * time. An <code>java.io.IOException</code> is thrown, if an attempt is made to
 * open the serial port with <code>Connector.open()</code> and the connection is
 * already open.
 * </P>
 * <P>
 * A URI with the type and parameters is used to open the connection. The scheme
 * (defined in RFC 2396) must be:<BR>
 * <code>comm:&lt;port identifier&gt;[&lt;optional parameters&gt;]</code>
 * </P>
 * <P>
 * The first parameter must be a port identifier, which is a logical device
 * name. These identifiers are most likely device specific and should be used
 * with care.
 * </P>
 * <P>
 * The valid identifiers for a particular device and OS can be queried through
 * the method <code>System.getProperty()</code> using the key
 * <em>"microedition.commports"</em>. A comma separated list of ports is
 * returned which can be combined with a <code>comm:</code> prefix as the URL
 * string to be used to open a serial port connection. (See port naming
 * convention below.)
 * </P>
 * <P>
 * Any additional parameters must be separated by a semi-colon (;) and spaces
 * are not allowed in the string. If a particular optional parameter is not
 * applicable to a particular port, the parameter MAY be ignored. The port
 * identifier MUST NOT contain a semi-colon (;).
 * </P>
 * <P>
 * Legal parameters are defined by the definition of the parameters below.
 * Illegal or unrecognized parameters cause an
 * <code>IllegalArgumentException</code>. If the value of a parameter is
 * supported by the device, it must be honored. If the value of a parameter is
 * not supported a <code>java.io.IOException</code> is thrown. If a
 * <code>baudrate</code> parameter is requested, it is treated in the same way
 * that the <code>setBaudRate</code> method handles baudrates. e.g., if the
 * baudrate requested is not supported the system MAY substitute a valid
 * baudrate, which can be discovered using the <code>getBaudRate</code> method.
 * </P>
 * <H2>Optional Parameters</H2>
 * <P>
 * <TABLE BORDER="1">
 * <TR>
 * <TH>Parameter</TH>
 * <TH>Default</TH>
 * <TH>Description</TH>
 * </TR>
 * <TR>
 * <TD><code>baudrate</code></TD>
 * <TD><code>platform dependent</code></TD>
 * <TD>The speed of the port.</TD>
 * </TR>
 * <TR>
 * <TD><code>bitsperchar</code></TD>
 * <TD><code>8</code></TD>
 * <TD>The number bits per character(<code>7</code> or <code>8</code>).</TD>
 * </TR>
 * <TR>
 * <TD><code>stopbits</code></TD>
 * <TD><code>1</code></TD>
 * <TD>The number of stop bits per char(<code>1</code> or <code>2</code>)</TD>
 * </TR>
 * <TR>
 * <TD><code>parity</code></TD>
 * <TD><code>none</code></TD>
 * <TD>The parity can be <code>odd</code>, <code>even</code>, or
 * <code>none</code>.</TD>
 * </TR>
 * <TR>
 * <TD><code>blocking</code></TD>
 * <TD><code>on</code></TD>
 * <TD>If <code>on</code>, wait for a full buffer when reading.</TD>
 * </TR>
 * <TR>
 * <TD><code>autocts</code></TD>
 * <TD><code>on</code></TD>
 * <TD>If <code>on</code>, wait for the CTS line to be on before writing.</TD>
 * </TR>
 * <TR>
 * <TD><code>autorts</code></TD>
 * <TD><code>on</code></TD>
 * <TD>If <code>on</code>, turn on the RTS line when the input buffer is not
 * full. If <code>off</code>, the RTS line is always on.</TD>
 * </TR>
 * </TABLE>
 * <H2>BNF Format for Connector.open() string</H2>
 * <P>
 * The URI must conform to the BNF syntax specified below. If the URI does not
 * conform to this syntax, an <code>IllegalArgumentException</code> is thrown.
 * </P>
 * <TABLE BORDER="1">
 * <TR>
 * <TD>&lt;comm_connection_string&gt;</TD>
 * <TD>::= "<strong>comm:</strong>"&lt;port_id&gt;[&lt;options_list&gt;] ;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;port_id&gt;</TD>
 * <TD>::= <I>string of alphanumeric characters</I></TD>
 * </TR>
 * <TR>
 * <TD>&lt;options_list&gt;</TD>
 * <TD>::= *(&lt;baud_rate_string&gt;| &lt;bitsperchar&gt;| &lt;stopbits&gt;|
 * &lt;parity&gt;| &lt;blocking&gt;| &lt;autocts&gt;| &lt;autorts&gt;) ; <BR>
 * ; if an option duplicates a previous option in the <BR>
 * ; option list, that option overrides the previous <BR>
 * ; option</TD>
 * </TR>
 * <TR>
 * <TD>&lt;baud_rate_string&gt;</TD>
 * <TD>::= "<strong>;baudrate=</strong>"&lt;baud_rate&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;baud_rate&gt;</TD>
 * <TD>::= <I>string of digits</I></TD>
 * </TR>
 * <TR>
 * <TD>&lt;bitsperchar&gt;</TD>
 * <TD>::= "<strong>;bitsperchar=</strong>"&lt;bit_value&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;bit_value&gt;</TD>
 * <TD>::= "7" | "8"</TD>
 * </TR>
 * <TR>
 * <TD>&lt;stopbits&gt;</TD>
 * <TD>::= "<strong>;stopbits=</strong>"&lt;stop_value&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;stop_value&gt;</TD>
 * <TD>::= "1" | "2"</TD>
 * </TR>
 * <TR>
 * <TD>&lt;parity&gt;</TD>
 * <TD>::= "<strong>;parity=</strong>"&lt;parity_value&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;parity_value&gt;</TD>
 * <TD>::= "even" | "odd" | "none"</TD>
 * </TR>
 * <TR>
 * <TD>&lt;blocking&gt;</TD>
 * <TD>::= "<strong>;blocking=</strong>"&lt;on_off&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;autocts&gt;</TD>
 * <TD>::= "<strong>;autocts=</strong>"&lt;on_off&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;autorts&gt;</TD>
 * <TD>::= "<strong>;autorts=</strong>"&lt;on_off&gt;</TD>
 * </TR>
 * <TR>
 * <TD>&lt;on_off&gt;</TD>
 * <TD>::= "on" | "off"</TD>
 * </TR>
 * </TABLE>
 * <H2>Security</H2>
 * <P>
 * Access to serial ports is restricted to prevent unauthorized transmission or
 * reception of data. The security model applied to the serial port connection
 * is defined in the implementing profile. The security model may be applied on
 * the invocation of the <code>Connector.open()</code> method with a valid
 * serial port connection string. Should the application not be granted access
 * to the serial port through the profile authorization scheme, a
 * <code>java.lang.SecurityException</code> will be thrown from the
 * <code>Connector.open()</code> method. The security model MAY also be applied
 * during execution, specifically when the methods
 * <code>openInputStream()</code>, <code>openDataInputStream()</code>,
 * <code>openOutputStream()</code>, and <code>openDataOutputStream()</code> are
 * invoked.
 * </P>
 * <H2>Examples</H2>
 * <P>
 * The following example shows how a <code>CommConnection</code> would be used
 * to access a simple loopback program.
 * </P>
 * 
 * <PRE>
 * CommConnection cc = ( CommConnection )Connector.open( &quot;comm:com0;baudrate=19200&quot; );
 * int baudrate = cc.getBaudRate();
 * InputStream is = cc.openInputStream();
 * OutputStream os = cc.openOutputStream();
 * int ch = 0;
 * while ( ch != 'Z' )
 * {
 *   os.write( ch );
 *   ch = is.read();
 *   ch++;
 * }
 * is.close();
 * os.close();
 * cc.close();
 * </PRE>
 * <P>
 * The following example shows how a <code>CommConnection</code> would be used
 * to discover available comm ports.
 * </P>
 * 
 * <PRE>
 * String port1;
 * String ports = System.getProperty( &quot;microedition.commports&quot; );
 * int comma = ports.indexOf( ',' );
 * if ( comma &gt; 0 )
 * {
 *   // Parse the first port from the available ports list.
 *   port1 = ports.substring( 0, comma );
 * }
 * else
 * {
 *   // Only one serial port available.
 *   port1 = ports;
 * }
 * </PRE>
 * 
 * <H2>Recommended Port Naming Convention</H2>
 * <P>
 * Logical port names can be defined to match platform naming conventions using
 * any combination of alphanumeric characters. However, it is recommended that
 * ports be named consistently among the implementations of this class according
 * to a proposed convention. VM implementations should follow the following
 * convention:<BR>
 * Port names contain a text abbreviation indicating port capabilities followed
 * by a sequential number for the port. The following device name types should
 * be used:
 * </P>
 * <UL>
 * <LI>COM#, where COM is for RS-232 ports and # is a number assigned to the
 * port</LI>
 * <LI>IR#, where IR is for IrDA IRCOMM ports and # is a number assigned to the
 * port</LI>
 * </UL>
 * <P>
 * This naming scheme allows API users to generally determine the type of port
 * that they would like to use. For instance, if a application desires to "beam"
 * a piece of data, the app could look for "IR#" ports for opening the
 * connection. The alternative is a trial and error approach with all available
 * ports.
 * </P>
 * 
 * @since MIDP 2.0
 */
public interface CommConnection extends StreamConnection
{

  /**
   * Gets the baudrate for the serial port connection.
   * 
   * @return the baudrate of the connection
   * @see #setBaudRate
   */
  public int getBaudRate();

  /**
   * Sets the baudrate for the serial port connection. If the requested
   * <code>baudrate</code> is not supported on the platform, then the system MAY
   * use an alternate valid setting. The alternate value can be accessed using
   * the <code>getBaudRate</code> method.
   * 
   * @param baudrate
   *          the baudrate for the connection
   * @return the previous baudrate of the connection
   * @see #getBaudRate
   */
  public int setBaudRate( int baudrate );

}
