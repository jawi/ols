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
package nl.lxtreme.ols.io.serial.test;


import java.io.*;

import javax.microedition.io.*;

import junit.framework.*;
import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;
import org.osgi.service.io.*;


/**
 * Tests for CommConnectionFactory.
 */
public class CommConnectionFactoryTest extends TestCase
{
  // METHODS

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  public void ignoreTestCreateExistingConnectionLinux() throws IOException,
      InvalidSyntaxException
  {
    if ( !HostUtils.getHostInfo().isUnix() )
    {
      return;
    }

    final Connection connection = getConnectionFactory().createConnection( "comm:/dev/ttyACM0", 0, false );
    assertNotNull( connection );
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  public void ignoreTestCreateExistingConnectionMacOS() throws IOException,
      InvalidSyntaxException
  {
    if ( !HostUtils.getHostInfo().isMacOS() )
    {
      return;
    }

    final Connection connection = getConnectionFactory().createConnection( "comm:/dev/tty.usbmodemfd131", 0,
        false );
    assertNotNull( "Failed to obtain a valid connection!", connection );
  }

  /**
   * Tests that creating a serial connection works on Solaris platforms.
   */
  public void ignoreTestCreateExistingConnectionSolaris() throws IOException,
      InvalidSyntaxException
  {
    if ( !HostUtils.getHostInfo().isSolaris() )
    {
      return;
    }

    final Connection connection = getConnectionFactory().createConnection( "comm:/dev/term/A", 0, false );
    assertNotNull( connection );
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  public void ignoreTestCreateExistingConnectionWin32() throws IOException,
      InvalidSyntaxException
  {
    if ( !HostUtils.getHostInfo().isWindows() )
    {
      return;
    }

    final Connection connection = getConnectionFactory().createConnection( "comm:com3", 0, false );
    assertNotNull( connection );
  }

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  public void ignoreTestCreateNonExistingConnectionLinux() throws Exception
  {
    if ( !HostUtils.getHostInfo().isUnix() )
    {
      return;
    }

    getConnectionFactory().createConnection( "comm:/dev/ttyS99", 0, false );
    fail( "I/O exception expected!" );
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  public void ignoreTestCreateNonExistingConnectionMacOS() throws Exception
  {
    if ( !HostUtils.getHostInfo().isMacOS() )
    {
      return;
    }

    getConnectionFactory().createConnection( "comm:/dev/tty.usbserial", 0, false );
    fail( "I/O exception expected!" );
  }

  /**
   * Tests that creating a serial connection works on Solaris platforms.
   */
  public void ignoreTestCreateNonExistingConnectionSolaris() throws Exception
  {
    if ( !HostUtils.getHostInfo().isUnix() )
    {
      return;
    }

    getConnectionFactory().createConnection( "comm:/dev/term/XYZ", 0, false );
    fail( "I/O exception expected!" );
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  public void ignoreTestCreateNonExistingConnectionWin32() throws Exception
  {
    if ( !HostUtils.getHostInfo().isWindows() )
    {
      return;
    }

    getConnectionFactory().createConnection( "comm:COM255", 0, false );
    fail( "I/O exception expected!" );
  }

  /**
   * Sets up the test case.
   */
  private ConnectionFactory getConnectionFactory() throws InvalidSyntaxException
  {
    BundleContext context = FrameworkUtil.getBundle( this.getClass() ).getBundleContext();

    final String filter = "(" + ConnectionFactory.IO_SCHEME + "=comm)";
    final ServiceReference[] serviceRefs = context.getServiceReferences( ConnectionFactory.class.getName(), filter );
    assertNotNull( serviceRefs );
    assertTrue( serviceRefs.length == 1 );

    return ( ConnectionFactory )context.getService( serviceRefs[0] );
  }
}
