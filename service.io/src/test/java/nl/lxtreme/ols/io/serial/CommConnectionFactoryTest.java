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


import static org.hamcrest.core.IsInstanceOf.*;
import static org.junit.Assert.*;
import static org.junit.Assume.*;
import static org.ops4j.pax.exam.CoreOptions.*;

import java.io.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.junit.runner.*;
import org.ops4j.pax.exam.*;
import org.ops4j.pax.exam.junit.*;
import org.osgi.framework.*;
import org.osgi.service.io.*;


/**
 * Tests for {@link CommConnectionFactory}.
 */
@Ignore
@RunWith( JUnit4TestRunner.class )
public class CommConnectionFactoryTest
{
  // METHODS

  /**
   * Sets up PAX exam to run with the bare minimum of bundles.
   */
  @Configuration
  public static Option[] configure()
  {
    final String env = System.getProperty( "user.home" ) + "/.m2/repository";

    return options( //
        cleanCaches(), //
        localRepository( env ), //
        junitBundles(), //
        mavenBundle().groupId( "org.osgi" ).artifactId( "org.osgi.compendium" ).version( "4.2.0" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "service.io" ).version( "1.0.1" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "util" ).version( "1.1.0-SNAPSHOT" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "org.rxtx" ).version( "2.2.0-10" ) //

        , vmOption( "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5006" ), systemTimeout( 10000 ) //
        );
  }

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  @Test
  public void testCreateExistingConnectionLinux( final BundleContext aContext ) throws IOException,
  InvalidSyntaxException
  {
    assumeTrue( HostUtils.getHostInfo().isUnix() );

    try
    {
      final Connection connection = getConnectionFactory( aContext ).createConnection( "comm:/dev/ttyACM0", 0, false );
      assertNotNull( connection );
    }
    catch ( IOException exception )
    {
      assumeNoException( exception );
    }
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  @Test
  public void testCreateExistingConnectionMacOS( final BundleContext aContext ) throws IOException,
  InvalidSyntaxException
  {
    assumeTrue( HostUtils.getHostInfo().isMacOS() );

    try
    {
      final Connection connection = getConnectionFactory( aContext ).createConnection( "comm:/dev/tty.usbmodemfd131",
          0, false );
      assertNotNull( "Failed to obtain a valid connection!", connection );
    }
    catch ( IOException exception )
    {
      assumeNoException( exception );
    }
  }

  /**
   * Tests that creating a serial connection works on Solaris platforms.
   */
  @Test
  public void testCreateExistingConnectionSolaris( final BundleContext aContext ) throws IOException,
  InvalidSyntaxException
  {
    assumeTrue( HostUtils.getHostInfo().isSolaris() );

    try
    {
      final Connection connection = getConnectionFactory( aContext ).createConnection( "comm:/dev/term/A", 0, false );
      assertNotNull( connection );
    }
    catch ( IOException exception )
    {
      assumeNoException( exception );
    }
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  @Test
  public void testCreateExistingConnectionWin32( final BundleContext aContext ) throws IOException,
  InvalidSyntaxException
  {
    assumeTrue( HostUtils.getHostInfo().isWindows() );

    try
    {
      final Connection connection = getConnectionFactory( aContext ).createConnection( "comm:com3", 0, false );
      assertNotNull( connection );
    }
    catch ( IOException exception )
    {
      assumeNoException( exception );
    }
  }

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  @Test
  public void testCreateNonExistingConnectionLinux( final BundleContext aContext ) throws IOException
  {
    assumeTrue( HostUtils.getHostInfo().isUnix() );

    try
    {
      getConnectionFactory( aContext ).createConnection( "comm:/dev/ttyS99", 0, false );
      fail( "I/O exception expected!" );
    }
    catch ( Exception exception )
    {
      assertThat( exception, instanceOf( IOException.class ) );
    }
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  @Test
  public void testCreateNonExistingConnectionMacOS( final BundleContext aContext ) throws IOException
  {
    assumeTrue( HostUtils.getHostInfo().isMacOS() );

    try
    {
      getConnectionFactory( aContext ).createConnection( "comm:/dev/tty.usbserial", 0, false );
      fail( "I/O exception expected!" );
    }
    catch ( Exception exception )
    {
      assertThat( exception, instanceOf( IOException.class ) );
    }
  }

  /**
   * Tests that creating a serial connection works on Solaris platforms.
   */
  @Test
  public void testCreateNonExistingConnectionSolaris( final BundleContext aContext ) throws IOException
  {
    assumeTrue( HostUtils.getHostInfo().isUnix() );

    try
    {
      getConnectionFactory( aContext ).createConnection( "comm:/dev/term/XYZ", 0, false );
      fail( "I/O exception expected!" );
    }
    catch ( Exception exception )
    {
      assertThat( exception, instanceOf( IOException.class ) );
    }
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  @Test
  public void testCreateNonExistingConnectionWin32( final BundleContext aContext ) throws IOException
  {
    assumeTrue( HostUtils.getHostInfo().isWindows() );

    try
    {
      getConnectionFactory( aContext ).createConnection( "comm:COM255", 0, false );
      fail( "I/O exception expected!" );
    }
    catch ( Exception exception )
    {
      assertThat( exception, instanceOf( IOException.class ) );
    }
  }

  /**
   * Sets up the test cases.
   */
  private ConnectionFactory getConnectionFactory( final BundleContext aContext ) throws InvalidSyntaxException
  {
    final String filter = "(" + ConnectionFactory.IO_SCHEME + "=" + CommConnectionFactory.SCHEME + ")";
    final ServiceReference<?>[] serviceRefs = aContext.getServiceReferences( ConnectionFactory.class.getName(), filter );
    assertNotNull( serviceRefs );
    assertTrue( serviceRefs.length == 1 );

    return ( ConnectionFactory )aContext.getService( serviceRefs[0] );
  }
}
