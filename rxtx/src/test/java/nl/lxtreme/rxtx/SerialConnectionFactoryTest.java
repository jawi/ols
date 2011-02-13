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


import static org.hamcrest.core.IsInstanceOf.*;
import static org.junit.Assert.*;
import static org.ops4j.pax.exam.CoreOptions.*;
import static org.ops4j.pax.exam.container.def.PaxRunnerOptions.*;

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
 * Tests for {@link SerialConnectionFactory}.
 */
@Ignore
@RunWith( JUnit4TestRunner.class )
public class SerialConnectionFactoryTest
{
  // VARIABLES

  private ConnectionFactory connectionFactory;

  @Inject
  private final BundleContext bundleContext = null;

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
        mavenBundle().groupId( "org.osgi" ).artifactId( "org.osgi.compendium" ).version( "4.2.0" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "service.io" ).version( "1.0.1" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "util" ).version( "1.0.5-SNAPSHOT" ), //
        mavenBundle().groupId( "nl.lxtreme.ols" ).artifactId( "org.rxtx" ).version( "2.2.0-8" ) //
    );
  }

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp() throws Exception
  {
    final String filter = "(" + ConnectionFactory.IO_SCHEME + "=" + SerialConnectionFactory.SCHEME + ")";
    final ServiceReference[] serviceRefs = this.bundleContext.getServiceReferences( ConnectionFactory.class.getName(),
        filter );
    assertNotNull( serviceRefs );
    assertTrue( serviceRefs.length == 1 );

    this.connectionFactory = ( ConnectionFactory )this.bundleContext.getService( serviceRefs[0] );
  }

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  @Test
  public void testCreateExistingConnectionLinux() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isUnix() );

    if ( HostUtils.isUnix() )
    {
      try
      {
        final Connection connection = this.connectionFactory.createConnection( "comm:/dev/ttyACM0", 0, false );
        assertNotNull( connection );
      }
      catch ( IOException exception )
      {
        Assume.assumeNoException( exception );
      }
    }
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  @Test
  public void testCreateExistingConnectionMacOS() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isMacOS() );

    if ( HostUtils.isMacOS() )
    {
      try
      {
        final Connection connection = this.connectionFactory.createConnection( "comm:/dev/tty.usbmodemfd131", 0, false );
        assertNotNull( connection );
      }
      catch ( IOException exception )
      {
        Assume.assumeNoException( exception );
      }
    }
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  @Test
  public void testCreateExistingConnectionWin32() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isWindows() );

    if ( HostUtils.isWindows() )
    {
      try
      {
        final Connection connection = this.connectionFactory.createConnection( "comm:com3", 0, false );
        assertNotNull( connection );
      }
      catch ( IOException exception )
      {
        Assume.assumeNoException( exception );
      }
    }
  }

  /**
   * Tests that creating a serial connection works on Linux/Unix platforms.
   */
  @Test
  public void testCreateNonExistingConnectionLinux() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isUnix() );

    if ( HostUtils.isUnix() )
    {
      try
      {
        this.connectionFactory.createConnection( "comm:/dev/ttyS99", 0, false );
        fail( "I/O exception expected!" );
      }
      catch ( Exception exception )
      {
        assertThat( exception, instanceOf( IOException.class ) );
      }
    }
  }

  /**
   * Tests that creating a serial connection works on Mac OS platforms.
   */
  @Test
  public void testCreateNonExistingConnectionMacOS() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isMacOS() );

    if ( HostUtils.isMacOS() )
    {
      try
      {
        this.connectionFactory.createConnection( "comm:/dev/tty.usbserial", 0, false );
        fail( "I/O exception expected!" );
      }
      catch ( Exception exception )
      {
        assertThat( exception, instanceOf( IOException.class ) );
      }
    }
  }

  /**
   * Tests that creating a serial connection works on Windows platforms.
   */
  @Test
  public void testCreateNonExistingConnectionWin32() throws IOException
  {
    // XXX this does not work correctly with PAX-Exam...
    // assumeTrue( HostUtils.isWindows() );

    if ( HostUtils.isWindows() )
    {
      try
      {
        this.connectionFactory.createConnection( "comm:COM255", 0, false );
        fail( "I/O exception expected!" );
      }
      catch ( Exception exception )
      {
        assertThat( exception, instanceOf( IOException.class ) );
      }
    }
  }
}
