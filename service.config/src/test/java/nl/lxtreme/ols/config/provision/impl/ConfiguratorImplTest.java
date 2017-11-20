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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.config.provision.impl;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import org.junit.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Test cases for {@link ConfiguratorImpl}.
 */
@SuppressWarnings( "unchecked" )
public class ConfiguratorImplTest
{
  // VARIABLES

  private ConfigurationAdmin configAdmin;
  private ConfiguratorImpl configurator;

  // METHODS

  /**
   * Tests the provisioning of a single file works as expected.
   */
  @Test
  public void testProvisionSingleFile() throws Exception
  {
    File configFile = createConfigFile();
    String pid = getConfigPid( configFile );

    Configuration config = mock( Configuration.class );
    when( config.getPid() ).thenReturn( pid );

    when( this.configAdmin.getConfiguration( eq( pid ), ( String )isNull() ) ).thenReturn( config );

    // We should be able to provision this "new" configuration resource...
    assertTrue( this.configurator.provision( configFile ) );

    // Verify that the configuration is actually updated...
    verify( config ).getPid();
    verify( config ).update( any( Dictionary.class ) );
    verifyNoMoreInteractions( config );

    // We should *not* be able to provision this configuration resource...
    assertFalse( this.configurator.provision( configFile ) );

    // Verify that the configuration is *not* updated...
    verify( config, times( 2 ) ).getPid();
    verifyNoMoreInteractions( config );

    // set the last modification time to somewhere in the future (note: we're
    // using 1 second difference as to circumvent situations in which the last
    // modification timestamp is rounded to the nearest second)...
    configFile.setLastModified( System.currentTimeMillis() + 1000 );

    // We should be able to provision this configuration resource...
    assertTrue( this.configurator.provision( configFile ) );

    // Verify that the configuration is updated again...
    verify( config, times( 3 ) ).getPid();
    verify( config, times( 2 ) ).update( any( Dictionary.class ) );
    verifyNoMoreInteractions( config );
  }

  @Before
  public void setUp()
  {
    LogService logService = mock( LogService.class );

    this.configAdmin = mock( ConfigurationAdmin.class );

    this.configurator = new ConfiguratorImpl();
    this.configurator.setConfigurationAdmin( this.configAdmin );
    this.configurator.setLogService( logService );
  }

  private String getConfigPid( final File file )
  {
    String name = file.getName();
    int idx = name.lastIndexOf( '.' );
    if ( idx > 0 )
    {
      return name.substring( 0, idx );
    }
    return null;
  }

  private File createConfigFile() throws IOException
  {
    File repoFile = File.createTempFile( "repo", ".cfg" );
    repoFile.deleteOnExit();
    return repoFile;
  }
}
