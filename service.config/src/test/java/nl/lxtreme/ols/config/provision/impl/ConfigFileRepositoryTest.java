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

import org.junit.*;
import org.osgi.service.cm.*;


/**
 * Test cases for {@link ConfigFileRepository}.
 */
@SuppressWarnings( "boxing" )
public class ConfigFileRepositoryTest
{
  // METHODS

  @Test
  public void testAddResourceWithStoringOk() throws Exception
  {
    File repoFile = createTempFile();

    ConfigFileResource resource1 = createResource( "myPid" );
    Configuration config = createConfiguration( "myPid" );

    ConfigFileRepository repo = new ConfigFileRepository();
    assertTrue( repo.add( config, resource1 ) );
    assertFalse( repo.add( config, resource1 ) );

    repo.store( repoFile );

    repo = new ConfigFileRepository();
    repo.load( repoFile );

    assertFalse( repo.add( config, resource1 ) );

    // Simulate the resource is changed...
    when( resource1.getLastModified() ).thenReturn( System.currentTimeMillis() );

    assertTrue( repo.add( config, resource1 ) );
    assertFalse( repo.add( config, resource1 ) );

    repo.store( repoFile );

    repo = new ConfigFileRepository();
    repo.load( repoFile );

    assertFalse( repo.add( config, resource1 ) );
  }

  @Test
  public void testAddChangedResourceOk() throws Exception
  {
    ConfigFileResource resource1 = createResource( "myPid" );
    Configuration config = createConfiguration( "myPid" );

    ConfigFileRepository repo = new ConfigFileRepository();
    assertTrue( repo.add( config, resource1 ) );
    assertFalse( repo.add( config, resource1 ) );

    // Simulate the resource is changed...
    when( resource1.getLastModified() ).thenReturn( System.currentTimeMillis() + 1 );

    assertTrue( repo.add( config, resource1 ) );
    assertFalse( repo.add( config, resource1 ) );
  }

  @Test
  public void testLoadEmptyRepoOk() throws Exception
  {
    File repoFile = createTempFile();
    new ConfigFileRepository().load( repoFile );
  }

  @Test
  public void testLoadNonExistingRepoOk() throws Exception
  {
    File repoFile = createTempFile();
    assertTrue( repoFile.delete() );

    new ConfigFileRepository().load( repoFile );
  }

  @Test
  public void testSaveEmptyRepoOk() throws Exception
  {
    File repoFile = createTempFile();

    ConfigFileRepository repo1 = new ConfigFileRepository();
    assertTrue( repo1.isEmpty() );
    repo1.store( repoFile );

    ConfigFileRepository repo2 = new ConfigFileRepository();
    repo2.load( repoFile );
    assertTrue( repo2.isEmpty() );
  }

  @Test
  public void testSaveRepoOk() throws Exception
  {
    File repoFile = createTempFile();

    ConfigFileResource resource1 = createResource( "myPid" );
    ConfigFileResource resource2 = createResource( "factory", "myPid" );

    ConfigFileRepository repo = new ConfigFileRepository();
    repo.add( createConfiguration( "myPid" ), resource1 );

    repo.store( repoFile ); // should succeed.

    repo = new ConfigFileRepository();
    repo.load( repoFile ); // should succeed.

    assertFalse( repo.isEmpty() );
    assertEquals( "myPid", repo.getConfigurationPid( resource1 ) );
    assertNull( repo.getConfigurationPid( resource2 ) );
  }

  private Configuration createConfiguration( final String pid )
  {
    return createConfiguration( null, pid );
  }

  private Configuration createConfiguration( final String factoryPID, final String pid )
  {
    Configuration config = mock( Configuration.class );
    when( config.getFactoryPid() ).thenReturn( factoryPID );
    when( config.getPid() ).thenReturn( pid );
    return config;
  }

  private ConfigFileResource createResource( final String pid )
  {
    return createResource( null, pid );
  }

  private ConfigFileResource createResource( final String factoryPID, final String pid )
  {
    ConfigFileResource res = mock( ConfigFileResource.class );
    when( res.getLastModified() ).thenReturn( System.currentTimeMillis() );
    when( res.getFactoryPid() ).thenReturn( factoryPID );
    when( res.getPid() ).thenReturn( pid );
    return res;
  }

  private File createTempFile() throws IOException
  {
    File repoFile = File.createTempFile( "repo", ".tmp" );
    repoFile.deleteOnExit();
    return repoFile;
  }
}
