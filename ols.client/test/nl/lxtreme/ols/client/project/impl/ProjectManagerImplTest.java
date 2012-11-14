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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.project.impl;


import static org.mockito.Mockito.*;

import java.io.*;

import junit.framework.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.host.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Test cases for {@link SimpleProjectManager}.
 */
public class ProjectManagerImplTest extends TestCase
{
  // VARIABLES

  private ProjectManagerImpl projectManager;

  // METHODS

  public static void assertArrayEquals( final int[] expected, final int[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  public static void assertArrayEquals( final long[] expected, final long[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  public static void assertArrayEquals( final Object[] expected, final Object[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  /**
   * Test method for {@link SimpleProjectManager#createNewProject()}.
   */
  public void testCreateNewProject()
  {
    final Project currentProject = this.projectManager.getCurrentProject();
    final Project newProject = this.projectManager.createNewProject();
    assertNotSame( "No new instance of a project created?!", currentProject, newProject );
  }

  /**
   * Test method for {@link SimpleProjectManager#createTemporaryProject()}.
   */
  public void testCreateTemporaryProject()
  {
    final Project currentProject = this.projectManager.getCurrentProject();
    final Project tempProject = this.projectManager.createTemporaryProject();

    assertSame( "A new instance of a project created?!", currentProject, this.projectManager.getCurrentProject() );
    assertNotSame( "No new instance of a project created?!", currentProject, tempProject );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#loadProject(java.io.InputStream)}.
   */
  public void testLoadInvalidProjectFail() throws IOException
  {
    final ByteArrayInputStream bais = new ByteArrayInputStream( "hello world!".getBytes() );

    this.projectManager.loadProject( bais );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#loadProject(java.io.InputStream)}.
   */
  public void testLoadNullProjectFail() throws IOException
  {
    this.projectManager.loadProject( null );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#saveProject(java.io.OutputStream)}.
   */
  public void testSaveNullProjectFail() throws IOException
  {
    this.projectManager.saveProject( null );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#saveProject(java.io.OutputStream)}.
   */
  public void testSaveProjectStoresCaptureResultsOk() throws IOException
  {
    final AcquisitionData mockedCapturedData = DataTestUtils.getMockedCapturedData();

    final Project project = this.projectManager.getCurrentProject();
    project.setCapturedData( mockedCapturedData );

    final ByteArrayOutputStream baos = new ByteArrayOutputStream( 1024 );
    this.projectManager.saveProject( baos ); // should succeed...

    // Make sure everyhing is gone...
    this.projectManager.createNewProject();

    final ByteArrayInputStream bais = new ByteArrayInputStream( baos.toByteArray() );
    this.projectManager.loadProject( bais );

    assertEquals( "Invalid capture data!", mockedCapturedData, this.projectManager.getCurrentProject().getDataSet() );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#saveProject(java.io.OutputStream)}.
   */
  public void testSaveProjectStoresChannelLabelsOk() throws IOException
  {
    final String[] labels = { "labelA", "labelB", "labelC", "labelD", "labelE", "labelF", "labelG", "labelH", //
        "labelI", "labelJ", "labelK", "labelL", "labelM", "labelN", "labelO", "labelP", //
        "labelQ", "labelR", "labelS", "labelT", "labelU", "labelV", "labelW", "labelX", //
        "labelY", "labelZ", "label0", "label1", "label2", "label3", "label4", "label5" //
    };

    final Project project = this.projectManager.getCurrentProject();
    final AcquisitionData dataSet = project.getDataSet();
    for ( int i = 0; i < labels.length; i++ )
    {
      dataSet.getChannel( i ).setLabel( labels[i] );
    }

    final ByteArrayOutputStream baos = new ByteArrayOutputStream( 1024 );
    this.projectManager.saveProject( baos ); // should succeed...

    // Make sure everyhing is gone...
    this.projectManager.createNewProject();

    final ByteArrayInputStream bais = new ByteArrayInputStream( baos.toByteArray() );
    this.projectManager.loadProject( bais );

    final AcquisitionData loadedDataSet = this.projectManager.getCurrentProject().getDataSet();

    for ( int i = 0; i < labels.length; i++ )
    {
      assertEquals( labels[i], loadedDataSet.getChannel( i ).getLabel() );
    }
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#saveProject(java.io.OutputStream)}.
   */
  public void testSaveProjectStoresProjectMetadataOk() throws IOException
  {
    String name = "testProject";

    final Project project = this.projectManager.getCurrentProject();
    project.setName( name );

    final ByteArrayOutputStream baos = new ByteArrayOutputStream( 1024 );
    this.projectManager.saveProject( baos ); // should succeed...

    // Make sure everyhing is gone...
    this.projectManager.createNewProject();

    final ByteArrayInputStream bais = new ByteArrayInputStream( baos.toByteArray() );
    this.projectManager.loadProject( bais );

    assertEquals( name, this.projectManager.getCurrentProject().getName() );
  }

  /**
   * Test method for
   * {@link SimpleProjectManager#saveProject(java.io.OutputStream)}.
   */
  public void testSaveProjectStoresProjectSettingsOk() throws IOException
  {
    String settingsName = "testProject";

    final Project project = this.projectManager.getCurrentProject();
    final UserSettings settings = project.getSettings( settingsName );
    settings.put( "key", "value" );

    final ByteArrayOutputStream baos = new ByteArrayOutputStream( 1024 );
    this.projectManager.saveProject( baos ); // should succeed...

    // Make sure everyhing is gone...
    this.projectManager.createNewProject();

    final ByteArrayInputStream bais = new ByteArrayInputStream( baos.toByteArray() );
    this.projectManager.loadProject( bais );

    assertNotSame( settings, this.projectManager.getCurrentProject().getSettings( settingsName ) );
    assertEquals( "value", this.projectManager.getCurrentProject().getSettings( settingsName ).get( "key", "default" ) );
  }

  /**
   * @throws java.lang.Exception
   */
  @Override
  protected void setUp() throws Exception
  {
    HostProperties mockProperties = mock( HostProperties.class );
    when( mockProperties.getFullName() ).thenReturn( "OLS" );

    this.projectManager = new ProjectManagerImpl();
    this.projectManager.setHostProperties( mockProperties );
  }

  /**
   * Asserts that the given acquisition results are equal to each other.
   * 
   * @param aMessage
   *          the message to display when the assertion fails;
   * @param aExpected
   *          the expected acquisition result, cannot be <code>null</code>;
   * @param aTested
   *          the acquisition result to test.
   */
  private void assertEquals( final String aMessage, final AcquisitionData aExpected, final AcquisitionData aTested )
  {
    assertNotNull( aExpected );
    assertNotNull( aTested );

    Assert.assertEquals( aExpected.getAbsoluteLength(), aTested.getAbsoluteLength() );
    Assert.assertEquals( aExpected.getChannelCount(), aTested.getChannelCount() );
    Assert.assertEquals( aExpected.getEnabledChannels(), aTested.getEnabledChannels() );
    Assert.assertEquals( aExpected.getSampleRate(), aTested.getSampleRate() );
    Assert.assertEquals( aExpected.hasTimingData(), aTested.hasTimingData() );
    Assert.assertEquals( aExpected.hasTriggerData(), aTested.hasTriggerData() );
    assertArrayEquals( aExpected.getTimestamps(), aTested.getTimestamps() );
    assertArrayEquals( aExpected.getValues(), aTested.getValues() );
  }

}
