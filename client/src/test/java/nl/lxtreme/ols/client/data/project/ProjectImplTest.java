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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.data.project;


import static org.junit.Assert.*;

import java.beans.*;
import java.io.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.data.settings.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;

import org.junit.*;


/**
 * Test cases for {@link ProjectImpl}.
 */
public class ProjectImplTest
{
  // VARIABLES

  private ProjectImpl project;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp()
  {
    this.project = new ProjectImpl();
  }

  /**
   * Test method for {@link ProjectImpl#getChannelLabels()}.
   */
  @Test
  public void testGetChannelLabels()
  {
    assertNotNull( this.project.getChannelLabels() );
  }

  /**
   * Test method for {@link ProjectImpl#getCursorPositions()}.
   */
  @Test
  public void testGetCursorPositions()
  {
    assertNotNull( this.project.getCursorPositions() );
  }

  /**
   * Test method for {@link ProjectImpl#getSettings(java.lang.String)}.
   */
  @Test
  public void testGetExistingSettingsYieldsSameInstance()
  {
    final ProjectImpl projectImpl = this.project;
    final UserSettings settings = projectImpl.getSettings( "test" );
    assertSame( settings, projectImpl.getSettings( "test" ) );
  }

  /**
   * Test method for {@link ProjectImpl#getSettings(java.lang.String)}.
   */
  @Test
  public void testGetUnknownSettingsYieldsNewInstance()
  {
    assertNotNull( this.project.getSettings( "test" ) );
  }

  /**
   * Test method for {@link ProjectImpl#ProjectImpl()}.
   */
  @Test
  public void testProjectImpl()
  {
    assertNotNull( this.project );
  }

  /**
   * Test method for
   * {@link ProjectImpl#addPropertyChangeListener(java.beans.PropertyChangeListener)}
   * .
   */
  @Test
  public void testPropertyChangeListener()
  {
    final Ensure ensure = new Ensure();

    final PropertyChangeListener listener = new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent aEvent )
      {
        final String name = aEvent.getPropertyName();
        if ( ProjectProperties.PROPERTY_CAPTURED_DATA.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 1 ).run();
        }
        else if ( ProjectProperties.PROPERTY_CHANGED.equals( name ) )
        {
          // Called multiple times...
        }
        else if ( ProjectProperties.PROPERTY_CHANNEL_LABELS.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 2 ).run();
        }
        else if ( ProjectProperties.PROPERTY_CURSORS.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 3 ).run();
        }
        else if ( ProjectProperties.PROPERTY_CURSORS_ENABLED.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 4 ).run();
        }
        else if ( ProjectProperties.PROPERTY_FILENAME.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 5 ).run();
        }
        else if ( ProjectProperties.PROPERTY_LAST_MODIFIED.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 6 ).run();
        }
        else if ( ProjectProperties.PROPERTY_NAME.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 7 ).run();
        }
        else if ( ProjectProperties.PROPERTY_SETTINGS.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 8 ).run();
        }
        else if ( ProjectProperties.PROPERTY_SOURCE_VERSION.equals( name ) )
        {
          Ensure.createRunnableStep( ensure, 9 ).run();
        }
        else
        {
          Ensure.createRunnableStep( ensure, -1 ).run();
        }
      }
    };
    this.project.addPropertyChangeListener( listener );

    this.project.setCapturedData( null );
    ensure.waitForStep( 1, 500 );

    this.project.setChannelLabels( "a", "b", "c" );
    ensure.waitForStep( 2, 500 );

    this.project.setCursorPositions( Long.valueOf( 100L ) );
    ensure.waitForStep( 3, 500 );

    this.project.setCursorsEnabled( true );
    ensure.waitForStep( 4, 500 );

    this.project.setFilename( new File( "test" ) );
    ensure.waitForStep( 5, 500 );

    this.project.setLastModified( new Date() );
    ensure.waitForStep( 6, 500 );

    this.project.setName( "test" );
    ensure.waitForStep( 7, 500 );

    this.project.setSettings( new UserSettingsImpl( "test" ) );
    ensure.waitForStep( 8, 500 );

    this.project.setSourceVersion( "test" );
    ensure.waitForStep( 9, 500 );
  }

  /**
   * Test method for
   * {@link ProjectImpl#setCapturedData(nl.lxtreme.ols.api.data.CapturedData)}.
   */
  @Test
  public void testSetCapturedData()
  {
    final CapturedData data = DataTestUtils.getMockedCapturedData();
    this.project.setCapturedData( data );

    assertSame( data, this.project.getCapturedData() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setChannelLabels(java.lang.String[])}.
   */
  @Test
  public void testSetChannelLabels()
  {
    final String[] labels = new String[CapturedData.MAX_CHANNELS];
    labels[0] = "labelA";
    labels[1] = "labelB";
    labels[2] = "labelC";
    this.project.setChannelLabels( labels );

    assertNotSame( labels, this.project.getChannelLabels() );
    assertArrayEquals( labels, this.project.getChannelLabels() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setCursorPositions(java.lang.Long[])}.
   */
  @Test
  public void testSetCursorPositions()
  {
    final Long[] cursors = new Long[CapturedData.MAX_CURSORS];
    cursors[0] = Long.valueOf( 1L );
    cursors[1] = Long.valueOf( 2L );
    cursors[2] = Long.valueOf( 3L );
    cursors[3] = Long.valueOf( 4L );
    this.project.setCursorPositions( cursors );

    assertNotSame( cursors, this.project.getCursorPositions() );
    assertArrayEquals( cursors, this.project.getCursorPositions() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setCursorsEnabled(boolean)}.
   */
  @Test
  public void testSetCursorsEnabled()
  {
    this.project.setCursorsEnabled( true );

    assertTrue( this.project.isCursorsEnabled() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setFilename(java.io.File)}.
   */
  @Test
  public void testSetFilename()
  {
    final File filename = new File( "." );
    this.project.setFilename( filename );

    assertEquals( filename, this.project.getFilename() );
    assertFalse( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setLastModified(java.util.Date)}.
   */
  @Test
  public void testSetLastModified()
  {
    final Date date = new Date();
    this.project.setLastModified( date );

    assertEquals( date, this.project.getLastModified() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setName(java.lang.String)}.
   */
  @Test
  public void testSetName()
  {
    final String name = "test";
    this.project.setName( name );

    assertEquals( name, this.project.getName() );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for
   * {@link ProjectImpl#setSettings(nl.lxtreme.ols.api.UserSettings)}.
   */
  @Test
  public void testSetSettings()
  {
    final String name = "test";
    final UserSettingsImpl settings = new UserSettingsImpl( name );
    this.project.setSettings( settings );

    assertEquals( settings, this.project.getSettings( name ) );
    assertTrue( this.project.isChanged() );
  }

  /**
   * Test method for {@link ProjectImpl#setSourceVersion(java.lang.String)}.
   */
  @Test
  public void testSetSourceVersion()
  {
    final String version = "test";
    this.project.setSourceVersion( version );

    assertEquals( version, this.project.getSourceVersion() );
    assertTrue( this.project.isChanged() );
  }
}
