/**
 * 
 */
package nl.lxtreme.ols.client.data.settings;


import static org.junit.Assert.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.data.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;


/**
 * Test cases for {@link UserSettingsManager}.
 */
public class UserSettingsManagerTest
{
  // VARIABLES

  private ProjectImpl project;
  private File baseFolder;

  // METHODS

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception
  {
    this.project = new ProjectImpl();
    this.baseFolder = new File( System.getProperty( "java.io.tmpdir" ) );
  }

  /**
   * Test method for {@link UserSettingsManager#loadUserSettings(File, Project)}
   */
  @Test
  public void testLoadInvalidUserSettingsOk() throws IOException
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    FileOutputStream fos = new FileOutputStream( file );
    fos.write( "hello world".getBytes() );
    HostUtils.closeResource( fos );

    UserSettingsManager.loadUserSettings( file, this.project );

    assertUserSettingsCount();
  }

  /**
   * Test method for {@link UserSettingsManager#loadUserSettings(File, Project)}
   */
  @Test
  public void testLoadNonExistingUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    UserSettingsManager.loadUserSettings( file, this.project );

    assertUserSettingsCount();
  }

  /**
   * Test method for {@link UserSettingsManager#saveUserSettings(File, Project)}
   */
  @Test
  public void testSaveNoUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    UserSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final ProjectImpl verify = new ProjectImpl();
    UserSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount();
  }

  /**
   * Test method for {@link UserSettingsManager#saveUserSettings(File, Project)}
   */
  @Test
  public void testSaveSingleUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.project.setSettings( new UserSettingsImpl( "test" ) );

    UserSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final ProjectImpl verify = new ProjectImpl();
    UserSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount( "test" );
  }

  /**
   * Test method for {@link UserSettingsManager#saveUserSettings(File, Project)}
   */
  @Test
  public void testSaveTwoUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.project.setSettings( new UserSettingsImpl( "test1" ) );
    this.project.setSettings( new UserSettingsImpl( "test2" ) );

    UserSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final ProjectImpl verify = new ProjectImpl();
    UserSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount( "test1", "test2" );
  }

  /**
   * Asserts that the current project contains the given amount of user
   * settings.
   * 
   * @param aNames
   *          the names of the expected user settings...
   */
  private void assertUserSettingsCount( final String... aNames )
  {
    final int[] count = { 0 };
    final List<String> pool = new ArrayList<String>( Arrays.asList( aNames ) );
    this.project.visit( new ProjectVisitor()
    {
      @Override
      public void visit( final UserSettings aSettings ) throws Exception
      {
        count[0]++;
        pool.remove( aSettings.getName() );
      }
    } );
    assertTrue( pool.isEmpty() );
    assertEquals( aNames.length, count[0] );
  }
}
