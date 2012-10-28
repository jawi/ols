/**
 * 
 */
package nl.lxtreme.ols.client.project.impl;


import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.log.*;


/**
 * Test cases for {@link UserSettingsManagerImpl}.
 */
public class UserSettingsManagerTest extends TestCase
{
  // VARIABLES

  private StubTestProject project;
  private UserSettingsManagerImpl userSettingsManager;
  private File baseFolder;

  // METHODS

  /**
   * Test method for
   * {@link UserSettingsManagerImpl#loadUserSettings(File, Project)}
   */
  public void testLoadInvalidUserSettingsOk() throws IOException
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    FileOutputStream fos = new FileOutputStream( file );
    fos.write( "hello world".getBytes() );
    IOUtil.closeResource( fos );

    this.userSettingsManager.loadUserSettings( file, this.project );

    assertUserSettingsCount();
  }

  /**
   * Test method for
   * {@link UserSettingsManagerImpl#loadUserSettings(File, Project)}
   */
  public void testLoadNonExistingUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.userSettingsManager.loadUserSettings( file, this.project );

    assertUserSettingsCount();
  }

  /**
   * Test method for
   * {@link UserSettingsManagerImpl#saveUserSettings(File, Project)}
   */
  public void testSaveNoUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.userSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final StubTestProject verify = new StubTestProject();
    this.userSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount();
  }

  /**
   * Test method for
   * {@link UserSettingsManagerImpl#saveUserSettings(File, Project)}
   */
  public void testSaveSingleUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.project.setSettings( new UserSettingsImpl( "test" ) );

    this.userSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final StubTestProject verify = new StubTestProject();
    this.userSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount( "test" );
  }

  /**
   * Test method for
   * {@link UserSettingsManagerImpl#saveUserSettings(File, Project)}
   */
  public void testSaveTwoUserSettingsOk()
  {
    final File file = new File( this.baseFolder, UUID.randomUUID().toString() );
    file.deleteOnExit();

    this.project.setSettings( new UserSettingsImpl( "test1" ) );
    this.project.setSettings( new UserSettingsImpl( "test2" ) );

    this.userSettingsManager.saveUserSettings( file, this.project );

    // Verify...
    final StubTestProject verify = new StubTestProject();
    this.userSettingsManager.loadUserSettings( file, verify );

    assertUserSettingsCount( "test1", "test2" );
  }

  /**
   * @throws java.lang.Exception
   */
  @Override
  protected void setUp() throws Exception
  {
    this.project = new StubTestProject();
    this.baseFolder = new File( System.getProperty( "java.io.tmpdir" ) );
    this.userSettingsManager = new UserSettingsManagerImpl();
    this.userSettingsManager.setLog( mock( LogService.class ) );
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
