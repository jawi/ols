/**
 * 
 */
package nl.lxtreme.ols.client.data.settings;


import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.zip.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a manager for loading/storing implicit user settings.
 */
public final class UserSettingsManager
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( UserSettingsManager.class.getName() );

  private static final String SETTINGS_ID_FILENAME = "settings.";

  // METHODS

  /**
   * Loads the user settings for the given project.
   * 
   * @param aUserSettingsFile
   *          the file to load the user settings from, cannot be
   *          <code>null</code>;
   * @param aProject
   *          the project to load the user settings for, cannot be
   *          <code>null</code>.
   */
  public static void loadUserSettings( final File aUserSettingsFile, final Project aProject )
  {
    if ( aUserSettingsFile == null )
    {
      throw new IllegalArgumentException( "User settings file cannot be null!" );
    }
    if ( aProject == null )
    {
      throw new IllegalArgumentException( "Project cannot be null!" );
    }

    if ( aUserSettingsFile.exists() )
    {
      InputStream is = null;
      ZipInputStream zipIS = null;

      try
      {
        is = new BufferedInputStream( new FileInputStream( aUserSettingsFile ) );
        zipIS = new ZipInputStream( is );

        ZipEntry ze = null;
        while ( ( ze = zipIS.getNextEntry() ) != null )
        {
          final String userSettingsName = ze.getName();
          // Ignore settings ID marker file...
          if ( userSettingsName.startsWith( SETTINGS_ID_FILENAME ) )
          {
            continue;
          }

          final Properties settings = new Properties();
          settings.load( zipIS );

          final UserSettingsImpl userSettings = new UserSettingsImpl( userSettingsName, settings );
          aProject.setSettings( userSettings );

          zipIS.closeEntry();
        }
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Failed to load implicit user settings...", exception );
      }
      finally
      {
        HostUtils.closeResource( zipIS );
        HostUtils.closeResource( is );
      }
    }
  }

  /**
   * Saves the user settings for the given project.
   * 
   * @param aUserSettingsFile
   *          the file to write the user settings to, cannot be
   *          <code>null</code>;
   * @param aProject
   *          the project to save the user settings for, cannot be
   *          <code>null</code>.
   */
  public static void saveUserSettings( final File aUserSettingsFile, final Project aProject )
  {
    if ( aUserSettingsFile == null )
    {
      throw new IllegalArgumentException( "User settings file cannot be null!" );
    }
    if ( aProject == null )
    {
      throw new IllegalArgumentException( "Project cannot be null!" );
    }

    OutputStream os = null;

    try
    {
      os = new BufferedOutputStream( new FileOutputStream( aUserSettingsFile ) );
      final ZipOutputStream zipOS = new ZipOutputStream( os );

      // Provide a "special" marker to ensure the ZIP file has at least one
      // entry and can be detected as settings file...
      final ZipEntry zipEntry = new ZipEntry( SETTINGS_ID_FILENAME + System.currentTimeMillis() );
      zipOS.putNextEntry( zipEntry );

      aProject.visit( new ProjectVisitor()
      {
        @Override
        public void visit( final UserSettings aSettings ) throws IOException
        {
          final ZipEntry zipEntry = new ZipEntry( aSettings.getName() );
          zipOS.putNextEntry( zipEntry );

          // Convert to a properties object...
          final Properties props = new Properties();
          for ( Map.Entry<String, Object> userSetting : aSettings )
          {
            props.put( userSetting.getKey(), userSetting.getValue() );
          }

          // Write the project settings
          props.store( zipOS, aSettings.getName().concat( " settings" ) );
        }
      } );

      zipOS.flush();
      zipOS.close();
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Failed to save implicit user settings...", exception );
      throw new RuntimeException( "Failed to save implicit user settings.", exception );
    }
    finally
    {
      HostUtils.closeResource( os );
    }
  }
}
