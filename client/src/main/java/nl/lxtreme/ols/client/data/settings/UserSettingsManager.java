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
  /** The name of the implicit user settings properties file name. */
  private static final String IMPLICIT_USER_SETTING_NAME = "nl.lxtreme.ols.client";

  // METHODS

  /**
   * Loads the user settings for the given project.
   * 
   * @param aProject
   *          the project to load the user settings for, cannot be
   *          <code>null</code>.
   */
  public static void loadUserSettings( final Project aProject )
  {
    final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME, "settings" );
    if ( userSettingsFile.exists() )
    {
      InputStream is = null;

      try
      {
        is = new BufferedInputStream( new FileInputStream( userSettingsFile ) );
        final ZipInputStream zipIS = new ZipInputStream( is );

        ZipEntry ze = null;
        while ( ( ze = zipIS.getNextEntry() ) != null )
        {
          final String userSettingsName = ze.getName();

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
        HostUtils.closeResource( is );
      }
    }
  }

  /**
   * Saves the user settings for the given project.
   * 
   * @param aProject
   *          the project to save the user settings for, cannot be
   *          <code>null</code>.
   */
  public static void saveUserSettings( final Project aProject )
  {
    final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME, "settings" );

    OutputStream os = null;

    try
    {
      os = new BufferedOutputStream( new FileOutputStream( userSettingsFile ) );
      final ZipOutputStream zipOS = new ZipOutputStream( os );

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
    }
    finally
    {
      HostUtils.closeResource( os );
    }
  }
}
