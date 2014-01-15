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
package nl.lxtreme.ols.client2.project;


import static nl.lxtreme.ols.common.OlsConstants.*;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.usersettings.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;
import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides an implementation of {@link ProjectManager}.
 */
public class ProjectManagerImpl implements ProjectManager
{
  // INNER TYPES

  private static class ProjectData
  {
    final String name;
    final String version;
    final Date created;

    public ProjectData( String aName, String aVersion, Date aCreated )
    {
      this.name = aName;
      this.version = aVersion;
      this.created = aCreated;
    }
  }

  // CONSTANTS

  private static final String FILENAME_PROJECT_METADATA = "ols.project";
  private static final String FILENAME_CHANNEL_LABELS = "channel.labels";
  private static final String FILENAME_PROJECT_SETTINGS = "settings/";
  private static final String FILENAME_CAPTURE_RESULTS = "data.ols";

  // VARIABLES

  // Injected by Felix DM...
  private volatile EventAdmin eventAdmin;
  private volatile UserSettingProvider userSettingProvider;
  private volatile LogService log;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void loadDataFile( File aFile ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }

    FileReader reader = new FileReader( aFile );
    try
    {
      AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

      OlsDataHelper.read( reader, builder );

      this.log.log( LogService.LOG_DEBUG, "OLS data file loaded..." );

      String name = getProjectName( aFile );

      postDataLoadedEvent( builder.build(), name, aFile );
    }
    finally
    {
      closeSilently( reader );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void loadProject( File aFile ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }

    FileInputStream in = new FileInputStream( aFile );
    ZipInputStream zipIS = new ZipInputStream( in );
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    ProjectData metadata = null;

    try
    {
      ZipEntry ze = null;
      while ( ( ze = zipIS.getNextEntry() ) != null )
      {
        final String name = ze.getName();
        if ( FILENAME_PROJECT_METADATA.equals( name ) )
        {
          metadata = loadProjectMetadata( zipIS );
        }
        else if ( FILENAME_CHANNEL_LABELS.equals( name ) )
        {
          loadChannelLabels( builder, zipIS );
        }
        else if ( FILENAME_CAPTURE_RESULTS.equals( name ) )
        {
          loadCapturedResults( builder, zipIS );
        }
        else if ( name.startsWith( FILENAME_PROJECT_SETTINGS ) )
        {
          final String userSettingsName = name.substring( FILENAME_PROJECT_SETTINGS.length() );
          loadProjectSettings( userSettingsName, zipIS );
        }

        zipIS.closeEntry();
      }

      if ( metadata == null )
      {
        throw new IOException( "Invalid project file!" );
      }

      this.log.log( LogService.LOG_DEBUG, "OLS project file loaded..." );

      postDataLoadedEvent( builder.build(), metadata.name, aFile );
    }
    finally
    {
      closeSilently( zipIS );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveDataFile( File aFile, Session aSession ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }
    if ( aSession == null )
    {
      throw new IllegalArgumentException( "Session cannot be null!" );
    }

    FileWriter writer = new FileWriter( aFile );
    try
    {
      OlsDataHelper.write( writer, aSession.getAcquiredData() );
    }
    finally
    {
      closeSilently( writer );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveProject( File aFile, Session aSession ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }
    if ( aSession == null )
    {
      throw new IllegalArgumentException( "Session cannot be null!" );
    }

    FileOutputStream os = new FileOutputStream( aFile );
    ZipOutputStream zipOS = new ZipOutputStream( os );

    zipOS.setComment( ClientConstants.FULL_NAME.concat( " project file" ) );

    try
    {
      AcquisitionData data = aSession.getAcquiredData();
      ProjectData metadata = new ProjectData( aSession.getName(), getVersion(), new Date() );

      storeProjectMetadata( zipOS, metadata );
      // Store the last capture results...
      storeCapturedResults( zipOS, data );
      // Store the channel labels...
      storeChannelLabels( zipOS, data );
      // Store the settings...
      storeProjectSettings( zipOS );
    }
    finally
    {
      closeSilently( zipOS );
      closeSilently( os );
    }
  }

  /**
   * Reads the capture results from the given ZIP-input stream.
   * 
   * @param aBuilder
   *          the project to read the capture results for;
   * @param aZipIS
   *          the ZIP input stream to read the capture results from.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void loadCapturedResults( final AcquisitionDataBuilder aBuilder, final ZipInputStream aZipIS )
      throws IOException
  {
    OlsDataHelper.read( new InputStreamReader( aZipIS ), aBuilder );
  }

  /**
   * Reads the project channel labels from the given ZIP-input stream.
   * 
   * @param aBuilder
   *          the project to read the channel labels for;
   * @param aZipIS
   *          the ZIP input stream to read the channel labels from.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void loadChannelLabels( final AcquisitionDataBuilder aBuilder, final ZipInputStream aZipIS )
      throws IOException
  {
    final InputStreamReader isReader = new InputStreamReader( aZipIS );
    final BufferedReader reader = new BufferedReader( isReader );

    String label = null;
    int idx = 0;
    while ( ( ( label = reader.readLine() ) != null ) && ( idx < OlsConstants.MAX_CHANNELS ) )
    {
      aBuilder.add( aBuilder.createChannel().setIndex( idx++ ).setLabel( label ) );
    }
  }

  /**
   * Reads the project metadata to the given ZIP-input stream.
   * 
   * @param aProject
   *          the project to read the metadata for;
   * @param aZipIS
   *          the ZIP input stream to read the metadata from.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected ProjectData loadProjectMetadata( final ZipInputStream aZipIS ) throws IOException
  {
    InputStreamReader isReader = new InputStreamReader( aZipIS );
    BufferedReader reader = new BufferedReader( isReader );

    String name = reader.readLine();
    String version = reader.readLine();
    Date savedAt = new Date( Long.valueOf( reader.readLine() ).longValue() );

    return new ProjectData( name, version, savedAt );
  }

  /**
   * Reads the project settings to the given ZIP-input stream.
   * 
   * @param aProject
   *          the project to read the settings for;
   * @param aUserSettingsName
   *          the name of the user settings that is to be loaded;
   * @param aZipIS
   *          the ZIP input stream to read the settings from.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void loadProjectSettings( String aUserSettingsName, ZipInputStream aZipIS ) throws IOException
  {
    Properties loaded = new Properties();
    loaded.load( aZipIS );

    UserSettings settings = this.userSettingProvider.getSettings( aUserSettingsName );
    for ( Object _key : loaded.keySet() )
    {
      String key = _key.toString();
      String value = loaded.getProperty( key );
      settings.put( key, value );
    }
  }

  /**
   * Stores the captured results to the given ZIP-output stream.
   * <p>
   * If the given project does not have capture results, this method does
   * nothing.
   * </p>
   * 
   * @param aProject
   *          the project to write the capture results for;
   * @param aZipOS
   *          the ZIP output stream to write the capture results to.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void storeCapturedResults( ZipOutputStream aZipOS, AcquisitionData aData ) throws IOException
  {
    if ( aData == null )
    {
      return;
    }

    ZipEntry zipEntry = new ZipEntry( FILENAME_CAPTURE_RESULTS );
    aZipOS.putNextEntry( zipEntry );

    OlsDataHelper.write( new OutputStreamWriter( aZipOS ), aData );
  }

  /**
   * Stores the channel labels to the given ZIP-output stream.
   * <p>
   * If the given project does not have channel labels, this method does
   * nothing.
   * </p>
   * 
   * @param aData
   *          the project to write the channel labels for;
   * @param aZipOS
   *          the ZIP output stream to write the channel labels to.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void storeChannelLabels( ZipOutputStream aZipOS, AcquisitionData aData ) throws IOException
  {
    Channel[] channels = aData.getChannels();

    ZipEntry zipEntry = new ZipEntry( FILENAME_CHANNEL_LABELS );
    aZipOS.putNextEntry( zipEntry );

    // Write the channel labels
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      for ( Channel channel : channels )
      {
        out.println( ( channel != null ) && channel.hasName() ? channel.getLabel() : "" );
      }
    }
    finally
    {
      out.flush();
      out = null;
    }
  }

  /**
   * Stores the project metadata to the given ZIP-output stream.
   * <p>
   * In case the given project does not have a project name, this method does
   * nothing.
   * </p>
   * 
   * @param aProject
   *          the project to write the metadata for;
   * @param aZipOS
   *          the ZIP output stream to write the metadata to.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void storeProjectMetadata( ZipOutputStream aZipOS, ProjectData aMetadata ) throws IOException
  {
    ZipEntry zipEntry = new ZipEntry( FILENAME_PROJECT_METADATA );
    aZipOS.putNextEntry( zipEntry );

    // Write the project metadata...
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      out.println( aMetadata.name );
      out.println( aMetadata.version );
      out.println( System.currentTimeMillis() );
    }
    finally
    {
      out.flush();
      out = null;
    }
  }

  /**
   * Stores the project settings to the given ZIP-output stream.
   * <p>
   * In case the given project does not have project settings, this method does
   * nothing.
   * </p>
   * 
   * @param aProject
   *          the project to write the settings for;
   * @param aZipOS
   *          the ZIP output stream to write the settings to.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void storeProjectSettings( ZipOutputStream aZipOS ) throws IOException
  {
    try
    {
      for ( UserSettings settings : this.userSettingProvider.getAllSettings() )
      {
        final String zipEntryName = FILENAME_PROJECT_SETTINGS.concat( settings.getName() );

        final ZipEntry zipEntry = new ZipEntry( zipEntryName );
        aZipOS.putNextEntry( zipEntry );

        // Convert to a properties object...
        final Properties props = new Properties();
        for ( Map.Entry<String, String> userSetting : settings.entrySet() )
        {
          props.put( userSetting.getKey(), userSetting.getValue() );
        }

        // Write the project settings
        props.store( aZipOS, settings.getName().concat( " settings" ) );
      }
    }
    finally
    {
      aZipOS.flush();
    }
  }

  private void closeSilently( final Closeable aCloseable )
  {
    try
    {
      if ( aCloseable != null )
      {
        aCloseable.close();
      }
    }
    catch ( IOException e )
    {
      // Ignore...
    }
  }

  private String getProjectName( File aFile )
  {
    return SwingComponentUtils.stripFileExtension( aFile )[0];
  }

  /**
   * @return the version of this client, never <code>null</code>.
   */
  private String getVersion()
  {
    Bundle bundle = FrameworkUtil.getBundle( getClass() );
    if ( bundle != null )
    {
      Dictionary<?, ?> headers = bundle.getHeaders();
      return ( String )headers.get( "X-ClientVersion" );
    }
    return "<unknown>";
  }

  /**
   * @param aResult
   */
  private void postDataLoadedEvent( AcquisitionData aResult, String aName, File aSource )
  {
    if ( this.eventAdmin != null )
    {
      Map<Object, Object> props = new Properties();
      props.put( TDL_DATA, aResult );
      props.put( TDL_FILE, aSource );
      props.put( TDL_NAME, aName );

      this.eventAdmin.postEvent( new Event( TOPIC_DATA_LOADED, props ) );
    }
  }
}
