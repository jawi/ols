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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui;


import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.zip.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.WindowManager.WindowStateManager;

import org.osgi.service.log.*;


/**
 * Provides a front-end controller for handling "native" data input and output.
 */
public final class ProjectController
{
  // INNER TYPES

  /**
   * Denotes the current project.
   */
  private static class Project
  {
    private final File file;
    private final boolean changed;

    /**
     * Creates a new {@link Project} instance.
     */
    public Project()
    {
      this( null, false );
    }

    /**
     * Creates a new {@link Project} instance.
     */
    public Project( final File aFile, final boolean aChanged )
    {
      this.file = aFile;
      this.changed = aChanged;
    }

    /**
     * @return
     */
    public String getName()
    {
      return this.file != null ? this.file.getName() : "<anonymous>";
    }

    /**
     * @return
     */
    public boolean isAnonymousProject()
    {
      return this.file == null;
    }
  }

  // CONSTANTS

  private static final String FILENAME_PROJECT_METADATA = "ols.project";
  private static final String FILENAME_CHANNEL_LABELS = "channel.labels";
  private static final String FILENAME_PROJECT_SETTINGS = "settings/";
  private static final String FILENAME_CAPTURE_RESULTS = "data.ols";

  // VARIABLES

  private final AtomicReference<Project> projectRef;

  // Injected by Felix DM...
  private volatile WindowStateManager windowStateManager;
  private volatile LogService logService;
  private volatile Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ProjectController} instance.
   */
  public ProjectController()
  {
    this.projectRef = new AtomicReference<Project>( new Project() );
  }

  // METHODS

  /**
   * Creates a new project.
   */
  public void createNewProject()
  {
    setProject( new Project() );

    this.session.reset();
  }

  /**
   * Returns the file to the current project.
   * 
   * @return the {@link File}-object corresponding to the current project, can
   *         be <code>null</code> if the current project is not yet saved.
   */
  public File getProjectFile()
  {
    return getProject().file;
  }

  /**
   * Returns whether or not the current project is "anonymous", i.e., not yet
   * saved.
   * 
   * @return <code>true</code> if the current project is anonymous,
   *         <code>false</code> otherwise.
   */
  public boolean isAnonymousProject()
  {
    return getProject().isAnonymousProject();
  }

  /**
   * Returns whether or not the current project is changed.
   * 
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> otherwise.
   */
  public boolean isProjectChanged()
  {
    return getProject().changed;
  }

  /**
   * Opens a given file as OLS-data file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openDataFile( final File aFile ) throws IOException
  {
    final FileReader reader = new FileReader( aFile );

    try
    {
      AcquisitionData data = OlsDataHelper.read( reader );

      this.session.setAcquisitionData( data );
    }
    finally
    {
      IOUtil.closeResource( reader );
    }
  }

  /**
   * Opens a given file as OLS-project file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openProjectFile( final File aFile ) throws IOException
  {
    InputStream is = null;

    try
    {
      is = new FileInputStream( aFile );

      Project newProject = new Project( aFile, false /* changed */);

      readProjectFile( newProject, is );

      // Update the current project...
      setProject( newProject );
    }
    finally
    {
      IOUtil.closeResource( is );
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-data file
   * format.
   * 
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveDataFile( final File aFile ) throws IOException
  {
    final FileWriter writer = new FileWriter( aFile );
    try
    {
      AcquisitionData data = this.session.getAcquisitionData();

      OlsDataHelper.write( writer, data );
    }
    finally
    {
      IOUtil.closeResource( writer );
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-project
   * file format.
   * 
   * @param aName
   *          the name of the project to store, cannot be <code>null</code>;
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveProjectFile( final String aName, final File aFile ) throws IOException
  {
    FileOutputStream out = null;
    try
    {
      out = new FileOutputStream( aFile );

      Project newProject = new Project( aFile, false /* changed */);

      saveProjectFile( newProject, out );

      setProject( newProject );
    }
    finally
    {
      IOUtil.closeResource( out );
    }
  }

  /**
   * Regards the given input stream as project file (ie a ZIP-file) and loads
   * its contents.
   * 
   * @param aInput
   *          the input stream to load as project, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading the project file.
   */
  final void readProjectFile( final Project aProject, final InputStream aInput ) throws IOException
  {
    final BufferedInputStream in = new BufferedInputStream( aInput );
    final ZipInputStream zipIS = new ZipInputStream( in );

    AcquisitionData data = null;
    List<String> labels = null;

    try
    {
      ZipEntry ze = null;
      boolean entriesSeen = false;
      while ( ( ze = zipIS.getNextEntry() ) != null )
      {
        final String name = ze.getName();
        if ( FILENAME_PROJECT_METADATA.equals( name ) )
        {
          readProjectMetadata( zipIS );
          entriesSeen = true;
        }
        else if ( FILENAME_CHANNEL_LABELS.equals( name ) )
        {
          labels = readChannelLabels( zipIS );
          entriesSeen = true;
        }
        else if ( FILENAME_CAPTURE_RESULTS.equals( name ) )
        {
          data = readCapturedResults( zipIS );
          entriesSeen = true;
        }
        else if ( name.startsWith( FILENAME_PROJECT_SETTINGS ) )
        {
          final String userSettingsName = name.substring( FILENAME_PROJECT_SETTINGS.length() );
          readProjectSettings( userSettingsName, zipIS );
          entriesSeen = true;
        }

        zipIS.closeEntry();
      }

      if ( !entriesSeen )
      {
        throw new IOException( "Invalid project file!" );
      }

      // Merge the channel labels with the channel-data in the acquisition data.
      // The channel labels are (for now) leading, and should be equal to the
      // ones in the acquisition data (if defined)...
      if ( ( data != null ) && ( labels != null ) && !labels.isEmpty() )
      {
        Channel[] channels = data.getChannels();

        int length = Math.min( channels.length, labels.size() );
        for ( int i = 0; i < length; i++ )
        {
          String label = labels.get( i );
          if ( ( label != null ) && !"".equals( label ) )
          {
            channels[i].setLabel( label );
          }
        }
      }

      // Publish the data to our session...
      this.session.setAcquisitionData( data );
    }
    finally
    {
      IOUtil.closeResource( zipIS );
    }
  }

  /**
   * Stores the current data to the given output stream.
   * 
   * @param aOutput
   *          the output stream to write the project data to, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems storing the data.
   */
  final void saveProjectFile( final Project aProject, final OutputStream aOutput ) throws IOException
  {
    final BufferedOutputStream os = new BufferedOutputStream( aOutput );
    final ZipOutputStream zipOS = new ZipOutputStream( os );

    zipOS.setComment( Platform.getFullName().concat( " project file" ) );

    String projectName = aProject.getName();
    String version = Client.getInstance().getVersion();

    try
    {
      writeProjectMetadata( zipOS, projectName, version );
      // Store the last capture results...
      writeCapturedResults( zipOS, this.session.getAcquisitionData() );
      // Store the channel labels...
      writeChannelLabels( zipOS, this.session.getAcquisitionData() );
      // Store the settings...
      writeProjectSettings( zipOS );
    }
    finally
    {
      IOUtil.closeResource( zipOS );
      IOUtil.closeResource( os );
    }
  }

  /**
   * @return the current project, never <code>null</code>.
   */
  private Project getProject()
  {
    return this.projectRef.get();
  }

  /**
   * Reads the capture results from the given ZIP-input stream.
   * 
   * @param aProject
   *          the project to read the capture results for;
   * @param aZipIS
   *          the ZIP input stream to read the capture results from.
   * @throws IOException
   *           in case of I/O problems.
   */
  private AcquisitionData readCapturedResults( final ZipInputStream aZipIS ) throws IOException
  {
    return OlsDataHelper.read( new InputStreamReader( aZipIS ) );
  }

  /**
   * Reads the project channel labels from the given ZIP-input stream.
   * 
   * @param aProject
   *          the project to read the channel labels for;
   * @param aZipIS
   *          the ZIP input stream to read the channel labels from.
   * @throws IOException
   *           in case of I/O problems.
   */
  private List<String> readChannelLabels( final ZipInputStream aZipIS ) throws IOException
  {
    final InputStreamReader isReader = new InputStreamReader( aZipIS );
    final BufferedReader reader = new BufferedReader( isReader );

    List<String> result = new ArrayList<String>();

    String label = null;
    int idx = 0;
    while ( ( ( label = reader.readLine() ) != null ) && ( idx < Ols.MAX_CHANNELS ) )
    {
      result.add( label );
    }

    return result;
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
  private void readProjectMetadata( final ZipInputStream aZipIS ) throws IOException
  {
    InputStreamReader isReader = new InputStreamReader( aZipIS );
    BufferedReader reader = new BufferedReader( isReader );

    String name = reader.readLine();
    String version = reader.readLine();

    this.logService.log( LogService.LOG_INFO, "Loading project '" + name + "' made with version v" + version + "." );
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
  private void readProjectSettings( final String aUserSettingsName, final ZipInputStream aZipIS ) throws IOException
  {
    final Properties settings = new Properties();
    settings.load( aZipIS );

    this.windowStateManager.putSettings( aUserSettingsName, settings );
  }

  /**
   * Sets the current project to the one given.
   * 
   * @param aProject
   *          the project to set, cannot be <code>null</code>.
   * @return the given project.
   */
  private Project setProject( final Project aProject )
  {
    Project project;
    do
    {
      project = this.projectRef.get();
    }
    while ( !this.projectRef.compareAndSet( project, aProject ) );
    return aProject;
  }

  /**
   * Stores the captured results to the given ZIP-output stream.
   * <p>
   * If the given project does not have capture results, this method does
   * nothing.
   * </p>
   * 
   * @param aZipOS
   *          the ZIP output stream to write the capture results to;
   * @param aData
   *          the acquisition data to write.
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeCapturedResults( final ZipOutputStream aZipOS, final AcquisitionData aData ) throws IOException
  {
    final ZipEntry zipEntry = new ZipEntry( FILENAME_CAPTURE_RESULTS );
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
   * @param aZipOS
   *          the ZIP output stream to write the channel labels to.
   * @param aDataSet
   *          the project to write the channel labels for;
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeChannelLabels( final ZipOutputStream aZipOS, final AcquisitionData aDataSet ) throws IOException
  {
    final ZipEntry zipEntry = new ZipEntry( FILENAME_CHANNEL_LABELS );
    aZipOS.putNextEntry( zipEntry );

    // Write the channel labels
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      final Channel[] channels = aDataSet.getChannels();
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
   * @param aZipOS
   *          the ZIP output stream to write the metadata to.
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeProjectMetadata( final ZipOutputStream aZipOS, final String aName, final String aVersion )
      throws IOException
  {
    final ZipEntry zipEntry = new ZipEntry( FILENAME_PROJECT_METADATA );
    aZipOS.putNextEntry( zipEntry );

    // Write the project metadata...
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      out.println( aName );
      out.println( aVersion );
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
  private void writeProjectSettings( final ZipOutputStream aZipOS ) throws IOException
  {
    try
    {
      Map.Entry<String, Properties>[] entries = this.windowStateManager.getAllSettings();
      for ( Map.Entry<String, Properties> entry : entries )
      {
        final String name = entry.getKey();
        final String zipEntryName = FILENAME_PROJECT_SETTINGS.concat( name );

        final ZipEntry zipEntry = new ZipEntry( zipEntryName );
        aZipOS.putNextEntry( zipEntry );

        // Write the project settings
        entry.getValue().store( aZipOS, name.concat( " settings" ) );
      }
    }
    finally
    {
      aZipOS.flush();
    }
  }
}
