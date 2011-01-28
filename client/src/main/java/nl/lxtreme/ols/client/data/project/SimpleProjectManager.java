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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.data.project;


import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.regex.*;
import java.util.zip.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a simple implementation of a project manager, which writes an entire
 * project as (compressed) ZIP-file.
 */
public class SimpleProjectManager implements ProjectManager
{
  // CONSTANTS

  private static final String FILENAME_PROJECT_METADATA = "ols.project";
  private static final String FILENAME_CHANNEL_LABELS = "channel.labels";
  private static final String FILENAME_PROJECT_SETTINGS = "settings.properties";
  private static final String FILENAME_CAPTURE_RESULTS = "data.ols";

  private static final Logger LOG = Logger.getLogger( SimpleProjectManager.class.getName() );

  /** The regular expression used to parse an (OLS-datafile) instruction. */
  private static final Pattern OLS_INSTRUCTION_PATTERN = Pattern.compile( "^;([^:]+):\\s+([^\r\n]+)$" );
  /** The regular expression used to parse an (OLS-datafile) data value. */
  private static final Pattern OLS_DATA_PATTERN = Pattern.compile( "^([0-9a-fA-F]+)@(\\d+)$" );

  // VARIABLES

  private final Host host;

  private Project project;

  // CONSTRUCTORS

  /**
   * Creates a new SimpleProjectManager instance.
   * 
   * @param aHost
   *          the host to use, cannot be <code>null</code>.
   */
  public SimpleProjectManager( final Host aHost )
  {
    this.host = aHost;

    this.project = new SimpleProject();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.project.ProjectManager#createNewProject()
   */
  public void createNewProject()
  {
    this.project = new SimpleProject();
  };

  /**
   * @see nl.lxtreme.ols.api.data.project.ProjectManager#getCurrentProject()
   */
  @Override
  public Project getCurrentProject()
  {
    return this.project;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.ProjectManager#loadProject(java.io.InputStream)
   */
  @Override
  public void loadProject( final InputStream aInput ) throws IOException
  {
    final SimpleProject newProject = new SimpleProject();

    final BufferedInputStream in = new BufferedInputStream( aInput );
    final ZipInputStream zipIS = new ZipInputStream( in );

    try
    {
      ZipEntry ze = null;
      while ( ( ze = zipIS.getNextEntry() ) != null )
      {
        if ( FILENAME_PROJECT_METADATA.equals( ze.getName() ) )
        {
          loadProjectMetadata( newProject, zipIS );
        }
        else if ( FILENAME_PROJECT_SETTINGS.equals( ze.getName() ) )
        {
          loadProjectSettings( newProject, zipIS );
        }
        else if ( FILENAME_CHANNEL_LABELS.equals( ze.getName() ) )
        {
          loadChannelLabels( newProject, zipIS );
        }
        else if ( FILENAME_CAPTURE_RESULTS.equals( ze.getName() ) )
        {
          loadCapturedResults( newProject, zipIS );
        }

        zipIS.closeEntry();
      }
    }
    finally
    {
      this.project = newProject;

      HostUtils.closeResource( zipIS );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.ProjectManager#saveProject(java.io.OutputStream)
   */
  @Override
  public void saveProject( final OutputStream aOutput ) throws IOException
  {
    final BufferedOutputStream os = new BufferedOutputStream( aOutput );
    final ZipOutputStream zipOS = new ZipOutputStream( os );

    zipOS.setComment( Host.FULL_NAME.concat( " project file" ) );

    try
    {
      storeProjectMetadata( this.project, zipOS );
      // Store the channel labels...
      storeChannelLabels( this.project, zipOS );
      // Store the settings...
      storeProjectSettings( this.project, zipOS );
      // Store the last capture results...
      storeCapturedResults( this.project, zipOS );
    }
    finally
    {
      HostUtils.closeResource( zipOS );
      HostUtils.closeResource( os );
    }
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
  protected void loadCapturedResults( final SimpleProject aProject, final ZipInputStream aZipIS ) throws IOException
  {
    int size = -1, rate = -1, channels = 32, enabledChannels = -1;
    long triggerPos = -1;

    long[] cursorPositions = new long[CapturedData.MAX_CURSORS];
    CapturedData capturedData = null;

    boolean cursors = false;
    boolean compressed = false;
    long absLen = 0;

    final InputStreamReader isReader = new InputStreamReader( aZipIS );
    final BufferedReader br = new BufferedReader( isReader );

    try
    {
      final List<String[]> dataValues = new ArrayList<String[]>();

      String line;
      while ( ( line = br.readLine() ) != null )
      {
        // Determine whether the line is an instruction, or data...
        final Matcher instructionMatcher = OLS_INSTRUCTION_PATTERN.matcher( line );
        final Matcher dataMatcher = OLS_DATA_PATTERN.matcher( line );

        if ( dataMatcher.matches() )
        {
          final String[] dataPair = new String[] { dataMatcher.group( 1 ), dataMatcher.group( 2 ) };
          dataValues.add( dataPair );
        }
        else if ( instructionMatcher.matches() )
        {
          // Ok; found an instruction...
          final String instrKey = instructionMatcher.group( 1 );
          final String instrValue = instructionMatcher.group( 2 );

          if ( "Size".equals( instrKey ) )
          {
            size = Integer.parseInt( instrValue );
          }
          else if ( "Rate".equals( instrKey ) )
          {
            rate = Integer.parseInt( instrValue );
          }
          else if ( "Channels".equals( instrKey ) )
          {
            channels = Integer.parseInt( instrValue );
          }
          else if ( "TriggerPosition".equals( instrKey ) )
          {
            triggerPos = Long.parseLong( instrValue );
          }
          else if ( "EnabledChannels".equals( instrKey ) )
          {
            enabledChannels = Integer.parseInt( instrValue );
          }
          else if ( "CursorEnabled".equals( instrKey ) )
          {
            cursors = Boolean.parseBoolean( instrValue );
          }
          else if ( "Compressed".equals( instrKey ) )
          {
            compressed = Boolean.parseBoolean( instrValue );
          }
          else if ( "AbsoluteLength".equals( instrKey ) )
          {
            absLen = Long.parseLong( instrValue );
          }
          else if ( "CursorA".equals( instrKey ) )
          {
            cursorPositions[0] = Long.parseLong( instrValue );
          }
          else if ( "CursorB".equals( instrKey ) )
          {
            cursorPositions[1] = Long.parseLong( instrValue );
          }
          else if ( instrKey.startsWith( "Cursor" ) )
          {
            final int idx = Integer.parseInt( instrKey.substring( 6 ) );
            final long pos = Long.parseLong( instrValue );
            cursorPositions[idx] = pos;
          }
        }
      }

      long absoluteLength;
      int[] values;
      long[] timestamps;

      if ( dataValues.isEmpty() || ( size < 0 ) )
      {
        throw new IOException( "File does not appear to be a valid datafile!" );
      }

      if ( !compressed )
      {
        throw new IOException(
            "Uncompressed data file found! Please sent this file to the OLS developers for further inspection!" );
      }
      else if ( size != dataValues.size() )
      {
        throw new IOException( "Data size mismatch! Corrupt file encountered!" );
      }
      else
      {
        // new compressed file format
        absoluteLength = absLen;
        values = new int[size];
        timestamps = new long[size];

        try
        {
          for ( int i = 0; i < dataValues.size(); i++ )
          {
            final String[] dataPair = dataValues.get( i );

            values[i] = Integer.parseInt( dataPair[0].substring( 0, 4 ), 16 ) << 16
                | Integer.parseInt( dataPair[0].substring( 4, 8 ), 16 );

            timestamps[i] = Long.parseLong( dataPair[1] );
          }
        }
        catch ( final NumberFormatException exception )
        {
          throw new IOException( "Invalid data encountered." );
        }
      }

      capturedData = new CapturedDataImpl( values, timestamps, triggerPos, rate, channels, enabledChannels,
          absoluteLength );
    }
    finally
    {
      if ( cursors )
      {
        aProject.setCursors( cursorPositions );
      }
      if ( capturedData != null )
      {
        aProject.setCapturedData( capturedData );
      }
    }
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
  protected void loadChannelLabels( final SimpleProject aProject, final ZipInputStream aZipIS ) throws IOException
  {
    final InputStreamReader isReader = new InputStreamReader( aZipIS );
    final BufferedReader reader = new BufferedReader( isReader );

    String[] labels = new String[CapturedData.MAX_CHANNELS];

    try
    {
      String label = null;
      int idx = 0;
      while ( ( ( label = reader.readLine() ) != null ) && ( idx < ( labels.length - 1 ) ) )
      {
        labels[idx++] = label;
      }
    }
    finally
    {
      aProject.setChannelLabels( labels );
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
  protected void loadProjectMetadata( final SimpleProject aProject, final ZipInputStream aZipIS ) throws IOException
  {
    final InputStreamReader isReader = new InputStreamReader( aZipIS );
    final BufferedReader reader = new BufferedReader( isReader );

    String name = null;
    String version = null;
    Date savedAt = null;

    try
    {
      name = reader.readLine();
      version = reader.readLine();
      savedAt = new Date( Long.valueOf( reader.readLine() ) );

      LOG.log( Level.INFO, "Reading project {0}, created at {1} by OLSv{2}.", new Object[] { name, savedAt, version } );
    }
    finally
    {
      aProject.setName( name );
      aProject.setSourceVersion( version );
      aProject.setLastModified( savedAt );
    }
  }

  /**
   * Reads the project settings to the given ZIP-input stream.
   * 
   * @param aProject
   *          the project to read the settings for;
   * @param aZipIS
   *          the ZIP input stream to read the settings from.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void loadProjectSettings( final SimpleProject aProject, final ZipInputStream aZipIS ) throws IOException
  {
    final Properties settings = new Properties();
    try
    {
      settings.load( aZipIS );
    }
    finally
    {
      aProject.setSettings( settings );
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
  protected void storeCapturedResults( final Project aProject, final ZipOutputStream aZipOS ) throws IOException
  {
    final CapturedData captureData = aProject.getCapturedData();
    if ( captureData == null )
    {
      return;
    }

    final long[] cursors = aProject.getCursors();
    final boolean cursorsEnabled = ( cursors != null ) && ( cursors.length > 0 );

    final ZipEntry zipEntry = new ZipEntry( FILENAME_CAPTURE_RESULTS );
    aZipOS.putNextEntry( zipEntry );

    PrintStream out = new PrintStream( aZipOS );

    try
    {
      final int[] values = captureData.getValues();
      final long[] timestamps = captureData.getTimestamps();

      out.print( ";Size: " );
      out.println( values.length );

      out.print( ";Rate: " );
      out.println( captureData.getSampleRate() );

      out.print( ";Channels: " );
      out.println( captureData.getChannels() );

      out.print( ";EnabledChannels: " );
      out.println( captureData.getEnabledChannels() );

      if ( captureData.hasTriggerData() )
      {
        out.print( ";TriggerPosition: " );
        out.println( captureData.getTriggerPosition() );
      }

      out.print( ";Compressed: " );
      out.println( true );

      out.print( ";AbsoluteLength: " );
      out.println( captureData.getAbsoluteLength() );

      out.print( ";CursorEnabled: " );
      out.println( cursorsEnabled );

      for ( int i = 0; cursorsEnabled && ( i < cursors.length ); i++ )
      {
        out.print( String.format( ";Cursor%d: ", i ) );
        out.println( cursors[i] );
      }
      for ( int i = 0; i < values.length; i++ )
      {
        final String hexVal = Integer.toHexString( values[i] );
        out.print( "00000000".substring( hexVal.length() ) );
        out.print( hexVal );
        out.print( "@" );
        out.println( timestamps[i] );
      }
    }
    finally
    {
      out.flush();
      out = null;
    }
  }

  /**
   * Stores the channel labels to the given ZIP-output stream.
   * <p>
   * If the given project does not have channel labels, this method does
   * nothing.
   * </p>
   * 
   * @param aProject
   *          the project to write the channel labels for;
   * @param aZipOS
   *          the ZIP output stream to write the channel labels to.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void storeChannelLabels( final Project aProject, final ZipOutputStream aZipOS ) throws IOException
  {
    final String[] labels = aProject.getChannelLabels();
    if ( ( labels == null ) || ( labels.length == 0 ) )
    {
      return;
    }

    final ZipEntry zipEntry = new ZipEntry( FILENAME_CHANNEL_LABELS );
    aZipOS.putNextEntry( zipEntry );

    // Write the channel labels
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      for ( String label : labels )
      {
        out.println( label );
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
  protected void storeProjectMetadata( final Project aProject, final ZipOutputStream aZipOS ) throws IOException
  {
    final String name = aProject.getName();
    if ( ( name == null ) || name.trim().isEmpty() )
    {
      return;
    }

    final ZipEntry zipEntry = new ZipEntry( FILENAME_PROJECT_METADATA );
    aZipOS.putNextEntry( zipEntry );

    // Write the project metadata...
    PrintStream out = new PrintStream( aZipOS );

    try
    {
      out.println( name );
      out.println( this.host.getVersion() );
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
  protected void storeProjectSettings( final Project aProject, final ZipOutputStream aZipOS ) throws IOException
  {
    final Properties settings = aProject.getSettings();
    if ( settings == null )
    {
      return;
    }

    final ZipEntry zipEntry = new ZipEntry( FILENAME_PROJECT_SETTINGS );
    aZipOS.putNextEntry( zipEntry );

    try
    {
      // Write the project settings
      settings.store( aZipOS, Host.FULL_NAME.concat( " project settings" ) );
    }
    finally
    {
      aZipOS.flush();
    }
  }
}
