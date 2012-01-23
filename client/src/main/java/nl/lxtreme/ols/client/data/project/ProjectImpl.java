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
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.client.data.project;


import java.beans.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.data.settings.*;
import nl.lxtreme.ols.client.signaldisplay.channel.*;
import nl.lxtreme.ols.client.signaldisplay.cursor.CursorImpl;


/**
 * Denotes a project implementation.
 */
final class ProjectImpl implements Project, ProjectProperties
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ProjectImpl.class.getName() );

  // VARIABLES

  private final PropertyChangeSupport propertyChangeSupport;

  private String name;
  private Channel[] channels;
  private final Cursor[] cursors;
  private final Map<String, UserSettings> settings;
  private AcquisitionResult capturedData;
  private boolean changed;
  private boolean cursorsEnabled;
  private Date lastModified;
  private String sourceVersion;
  private File filename;

  // CONSTRUCTORS

  /**
   * Creates a new ProjectImpl.
   */
  public ProjectImpl()
  {
    super();

    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.cursors = new Cursor[Ols.MAX_CURSORS];
    for ( int i = 0; i < this.cursors.length; i++ )
    {
      this.cursors[i] = new CursorImpl( i );
    }
    this.channels = new Channel[Ols.MAX_CHANNELS];

    this.changed = false;
    this.cursorsEnabled = false;

    this.settings = new HashMap<String, UserSettings>();
  }

  // METHODS

  /**
   * Adds the given listener to the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.addPropertyChangeListener( aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult getCapturedData()
  {
    return this.capturedData;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel getChannel( final int aIndex )
  {
    Channel channel = this.channels[aIndex];
    if ( channel == null )
    {
      channel = new ChannelImpl( aIndex );
      this.channels[aIndex] = channel;
    }
    return channel;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel[] getChannels()
  {
    return Arrays.copyOf( this.channels, this.channels.length );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor getCursor( final int aIndex )
  {
    return this.cursors[aIndex];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor[] getCursors()
  {
    return Arrays.copyOf( this.cursors, this.cursors.length );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getFilename()
   */
  @Override
  public File getFilename()
  {
    return this.filename;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getLastModified()
   */
  @Override
  public Date getLastModified()
  {
    return this.lastModified;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getName()
   */
  @Override
  public String getName()
  {
    return this.name;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getSettings(java.lang.String)
   */
  @Override
  public UserSettings getSettings( final String aName )
  {
    UserSettings result = this.settings.get( aName );
    if ( result == null )
    {
      result = new UserSettingsImpl( aName );
      this.settings.put( aName, result );
    }
    return result;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getSourceVersion()
   */
  @Override
  public String getSourceVersion()
  {
    return this.sourceVersion;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#isChanged()
   */
  @Override
  public boolean isChanged()
  {
    return this.changed;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#isCursorsEnabled()
   */
  @Override
  public boolean isCursorsEnabled()
  {
    return this.cursorsEnabled;
  }

  /**
   * Removes the given listener from the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.removePropertyChangeListener( aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setCapturedData( final AcquisitionResult aCapturedData )
  {
    final AcquisitionResult old = this.capturedData;
    this.capturedData = aCapturedData;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CAPTURED_DATA, old, aCapturedData );

    int channelCount = aCapturedData.getChannels();
    this.channels = new Channel[channelCount];
    for ( int c = 0; c < channelCount; c++ )
    {
      this.channels[c] = new ChannelImpl( c );
    }

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setChanged(boolean)
   */
  @Override
  public void setChanged( final boolean aChanged )
  {
    final boolean old = this.changed;
    this.changed = aChanged;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CHANGED, old, aChanged );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setChannels( final Channel... aChannels )
  {
    if ( aChannels == null )
    {
      throw new IllegalArgumentException( "Channel labels cannot be null!" );
    }

    final Channel[] old = getChannels();
    Arrays.fill( this.channels, null );
    System.arraycopy( aChannels, 0, this.channels, 0, aChannels.length );

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CHANNEL_LABELS, old, aChannels );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setCursors( final Cursor... aCursors )
  {
    if ( aCursors == null )
    {
      throw new IllegalArgumentException( "Cursors cannot be null!" );
    }

    final Cursor[] old = getCursors();
    Arrays.fill( this.cursors, null );
    System.arraycopy( aCursors, 0, this.cursors, 0, aCursors.length );

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CURSORS, old, aCursors );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorsEnabled(boolean)
   */
  @Override
  public void setCursorsEnabled( final boolean aEnabled )
  {
    final boolean old = this.cursorsEnabled;
    this.cursorsEnabled = aEnabled;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CURSORS_ENABLED, old, aEnabled );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setFilename(java.io.File)
   */
  @Override
  public void setFilename( final File aFilename )
  {
    final File old = this.filename;
    this.filename = aFilename;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_FILENAME, old, aFilename );

    // We don't mark the project as saved; as this is probably a bit weird: we
    // save a new project, thereby knowing its filename, and yet we're marking
    // it immediately as changed...
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setLastModified(java.util.Date)
   */
  @Override
  public void setLastModified( final Date aLastModified )
  {
    final Date old = this.lastModified;
    this.lastModified = aLastModified;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_LAST_MODIFIED, old, aLastModified );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setName(java.lang.String)
   */
  @Override
  public void setName( final String aName )
  {
    final String old = this.name;
    this.name = aName;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_NAME, old, aName );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSettings(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void setSettings( final UserSettings aSettings )
  {
    if ( aSettings == null )
    {
      throw new IllegalArgumentException( "Settings cannot be null!" );
    }

    final UserSettings old = this.settings.get( aSettings.getName() );
    this.settings.put( aSettings.getName(), aSettings );

    this.propertyChangeSupport.firePropertyChange( PROPERTY_SETTINGS, old, aSettings );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSourceVersion(java.lang.String)
   */
  @Override
  public void setSourceVersion( final String aSourceVersion )
  {
    final String old = this.sourceVersion;
    this.sourceVersion = aSourceVersion;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_SOURCE_VERSION, old, aSourceVersion );

    // Mark this project as modified...
    setChanged( true );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#visit(nl.lxtreme.ols.api.data.project.ProjectVisitor)
   */
  @Override
  public void visit( final ProjectVisitor aVisitor )
  {
    final List<UserSettings> userSettings = new ArrayList<UserSettings>( this.settings.values() );
    for ( UserSettings settings : userSettings )
    {
      try
      {
        aVisitor.visit( settings );
      }
      catch ( Exception exception )
      {
        LOG.log( Level.INFO, "Exception during visiting project! Continuing anyway...", exception );
      }
    }
  }

  /**
   * Returns the current set of property change listeners.
   * 
   * @return an array of property change listeners, never <code>null</code>.
   */
  final PropertyChangeListener[] getPropertyChangeListeners()
  {
    return this.propertyChangeSupport.getPropertyChangeListeners();
  }
}
