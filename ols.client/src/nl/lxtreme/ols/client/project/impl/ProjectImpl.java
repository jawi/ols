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
package nl.lxtreme.ols.client.project.impl;


import java.beans.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.AcquisitionDataBuilder.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Denotes a project implementation.
 */
public final class ProjectImpl implements Project, ProjectProperties, PropertyChangeListener
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ProjectImpl.class.getName() );

  // VARIABLES

  private final PropertyChangeSupport propertyChangeSupport;
  private final Map<String, UserSettings> settings;

  private AcquisitionData data;
  private String name;
  private boolean changed;
  private Date lastModified;
  private String sourceVersion;
  private File filename;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ProjectImpl} instance with 10 cursors and 32 channels.
   */
  public ProjectImpl()
  {
    this.propertyChangeSupport = new PropertyChangeSupport( this );
    this.settings = new HashMap<String, UserSettings>();
    this.changed = false;
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
  public AcquisitionData getDataSet()
  {
    return this.data;
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#getFilename()
   */
  @Override
  public File getFilename()
  {
    return this.filename;
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#getLastModified()
   */
  @Override
  public Date getLastModified()
  {
    return this.lastModified;
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#getName()
   */
  @Override
  public String getName()
  {
    return this.name;
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#getSettings(java.lang.String)
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
   * @see nl.lxtreme.ols.client.project.Project#getSourceVersion()
   */
  @Override
  public String getSourceVersion()
  {
    return this.sourceVersion;
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#isChanged()
   */
  @Override
  public boolean isChanged()
  {
    return this.changed;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    String name = aEvent.getPropertyName();
    if ( PROPERTY_CURSORS_ENABLED.equals( name ) || name.startsWith( "channel" ) || name.startsWith( "cursor" ) )
    {
      setChanged( true );
    }

    this.propertyChangeSupport.firePropertyChange( aEvent );
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
  public void setCapturedData( final AcquisitionData aData )
  {
    final AcquisitionData old = this.data;

    if ( old != null && UIManager.getBoolean( "ols.retain.annotations.boolean" ) )
    {
      AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
      builder.applyTemplate( old, IncludeSamples.NO, IncludeAnnotations.YES );
      builder.applyTemplate( aData, IncludeSamples.YES, IncludeAnnotations.NO );

      this.data = builder.build();
    }
    else
    {
      this.data = aData;
    }

    // Mark this project as modified...
    setChanged( true );

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CAPTURED_DATA, old, this.data );
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#setChanged(boolean)
   */
  @Override
  public void setChanged( final boolean aChanged )
  {
    final boolean old = this.changed;
    this.changed = aChanged;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CHANGED, old, aChanged );
  }

  /**
   * @see nl.lxtreme.ols.client.project.Project#setFilename(java.io.File)
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
   * @see nl.lxtreme.ols.client.project.Project#setLastModified(java.util.Date)
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
   * @see nl.lxtreme.ols.client.project.Project#setName(java.lang.String)
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
   * @see nl.lxtreme.ols.client.project.Project#setSettings(nl.lxtreme.ols.api.UserSettings)
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
   * @see nl.lxtreme.ols.client.project.Project#setSourceVersion(java.lang.String)
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
   * @see nl.lxtreme.ols.client.project.Project#visit(nl.lxtreme.ols.client.project.ProjectVisitor)
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
