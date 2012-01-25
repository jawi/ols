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
package nl.lxtreme.ols.test.data.project;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.test.data.*;


/**
 * Provides a "stub" project for testing purposes, that can be modified
 * externally and provides some additional convenience methods to assist
 * testing.
 */
public class StubTestProject implements Project
{
  // VARIABLES

  private final StubDataSet dataSet;
  private String sourceVersion;
  private final Map<String, UserSettings> settings = new HashMap<String, UserSettings>();
  private String name;
  private Date lastModified;
  private File file;
  private boolean changed;

  // CONSTRUCTORS

  /**
   * Creates a new StubTestProject instance.
   */
  public StubTestProject()
  {
    this.dataSet = new StubDataSet();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public DataSet getDataSet()
  {
    return this.dataSet;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getFilename()
   */
  @Override
  public File getFilename()
  {
    return this.file;
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
    UserSettings userSettings = this.settings.get( aName );
    if ( userSettings == null )
    {
      userSettings = mock( UserSettings.class );
    }
    return userSettings;
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
   * {@inheritDoc}
   */
  @Override
  public void readData( final Reader aReader ) throws IOException
  {
    OlsDataHelper.read( this.dataSet, aReader );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCapturedData(AcquisitionResult)
   */
  @Override
  public void setCapturedData( final AcquisitionResult aCapturedData )
  {
    this.dataSet.setCapturedData( aCapturedData );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setChanged(boolean)
   */
  @Override
  public void setChanged( final boolean aChanged )
  {
    this.changed = aChanged;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setFilename(java.io.File)
   */
  @Override
  public void setFilename( final File aFilename )
  {
    this.file = aFilename;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setLastModified(java.util.Date)
   */
  @Override
  public void setLastModified( final Date aLastModified )
  {
    this.lastModified = aLastModified;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setName(java.lang.String)
   */
  @Override
  public void setName( final String aName )
  {
    this.name = aName;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSettings(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void setSettings( final UserSettings aSettings )
  {
    this.settings.put( aSettings.getName(), aSettings );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSourceVersion(java.lang.String)
   */
  @Override
  public void setSourceVersion( final String aSourceVersion )
  {
    this.sourceVersion = aSourceVersion;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#visit(nl.lxtreme.ols.api.data.project.ProjectVisitor)
   */
  @Override
  public void visit( final ProjectVisitor aVisitor )
  {
    for ( UserSettings settings : this.settings.values() )
    {
      try
      {
        aVisitor.visit( settings );
      }
      catch ( Exception exception )
      {
        fail( exception.toString() );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void writeData( final Writer aWriter ) throws IOException
  {
    OlsDataHelper.write( this.dataSet, aWriter );
  }
}
