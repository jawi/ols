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


import java.io.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;

import org.osgi.service.log.*;


/**
 * Provides an implementation of {@link ProjectManager}.
 */
public class ProjectManagerImpl implements ProjectManager
{
  // VARIABLES

  // Injected by Felix DM...
  private volatile SessionProvider sessionProvider;
  private volatile LogService log;
  // Locally managed...
  private volatile File projectFile;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void createNewProject()
  {
    this.log.log( LogService.LOG_INFO, "Creating new project..." );

    this.projectFile = null;

    for ( Session session : this.sessionProvider.getSessions() )
    {
      this.log.log( LogService.LOG_DEBUG, "Closing session: " + session );

      session.close();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public File getProjectFile()
  {
    return this.projectFile;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isProjectChanged()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void loadDataFile( File aFile ) throws IOException
  {
    FileReader reader = new FileReader( aFile );
    try
    {
      AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

      OlsDataHelper.read( reader, builder );
      
      // TODO
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
    // TODO Auto-generated method stub
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveDataFile( File aFile, Session aSession ) throws IOException
  {
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
    // TODO Auto-generated method stub
    this.projectFile = aFile;
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
}
