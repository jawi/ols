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


import java.beans.*;
import java.io.*;

import nl.lxtreme.ols.api.data.project.*;


/**
 * Provides a "stub" project manager for testing purposes, that can be modified
 * externally.
 */
public class StubTestProjectManager implements ProjectManager
{
  // VARIABLES

  private Project project;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    // NO-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Project createNewProject()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Project createTemporaryProject()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Project getCurrentProject()
  {
    return this.project;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void loadProject( final InputStream aInput ) throws IOException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    // NO-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveProject( final OutputStream aOutput ) throws IOException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @param aProject
   */
  public void setCurrentProject( final Project aProject )
  {
    this.project = aProject;
  }

}
