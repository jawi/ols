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


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;


/**
 * @author jawi
 */
public class SimpleProject implements Project
{
  // VARIABLES

  private String name;
  private final String[] channelLabels;
  private final Long[] cursors;
  private Properties settings;
  private CapturedData capturedData;
  private boolean changed;
  private boolean cursorsEnabled;
  private Date lastModified;
  private String sourceVersion;

  // CONSTRUCTORS

  /**
   * Creates a new SimpleProject.
   */
  public SimpleProject()
  {
    super();

    this.cursors = new Long[CapturedData.MAX_CURSORS];
    this.channelLabels = new String[CapturedData.MAX_CHANNELS];

    this.changed = false;
    this.cursorsEnabled = false;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getCapturedData()
   */
  @Override
  public CapturedData getCapturedData()
  {
    return this.capturedData;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getChannelLabels()
   */
  @Override
  public String[] getChannelLabels()
  {
    return this.channelLabels;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#getCursorPositions()
   */
  @Override
  public Long[] getCursorPositions()
  {
    return this.cursors;
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
   * @see nl.lxtreme.ols.api.data.project.Project#getSettings()
   */
  @Override
  public Properties getSettings()
  {
    return this.settings;
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
   * @see nl.lxtreme.ols.api.data.project.Project#setCapturedData(nl.lxtreme.ols.api.data.CapturedData)
   */
  @Override
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.capturedData = aCapturedData;
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
   * @see nl.lxtreme.ols.api.data.project.Project#setChannelLabels(java.lang.String[])
   */
  @Override
  public void setChannelLabels( final String... aChannelLabels )
  {
    if ( aChannelLabels != null )
    {
      System.arraycopy( aChannelLabels, 0, this.channelLabels, 0, aChannelLabels.length );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorPositions(long[])
   */
  @Override
  public void setCursorPositions( final Long... aCursors )
  {
    if ( aCursors != null )
    {
      System.arraycopy( aCursors, 0, this.cursors, 0, aCursors.length );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorsEnabled(boolean)
   */
  @Override
  public void setCursorsEnabled( final boolean aEnabled )
  {
    this.cursorsEnabled = aEnabled;
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
   * @see nl.lxtreme.ols.api.data.project.Project#setSettings(java.util.Properties)
   */
  @Override
  public void setSettings( final Properties aSettings )
  {
    this.settings = aSettings;
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setSourceVersion(java.lang.String)
   */
  @Override
  public void setSourceVersion( final String aSourceVersion )
  {
    this.sourceVersion = aSourceVersion;
  }
}
