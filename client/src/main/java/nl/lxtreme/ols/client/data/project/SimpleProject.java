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
  private String[] channelLabels;
  private long[] cursors;
  private Properties settings;
  private CapturedData capturedData;
  private boolean changed;
  private Date lastModified;
  private String sourceVersion;

  // CONSTRUCTORS

  /**
   * Creates a new SimpleProject.
   */
  public SimpleProject()
  {
    super();

    this.changed = false;
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
   * @see nl.lxtreme.ols.api.data.project.Project#getCursors()
   */
  @Override
  public long[] getCursors()
  {
    return this.cursors;
  }

  /**
   * @return the lastModified
   */
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
   * @return the sourceVersion
   */
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
   * @param aCapturedData
   *          the capturedData to set
   */
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.capturedData = aCapturedData;
  }

  /**
   * @param aChanged
   *          the changed to set
   */
  public void setChanged( final boolean aChanged )
  {
    this.changed = aChanged;
  }

  /**
   * @param aChannelLabels
   *          the channelLabels to set
   */
  public void setChannelLabels( final String... aChannelLabels )
  {
    if ( aChannelLabels != null )
    {
      this.channelLabels = new String[CapturedData.MAX_CHANNELS];
      System.arraycopy( aChannelLabels, 0, this.channelLabels, 0, aChannelLabels.length );
    }
  }

  /**
   * @param aCursors
   *          the cursors to set
   */
  public void setCursors( final long... aCursors )
  {
    if ( aCursors != null )
    {
      this.cursors = new long[CapturedData.MAX_CURSORS];
      System.arraycopy( aCursors, 0, this.cursors, 0, aCursors.length );
    }
  }

  /**
   * @param aLastModified
   *          the lastModified to set
   */
  public void setLastModified( final Date aLastModified )
  {
    this.lastModified = aLastModified;
  }

  /**
   * Sets the project name.
   * 
   * @param aName
   *          the name to set, may be <code>null</code>.
   */
  public void setName( final String aName )
  {
    this.name = aName;
  }

  /**
   * Sets the project settings.
   * 
   * @param aSettings
   *          the settings to set, can be <code>null</code>.
   */
  public void setSettings( final Properties aSettings )
  {
    this.settings = aSettings;
  }

  /**
   * @param aSourceVersion
   *          the sourceVersion to set
   */
  public void setSourceVersion( final String aSourceVersion )
  {
    this.sourceVersion = aSourceVersion;
  }

}
