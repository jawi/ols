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
package nl.lxtreme.ols.client.usersettings;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.util.swing.WindowManager.*;


/**
 * Provides a user-settings manager.
 */
public class UserSettingsManager implements WindowStateManager
{
  // INNER TYPES

  // VARIABLES

  private final Map<String, Rectangle> bounds = new HashMap<String, Rectangle>();
  private final Map<String, Properties> settings = new HashMap<String, Properties>();

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "unchecked" )
  public Map.Entry<String, Rectangle>[] getAllBounds()
  {
    Collection<Map.Entry<String, Rectangle>> entries = this.bounds.entrySet();
    return entries.toArray( new Map.Entry[entries.size()] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "unchecked" )
  public Map.Entry<String, Properties>[] getAllSettings()
  {
    Collection<Map.Entry<String, Properties>> entries = this.settings.entrySet();
    return entries.toArray( new Map.Entry[entries.size()] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Rectangle getBounds( final String aKey )
  {
    return this.bounds.get( aKey );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Properties getSettings( final String aKey )
  {
    Properties settings = this.settings.get( aKey );
    if ( settings == null )
    {
      settings = new Properties();
      this.settings.put( aKey, settings );
    }
    return settings;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putBounds( final String aKey, final Rectangle aBounds )
  {
    this.bounds.put( aKey, aBounds );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putSettings( final String aKey, final Properties aSettings )
  {
    this.settings.put( aKey, aSettings );
  }
}
