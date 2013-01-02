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
package nl.lxtreme.ols.common;


import java.util.*;


/**
 * Provides an abstraction for an editor of {@link Configuration}s.
 */
public interface ConfigurationEditor
{
  // INNER TYPES

  /**
   * Configuration change listener.
   */
  public static interface ConfigurationChangeListener extends EventListener
  {
    // METHODS

    /**
     * Called when the configuration editor acknowledges that the given
     * dictionary is the "definitive" configuration.
     * 
     * @param aPID
     *          the PID of the configuration that was changed, never
     *          <code>null</code>;
     * @param aConfiguration
     *          the changed configuration, never <code>null</code>.
     */
    void onConfigurationAcknowledged( String aPID, Dictionary<Object, Object> aConfiguration );

    /**
     * Called when all changes to the configuration are discarded.
     */
    void onConfigurationDiscarded();
  }

  // CONSTANTS

  /**
   * The bundle header that should be used to register a custom configuration
   * editor instance for a bundle.
   */
  public static final String BUNDLE_HEADER_KEY = "X-ConfigurationEditor";

  // METHODS

  /**
   * @param aListener
   *          the listener to add, can be <code>null</code> in which case this
   *          method does nothing.
   */
  void addConfigurationChangeListener( ConfigurationChangeListener aListener );

  /**
   * @param aListener
   *          the listener to remove, can be <code>null</code> in which case
   *          this method does nothing.
   */
  void removeConfigurationChangeListener( ConfigurationChangeListener aListener );
}
