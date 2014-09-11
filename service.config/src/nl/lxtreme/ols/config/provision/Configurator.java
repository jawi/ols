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
package nl.lxtreme.ols.config.provision;


import java.io.*;

import org.osgi.service.cm.*;


/**
 * Provides the ability to provision configurations.
 */
public interface Configurator
{
  // METHODS

  /**
   * Provisions all property-based configuration file resources found in the
   * configuration directory.
   * <p>
   * This method tries to install <em>all</em> found configuration file
   * resources, continuing with the next file upon exceptions. Only the
   * <em>first</em> exception is rethrown after all resources have been
   * processed.
   * </p>
   * <p>
   * This method considers any file ending in ".cfg" to be a configuration file.
   * </p>
   *
   * @param dir
   *          the directory to provision from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O errors accessing Property-file resources.
   */
  void provisionAll( File dir ) throws IOException;

  /**
   * Provisions the given {@link File} as property-based configuration.
   *
   * @param file
   *          the {@link File} to install as Property-file resource, cannot be
   *          <code>null</code>.
   * @return <code>true</code> if the configuration was successfully
   *         provisioned, <code>false</code> otherwise. Note that a result of
   *         <code>true</code> is <b>no</b> indication that the configuration
   *         itself is accepted, it can still be rejected by the
   *         {@link ManagedService} or {@link ManagedServiceFactory}.
   * @throws IOException
   *           in case of I/O errors accessing the Property-file resource.
   */
  boolean provision( File file ) throws IOException;

}
