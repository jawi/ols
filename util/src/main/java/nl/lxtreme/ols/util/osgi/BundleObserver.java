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
package nl.lxtreme.ols.util.osgi;


import org.osgi.framework.*;


/**
 * Provides a bundle observer that is called for each bundle that matches
 * certain criteria.
 */
public interface BundleObserver
{
  // METHODS

  /**
   * Called when a bundle is activated (started) that matches certain criteria
   * in its bundle manifest.
   * 
   * @param aBundle
   *          the bundle that is matched, never <code>null</code>;
   * @param aEntries
   *          the matched manifest entries, never <code>null</code>.
   */
  void added( final Bundle aBundle, final ManifestHeader... aEntries );

  /**
   * Called when a bundle is deactivated (stopped) that matches certain criteria
   * in its bundle manifest.
   * 
   * @param aBundle
   *          the bundle that is matched, never <code>null</code>;
   * @param aEntries
   *          the matched manifest entries, never <code>null</code>.
   */
  void removed( final Bundle aBundle, final ManifestHeader... aEntries );
}
