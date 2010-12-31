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


import java.util.*;

import org.osgi.framework.*;


/**
 * Provides an abstract manifest scanner.
 */
public abstract class AbstractManifestScanner implements BundleScanner
{
  // METHODS

  /**
   * @see nl.lxtreme.ols.util.osgi.BundleScanner#scan(org.osgi.framework.Bundle)
   */
  @Override
  public ManifestHeader[] scan( final Bundle aBundle )
  {
    final Dictionary<?, ?> headers = aBundle.getHeaders();

    final List<ManifestHeader> entries = new ArrayList<ManifestHeader>();

    final Enumeration<?> headerKeys = headers.keys();
    while ( headerKeys.hasMoreElements() )
    {
      final String headerKey = ( String )headerKeys.nextElement();
      if ( matches( headerKey ) )
      {
        final String headerValue = ( String )headers.get( headerKey );
        entries.add( new ManifestHeader( headerKey, headerValue ) );
      }
    }

    if ( entries.isEmpty() )
    {
      return null;
    }

    return entries.toArray( new ManifestHeader[entries.size()] );
  }

  /**
   * Checks whether the given manifest header key is of particular interest, if
   * so, it should return <code>true</code>, otherwise <code>false</code>.
   * 
   * @param aKey
   *          the manifest header key, never <code>null</code>.
   * @return <code>true</code> if the given manifest header is any interest,
   *         <code>false</code> otherwise.
   */
  protected abstract boolean matches( final String aKey );
}
