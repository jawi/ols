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


import java.util.regex.*;


/**
 * Provides a manifest header scanner that uses regular expressions to match the
 * manifest header keys of interest.
 */
public class RegExManifestScanner extends AbstractManifestScanner
{
  // VARIABLES

  private final Pattern pattern;

  // CONSTRUCTORS

  /**
   * Creates a new RegExManifestScanner instance.
   * 
   * @param aRegEx
   *          the regular expression for the manifest header key to match,
   *          cannot be <code>null</code>.
   */
  public RegExManifestScanner( final String aRegEx )
  {
    this.pattern = Pattern.compile( aRegEx );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.osgi.AbstractManifestScanner#matches(java.lang.String)
   */
  @Override
  protected boolean matches( final String aKey )
  {
    return this.pattern.matcher( aKey ).matches();
  }
}
