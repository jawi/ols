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
 * Provides an abstract bundle observer implementation with some convenience
 * methods.
 */
public abstract class AbstractBundleObserver implements BundleObserver
{
  // VARIABLES

  private final String magicKey;
  private final String magicValue;

  // CONSTRUCTORS

  /**
   * Creates a new AbstractBundleObserver instance.
   * 
   * @param aMagicKey
   *          the magic key to look for, cannot be <code>null</code>;
   * @param aMagicValue
   *          the magic value to look for, cannot be <code>null</code>.
   */
  public AbstractBundleObserver( final String aMagicKey, final String aMagicValue )
  {
    this.magicKey = aMagicKey;
    this.magicValue = aMagicValue;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.osgi.BundleObserver#added(org.osgi.framework.Bundle,
   *      nl.lxtreme.ols.util.osgi.ManifestHeader[])
   * @see #doAdded(Bundle, ManifestHeader...)
   */
  @Override
  public final void added( final Bundle aBundle, final ManifestHeader... aEntries )
  {
    if ( containsMagicToken( aEntries ) )
    {
      doAdded( aBundle, aEntries );
    }
  }

  /**
   * @see nl.lxtreme.ols.util.osgi.BundleObserver#removed(org.osgi.framework.Bundle,
   *      nl.lxtreme.ols.util.osgi.ManifestHeader[])
   * @see #doRemoved(Bundle, ManifestHeader...)
   */
  @Override
  public final void removed( final Bundle aBundle, final ManifestHeader... aEntries )
  {
    if ( containsMagicToken( aEntries ) )
    {
      doRemoved( aBundle, aEntries );
    }
  }

  /**
   * Called when a bundle is activated (started) that matches certain criteria
   * in its bundle manifest.
   * 
   * @param aBundle
   *          the bundle that is matched, never <code>null</code>;
   * @param aEntries
   *          the matched manifest entries, never <code>null</code>.
   */
  protected abstract void doAdded( final Bundle aBundle, final ManifestHeader... aEntries );

  /**
   * Called when a bundle is deactivated (stopped) that matches certain criteria
   * in its bundle manifest.
   * 
   * @param aBundle
   *          the bundle that is matched, never <code>null</code>;
   * @param aEntries
   *          the matched manifest entries, never <code>null</code>.
   */
  protected abstract void doRemoved( final Bundle aBundle, final ManifestHeader... aEntries );

  /**
   * Returns the value of the manifest header with the given key.
   * 
   * @param aKey
   *          the manifest header key to look for, cannot be <code>null</code>;
   * @param aEntries
   *          the manifest headers to look in.
   * @return the manifest header value, can be <code>null</code> if not found.
   */
  protected final String getManifestHeaderValue( final String aKey, final ManifestHeader... aEntries )
  {
    for ( ManifestHeader entry : aEntries )
    {
      if ( aKey.equals( entry.getKey() ) )
      {
        return entry.getValue();
      }
    }
    return null;
  }

  /**
   * Returns the values of the manifest header with the given key, split on the
   * ", "-token.
   * 
   * @param aKey
   *          the manifest header key to look for, cannot be <code>null</code>;
   * @param aEntries
   *          the manifest headers to look in.
   * @return the manifest header values, never <code>null</code>, but an empty
   *         array if not found.
   */
  protected final String[] getManifestHeaderValues( final String aKey, final ManifestHeader... aEntries )
  {
    for ( ManifestHeader entry : aEntries )
    {
      if ( aKey.equals( entry.getKey() ) )
      {
        return entry.splitValue();
      }
    }
    return new String[0];
  }

  /**
   * Returns whether the given set of manifest headers contains the "magic"
   * token.
   * 
   * @param aEntries
   *          the entries to look in, cannot be <code>null</code>.
   * @return <code>true</code> if the magic token is found in the given manifest
   *         entries, <code>false</code> otherwise.
   */
  private boolean containsMagicToken( final ManifestHeader... aEntries )
  {
    for ( ManifestHeader entry : aEntries )
    {
      if ( this.magicKey.equals( entry.getKey() ) && this.magicValue.equals( entry.getValue() ) )
      {
        return true;
      }
    }
    return false;
  }

}
