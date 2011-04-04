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
package nl.lxtreme.ols.util.osgi;


import java.util.*;
import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides a bundle watcher, that watches the (de)registration of bundles. When
 * a bundle is (de)registered, and it matches certain criteria (as defined by a
 * {@link BundleScanner}), all registered {@link BundleObserver} instances are
 * being called.
 * <p>
 * Code based on PAX Extender and <a
 * href="http://www.toedter.com/blog/?p=236">this blog</a> posting.
 * </p>
 */
public final class BundleWatcher
{
  // INNER TYPES

  /**
   * The bundle tracker for implementing the extender pattern.
   */
  final class ExtenderBundleTracker extends BundleTracker
  {
    // CONSTRUCTORS

    /**
     * Creates a new ExtenderBundleTracker instance.
     * 
     * @param aContext
     *          the current bundle context to use, cannot be <code>null</code>.
     */
    public ExtenderBundleTracker( final BundleContext aContext )
    {
      super( aContext, Bundle.ACTIVE, null /* aCustomizer */);
    }

    // METHODS

    /**
     * @see org.osgi.util.tracker.BundleTracker#addingBundle(org.osgi.framework.Bundle,
     *      org.osgi.framework.BundleEvent)
     */
    @Override
    public Object addingBundle( final Bundle aBundle, final BundleEvent aEvent )
    {
      bundleStarted( aBundle );
      return aBundle;
    }

    /**
     * @see org.osgi.util.tracker.BundleTracker#removedBundle(org.osgi.framework.Bundle,
     *      org.osgi.framework.BundleEvent, java.lang.Object)
     */
    @Override
    public void removedBundle( final Bundle aBundle, final BundleEvent aEvent, final Object aObject )
    {
      bundleStopped( aBundle );
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BundleWatcher.class.getName() );

  // VARIABLES

  private final BundleContext bundleContext;
  private final BundleScanner bundleScanner;

  private final Map<Long, ManifestHeader[]> registry;
  private final List<BundleObserver> observers;

  private volatile BundleTracker bundleTracker;

  // CONSTRUCTORS

  /**
   * Creates a new BundleWatcher instance.
   * 
   * @param aBundleContext
   *          the bundle context to use, cannot be <code>null</code>;
   * @param aBundleScanner
   *          the bundle scanner to use, cannot be <code>null</code>.
   */
  public BundleWatcher( final BundleContext aBundleContext, final BundleScanner aBundleScanner )
  {
    this.bundleContext = aBundleContext;
    this.bundleScanner = aBundleScanner;

    this.registry = new HashMap<Long, ManifestHeader[]>();
    this.observers = new ArrayList<BundleObserver>();
  }

  // METHODS

  /**
   * Convenience factory method to create a bundle watcher that searches for
   * bundle's containing a certain manifest header matching the given keyword.
   * 
   * @param aBundleContext
   *          the bundle context to use, cannot be <code>null</code>;
   * @param aKeyword
   *          the keyword to match the manifest header's key against, cannot be
   *          <code>null</code>.
   * @return a bundle watcher instance, never <code>null</code>.
   */
  public static BundleWatcher createKeywordBundleWatcher( final BundleContext aBundleContext, final String aKeyword )
  {
    return new BundleWatcher( aBundleContext, new KeywordManifestScanner( aKeyword ) );
  }

  /**
   * Convenience factory method to create a bundle watcher that searches for
   * bundle's containing a certain manifest header matching a regular
   * expression.
   * 
   * @param aBundleContext
   *          the bundle context to use, cannot be <code>null</code>;
   * @param aRegEx
   *          the regular expression to match the manifest header's key against,
   *          cannot be <code>null</code>.
   * @return a bundle watcher instance, never <code>null</code>.
   */
  public static BundleWatcher createRegExBundleWatcher( final BundleContext aBundleContext, final String aRegEx )
  {
    return new BundleWatcher( aBundleContext, new RegExManifestScanner( aRegEx ) );
  }

  /**
   * Adds a given bundle observer to the list of observers.
   * <p>
   * Bundle observers are <em>all</em> being called when a bundle is
   * started/stopped that matches the criteria of the defined bundle scanner. It
   * is up to the bundle observer itself to do anything interesting with that
   * particular information.
   * </p>
   * 
   * @param aObserver
   *          the bundle observer to install, cannot be <code>null</code>.
   * @return this bundle watcher, never <code>null</code>.
   */
  public BundleWatcher add( final BundleObserver aObserver )
  {
    if ( aObserver != null )
    {
      if ( !this.observers.contains( aObserver ) )
      {
        LOG.log( Level.FINE, "Adding new bundle observer ..." );
        this.observers.add( aObserver );
      }
    }

    return this;
  }

  /**
   * Starts the bundle watcher for registration/deregistration of bundles.
   * 
   * @throws IllegalStateException
   *           in case the bundle watcher is already started.
   */
  public void start() throws IllegalStateException
  {
    if ( this.bundleTracker != null )
    {
      throw new IllegalStateException( "Bundle Watcher already started?!" );
    }

    this.bundleTracker = new ExtenderBundleTracker( this.bundleContext );
    this.bundleTracker.open();

    LOG.log( Level.FINE, "Bundle watcher started ..." );
  }

  /**
   * Stops the bundle watcher for registration/deregistration of bundles.
   * 
   * @throws IllegalStateException
   *           in case the bundle watcher is already stopped.
   */
  public void stop()
  {
    if ( this.bundleTracker == null )
    {
      throw new IllegalStateException( "Bundle Watcher not started?!" );
    }

    this.bundleTracker.close();
    this.bundleTracker = null;

    this.registry.clear();

    LOG.log( Level.FINE, "Bundle watcher stopped ..." );
  }

  /**
   * Handles all "bundle started" events.
   * 
   * @param aBundle
   *          the started bundle, never <code>null</code>.
   */
  final void bundleStarted( final Bundle aBundle )
  {
    final ManifestHeader[] entries = this.bundleScanner.scan( aBundle );
    if ( ( entries == null ) || ( entries.length == 0 ) )
    {
      return;
    }

    synchronized ( this.registry )
    {
      final Long bundleId = Long.valueOf( aBundle.getBundleId() );
      this.registry.put( bundleId, entries );
    }

    synchronized ( this.observers )
    {
      for ( BundleObserver observer : this.observers )
      {
        observer.added( aBundle, entries );
      }
    }
  }

  /**
   * Handles all "bundle stopped" events.
   * 
   * @param aBundle
   *          the stopped bundle, never <code>null</code>.
   */
  final void bundleStopped( final Bundle aBundle )
  {
    final ManifestHeader[] entries;

    synchronized ( this.registry )
    {
      final Long bundleId = Long.valueOf( aBundle.getBundleId() );
      entries = this.registry.remove( bundleId );
    }

    if ( entries == null )
    {
      return;
    }

    synchronized ( this.observers )
    {
      for ( BundleObserver observer : this.observers )
      {
        observer.removed( aBundle, entries );
      }
    }
  }
}
