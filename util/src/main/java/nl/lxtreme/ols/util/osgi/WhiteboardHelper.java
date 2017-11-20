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
import org.osgi.util.tracker.*;


/**
 * Provides a helper class for easy implementation of the whiteboard pattern as
 * used in OSGi.
 */
public class WhiteboardHelper<T> extends ServiceTracker<T, T>
{
  // INNER TYPES

  /**
   * To be implemented by the class that wants to obtain a reference to a
   * certain service.
   *
   * @param <T>
   *          the type of the service that is to be visited.
   */
  public static interface Visitor<T>
  {
    /**
     * Called for each service.
     *
     * @param aService
     *          the service that is to be visited, never <code>null</code>.
     */
    void visit( T aService );
  }

  // CONSTRUCTORS

  /**
   * Creates a new WhiteboardHelper instance.
   *
   * @param aContext
   *          the bundle context to use;
   * @param aServiceClass
   *          the class of the service we should track, cannot be
   *          <code>null</code>.
   */
  public WhiteboardHelper( final BundleContext aContext, final Class<T> aServiceClass )
  {
    super( aContext, aServiceClass.getName(), null /* aCustomizer */ );
  }

  /**
   * Creates a new WhiteboardHelper instance.
   *
   * @param aContext
   *          the bundle context to use;
   * @param aFilter
   *          the filter to use while filtering on the services, cannot be
   *          <code>null</code>;
   */
  public WhiteboardHelper( final BundleContext aContext, final Filter aFilter )
  {
    super( aContext, aFilter, null );
  }

  // METHODS

  /**
   * Called to invoke the given {@link Visitor} for each found service.
   *
   * @param aVisitor
   *          the visitor to call for each found service, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given visitor was <code>null</code>.
   */
  @SuppressWarnings( "unchecked" )
  public final void accept( final Visitor<? super T> aVisitor )
  {
    if ( aVisitor == null )
    {
      throw new IllegalArgumentException( "Invalid argument: Visitor cannot be null!" );
    }

    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object serviceObj : services )
      {
        aVisitor.visit( ( T )serviceObj );
      }
    }
  }

  /**
   * Called to invoke the given {@link Visitor} for the first found service.
   *
   * @param aVisitor
   *          the visitor to call for the first found service, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given visitor was <code>null</code>.
   */
  @SuppressWarnings( "unchecked" )
  public final void acceptFirst( final Visitor<? super T> aVisitor )
  {
    if ( aVisitor == null )
    {
      throw new IllegalArgumentException( "Invalid argument: Visitor cannot be null!" );
    }

    final Object serviceObj = getService();
    if ( serviceObj != null )
    {
      aVisitor.visit( ( T )serviceObj );
    }
  }
}
