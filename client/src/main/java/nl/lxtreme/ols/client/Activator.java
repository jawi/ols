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
package nl.lxtreme.ols.client;


import java.lang.reflect.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * 
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private Host host = null;

  // METHODS

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.host = new Host( aContext );

    // This has to be done *before* any other Swing related code is executed
    // so this also means the #invokeLater call done below...
    HostUtils.initOSSpecifics( Host.getShortName(), this.host );

    final Runnable initTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.initialize();
      }
    };

    final Runnable startTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.start();
      }
    };
    invokeAndWait( initTask );

    SwingUtilities.invokeLater( startTask );
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    final Runnable stopTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.stop();
      }
    };
    invokeAndWait( stopTask );

    final Runnable shutdownTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.shutdown();
      }
    };
    invokeAndWait( shutdownTask );
  }

  /**
   * Invokes the given {@link Runnable}'s run method and waits until its
   * execution is finished.
   * 
   * @param aRunnable
   *          the runnable to execute, cannot be <code>null</code>.
   * @throws InterruptedException
   *           in case the thread was interrupted;
   * @throws InvocationTargetException
   *           in case the given runnable caused an exception.
   */
  private void invokeAndWait( final Runnable aRunnable ) throws InterruptedException, InvocationTargetException
  {
    if ( SwingUtilities.isEventDispatchThread() )
    {
      aRunnable.run();
    }
    else
    {
      SwingUtilities.invokeAndWait( aRunnable );
    }
  }
}

/* EOF */
