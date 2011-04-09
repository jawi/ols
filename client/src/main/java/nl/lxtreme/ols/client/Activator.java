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
package nl.lxtreme.ols.client;


import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.osgi.*;

import org.osgi.framework.*;


/**
 * Provides the client bundle activator, which is responsible for starting the
 * entire client UI.
 */
public class Activator implements BundleActivator
{
  // CONSTANTS

  private static final String OLS_TOOL_MAGIC_KEY = "OLS-Tool";
  private static final String OLS_TOOL_MAGIC_VALUE = "1.0";
  private static final String OLS_TOOL_CLASS_KEY = "OLS-ToolClass";

  private static final String OLS_DEVICE_MAGIC_KEY = "OLS-Device";
  private static final String OLS_DEVICE_MAGIC_VALUE = "1.0";
  private static final String OLS_DEVICE_CLASS_KEY = "OLS-DeviceClass";

  private static final String OLS_EXPORTER_MAGIC_KEY = "OLS-Exporter";
  private static final String OLS_EXPORTER_MAGIC_VALUE = "1.0";
  private static final String OLS_EXPORTER_CLASS_KEY = "OLS-ExporterClass";

  private static final String OLS_COMPONENT_PROVIDER_MAGIC_KEY = "OLS-ComponentProvider";
  private static final String OLS_COMPONENT_PROVIDER_CLASS_KEY = "OLS-ComponentProviderClass";
  /** a RegEx for the supported components. */
  private static final String OLS_COMPONENT_PROVIDER_MAGIC_VALUE = "(Menu)";

  // VARIABLES

  private BundleWatcher bundleWatcher;
  private Host host;
  private LogReaderTracker logReaderTracker;

  // METHODS

  /**
   * Creats the bundle observer for component providers.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createComponentProviderBundleObserver()
  {
    return new BundleServiceObserver( OLS_COMPONENT_PROVIDER_MAGIC_KEY, OLS_COMPONENT_PROVIDER_MAGIC_VALUE,
        OLS_COMPONENT_PROVIDER_CLASS_KEY, nl.lxtreme.ols.api.ui.ComponentProvider.class.getName() )
    {
      @Override
      protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService,
          final ManifestHeader... aEntries )
      {
        final Properties properties = new Properties();
        final String componentKind = getManifestHeaderValue( OLS_COMPONENT_PROVIDER_MAGIC_KEY, aEntries );
        properties.put( ComponentProvider.COMPONENT_ID_KEY, componentKind );
        return properties;
      }

      @Override
      protected boolean matchesMagicValue( final ManifestHeader aHeaderEntry, final String aMagicValue )
      {
        return aHeaderEntry.getValue().matches( aMagicValue );
      }
    };
  }

  /**
   * Creates the bundle observer for device(-controller)s.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createDeviceBundleObserver()
  {
    return new BundleServiceObserver( OLS_DEVICE_MAGIC_KEY, OLS_DEVICE_MAGIC_VALUE, OLS_DEVICE_CLASS_KEY,
        nl.lxtreme.ols.api.devices.DeviceController.class.getName() );
  }

  /**
   * Creates the bundle observer for exporters.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createExporterBundleObserver()
  {
    return new BundleServiceObserver( OLS_EXPORTER_MAGIC_KEY, OLS_EXPORTER_MAGIC_VALUE, OLS_EXPORTER_CLASS_KEY,
        nl.lxtreme.ols.api.data.export.Exporter.class.getName() );
  }

  /**
   * Creates the bundle observer for tools.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createToolBundleObserver()
  {
    return new BundleServiceObserver( OLS_TOOL_MAGIC_KEY, OLS_TOOL_MAGIC_VALUE, OLS_TOOL_CLASS_KEY,
        nl.lxtreme.ols.api.tools.Tool.class.getName() );
  }

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    final Runnable startTask = new Runnable()
    {
      @Override
      public void run()
      {
        final Host _host = getHost();
        if ( _host != null )
        {
          // First let the host initialize itself...
          _host.initialize();

          // Then start it...
          _host.start();
        }
      }
    };

    this.host = new Host( aContext );

    this.logReaderTracker = new LogReaderTracker( aContext );

    // This has to be done *before* any other Swing related code is executed
    // so this also means the #invokeLater call done below...
    HostUtils.initOSSpecifics( Host.getShortName(), this.host );

    this.bundleWatcher = BundleWatcher.createRegExBundleWatcher( aContext, "^OLS-.*" );
    this.bundleWatcher //
        .add( createToolBundleObserver() ) //
        .add( createDeviceBundleObserver() ) //
        .add( createExporterBundleObserver() ) //
        .add( createComponentProviderBundleObserver() );

    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( startTask );

    this.logReaderTracker.open();
    // Start watching all bundles for extenders...
    this.bundleWatcher.start();
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    final Runnable shutdownTask = new Runnable()
    {
      @Override
      public void run()
      {
        final Host _host = getHost();
        if ( _host != null )
        {
          _host.shutdown();
          _host.stop();
        }
      }
    };

    if ( this.bundleWatcher != null )
    {
      this.bundleWatcher.stop();
      this.bundleWatcher = null;
    }

    SwingUtilities.invokeLater( shutdownTask );

    if ( this.logReaderTracker != null )
    {
      this.logReaderTracker.close();
      this.logReaderTracker = null;
    }
  }

  /**
   * Returns the actual Swing-host.
   * 
   * @return the host, can be <code>null</code>.
   */
  final Host getHost()
  {
    return this.host;
  }
}

/* EOF */
