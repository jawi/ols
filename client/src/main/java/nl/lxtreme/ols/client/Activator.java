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

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.data.project.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.osgi.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.prefs.*;


/**
 * Provides the client bundle activator, which is responsible for starting the
 * entire client UI.
 */
public class Activator extends DependencyActivatorBase
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
  private LogReaderTracker logReaderTracker;

  // METHODS

  /**
   * Creates the bundle observer for component providers.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createComponentProviderBundleObserver()
  {
    return new BundleServiceObserver( OLS_COMPONENT_PROVIDER_MAGIC_KEY, OLS_COMPONENT_PROVIDER_MAGIC_VALUE,
        OLS_COMPONENT_PROVIDER_CLASS_KEY, ComponentProvider.class.getName() )
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
        Device.class.getName() )
    {
      @Override
      protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService,
          final ManifestHeader... aEntries )
      {
        Properties result = new Properties();
        result.put( Action.NAME, ( ( Device )aService ).getName() );
        return result;
      }
    };
  }

  /**
   * Creates the bundle observer for exporters.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createExporterBundleObserver()
  {
    return new BundleServiceObserver( OLS_EXPORTER_MAGIC_KEY, OLS_EXPORTER_MAGIC_VALUE, OLS_EXPORTER_CLASS_KEY,
        Exporter.class.getName() )
    {
      @Override
      protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService,
          final ManifestHeader... aEntries )
      {
        Properties result = new Properties();
        result.put( Action.NAME, ( ( Exporter )aService ).getName() );
        return result;
      }
    };
  }

  /**
   * Creates the bundle observer for tools.
   * 
   * @return a bundle observer, never <code>null</code>.
   */
  private static BundleObserver createToolBundleObserver()
  {
    return new BundleServiceObserver( OLS_TOOL_MAGIC_KEY, OLS_TOOL_MAGIC_VALUE, OLS_TOOL_CLASS_KEY,
        Tool.class.getName() )
    {
      @Override
      protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService,
          final ManifestHeader... aEntries )
      {
        Properties result = new Properties();
        result.put( Action.NAME, ( ( Tool<?> )aService ).getName() );
        return result;
      }
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    this.bundleWatcher.stop();
    this.logReaderTracker.close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    final ProjectManager projectManager = new SimpleProjectManager();

    final ClientController clientController = new ClientController( aContext );

    this.logReaderTracker = new LogReaderTracker( aContext );
    this.logReaderTracker.open();

    this.bundleWatcher = BundleWatcher.createRegExBundleWatcher( aContext, "^OLS-.*" );
    this.bundleWatcher //
        .add( createToolBundleObserver() ) //
        .add( createDeviceBundleObserver() ) //
        .add( createExporterBundleObserver() ) //
        .add( createComponentProviderBundleObserver() );
    // Start watching all bundles for extenders...
    this.bundleWatcher.start();

    // Project manager...
    aManager.add( //
        createComponent() //
            .setInterface( ProjectManager.class.getName(), null ) //
            .setImplementation( projectManager ) //
            .add( createServiceDependency() //
                .setService( HostProperties.class ) //
                .setRequired( true ) //
            ) //
        );

    // User session manager...
    aManager.add( //
        createComponent() //
            .setImplementation( new UserSessionManager() ) //
            .add( createServiceDependency() //
                .setService( ProjectManager.class ) //
                .setRequired( true ) ) //
            .add( createServiceDependency() //
                .setService( PreferencesService.class ) //
                .setRequired( true ) //
            ) //
        );

    // All the interfaces we're registering the client controller under...
    final String[] interfaceNames = new String[] { AcquisitionDataListener.class.getName(),
        AcquisitionProgressListener.class.getName(), AcquisitionStatusListener.class.getName(),
        AnnotationListener.class.getName(), ApplicationCallback.class.getName() };

    // Client controller...
    aManager.add( //
        createComponent() //
            .setInterface( interfaceNames, null ) //
            .setImplementation( clientController ) //
            .add( createServiceDependency() //
                .setService( HostProperties.class ) //
                .setRequired( true ) //
            ) //
            .add( createServiceDependency() //
                .setService( ProjectManager.class ) //
                .setRequired( true ) //
                .setCallbacks( "setProjectManager", "removeProjectManager" ) //
            ) //
            .add( createServiceDependency() //
                .setService( DataAcquisitionService.class ) //
                .setRequired( true ) //
            ) //
            .add( createServiceDependency() //
                .setService( ComponentProvider.class, "(component.id=Menu)" ) //
                .setCallbacks( "addMenu", "removeMenu" ) //
                .setRequired( false ) //
            ) //
            .add( createServiceDependency() //
                .setService( Device.class ) //
                .setCallbacks( "addDevice", "removeDevice" ) //
                .setRequired( false ) //
            ) //
            .add( createServiceDependency() //
                .setService( Tool.class ) //
                .setCallbacks( "addTool", "removeTool" ) //
                .setRequired( false ) //
            ) //
            .add( createServiceDependency() //
                .setService( Exporter.class ) //
                .setCallbacks( "addExporter", "removeExporter" ) //
                .setRequired( false ) //
            ) //
        );
  }
}

/* EOF */
