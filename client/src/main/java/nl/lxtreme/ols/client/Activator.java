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


import java.awt.GraphicsEnvironment;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;
import org.osgi.service.prefs.*;


/**
 * Provides the client bundle activator, which is responsible for starting the
 * entire client UI.
 */
public class Activator extends DependencyActivatorBase
{
  // INNER TYPES

  static class ComponentProviderBundleAdapter extends GenericBundleAdapter<ComponentProvider>
  {
    /**
     * Creates a new Activator.ComponentProviderBundleAdapter instance.
     */
    public ComponentProviderBundleAdapter()
    {
      super( ComponentProvider.class, OLS_COMPONENT_PROVIDER_CLASS_KEY );
    }
  }

  static class DeviceBundleAdapter extends GenericBundleAdapter<Device>
  {
    /**
     * Creates a new {@link DeviceBundleAdapter} instance.
     */
    public DeviceBundleAdapter()
    {
      super( Device.class, OLS_DEVICE_CLASS_KEY );
    }
  }

  static class ExporterBundleAdapter extends GenericBundleAdapter<Exporter>
  {
    /**
     * Creates a new Activator.ExporterBundleAdapter instance.
     */
    public ExporterBundleAdapter()
    {
      super( Exporter.class, OLS_EXPORTER_CLASS_KEY );
    }
  }

  @SuppressWarnings( "rawtypes" )
  static class ToolBundleAdapter extends GenericBundleAdapter<Tool>
  {
    /**
     * Creates a new {@link ToolBundleAdapter} instance.
     */
    public ToolBundleAdapter()
    {
      super( Tool.class, OLS_TOOL_CLASS_KEY );
    }
  }

  // CONSTANTS

  private static final String OLS_TOOL_MAGIC_KEY = "OLS-Tool";
  private static final String OLS_TOOL_MAGIC_VALUE = "1.0";
  private static final String OLS_TOOL_CLASS_KEY = "OLS-ToolClass";
  private static final String TOOL_BUNDLE_FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_TOOL_MAGIC_KEY,
      OLS_TOOL_MAGIC_VALUE, OLS_TOOL_CLASS_KEY );

  private static final String OLS_DEVICE_MAGIC_KEY = "OLS-Device";
  private static final String OLS_DEVICE_MAGIC_VALUE = "1.0";
  private static final String OLS_DEVICE_CLASS_KEY = "OLS-DeviceClass";
  private static final String DEVICE_BUNDLE_FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_DEVICE_MAGIC_KEY,
      OLS_DEVICE_MAGIC_VALUE, OLS_DEVICE_CLASS_KEY );

  private static final String OLS_EXPORTER_MAGIC_KEY = "OLS-Exporter";
  private static final String OLS_EXPORTER_MAGIC_VALUE = "1.0";
  private static final String OLS_EXPORTER_CLASS_KEY = "OLS-ExporterClass";
  private static final String EXPORTER_BUNDLE_FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_EXPORTER_MAGIC_KEY,
      OLS_EXPORTER_MAGIC_VALUE, OLS_EXPORTER_CLASS_KEY );

  private static final String OLS_COMPONENT_PROVIDER_MAGIC_KEY = "OLS-ComponentProvider";
  private static final String OLS_COMPONENT_PROVIDER_MAGIC_VALUE = "Menu";
  private static final String OLS_COMPONENT_PROVIDER_CLASS_KEY = "OLS-ComponentProviderClass";
  private static final String CP_BUNDLE_FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_COMPONENT_PROVIDER_MAGIC_KEY,
      OLS_COMPONENT_PROVIDER_MAGIC_VALUE, OLS_COMPONENT_PROVIDER_CLASS_KEY );

  // METHODS

  /**
   * Returns whether or not we're running in debug mode.
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  public static boolean isDebugMode()
  {
    return Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    // Nothing...
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    // Do not start if we're running headless...
    if ( GraphicsEnvironment.isHeadless() )
    {
      throw new RuntimeException( "Cannot start client: running headless." );
    }

    final ClientController clientController = new ClientController( aContext );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, CP_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ComponentProviderBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, TOOL_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ToolBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, DEVICE_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( DeviceBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, EXPORTER_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ExporterBundleAdapter.class ) );

    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, UIManagerConfigurator.PID );

    String[] serviceNames = new String[] { UIManagerConfigurator.class.getName(), ManagedService.class.getName() };

    // UI Manager Configuration...
    aManager.add( createComponent() //
        .setInterface( serviceNames, props ) //
        .setImplementation( UIManagerConfigurator.class ) //
        );

    props.put( Constants.SERVICE_PID, UIColorSchemeManager.PID );

    serviceNames = new String[] { UIColorSchemeManager.class.getName(), ManagedServiceFactory.class.getName() };

    // UI Manager Configuration...
    aManager.add( createComponent() //
        .setInterface( serviceNames, props ) //
        .setImplementation( UIColorSchemeManager.class ) );

    // User session manager...
    aManager.add( createComponent() //
        .setImplementation( new UserSessionManager() ) //
        .add( createServiceDependency() //
            .setService( ProjectManager.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( UserSettingsManager.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( PreferencesService.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) //
        ) );

    // All the interfaces we're registering the client controller under...
    serviceNames = new String[] { AcquisitionDataListener.class.getName(), AcquisitionProgressListener.class.getName(),
        AcquisitionStatusListener.class.getName(), AnnotationListener.class.getName(), PlatformCallback.class.getName() };

    // Client controller...
    aManager.add( createComponent() //
        .setInterface( serviceNames, null ) //
        .setImplementation( clientController ) //
        .add( createServiceDependency() //
            .setService( HostProperties.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( ProjectManager.class ) //
            .setRequired( true ) //
            .setCallbacks( "setProjectManager", "removeProjectManager" ) ) //
        .add( createServiceDependency() //
            .setService( DataAcquisitionService.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( UIColorSchemeManager.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( ComponentProvider.class, "(OLS-ComponentProvider=Menu)" ) //
            .setCallbacks( "addMenu", "removeMenu" ) //
            .setRequired( false ) ) //
        .add( createServiceDependency() //
            .setService( Device.class ) //
            .setCallbacks( "addDevice", "removeDevice" ) //
            .setRequired( false ) ) //
        .add( createServiceDependency() //
            .setService( Tool.class ) //
            .setCallbacks( "addTool", "removeTool" ) //
            .setRequired( false ) ) //
        .add( createServiceDependency() //
            .setService( Exporter.class ) //
            .setCallbacks( "addExporter", "removeExporter" ) //
            .setRequired( false ) ) //
        .add( createConfigurationDependency() //
            .setPid( UIManagerConfigurator.PID ) ) //
        );
  }
}

/* EOF */
