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

import nl.lxtreme.ols.acquisition.service.*;
import nl.lxtreme.ols.client.acquisition.*;
import nl.lxtreme.ols.client.componentprovider.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.client.project.impl.*;
import nl.lxtreme.ols.client.tool.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.export.*;
import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;
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

  // CONSTANTS

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
    registerBundleAdapters( aManager );
    registerApplicationCallbackFacade( aManager );

    registerUIManagerConfigurator( aManager );
    registerUIColorSchemeManager( aManager );

    registerUserSettingsManager( aManager );
    registerUserSessionManager( aManager );

    registerClient( aManager );
  }

  /**
   * Registers a facade for the {@link ApplicationCallback} service.
   */
  private void registerApplicationCallbackFacade( final DependencyManager aManager )
  {
    aManager.add( createComponent() //
        .setImplementation( ApplicationCallbackFacade.class ) //
        .add( createServiceDependency() //
            .setService( ApplicationCallback.class ) //
            .setRequired( true ) ) //
        );
  }

  /**
   * Registers the various bundle adapters.
   */
  private void registerBundleAdapters( final DependencyManager aManager )
  {
    aManager.add( createBundleAdapterService( Bundle.ACTIVE, CP_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ComponentProviderBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, ToolBundleAdapter.TOOL_BUNDLE_FILTER, true ) //
        .setImplementation( ToolBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, DEVICE_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( DeviceBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, EXPORTER_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ExporterBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );
  }

  /**
   * @param aManager
   */
  private void registerClient( final DependencyManager aManager )
  {
    final Client client = Client.getInstance();

    // Expose all of our "static" controllers as OSGi-managed components...
    // @formatter:off
    final String[] interfaces = new String[] { EventHandler.class.getName(), ApplicationCallback.class.getName(),
        StatusListener.class.getName(), ManagedService.class.getName() };

    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, ClientConfig.class.getName() );
    props.put( EventConstants.EVENT_TOPIC, new String[] { Session.TOPIC_ANY, DataAcquisitionService.TOPIC_ANY,
        ToolInvoker.TOPIC_ANY } );

    aManager.add( createComponent()
        .setImplementation( client )
        .setInterface( interfaces, props )
        .setComposition( "getComposition" )
        .add( createServiceDependency().setService( ComponentProvider.class, "(OLS-ComponentProvider=Menu)" ).setCallbacks( "addMenu", "removeMenu" ).setRequired( false ) )
        .add( createServiceDependency().setService( Exporter.class ).setCallbacks( client.getImportExportController(), "addExporter", "removeExporter" ).setRequired( false ) )
        .add( createServiceDependency().setService( Device.class ).setCallbacks( client.getDeviceController(), "addDevice", "removeDevice" ).setRequired( false ) )
        .add( createServiceDependency().setService( ToolInvoker.class ).setCallbacks( client.getToolController(), "addTool", "removeTool" ).setRequired( false ) )
        .add( createServiceDependency().setService( UIManagerConfigurator.class ).setRequired( true ) )
        .add( createServiceDependency().setService( ColorSchemeManager.class ).setRequired( true ) )
        .add( createServiceDependency().setService( ConfigurationAdmin.class ).setRequired( true ) )
        .add( createServiceDependency().setService( MetaTypeService.class ).setRequired( true ) )
        .add( createServiceDependency().setService( StatusListener.class ).setRequired( false ) )
        .add( createServiceDependency().setService( IDataAcquirer.class ).setRequired( true ) ) 
        .add( createServiceDependency().setService( LogService.class ).setRequired( false ) )
        .add( createServiceDependency().setService( Session.class ).setRequired( true ) )
    );
    // @formatter:on
  }

  /**
   * Registers the {@link ColorSchemeManager} as service.
   */
  private void registerUIColorSchemeManager( final DependencyManager aManager )
  {
    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, ColorSchemeManager.PID );

    String[] serviceNames = new String[] { ColorSchemeManager.class.getName(), ManagedServiceFactory.class.getName() };

    // UI Manager Configuration...
    aManager.add( createComponent() //
        .setInterface( serviceNames, props ) //
        .setImplementation( ColorSchemeManager.class ) );
  }

  /**
   * Register the {@link UIManagerConfigurator} as service.
   */
  private void registerUIManagerConfigurator( final DependencyManager aManager )
  {
    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, UIManagerConfigurator.PID );

    String[] serviceNames = new String[] { UIManagerConfigurator.class.getName(), ManagedService.class.getName() };

    // UI Manager Configuration...
    aManager.add( createComponent() //
        .setInterface( serviceNames, props ) //
        .setImplementation( UIManagerConfigurator.class ) //
        .add( createConfigurationDependency() //
            .setPid( UIManagerConfigurator.PID ) ) //
        );
  }

  /**
   * @param aManager
   */
  private void registerUserSessionManager( final DependencyManager aManager )
  {
    // User session manager...
    aManager.add( createComponent() //
        .setImplementation( new UserSessionManager() ) //
        .add( createServiceDependency() //
            .setService( ProjectController.class ) //
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
  }

  /**
   * @param aManager
   */
  private void registerUserSettingsManager( final DependencyManager aManager )
  {
    aManager.add( //
        createComponent() //
            .setInterface( UserSettingsManager.class.getName(), null ) //
            .setImplementation( new UserSettingsManagerImpl() ) //
            .add( createServiceDependency() //
                .setService( LogService.class ) //
                .setRequired( false ) //
            ) //
        );
  }
}

/* EOF */
