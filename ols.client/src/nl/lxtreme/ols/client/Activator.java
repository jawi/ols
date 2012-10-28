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
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.client.project.impl.*;
import nl.lxtreme.ols.client.session.*;
import nl.lxtreme.ols.client.tool.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.export.*;
import nl.lxtreme.ols.host.*;

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
    final ClientController clientController = new ClientController( aContext );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, CP_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ComponentProviderBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, ToolBundleAdapter.TOOL_BUNDLE_FILTER, true ) //
        .setImplementation( ToolBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );

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

    aManager.add( createComponent() //
        .setImplementation( ApplicationCallbackFacade.class ) //
        .add( createServiceDependency() //
            .setService( ApplicationCallback.class ) //
            .setRequired( false ) ) //
        );

    aManager.add( createComponent() //
        .setInterface( Session.class.getName(), null ) //
        .setImplementation( SessionImpl.class ) //
        .add( createServiceDependency() //
            .setService( EventAdmin.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );

    // Project manager...
    aManager.add( //
        createComponent() //
            .setInterface( UserSettingsManager.class.getName(), null ) //
            .setImplementation( new UserSettingsManagerImpl() ) //
            .add( createServiceDependency() //
                .setService( LogService.class ) //
                .setRequired( false ) //
            ) //
        );

    aManager.add( //
        createComponent() //
            .setInterface( ProjectManager.class.getName(), null ) //
            .setImplementation( new ProjectManagerImpl() ) //
            .add( createServiceDependency() //
                .setService( HostProperties.class ) //
                .setRequired( true ) //
            ) //
            .add( createServiceDependency() //
                .setService( LogService.class ) //
                .setRequired( false ) //
            ) //
        );

    props = new Properties();
    props.put( EventConstants.EVENT_TOPIC, new String[] { DataAcquisitionService.TOPIC_ACQUISITION_STATUS } );

    aManager.add( createComponent() //
        .setInterface( new String[] { DataAcquirer.class.getName(), EventHandler.class.getName() }, props ) //
        .setImplementation( DataAcquirerImpl.class ) //
        .add( createServiceDependency() //
            .setService( DataAcquisitionService.class ) //
            .setRequired( false ) ) //
        );

    // All the interfaces we're registering the client controller under...
    serviceNames = new String[] { EventHandler.class.getName(), AnnotationListener.class.getName(),
        ApplicationCallback.class.getName() };

    props = new Properties();
    props.put( EventConstants.EVENT_TOPIC, new String[] { Session.TOPIC_ACQUISITION_DATA_CHANGED,
        DataAcquisitionService.TOPIC_ANY } );

    // Client controller...
    aManager.add( createComponent() //
        .setInterface( serviceNames, props ) //
        .setImplementation( clientController ) //
        .add( createServiceDependency() //
            .setService( HostProperties.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( Session.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( ProjectManager.class ) //
            .setRequired( true ) //
            .setCallbacks( "setProjectManager", "removeProjectManager" ) ) //
        .add( createServiceDependency() //
            .setService( DataAcquirer.class ) //
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
            .setService( ToolInvoker.class ) //
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
