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
  private static final String OLS_COMPONENT_PROVIDER_MAGIC_VALUE = "Menu";
  private static final String OLS_COMPONENT_PROVIDER_CLASS_KEY = "OLS-ComponentProviderClass";

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
  @SuppressWarnings( "rawtypes" )
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    final ClientController clientController = new ClientController( aContext );

    String filter;
    filter = String.format( "(&(%s=%s)(%s=*))", OLS_COMPONENT_PROVIDER_MAGIC_KEY, OLS_COMPONENT_PROVIDER_MAGIC_VALUE,
        OLS_COMPONENT_PROVIDER_CLASS_KEY );
    aManager.add( //
        createBundleAdapterService( Bundle.ACTIVE, filter, true /* propagate */) //
            .setImplementation(
                new GenericBundleAdapter<ComponentProvider>( ComponentProvider.class, OLS_COMPONENT_PROVIDER_CLASS_KEY ) ) //
        );

    filter = String.format( "(&(%s=%s)(%s=*))", OLS_TOOL_MAGIC_KEY, OLS_TOOL_MAGIC_VALUE, OLS_TOOL_CLASS_KEY );
    aManager.add( //
        createBundleAdapterService( Bundle.ACTIVE, filter, true /* propagate */) //
            .setImplementation( new GenericBundleAdapter<Tool>( Tool.class, OLS_TOOL_CLASS_KEY ) ) //
        );

    filter = String.format( "(&(%s=%s)(%s=*))", OLS_DEVICE_MAGIC_KEY, OLS_DEVICE_MAGIC_VALUE, OLS_DEVICE_CLASS_KEY );
    aManager.add( //
        createBundleAdapterService( Bundle.ACTIVE, filter, true /* propagate */) //
            .setImplementation( new GenericBundleAdapter<Device>( Device.class, OLS_DEVICE_CLASS_KEY ) ) //
        );

    filter = String.format( "(&(%s=%s)(%s=*))", OLS_EXPORTER_MAGIC_KEY, OLS_EXPORTER_MAGIC_VALUE,
        OLS_EXPORTER_CLASS_KEY );
    aManager.add( //
        createBundleAdapterService( Bundle.ACTIVE, filter, true /* propagate */) //
            .setImplementation( new GenericBundleAdapter<Exporter>( Exporter.class, OLS_EXPORTER_CLASS_KEY ) ) //
        );

    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, UIManagerConfigurator.PID );

    // UI Manager Configuration...
    aManager.add( //
        createComponent() //
            .setInterface( new String[] { UIManagerConfigurator.class.getName(), ManagedService.class.getName() },
                props ) //
            .setImplementation( UIManagerConfigurator.class ) //
        );

    props.put( Constants.SERVICE_PID, UIColorSchemeManager.PID );

    // UI Manager Configuration...
    aManager.add( //
        createComponent() //
            .setInterface(
                new String[] { UIColorSchemeManager.class.getName(), ManagedServiceFactory.class.getName() }, props ) //
            .setImplementation( UIColorSchemeManager.class ) );

    // User session manager...
    aManager.add( //
        createComponent() //
            .setImplementation( new UserSessionManager() ) //
            .add( createServiceDependency() //
                .setService( ProjectManager.class ) //
                .setRequired( true ) ) //
            .add( createServiceDependency() //
                .setService( UserSettingsManager.class ) //
                .setRequired( true ) ) //
            .add( createServiceDependency() //
                .setService( PreferencesService.class ) //
                .setRequired( true ) //
            ).add( createServiceDependency() //
                .setService( LogService.class ) //
                .setRequired( false ) //
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
                .setService( UIColorSchemeManager.class ) //
                .setRequired( true ) //
            ) //
            .add( createServiceDependency() //
                .setService( ComponentProvider.class, "(OLS-ComponentProvider=Menu)" ) //
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
            .add( createConfigurationDependency() //
                .setPid( UIManagerConfigurator.PID ) ) //
        );
  }
}

/* EOF */
