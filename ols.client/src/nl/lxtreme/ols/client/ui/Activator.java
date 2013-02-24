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
package nl.lxtreme.ols.client.ui;


import java.util.*;

import nl.lxtreme.ols.client.componentprovider.*;
import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.client.ui.tool.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.export.*;
import nl.lxtreme.ols.util.swing.WindowManager.WindowStateManager;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides the client bundle activator, which is responsible for starting the
 * entire client UI.
 */
public class Activator extends DependencyActivatorBase
{
  // INNER TYPES

  static class ComponentProviderBundleAdapter extends AbstractBundleAdapter<ComponentProvider>
  {
    // CONSTANTS

    private static final String OLS_COMPONENT_PROVIDER_MAGIC_KEY = "OLS-ComponentProvider";
    private static final String OLS_COMPONENT_PROVIDER_MAGIC_VALUE = "Menu";
    private static final String OLS_COMPONENT_PROVIDER_CLASS_KEY = "OLS-ComponentProviderClass";

    public static final String FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_COMPONENT_PROVIDER_MAGIC_KEY,
        OLS_COMPONENT_PROVIDER_MAGIC_VALUE, OLS_COMPONENT_PROVIDER_CLASS_KEY );

    // CONSTRUCTORS

    /**
     * Creates a new Activator.ComponentProviderBundleAdapter instance.
     */
    public ComponentProviderBundleAdapter()
    {
      super( ComponentProvider.class, OLS_COMPONENT_PROVIDER_CLASS_KEY );
    }
  }

  static class ExporterBundleAdapter extends AbstractBundleAdapter<Exporter>
  {
    // CONSTANTS

    private static final String OLS_EXPORTER_MAGIC_KEY = "OLS-Exporter";
    private static final String OLS_EXPORTER_MAGIC_VALUE = "1.0";
    private static final String OLS_EXPORTER_CLASS_KEY = "OLS-ExporterClass";

    public static final String FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_EXPORTER_MAGIC_KEY,
        OLS_EXPORTER_MAGIC_VALUE, OLS_EXPORTER_CLASS_KEY );

    // CONSTRUCTORS

    /**
     * Creates a new Activator.ExporterBundleAdapter instance.
     */
    public ExporterBundleAdapter()
    {
      super( Exporter.class, OLS_EXPORTER_CLASS_KEY );
    }
  }

  // CONSTANTS

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
    com.jidesoft.utils.Lm.clearLicense();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    com.jidesoft.utils.Lm.verifyLicense( "Jan Willem Janssen", "OLS client", "zGiivJPzfPBGyRP5g.0P7xl8:pdUEzR2" );

    registerBundleAdapters( aManager );

    registerUIManagerConfigurator( aManager );
    registerUIColorSchemeManager( aManager );

    registerClient( aManager );
  }

  /**
   * Registers the various bundle adapters.
   */
  private void registerBundleAdapters( final DependencyManager aManager )
  {
    int mask = Bundle.ACTIVE;

    aManager.add( createBundleAdapterService( mask, ToolBundleAdapter.FILTER, true ) //
        .setImplementation( ToolBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );

    aManager.add( createBundleAdapterService( mask, DeviceBundleAdapter.FILTER, true ) //
        .setImplementation( DeviceBundleAdapter.class ) //
        .add( createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        );

    aManager.add( createBundleAdapterService( mask, ExporterBundleAdapter.FILTER, true ) //
        .setImplementation( ExporterBundleAdapter.class ) //
        );

    aManager.add( createBundleAdapterService( mask, ComponentProviderBundleAdapter.FILTER, true ) //
        .setImplementation( ComponentProviderBundleAdapter.class ) //
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
    final String[] interfaces = new String[] { EventHandler.class.getName(), StatusListener.class.getName(), ManagedService.class.getName() };

    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, ClientConfig.class.getName() );
    props.put( EventConstants.EVENT_TOPIC, new String[] { Session.TOPIC_ANY, AcquisitionController.TOPIC_ANY, ToolInvoker.TOPIC_ANY } );

    aManager.add( createComponent()
        .setImplementation( client )
        .setInterface( interfaces, props )
        .setComposition( "getComposition" )
        .add( createServiceDependency().setService( ComponentProvider.class, "(OLS-ComponentProvider=Menu)" ).setCallbacks( "addMenu", "removeMenu" ).setRequired( false ) )
        .add( createServiceDependency().setService( Exporter.class ).setCallbacks( client.getImportExportController(), "addExporter", "removeExporter" ).setRequired( false ) )
        .add( createServiceDependency().setService( AcquisitionDevice.class ).setCallbacks( client.getDeviceController(), "addDevice", "removeDevice" ).setRequired( false ) )
        .add( createServiceDependency().setService( ToolInvoker.class ).setCallbacks( client.getToolController(), "addTool", "removeTool" ).setRequired( false ) )
        .add( createServiceDependency().setService( UIManagerConfigurator.class ).setRequired( true ) )
        .add( createServiceDependency().setService( ColorSchemeManager.class ).setRequired( true ) )
        .add( createServiceDependency().setService( ConfigurationAdmin.class ).setRequired( true ) )
        .add( createServiceDependency().setService( WindowStateManager.class ).setRequired( true ) )
        .add( createServiceDependency().setService( MetaTypeService.class ).setRequired( true ) )
        .add( createServiceDependency().setService( StatusListener.class ).setRequired( false ) )
        .add( createServiceDependency().setService( LogService.class ).setRequired( false ) )
        .add( createServiceDependency().setService( EventAdmin.class ).setRequired( true ) )
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
}

/* EOF */
