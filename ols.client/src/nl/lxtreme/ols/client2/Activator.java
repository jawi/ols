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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.client.api.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.colorscheme.*;
import nl.lxtreme.ols.client2.menu.*;
import nl.lxtreme.ols.client2.session.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.export.api.*;
import nl.lxtreme.ols.task.execution.*;
import nl.lxtreme.ols.tool.api.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.framework.Constants;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;

import com.jidesoft.utils.*;


/**
 * Entry point from the OSGi world to Swing.
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

    /**
     * {@inheritDoc}
     */
    @Override
    protected void configureComponent( Component aComponent )
    {
      DependencyManager dm = aComponent.getDependencyManager();
      aComponent.add( dm.createServiceDependency().setService( TaskExecutionService.class ).setRequired( true ) );
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
   * {@inheritDoc}
   */
  @Override
  public void destroy( BundleContext aContext, DependencyManager aManager ) throws Exception
  {
    // TODO Auto-generated method stub
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( BundleContext aContext, DependencyManager aManager ) throws Exception
  {
    // Do not start if we're running headless...
    if ( GraphicsEnvironment.isHeadless() )
    {
      throw new RuntimeException( "Cannot start client: running headless." );
    }

    // Verify the license for JIDE-docking; thanks to JIDE Software for
    // providing a free license...
    Lm.verifyLicense( "Jan Willem Janssen", "OLS client", "zGiivJPzfPBGyRP5g.0P7xl8:pdUEzR2" );

    createBundleAdapters( aManager );
    createActionManager( aManager );
    createMenuManager( aManager );
    createColorSchemeManager( aManager );

    // Helpers...
    aManager.add( createComponent().setInterface( //
        new String[] { SessionProvider.class.getName(), AcquisitionDataListener.class.getName() }, null ) //
        .setImplementation( SessionProviderImpl.class ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );

    // Client...
    String[] serviceNames = { AcquisitionStatusListener.class.getName(), AcquisitionProgressListener.class.getName() };
    aManager.add( createComponent() //
        .setInterface( serviceNames, null ) //
        .setImplementation( Client.class ) //
        .add( createServiceDependency() //
            .setService( SessionProvider.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( ColorSchemeProvider.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( DataAcquisitionService.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( ActionManager.class ) //
            .setRequired( true ) ) //
        .add( createServiceDependency() //
            .setService( MenuManager.class ) //
            .setRequired( true ) ) //
        );
  }

  private void createColorSchemeManager( DependencyManager aManager )
  {
    Properties props = new Properties();
    props.put( Constants.SERVICE_PID, ColorSchemeManager.PID );

    aManager.add( createComponent() //
        .setInterface( new String[] { ColorSchemeProvider.class.getName(), ManagedServiceFactory.class.getName() }, props ) //
        .setImplementation( ColorSchemeManager.class ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );
  }

  private void createActionManager( DependencyManager aManager )
  {
    aManager.add( createComponent() //
        .setInterface( ActionManager.class.getName(), null ) //
        .setImplementation( ActionManagerImpl.class ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
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
        );
  }

  private void createBundleAdapters( DependencyManager aManager )
  {
    aManager.add( createBundleAdapterService( Bundle.ACTIVE, CP_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ComponentProviderBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, TOOL_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ToolBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, DEVICE_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( DeviceBundleAdapter.class ) );

    aManager.add( createBundleAdapterService( Bundle.ACTIVE, EXPORTER_BUNDLE_FILTER, true /* propagate */) //
        .setImplementation( ExporterBundleAdapter.class ) );
  }

  private void createMenuManager( DependencyManager aManager )
  {
    aManager.add( createComponent() //
        .setInterface( MenuManager.class.getName(), null ) //
        .setImplementation( MenuManagerImpl.class ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        .add( createServiceDependency() //
            .setService( ComponentProvider.class, "(OLS-ComponentProvider=Menu)" ) //
            .setCallbacks( "addMenu", "removeMenu" ) //
            .setRequired( false ) ) //
        .add( createServiceDependency() //
            .setService( ManagedAction.class, "(menuName=*)" ) //
            .setCallbacks( "addAction", "removeAction" ) //
            .setRequired( false ) ) //
        );
  }
}
