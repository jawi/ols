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


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.data.project.*;
import nl.lxtreme.ols.client.data.settings.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.osgi.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;


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

  /** The name of the implicit user settings properties file name. */
  private static final String IMPLICIT_USER_SETTING_NAME_PREFIX = "nl.lxtreme.ols.client";
  private static final String IMPLICIT_USER_SETTING_NAME_SUFFIX = "settings";

  private static final Logger LOG = Logger.getLogger( Activator.class.getName() );

  // VARIABLES

  private ProjectManager projectManager;

  private BundleWatcher bundleWatcher;

  private LogReaderTracker logReaderTracker;
  private ComponentProviderTracker menuTracker;
  private PreferenceServiceTracker preferencesServiceTracker;
  private SimpleDataAcquisitionService dataAcquisitionServiceTracker;
  private ClientController clientController;

  // METHODS

  /**
   * Creates the bundle observer for component providers.
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
    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        final MainFrame mainFrame = Activator.this.clientController.getMainFrame();
        if ( mainFrame != null )
        {
          // Safety guard: also loop through all unclosed frames and close them
          // as well...
          final Window[] openWindows = Window.getWindows();
          for ( Window window : openWindows )
          {
            LOG.log( Level.FINE, "(Forced) closing window {0} ...", window );

            window.setVisible( false );
            window.dispose();
          }

          Activator.this.clientController.setMainFrame( null );
        }

        JErrorDialog.uninstallSwingExceptionHandler();

        LOG.info( "Client stopped ..." );
      }
    } );

    // Store the implicit user settings...
    saveImplicitUserSettings( this.projectManager );

    this.preferencesServiceTracker.close();
    this.dataAcquisitionServiceTracker.close();
    this.menuTracker.close();
    this.bundleWatcher.stop();
    this.logReaderTracker.close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    this.projectManager = new SimpleProjectManager();
    // Restore the implicit user settings...
    loadImplicitUserSettings( this.projectManager );

    this.dataAcquisitionServiceTracker = new SimpleDataAcquisitionService( aContext );
    this.dataAcquisitionServiceTracker.open();

    this.clientController = new ClientController( aContext );
    this.clientController.setProjectManager( this.projectManager );
    this.clientController.setDataAcquisitionService( this.dataAcquisitionServiceTracker );

    this.preferencesServiceTracker = new PreferenceServiceTracker( aContext, this.projectManager );
    this.preferencesServiceTracker.open();

    this.menuTracker = new ComponentProviderTracker( aContext, this.clientController );
    this.menuTracker.open( true /* trackAllServices */);

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

    aManager.add( //
        createComponent() //
            .setInterface( ProjectManager.class.getName(), null ) //
            .setImplementation( this.projectManager ) //
            .add( createServiceDependency() //
                .setService( HostProperties.class ) //
                .setRequired( true ) //
            ) //
        );

    // All the interfaces we're registering the client controller under...
    final String[] interfaceNames = new String[] { AcquisitionDataListener.class.getName(),
        AcquisitionProgressListener.class.getName(), AcquisitionStatusListener.class.getName(),
        AnnotationListener.class.getName(), ApplicationCallback.class.getName() };

    aManager.add( //
        createComponent() //
            .setInterface( interfaceNames, null ) //
            .setImplementation( this.clientController ) //
            .add( createServiceDependency() //
                .setService( HostProperties.class ) //
                .setRequired( true ) //
            ) //
        );

    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        // Cause exceptions to be shown in a more user-friendly way...
        JErrorDialog.installSwingExceptionHandler();

        final MainFrame mainFrame = MainFrame.createMainFrame( Activator.this.clientController );
        mainFrame.setVisible( true );

        LOG.info( "Client started ..." );
      }
    } );
  }

  /**
   * Loads the implicit user settings for the given project manager.
   * 
   * @param aProjectManager
   *          the project manager to load the implicit user settings for, cannot
   *          be <code>null</code>.
   */
  private void loadImplicitUserSettings( final ProjectManager aProjectManager )
  {
    final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME_PREFIX,
        IMPLICIT_USER_SETTING_NAME_SUFFIX );
    final Project currentProject = aProjectManager.getCurrentProject();
    try
    {
      UserSettingsManager.loadUserSettings( userSettingsFile, currentProject );
    }
    finally
    {
      currentProject.setChanged( false );
    }
  }

  /**
   * Saves the implicit user settings for the given project manager.
   * 
   * @param aProjectManager
   *          the project manager to save the implicit user settings for, cannot
   *          be <code>null</code>.
   */
  private void saveImplicitUserSettings( final ProjectManager aProjectManager )
  {
    final Project currentProject = aProjectManager.getCurrentProject();
    if ( currentProject.isChanged() )
    {
      final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME_PREFIX,
          IMPLICIT_USER_SETTING_NAME_SUFFIX );
      try
      {
        UserSettingsManager.saveUserSettings( userSettingsFile, currentProject );
      }
      finally
      {
        currentProject.setChanged( false );
      }
    }
  }
}

/* EOF */
