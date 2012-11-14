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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import static nl.lxtreme.ols.acquisition.service.DataAcquisitionService.*;
import static nl.lxtreme.ols.client.tool.ToolInvoker.*;
import static nl.lxtreme.ols.common.session.Session.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.about.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.componentprovider.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.host.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.event.Event;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides the main entry point for the client.
 * <p>
 * The client will function as a kind of bridge between the OSGi-world and
 * Swing-world ensuring that events from the OSGi-world are properly transferred
 * to the Swing-world and the other way around.
 * </p>
 * <p>
 * This class is implemented as <em>singleton</em> to allow other parts of the
 * Swing-UI to get hold of the various services without having to create a
 * massive tree of dependencies for all components.
 * </p>
 */
public class Client implements StatusListener, ApplicationCallback, EventHandler
{
  // CONSTANTS

  /** This will ensure there's only one instance in this classloader hierarchy. */
  private static final Client INSTANCE = new Client();

  // VARIABLES

  private final AcquisitionController acquisitionController;
  private final ActionManager actionManager;
  private final CursorController cursorController;
  private final DeviceController deviceController;
  private final ImportExportController importExportController;
  private final ProjectController projectController;
  private final ToolController toolController;
  private final SignalDiagramController signalDiagramController;

  // Injected by Felix DM...
  private volatile UIColorSchemeManager colorSchemeManager;
  private volatile MetaTypeService metaTypeService;
  private volatile ConfigurationAdmin configAdmin;
  private volatile HostProperties hostProperties;
  private volatile BundleContext bundleContext;
  private volatile Session session;
  private volatile LogService log;

  private volatile MainFrame mainFrame;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Client} instance.
   */
  private Client()
  {
    this.acquisitionController = new AcquisitionController();
    this.actionManager = new ActionManager();
    this.cursorController = new CursorController();
    this.deviceController = new DeviceController();
    this.importExportController = new ImportExportController();
    this.projectController = new ProjectController();
    this.toolController = new ToolController();
    this.signalDiagramController = new SignalDiagramController();
  }

  // METHODS

  /**
   * Returns the client instance.
   * 
   * @return an instance of this client, never <code>null</code>.
   */
  public static Client getInstance()
  {
    return INSTANCE;
  }

  /**
   * Adds the given component provider to this controller, and does this
   * synchronously on the EDT.
   * <p>
   * This method is called by the Dependency Manager.
   * </p>
   * 
   * @param aProvider
   *          the component provider, cannot be <code>null</code>.
   */
  public void addMenu( final ComponentProvider aProvider )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JMenuBar menuBar = getMainFrame().getJMenuBar();

        if ( menuBar != null )
        {
          final JMenu menu = ( JMenu )aProvider.getComponent();
          menuBar.add( menu );

          aProvider.addedToContainer();

          menuBar.revalidate();
          menuBar.repaint();
        }
      }
    } );
  }

  /**
   * @return the acquisition controller, never <code>null</code>.
   */
  public final AcquisitionController getAcquisitionController()
  {
    return this.acquisitionController;
  }

  /**
   * @return the action manager, never <code>null</code>.
   */
  public final ActionManager getActionManager()
  {
    return this.actionManager;
  }

  /**
   * @return the current (OSGi) bundle context, never <code>null</code>.
   */
  public final BundleContext getBundleContext()
  {
    return this.bundleContext;
  }

  /**
   * @return the cursor controller, never <code>null</code>.
   */
  public final CursorController getCursorController()
  {
    return this.cursorController;
  }

  /**
   * @return the device controller, never <code>null</code>.
   */
  public final DeviceController getDeviceController()
  {
    return this.deviceController;
  }

  /**
   * @return the current host properties, never <code>null</code>.
   */
  public final HostProperties getHostProperties()
  {
    return this.hostProperties;
  }

  /**
   * @return the import/export controller, never <code>null</code>.
   */
  public final ImportExportController getImportExportController()
  {
    return this.importExportController;
  }

  /**
   * Returns the current value of log.
   * 
   * @return the log service, never <code>null</code>.
   */
  public final LogService getLogService()
  {
    return this.log;
  }

  /**
   * Returns the current value of metaTypeService.
   * 
   * @return the metaTypeService
   */
  public MetaTypeService getMetaTypeService()
  {
    return this.metaTypeService;
  }

  /**
   * @return the current project controller, never <code>null</code>.
   */
  public final ProjectController getProjectController()
  {
    return this.projectController;
  }

  /**
   * @return the current session, never <code>null</code>.
   */
  public final Session getSession()
  {
    return this.session;
  }

  /**
   * @return the current signal diagram controller, never <code>null</code>.
   */
  public final SignalDiagramController getSignalDiagramController()
  {
    return this.signalDiagramController;
  }

  /**
   * @return the tool controller, never <code>null</code>.
   */
  public final ToolController getToolController()
  {
    return this.toolController;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean handleAbout()
  {
    final HostProperties hp = getHostProperties();

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        new AboutBox( hp.getShortName(), hp.getVersion() ).showDialog();
      }
    } );

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( final Event aEvent )
  {
    String topic = aEvent.getTopic();
    if ( TOPIC_ACQUISITION_DATA_CHANGED.equals( topic ) )
    {
      AcquisitionData newData = ( AcquisitionData )aEvent.getProperty( KEY_ACQUISITION_DATA );

      getSignalDiagramController().setAcquisitionData( newData );
      getActionManager().updateActionStates();
    }
    else if ( TOPIC_ANNOTATION_ADDED.equals( topic ) || TOPIC_ANNOTATION_CLEARED.equals( topic ) )
    {
      // Accumulate repaint events to avoid an avalanche of events on the
      // EDT...
      scheduleRepaint();
    }
    else if ( TOPIC_ACQUISITION_PROGRESS.equals( topic ) )
    {
      final Integer percentage = ( Integer )aEvent.getProperty( KEY_PROGRESS );

      scheduleProgressUpdate( percentage );
    }
    else if ( TOPIC_ACQUISITION_STATUS.equals( topic ) )
    {
      String status = ( String )aEvent.getProperty( KEY_STATUS );
      String device = ( String )aEvent.getProperty( KEY_DEVICE );
      Long startTime = ( Long )aEvent.getProperty( KEY_START_TIME );

      if ( STATUS_SUCCESS.equals( status ) )
      {
        long time = System.currentTimeMillis() - startTime.longValue();

        setStatus( "Acquisition from {2} finished at {0,date,medium} {0,time,medium}, and took {1}.", new Date(),
            UnitOfTime.format( time / 1.0e3 ), device );
      }
      else if ( STATUS_FAILED.equals( status ) )
      {
        Exception exception = ( Exception )aEvent.getProperty( KEY_EXCEPTION );

        setStatus( "Acquisition from {1} failed! Possible reason: {0}.", exception.getMessage(), device );
      }
      else if ( STATUS_CANCELLED.equals( status ) )
      {
        setStatus( "Acquisition cancelled for {0}.", device );
      }

      getActionManager().updateActionStates();
    }
    else if ( TOPIC_TOOL_PROGRESS.equals( topic ) )
    {
      Integer progress = ( Integer )aEvent.getProperty( KEY_TOOL_PROGRESS );

      scheduleProgressUpdate( progress );
    }
    else if ( TOPIC_TOOL_STATUS.equals( topic ) )
    {
      String tool = ( String )aEvent.getProperty( KEY_TOOL_NAME );
      String status = ( String )aEvent.getProperty( KEY_TOOL_STATE );
      Long startTime = ( Long )aEvent.getProperty( KEY_TOOL_START_TIME );

      if ( TOOL_STATUS_SUCCESS.equals( status ) )
      {
        long time = System.currentTimeMillis() - startTime.longValue();

        setStatus( "Tool {2} finished at {0,date,medium} {0,time,medium}, and took {1}.", new Date(),
            UnitOfTime.format( time / 1.0e3 ), tool );
      }
      else if ( TOOL_STATUS_FAILED.equals( status ) )
      {
        Exception exception = ( Exception )aEvent.getProperty( KEY_TOOL_EXCEPTION );

        setStatus( "Tool {1} failed! Possible reason: {0}.", exception.getMessage(), tool );
      }
      else if ( TOOL_STATUS_CANCELLED.equals( status ) )
      {
        setStatus( "Tool {1} was cancelled.", tool );
      }

      getActionManager().updateActionStates();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean handlePreferences()
  {
    showPreferencesDialog( getMainFrame() );
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean handleQuit()
  {
    exit();
    // On Mac OS, it appears that if we acknowledge this event, the system
    // shuts down our application for us, thereby not calling our stop/shutdown
    // hooks... By returning false, we're not acknowledging the quit action to
    // the system, but instead do it all on our own...
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean hasPreferences()
  {
    return true;
  }

  /**
   * Called by Felix DM when initializing this component.
   */
  public void init( final Component aComponent )
  {
    // The things done in this method need to be done as early as possible,
    // hence we're doing this in the init() method...
    initOSSpecifics();
  }

  /**
   * Convenience method to publish the current progress of the client.
   * 
   * @param aPercentage
   *          a percentage, >= 0 && <= 100.
   */
  public final void setProgress( final int aPercentage )
  {
    scheduleProgressUpdate( Integer.valueOf( aPercentage ) );
  }

  /**
   * Convenience method to publish the current status of the client.
   * 
   * @param aMessage
   *          the message to publish;
   * @param aMessageArgs
   *          the optional arguments.
   */
  public final void setStatus( final String aMessage, final Object... aMessageArgs )
  {
    final String message;
    if ( ( aMessageArgs != null ) && ( aMessageArgs.length > 0 ) )
    {
      message = MessageFormat.format( aMessage, aMessageArgs );
    }
    else
    {
      message = aMessage;
    }

    this.log.log( LogService.LOG_INFO, message );

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        getMainFrame().doSetStatus( message );
      }
    } );
  }

  /**
   * Removes the given component provider from this controller, and does this
   * synchronously on the EDT.
   * <p>
   * This method is called by the Dependency Manager.
   * </p>
   * 
   * @param aProvider
   *          the menu component provider, cannot be <code>null</code>.
   */
  public void removeMenu( final ComponentProvider aProvider )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JMenuBar menuBar = getMainFrame().getJMenuBar();
        if ( menuBar != null )
        {
          aProvider.removedFromContainer();

          menuBar.remove( aProvider.getComponent() );

          menuBar.revalidate();
          menuBar.repaint();
        }
      }
    } );
  }

  /**
   * Called by Felix DM when starting this component.
   */
  public void start( final Component aComponent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        startOnEDT();
      }
    } );
  }

  /**
   * Called by Felix DM when stopping this component.
   */
  public void stop( final Component aComponent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        stopOnEDT();
      }
    } );
  }

  /**
   * Starts the client from inside the EDT.
   */
  void startOnEDT()
  {
    // Construct the action manager...
    ActionManagerFactory.createActions( getActionManager() );

    // Construct the main frame...
    final MainFrame mf = new MainFrame( this.actionManager, this.deviceController, this.hostProperties );
    this.mainFrame = mf;

    mf.startOnEDT( this.bundleContext, this.signalDiagramController );
  }

  /**
   * Stops the client from inside the EDT.
   */
  void stopOnEDT()
  {
    MainFrame mf = this.mainFrame;
    if ( mf != null )
    {
      mf.stopOnEDT();

      this.mainFrame = null;
    }
  }

  /**
   * Terminates & shuts down the client.
   */
  private void exit()
  {
    // Close the main window ourselves; we should do this explicitly...
    MainFrame mf = this.mainFrame;
    if ( mf != null )
    {
      this.mainFrame = null;
      mf.close();
    }

    try
    {
      // Stop the framework bundle; which should stop all other bundles as
      // well; the STOP_TRANSIENT option ensures the bundle is restarted the
      // next time...
      this.bundleContext.getBundle( 0 ).stop( Bundle.STOP_TRANSIENT );
    }
    catch ( final IllegalStateException ex )
    {
      this.log.log( LogService.LOG_WARNING, "Bundle context no longer valid while shutting down client?!" );

      // The bundle context is no longer valid; we're going to exit anyway, so
      // lets ignore this exception for now...
      System.exit( -1 );
    }
    catch ( final BundleException be )
    {
      this.log.log( LogService.LOG_WARNING, "Bundle context no longer valid while shutting down client?!" );

      System.exit( -1 );
    }
  }

  /**
   * @return the current main frame, never <code>null</code>.
   */
  private MainFrame getMainFrame()
  {
    return this.mainFrame;
  }

  /**
   * Initializes the OS-specifics of the client.
   */
  private void initOSSpecifics()
  {
    if ( this.hostProperties.isMacOS() )
    {
      // Moves the main menu bar to the screen menu bar location...
      System.setProperty( "apple.laf.useScreenMenuBar", "true" );
      System.setProperty( "apple.awt.graphics.EnableQ2DX", "true" );
      System.setProperty( "com.apple.mrj.application.apple.menu.about.name", this.hostProperties.getShortName() );
      System.setProperty( "com.apple.mrj.application.growbox.intrudes", "false" );
      System.setProperty( "com.apple.mrj.application.live-resize", "false" );
      System.setProperty( "com.apple.macos.smallTabs", "true" );
      System.setProperty( "apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS" );

      // Install an additional accelerator (Cmd+W) for closing option panes...
      ActionMap map = ( ActionMap )UIManager.get( "OptionPane.actionMap" );
      if ( map == null )
      {
        map = new ActionMap();
        UIManager.put( "OptionPane.actionMap", map );
      }
      map.put( "close", new CloseOptionPaneAction() );

      UIManager.put( "OptionPane.windowBindings", //
          new Object[] { SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_W ), "close", "ESCAPE", "close" } );
    }
    else if ( this.hostProperties.isUnix() )
    {
      UIManager.put( "Application.useSystemFontSettings", Boolean.FALSE );
      setLookAndFeel( "com.jgoodies.looks.plastic.Plastic3DLookAndFeel" );
    }
    else if ( this.hostProperties.isWindows() )
    {
      UIManager.put( "Application.useSystemFontSettings", Boolean.TRUE );
      setLookAndFeel( "com.jgoodies.looks.plastic.PlasticXPLookAndFeel" );
    }

    // Use the defined email address...
    System.setProperty( JErrorDialog.PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS,
        this.hostProperties.getReportIncidentAddress() );

    if ( this.hostProperties.isDebugMode() )
    {
      // Install a custom repaint manager that detects whether Swing
      // components are created outside the EDT; if so, it will yield a
      // stack trace to the offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
      // Install a custom event queue to monitor whether we're not violating the
      // Swing semantics on offloading all major processing from the EDT...
      EventDispatchThreadHangMonitor.install();
    }

    // Cause exceptions to be shown in a more user-friendly way...
    JErrorDialog.installSwingExceptionHandler();
  }

  /**
   * Schedules a progress update.
   * 
   * @param aPercentage
   *          the percentage to set, cannot be <code>null</code>.
   */
  private void scheduleProgressUpdate( final Integer aPercentage )
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.scheduleProgressUpdate( aPercentage );
    }
  }

  /**
   * Schedules a repaint of the entire main frame.
   */
  private void scheduleRepaint()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.scheduleRepaint();
    }
  }

  /**
   * @param aLookAndFeelClass
   */
  private void setLookAndFeel( final String aLookAndFeelClassName )
  {
    final UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    // to make sure we always use system class loader
    defaults.put( "ClassLoader", Client.class.getClassLoader() );

    try
    {
      UIManager.setLookAndFeel( aLookAndFeelClassName );
    }
    catch ( Exception exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        System.err.println( "Failed to set look and feel to: " + aLookAndFeelClassName );
        setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
      }
    }
  }

  /**
   * Shows the global preferences dialog.
   * 
   * @param aParent
   *          the parent window of the dialog, can be <code>null</code>.
   */
  private void showPreferencesDialog( final Window aParent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        try
        {
          Configuration config = Client.this.configAdmin.getConfiguration( UIManagerConfigurator.PID );

          final PreferencesDialog dialog = new PreferencesDialog( aParent, Client.this.colorSchemeManager, config );
          if ( dialog.showDialog() )
          {
            // Ensure all UI-related changes are immediately visible...
            scheduleRepaint();
          }
        }
        catch ( IOException exception )
        {
          JErrorDialog.showDialog( aParent, "Failed to obtain preferences!", exception );
        }
      }
    } );
  }
}
