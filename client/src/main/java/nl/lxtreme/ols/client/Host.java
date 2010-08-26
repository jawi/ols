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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.signal.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.HostUtils.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * Denotes the client host.
 */
public final class Host implements ApplicationCallback
{
  // INNER TYPES

  /**
   * Denotes the main UI.
   */
  public static final class MainFrame extends JFrame implements ActionProvider, CaptureCallback, AnalysisCallback
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final IActionManager actionManager;
    private final DiagramScrollPane diagramScrollPane;
    private final JTextStatusBar status;

    private JMenu deviceMenu;
    private JMenu toolsMenu;
    private JMenu windowMenu;

    private volatile DeviceController currentDevCtrl;

    // CONSTRUCTORS

    /**
     * Creates a new MainFrame instance.
     * 
     * @param aCaption
     *          the caption of this frame.
     */
    public MainFrame( final Host aHost )
    {
      super( FULL_NAME );

      this.actionManager = new ActionManager();

      this.diagramScrollPane = new DiagramScrollPane( this );
      this.status = new JTextStatusBar();

      this.actionManager.add( new NewProjectAction( aHost.project ) );
      this.actionManager.add( new OpenProjectAction( aHost.project ) );
      this.actionManager.add( new SaveProjectAction( aHost.project ) );
      this.actionManager.add( new OpenDataFileAction( this.diagramScrollPane ) );
      this.actionManager.add( new SaveDataFileAction( this.diagramScrollPane ) );
      this.actionManager.add( new ExitAction( aHost ) );

      this.actionManager.add( new CaptureAction( this ) );
      this.actionManager.add( new RepeatCaptureAction( this ) );

      this.actionManager.add( new ZoomInAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new ZoomOutAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new ZoomDefaultAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new ZoomFitAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new GotoTriggerAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new GotoCursor1Action( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new GotoCursor2Action( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new SetCursorModeAction( this.diagramScrollPane ) ).setEnabled( false );
      this.actionManager.add( new ShowDiagramSettingsAction( this.diagramScrollPane ) );
      this.actionManager.add( new ShowDiagramLabelsAction( this.diagramScrollPane ) );

      setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE );
      setSize( 1200, 600 );

      final Container contentPane = getContentPane();
      contentPane.setLayout( new BorderLayout() );
      contentPane.add( this.diagramScrollPane, BorderLayout.CENTER );

      final JToolBar tools = createMenuBars();
      contentPane.add( tools, BorderLayout.PAGE_START );

      contentPane.add( this.status, BorderLayout.PAGE_END );

      // Install a global window state listener...
      Toolkit.getDefaultToolkit().addAWTEventListener( new AWTWindowTracker( this.windowMenu ),
          AWTEvent.WINDOW_EVENT_MASK );
    }

    // METHODS

    /**
     * @see nl.lxtreme.ols.api.tools.AnalysisCallback#analysisAborted(java.lang.String)
     */
    @Override
    public void analysisAborted( final String aReason )
    {
      setStatus( "Analysis aborted! " + aReason );
    }

    /**
     * @see nl.lxtreme.ols.api.tools.AnalysisCallback#analysisComplete(nl.lxtreme.ols.api.data.CapturedData)
     */
    @Override
    public void analysisComplete( final CapturedData aNewCapturedData )
    {
      setCapturedData( aNewCapturedData, true /* aEnable */);
    }

    /**
     * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureAborted(java.lang.String)
     */
    @Override
    public void captureAborted( final String aReason )
    {
      setStatus( "Capture aborted! " + aReason );
    }

    /**
     * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureComplete(nl.lxtreme.ols.api.data.CapturedData)
     */
    @Override
    public void captureComplete( final CapturedData aCapturedData )
    {
      final boolean actionsEnabled = aCapturedData != null;
      setCapturedData( aCapturedData, actionsEnabled );

      updateActions();
    }

    /**
     * @see nl.lxtreme.ols.client.ActionProvider#getAction(java.lang.String)
     */
    public Action getAction( final String aID )
    {
      return this.actionManager.getAction( aID );
    }

    /**
     * Returns the current annotated data.
     * 
     * @return the annotated data, never <code>null</code>.
     */
    public AnnotatedData getAnnotatedData()
    {
      return this.diagramScrollPane.getAnnotatedData();
    }

    /**
     * Returns the current device controller.
     * 
     * @return the current device controller, can be <code>null</code>.
     */
    public DeviceController getCurrentDeviceController()
    {
      return this.currentDevCtrl;
    }

    /**
     * Sets the current device controller to the given value.
     * 
     * @param aCurrentDevCtrl
     *          the device controller to set, cannot be <code>null</code>.
     */
    public void setCurrentDeviceController( final DeviceController aCurrentDevCtrl )
    {
      if ( LOG.isLoggable( Level.INFO ) )
      {
        final String name = ( aCurrentDevCtrl == null ) ? "no device" : aCurrentDevCtrl.getName();
        LOG.log( Level.INFO, "Setting current device controller to: \"{0}\" ...", name );
      }

      this.currentDevCtrl = aCurrentDevCtrl;

      updateActions();
    }

    /**
     * Shows the main about box.
     * 
     * @param aVersion
     *          the version to display in this about box.
     */
    public void showAboutBox( final String aVersion )
    {
      final String message = FULL_NAME + "\n\n" //
          + "Copyright 2006-2010 Michael Poppitz\n" //
          + "Copyright 2010 J.W. Janssen\n\n" //
          + "This software is released under the GNU GPL.\n\n" //
          + "Version: {0}\n\n" //
          + "For more information see:\n" //
          + "  <http://www.lxtreme.nl/ols/>\n" //
          + "  <http://dangerousprototypes.com/open-logic-sniffer/>\n" //
          + "  <http://www.gadgetfactory.net/gf/project/butterflylogic/>\n" //
          + "  <http://www.sump.org/projects/analyzer/>";

      final JOptionPane aboutDialogFactory = new JOptionPane( String.format( message, aVersion ), //
          JOptionPane.INFORMATION_MESSAGE, JOptionPane.DEFAULT_OPTION );

      final JDialog aboutDialog = aboutDialogFactory.createDialog( this, "About ..." );
      aboutDialog.setVisible( true );
    }

    /**
     * @see nl.lxtreme.ols.api.ProgressCallback#updateProgress(int)
     */
    public void updateProgress( final int aPercentage )
    {
      this.status.setProgress( aPercentage );
    }

    /**
     * Creates the menu bar with all menu's and the accompanying toolbar.
     * 
     * @return the toolbar, never <code>null</code>.
     */
    private JToolBar createMenuBars()
    {
      final JMenuBar bar = new JMenuBar();
      setJMenuBar( bar );

      final JMenu file = new JMenu( "File" );
      bar.add( file );

      file.add( getAction( NewProjectAction.ID ) );
      file.add( getAction( OpenProjectAction.ID ) );
      file.add( getAction( SaveProjectAction.ID ) );
      file.addSeparator();
      file.add( getAction( OpenDataFileAction.ID ) );
      file.add( getAction( SaveDataFileAction.ID ) );

      if ( HostUtils.needsExitMenuItem() )
      {
        file.add( new JSeparator() );
        file.add( getAction( ExitAction.ID ) );
      }

      this.deviceMenu = bar.add( new JMenu( "Device" ) );
      this.toolsMenu = bar.add( new JMenu( "Tools" ) );

      final JMenu diagramMenu = bar.add( new JMenu( "Diagram" ) );

      diagramMenu.add( getAction( ZoomInAction.ID ) );
      diagramMenu.add( getAction( ZoomOutAction.ID ) );
      diagramMenu.add( getAction( ZoomDefaultAction.ID ) );
      diagramMenu.add( getAction( ZoomFitAction.ID ) );
      diagramMenu.addSeparator();
      diagramMenu.add( getAction( GotoTriggerAction.ID ) );
      diagramMenu.add( getAction( GotoCursor1Action.ID ) );
      diagramMenu.add( getAction( GotoCursor2Action.ID ) );
      diagramMenu.addSeparator();
      diagramMenu.add( new JCheckBoxMenuItem( getAction( SetCursorModeAction.ID ) ) );
      diagramMenu.add( getAction( ShowDiagramSettingsAction.ID ) );
      diagramMenu.add( getAction( ShowDiagramLabelsAction.ID ) );

      this.windowMenu = bar.add( new JMenu( "Window" ) );
      for ( Window window : Window.getWindows() )
      {
        this.windowMenu.add( new JMenuItem( new FocusWindowAction( window ) ) );
      }

      final JToolBar toolbar = new JToolBar();
      toolbar.setRollover( true );

      toolbar.add( getAction( OpenDataFileAction.ID ) );
      toolbar.add( getAction( SaveDataFileAction.ID ) );
      toolbar.addSeparator();

      toolbar.add( getAction( CaptureAction.ID ) );
      toolbar.add( getAction( RepeatCaptureAction.ID ) );
      toolbar.addSeparator();

      toolbar.add( getAction( ZoomInAction.ID ) );
      toolbar.add( getAction( ZoomOutAction.ID ) );
      toolbar.add( getAction( ZoomDefaultAction.ID ) );
      toolbar.add( getAction( ZoomFitAction.ID ) );
      toolbar.addSeparator();

      toolbar.add( getAction( GotoTriggerAction.ID ) );
      toolbar.add( getAction( GotoCursor1Action.ID ) );
      toolbar.add( getAction( GotoCursor2Action.ID ) );
      // toolbar.addSeparator();

      return toolbar;
    }

    /**
     * @param aCapturedData
     * @param aEnableActions
     */
    private void setCapturedData( final CapturedData aCapturedData, final boolean aEnableActions )
    {
      getAction( ZoomInAction.ID ).setEnabled( aEnableActions );
      getAction( ZoomOutAction.ID ).setEnabled( aEnableActions );
      getAction( ZoomDefaultAction.ID ).setEnabled( aEnableActions );
      getAction( ZoomFitAction.ID ).setEnabled( aEnableActions );
      getAction( GotoTriggerAction.ID ).setEnabled(
          aEnableActions && ( ( aCapturedData != null ) && aCapturedData.hasTriggerData() ) );
      getAction( GotoCursor1Action.ID ).setEnabled( aEnableActions );
      getAction( GotoCursor2Action.ID ).setEnabled( aEnableActions );
      getAction( SetCursorModeAction.ID ).setEnabled( aEnableActions );

      if ( aCapturedData != null )
      {
        this.diagramScrollPane.setCapturedData( aCapturedData );
        this.diagramScrollPane.zoomToFit();
      }
      else
      {
        this.diagramScrollPane.repaint();
      }

      setStatus( "" );
    }

    /**
     * @param aText
     */
    private void setStatus( final String aText )
    {
      this.status.showProgressBar( false );
      this.status.setText( aText );
    }

    /**
     * Synchronizes the state of the actions to the current state of this host.
     */
    private void updateActions()
    {
      final DeviceController currentDeviceController = getCurrentDeviceController();
      final boolean deviceControllerSet = currentDeviceController != null;

      getAction( CaptureAction.ID ).setEnabled( deviceControllerSet );
      getAction( RepeatCaptureAction.ID ).setEnabled( deviceControllerSet && currentDeviceController.isSetup() );
    }
  }

  /**
   * Tracks the number of open/closed AWT windows.
   */
  static final class AWTWindowTracker implements AWTEventListener
  {
    private final JMenu windowMenu;

    /**
     * @param aWindowMenu
     */
    public AWTWindowTracker( final JMenu aWindowMenu )
    {
      this.windowMenu = aWindowMenu;
    }

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final int id = aEvent.getID();
      if ( ( id == WindowEvent.WINDOW_ACTIVATED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component component = event.getComponent();

        System.out.println( "Window opened: " + component );
      }
      else if ( ( id == WindowEvent.WINDOW_CLOSED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component component = event.getComponent();

        System.out.println( "Window closed: " + component );
      }
    }

  }

  /**
   * Defines a (global) AWT event listener for storing/retrieving the window
   * state.
   */
  static final class WindowStateListener implements AWTEventListener
  {
    // CONSTANTS

    private static final Logger LOG = Logger.getLogger( WindowStateListener.class.getName() );

    // VARIABLES

    private final Properties properties;

    // CONSTRUCTORS

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aProperties
     *          the properties to pass to the individual opened windows.
     */
    public WindowStateListener( final Properties aProperties )
    {
      this.properties = aProperties;
    }

    // METHODS

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final int id = aEvent.getID();
      if ( ( id == WindowEvent.WINDOW_OPENED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component component = event.getComponent();

        try
        {
          final String namespace = component.getClass().getName();
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Reading dialog-specific properties for '{0}' ...", namespace );
            ( ( Configurable )component ).readProperties( namespace, this.properties );
          }

          if ( component instanceof Window )
          {
            LOG.log( Level.FINE, "Reading window-properties for '{0}' ...", namespace );
            SwingComponentUtils.loadWindowState( namespace, this.properties, ( Window )component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Reading dialog properties failed!", exception );
        }
      }
      else if ( ( id == WindowEvent.WINDOW_CLOSED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component component = event.getComponent();

        try
        {
          final String namespace = component.getClass().getName();
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Writing dialog-specific properties for '{0}' ...", namespace );
            ( ( Configurable )component ).writeProperties( namespace, this.properties );
          }

          if ( component instanceof Window )
          {
            LOG.log( Level.FINE, "Writing window-properties for '{0}' ...", namespace );
            SwingComponentUtils.saveWindowState( namespace, this.properties, ( Window )component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Writing dialog properties failed!", exception );
        }
      }
    }
  }

  // CONSTANT

  private static final Logger LOG = Logger.getLogger( Host.class.getName() );

  private static final String PROPERTIES_NAME = "nl.lxtreme.ols.client";
  private static final String SHORT_NAME = "LogicSniffer";
  private static final String FULL_NAME = SHORT_NAME.concat( " - Logic Analyzer Client" );

  // VARIABLES

  private final BundleContext context;
  private final Project project;

  private MainFrame mainFrame;
  private MenuTracker menuTracker;
  private DeviceControllerTracker deviceControllerTracker;
  private ToolTracker toolTracker;
  private Properties userProperties;

  // CONSTRUCTORS

  /**
   * Creates a new Host instance.
   * 
   * @param aBundleContext
   *          the bundle context to use.
   */
  public Host( final BundleContext aBundleContext )
  {
    if ( aBundleContext == null )
    {
      throw new IllegalArgumentException( "Bundle context cannot be null!" );
    }
    this.context = aBundleContext;

    this.project = new Project();
  }

  // METHODS

  /**
   * Returns a symbolic name for this host.
   * 
   * @return a symbolic name, never <code>null</code>.
   */
  public static final String getShortName()
  {
    return SHORT_NAME;
  }

  /**
   * Exits this host by stopping the framework bundle.
   */
  public void exit()
  {
    try
    {
      // Stop the framework bundle; which should stop all other bundles as
      // well...
      this.context.getBundle( 0 ).stop();
    }
    catch ( BundleException be )
    {
      System.exit( -1 );
    }
  }

  /**
   * @return
   */
  public AnnotatedData getAnnotatedData()
  {
    return this.mainFrame.getAnnotatedData();
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleAbout()
   */
  @Override
  public boolean handleAbout()
  {
    this.mainFrame.showAboutBox( "0.8.3" );
    return true;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handlePreferences()
   */
  @Override
  public boolean handlePreferences()
  {
    return false;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleQuit()
   */
  @Override
  public boolean handleQuit()
  {
    exit();
    return true;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#hasPreferences()
   */
  @Override
  public boolean hasPreferences()
  {
    return false;
  }

  /**
   * Initializes this host application. May <em>not</em> be called from outside
   * the EDT.
   */
  public void initialize()
  {
    // Read back the user preferences from last time...
    this.userProperties = HostUtils.readProperties( PROPERTIES_NAME );
    if ( this.userProperties == null )
    {
      this.userProperties = new Properties();
    }

    if ( Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) ) )
    {
      // Install a custom repaint manager that detects whether Swing components
      // are created outside the EDT; if so, it will yield a stack trace to the
      // offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    // Install a global window state listener...
    Toolkit.getDefaultToolkit().addAWTEventListener( new WindowStateListener( this.userProperties ),
        AWTEvent.WINDOW_EVENT_MASK );

    this.mainFrame = new MainFrame( this );

    this.menuTracker = new MenuTracker( this.context, this.mainFrame.getJMenuBar() );
    this.deviceControllerTracker = new DeviceControllerTracker( this.context, this.mainFrame, this.mainFrame.deviceMenu );
    this.toolTracker = new ToolTracker( this.context, this.mainFrame, this.mainFrame.toolsMenu );
  }

  /**
   * Shutdown hook, called after {@link #stop()} is called and can be used to
   * write down preferences and such. This method may <em>not</em> be called
   * from outside the EDT.
   */
  public void shutdown()
  {
    // Write back the user properties for use next time...
    HostUtils.writeProperties( PROPERTIES_NAME, this.userProperties );
  }

  /**
   * Starts this host by making the main frame visible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void start()
  {
    this.deviceControllerTracker.open();
    this.toolTracker.open();
    this.menuTracker.open();

    this.mainFrame.setVisible( true );
  }

  /**
   * Stops this host by making the main frame invisible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void stop()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.dispose();
      this.mainFrame = null;
    }

    this.deviceControllerTracker.close();
    this.toolTracker.close();
    this.menuTracker.close();
  }
}

/* EOF */
