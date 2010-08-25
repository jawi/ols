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
 * Denotes the UI-host.
 */
public final class Host implements ActionProvider, CaptureCallback, AnalysisCallback, ApplicationCallback
{
  // INNER TYPES

  /**
   * Defines a (global) AWT event listener for storing/retrieving the window
   * state.
   */
  static final class FrameStateListener implements AWTEventListener
  {
    private final Properties properties;

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aProperties
     *          the properties to pass to the individual opened windows.
     */
    public FrameStateListener( final Properties aProperties )
    {
      this.properties = aProperties;
    }

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final int id = aEvent.getID();
      if ( aEvent instanceof ComponentEvent )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component component = event.getComponent();

        // TODO this is a mere hack!
        Configurable configurable = null;
        if ( component instanceof Configurable )
        {
          configurable = ( Configurable )component;
        }
        else if ( component instanceof JDialog )
        {
          JDialog window = ( JDialog )component;
          if ( window.getContentPane() instanceof Configurable )
          {
            configurable = ( Configurable )window.getContentPane();
          }
        }
        else if ( component instanceof JFrame )
        {
          JFrame window = ( JFrame )component;
          if ( window.getContentPane() instanceof Configurable )
          {
            configurable = ( Configurable )window.getContentPane();
          }
        }

        if ( configurable != null )
        {
          final String namespace = configurable.getClass().getName();
          if ( id == WindowEvent.WINDOW_OPENED )
          {
            try
            {
              System.out.println( "Reading properties for " + namespace );
              configurable.readProperties( namespace, this.properties );
            }
            catch ( Exception exception )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, "Reading dialog properties failed!",
                  exception );
            }

            if ( component instanceof Window )
            {
              SwingComponentUtils.loadWindowState( namespace, this.properties, ( Window )component );
            }
          }
          else if ( id == WindowEvent.WINDOW_CLOSED )
          {
            try
            {
              System.out.println( "Writing properties for " + namespace );
              configurable.writeProperties( namespace, this.properties );
            }
            catch ( Exception exception )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, "Writing dialog properties failed!",
                  exception );
            }

            if ( component instanceof Window )
            {
              SwingComponentUtils.saveWindowState( namespace, this.properties, ( Window )component );
            }
          }
        }
      }
    }
  }

  // CONSTANT

  private static final Logger LOG = Logger.getLogger( Host.class.getName() );

  private static final String PROPERTIES_NAME = "nl.lxtreme.ols.client";

  // VARIABLES

  private final IActionManager actionManager;
  private final BundleContext context;
  private final Project project;

  private volatile DeviceController currentDevCtrl;

  private JFrame mainFrame;
  private JMenu deviceMenu;
  private JMenu toolsMenu;
  private DiagramScrollPane diagramScrollPane;
  private JTextStatusBar status;

  private MenuTracker menuTracker;
  private DeviceControllerTracker deviceControllerTracker;
  private ToolTracker toolTracker;

  // CONSTRUCTORS

  private Properties userProperties;

  // METHODS

  /**
   * Creates a new Host instance.
   */
  public Host( final BundleContext aBundleContext )
  {
    if ( aBundleContext == null )
    {
      throw new IllegalArgumentException( "Bundle context cannot be null!" );
    }
    this.context = aBundleContext;

    this.project = new Project();
    this.actionManager = new ActionManager();
  }

  /**
   * @see nl.lxtreme.ols.api.tools.AnalysisCallback#analysisAborted(java.lang.String)
   */
  @Override
  public void analysisAborted( final String aReason )
  {
    this.status.showProgressBar( false );

    this.status.setText( "Analysis aborted! " + aReason );
  }

  /**
   * @see nl.lxtreme.ols.api.tools.AnalysisCallback#analysisComplete(nl.lxtreme.ols.api.data.CapturedData)
   */
  @Override
  public void analysisComplete( final CapturedData aNewCapturedData )
  {
    this.status.showProgressBar( false );

    if ( aNewCapturedData != null )
    {
      this.diagramScrollPane.setCapturedData( aNewCapturedData );
      this.diagramScrollPane.zoomToFit();
    }
    else
    {
      // Simply repaint the scroll pane...
      this.diagramScrollPane.repaint();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureAborted(java.lang.String)
   */
  @Override
  public void captureAborted( final String aReason )
  {
    this.status.showProgressBar( false );

    this.status.setText( "Capture aborted! " + aReason );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureComplete(nl.lxtreme.ols.api.data.CapturedData)
   */
  @Override
  public void captureComplete( final CapturedData aCapturedData )
  {
    final boolean actionsEnabled = aCapturedData != null;
    final boolean hasTriggerPoint = actionsEnabled && aCapturedData.hasTriggerData();

    getAction( ZoomInAction.ID ).setEnabled( actionsEnabled );
    getAction( ZoomOutAction.ID ).setEnabled( actionsEnabled );
    getAction( ZoomDefaultAction.ID ).setEnabled( actionsEnabled );
    getAction( ZoomFitAction.ID ).setEnabled( actionsEnabled );
    getAction( GotoTriggerAction.ID ).setEnabled( hasTriggerPoint );
    getAction( GotoCursor1Action.ID ).setEnabled( actionsEnabled );
    getAction( GotoCursor2Action.ID ).setEnabled( actionsEnabled );
    getAction( SetCursorModeAction.ID ).setEnabled( actionsEnabled );

    if ( aCapturedData != null )
    {
      this.diagramScrollPane.setCapturedData( aCapturedData );
      this.diagramScrollPane.zoomToFit();
    }

    this.status.showProgressBar( false );
  }

  /**
   * Exits this host.
   */
  public void exit()
  {
    //
    stop();

    try
    {
      this.context.getBundle( 0 ).stop();
      System.exit( 0 );
    }
    catch ( BundleException be )
    {
      System.exit( -1 );
    }
  }

  /**
   * @see nl.lxtreme.ols.client.ActionProvider#getAction(java.lang.String)
   */
  @Override
  public final Action getAction( final String aID )
  {
    return this.actionManager.getAction( aID );
  }

  /**
   * @return
   */
  public AnnotatedData getAnnotatedData()
  {
    return this.diagramScrollPane.getAnnotatedData();
  }

  /**
   * Returns the current device controller.
   * 
   * @return the current device controller, never <code>null</code>.
   */
  public DeviceController getCurrentDeviceController()
  {
    return this.currentDevCtrl;
  }

  /**
   * Returns the full name for this host.
   * 
   * @return a full name, never <code>null</code>.
   * @see #getShortName()
   */
  public final String getFullName()
  {
    return getShortName().concat( " - Logic Analyzer Client" );
  }

  /**
   * Returns a symbolic name for this host.
   * 
   * @return a symbolic name, never <code>null</code>.
   */
  public final String getShortName()
  {
    return "LogicSniffer";
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleAbout()
   */
  @Override
  public boolean handleAbout()
  {
    final String message = getFullName() + "\n\n" //
        + "Copyright 2006-2010 Michael Poppitz\n" //
        + "Copyright 2010 J.W. Janssen\n\n" //
        + "This software is released under the GNU GPL.\n\n" //
        + "Version: {0}\n\n" //
        + "For more information see:\n" //
        + "  <http://www.lxtreme.nl/ols/>\n" //
        + "  <http://dangerousprototypes.com/open-logic-sniffer/>\n" //
        + "  <http://www.gadgetfactory.net/gf/project/butterflylogic/>\n" //
        + "  <http://www.sump.org/projects/analyzer/>";

    final JOptionPane aboutDialogFactory = new JOptionPane( String.format( message, "0.8.3" ), //
        JOptionPane.INFORMATION_MESSAGE, JOptionPane.DEFAULT_OPTION );

    final JDialog aboutDialog = aboutDialogFactory.createDialog( this.mainFrame, "About ..." );
    aboutDialog.setVisible( true );

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
  public void initialize() throws Exception
  {
    if ( Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) ) )
    {
      // Install a custom repaint manager that detects whether Swing components
      // are created outside the EDT; if so, it will yield a stack trace to the
      // offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    // Read back the user preferences from last time...
    this.userProperties = HostUtils.readProperties( PROPERTIES_NAME );
    if ( this.userProperties == null )
    {
      this.userProperties = new Properties();
    }

    // Install a global window state listener...
    Toolkit.getDefaultToolkit().addAWTEventListener( new FrameStateListener( this.userProperties ),
        AWTEvent.WINDOW_EVENT_MASK );

    this.mainFrame = new JFrame( getShortName().concat( " - Logic Analyzer Client" ) );
    this.diagramScrollPane = new DiagramScrollPane( this );
    this.status = new JTextStatusBar();

    this.actionManager.add( new NewProjectAction( this.project ) );
    this.actionManager.add( new OpenProjectAction( this.project ) );
    this.actionManager.add( new SaveProjectAction( this.project ) );
    this.actionManager.add( new OpenDataFileAction( this.diagramScrollPane ) );
    this.actionManager.add( new SaveDataFileAction( this.diagramScrollPane ) );
    this.actionManager.add( new ExitAction( this ) );

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

    setContent();

    this.menuTracker = new MenuTracker( this.context, this.mainFrame.getJMenuBar() );
    this.deviceControllerTracker = new DeviceControllerTracker( this.context, this, this.deviceMenu );
    this.toolTracker = new ToolTracker( this.context, this, this.toolsMenu );
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
      final String name = aCurrentDevCtrl == null ? "<no device>" : aCurrentDevCtrl.getName();
      LOG.info( "Setting current device controller to: " + name );
    }
    this.currentDevCtrl = aCurrentDevCtrl;
  }

  /**
   * Starts this UI-host, may <em>not</em> be called from outside the EDT.
   */
  public void start() throws Exception
  {
    this.deviceControllerTracker.open();
    this.toolTracker.open();
    this.menuTracker.open();

    this.mainFrame.setVisible( true );
  }

  /**
   * Stops this UI-host, may <em>not</em> be called from outside the EDT.
   */
  public void stop()
  {
    // Write back the user properties for use next time...
    HostUtils.writeProperties( PROPERTIES_NAME, this.userProperties );

    this.deviceControllerTracker.close();
    this.toolTracker.close();
    this.menuTracker.close();

    if ( this.mainFrame != null )
    {
      this.mainFrame.dispose();
      this.mainFrame = null;
    }
  }

  /**
   * @see nl.lxtreme.ols.api.ProgressCallback#updateProgress(int)
   */
  @Override
  public void updateProgress( final int aPercentage )
  {
    this.status.setProgress( aPercentage );
  }

  /**
   * @param aProject
   * @param aMainFrame
   * @param aDiagram
   * @return
   */
  private JToolBar createMenuBars( final Project aProject, final JFrame aMainFrame, final DiagramScrollPane aDiagram )
  {
    final JMenuBar bar = new JMenuBar();
    aMainFrame.setJMenuBar( bar );

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

    this.deviceMenu = new JMenu( "Device" );
    bar.add( this.deviceMenu );

    this.toolsMenu = new JMenu( "Tools" );
    bar.add( this.toolsMenu );

    final JMenu diagramMenu = new JMenu( "Diagram" );
    bar.add( diagramMenu );

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

    // final JMenu windowMenu = new JMenu( "Window" );
    // bar.add( windowMenu );

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
   * Sets up the contents of the main frame.
   */
  private void setContent() throws TooManyListenersException
  {
    this.mainFrame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    this.mainFrame.setSize( 1200, 600 );

    final Container contentPane = this.mainFrame.getContentPane();
    contentPane.setLayout( new BorderLayout() );
    contentPane.add( this.diagramScrollPane, BorderLayout.CENTER );

    final JToolBar tools = createMenuBars( this.project, this.mainFrame, this.diagramScrollPane );
    contentPane.add( tools, BorderLayout.PAGE_START );

    contentPane.add( this.status, BorderLayout.PAGE_END );
  }
}

/* EOF */
