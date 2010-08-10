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
import java.util.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.signal.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.HostUtils.ApplicationCallback;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * Denotes the UI-host.
 */
public final class Host implements CaptureCallback, AnalysisCallback, ApplicationCallback
{
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
    this.status.showProgressBar( false );

    if ( aCapturedData != null )
    {
      this.diagramScrollPane.setCapturedData( aCapturedData );
      this.diagramScrollPane.zoomToFit();
    }
  }

  /**
   * Exits this host.
   */
  public void exit()
  {
    // Write back the user properties for use next time...
    HostUtils.writeProperties( PROPERTIES_NAME, this.project.getProperties() );

    this.mainFrame.setVisible( false );
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
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleAbout()
   */
  @Override
  public boolean handleAbout()
  {
    JOptionPane.showMessageDialog( this.mainFrame, "OpenBench Logic Sniffer\n\n" //
        + "Copyright 2006-2010 Michael Poppitz\n" //
        + "Copyright 2010 J.W. Janssen\n\n" //
        + "This software is released under the GNU GPL.\n\n" //
        + "Version: 0.7.0\n\n" //
        + "For more information see:\n" //
        + "  <http://www.lxtreme.nl/ols/>\n" //
        + "  <http://dangerousprototypes.com/open-logic-sniffer/>\n" //
        + "  <http://www.gadgetfactory.net/gf/project/butterflylogic/>\n" //
        + "  <http://www.sump.org/projects/analyzer/>", //
        "About", JOptionPane.INFORMATION_MESSAGE );
    return true;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handlePreferences()
   */
  @Override
  public boolean handlePreferences()
  {
    // TODO Auto-generated method stub
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
   * Initializes this host application.
   */
  public void initialize() throws Exception
  {
    HostUtils.initOSSpecifics( "LogicSniffer", this );

    this.mainFrame = new JFrame( "LogicSniffer - Logic Analyzer Client" );
    this.diagramScrollPane = new DiagramScrollPane( this.project )
    {
      private static final long serialVersionUID = 1L;

      /**
       * @see nl.lxtreme.ols.client.ActionProvider#getAction(java.lang.String)
       */
      @Override
      public Action getAction( final String aID )
      {
        return Host.this.getAction( aID );
      }
    };
    this.status = new JTextStatusBar();

    this.actionManager.add( new NewProjectAction( this.project ) );
    this.actionManager.add( new OpenProjectAction( this.project ) );
    this.actionManager.add( new SaveProjectAction( this.project ) );
    this.actionManager.add( new OpenDataFileAction( this.diagramScrollPane ) );
    this.actionManager.add( new SaveDataFileAction( this.diagramScrollPane ) );
    this.actionManager.add( new ExitAction( this ) );

    this.actionManager.add( new CaptureAction( this, this.project ) );
    this.actionManager.add( new RepeatCaptureAction( this, this.project ) );

    this.actionManager.add( new ZoomInAction( this.diagramScrollPane ) );
    this.actionManager.add( new ZoomOutAction( this.diagramScrollPane ) );
    this.actionManager.add( new ZoomDefaultAction( this.diagramScrollPane ) );
    this.actionManager.add( new ZoomFitAction( this.diagramScrollPane ) );
    this.actionManager.add( new GotoTriggerAction( this.diagramScrollPane ) );
    this.actionManager.add( new GotoCursor1Action( this.diagramScrollPane ) ).setEnabled( false );
    this.actionManager.add( new GotoCursor2Action( this.diagramScrollPane ) ).setEnabled( false );
    this.actionManager.add( new SetCursorModeAction( this.diagramScrollPane ) );
    this.actionManager.add( new ShowDiagramSettingsAction( this.diagramScrollPane ) );
    this.actionManager.add( new ShowDiagramLabelsAction( this.diagramScrollPane ) );

    setContent();

    this.menuTracker = new MenuTracker( this.context, this.mainFrame.getJMenuBar() );
    this.deviceControllerTracker = new DeviceControllerTracker( this.context, this, this.deviceMenu );
    this.toolTracker = new ToolTracker( this.context, this, this.project, this.toolsMenu );
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
   * Starts this UI-host, may be called from outside the EDT.
   */
  public void start() throws Exception
  {
    // Read back the user preferences from last time...
    this.userProperties = HostUtils.readProperties( PROPERTIES_NAME );

    this.deviceControllerTracker.open();
    this.toolTracker.open();
    this.menuTracker.open();

    this.mainFrame.setVisible( true );
  }

  /**
   * Stops this UI-host, may be called from outside the EDT.
   */
  public void stop() throws Exception
  {
    this.deviceControllerTracker.close();
    this.toolTracker.close();
    this.menuTracker.close();

    if ( this.mainFrame != null )
    {
      this.mainFrame.dispose();
    }

    // Write back the user properties for use next time...
    HostUtils.writeProperties( PROPERTIES_NAME, this.userProperties );
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
   * @param aID
   * @return
   */
  protected final Action getAction( final String aID )
  {
    return this.actionManager.getAction( aID );
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
