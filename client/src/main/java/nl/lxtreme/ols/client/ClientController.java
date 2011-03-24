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
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.settings.*;
import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * Denotes a front-end controller for the client.
 */
public final class ClientController implements ActionProvider, CaptureCallback, AnalysisCallback, IClientController
{
  // INNER TYPES

  /**
   * Provides a default tool context implementation.
   */
  static final class DefaultToolContext implements ToolContext
  {
    // VARIABLES

    private final int startSampleIdx;
    private final int endSampleIdx;
    private final int channels;
    private final int enabledChannels;

    // CONSTRUCTORS

    /**
     * Creates a new DefaultToolContext instance.
     * 
     * @param aStartSampleIdx
     *          the starting sample index;
     * @param aEndSampleIdx
     *          the ending sample index;
     * @param aChannels
     *          the available channels in the acquisition result;
     * @param aEnabledChannels
     *          the enabled channels in the acquisition result.
     */
    public DefaultToolContext( final int aStartSampleIdx, final int aEndSampleIdx, final int aChannels,
        final int aEnabledChannels )
    {
      this.startSampleIdx = aStartSampleIdx;
      this.endSampleIdx = aEndSampleIdx;
      this.channels = aChannels;
      this.enabledChannels = aEnabledChannels;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannels()
    {
      return this.channels;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getEnabledChannels()
    {
      return this.enabledChannels;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getEndSampleIndex()
    {
      return this.endSampleIdx;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLength()
    {
      return Math.max( 0, this.endSampleIdx - this.startSampleIdx );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getStartSampleIndex()
    {
      return this.startSampleIdx;
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClientController.class.getName() );

  // VARIABLES

  private final IActionManager actionManager;
  private final BundleContext bundleContext;
  private final DataContainer dataContainer;
  private final EventListenerList evenListeners;
  private final ProjectManager projectManager;
  private final IHost host;

  private MainFrame mainFrame;

  private volatile DeviceController currentDevCtrl;

  // CONSTRUCTORS

  /**
   * Creates a new ClientController instance.
   * 
   * @param aBundleContext
   *          the bundle context to use for interaction with the OSGi framework;
   * @param aHost
   *          the current host to use, cannot be <code>null</code>;
   * @param aProjectManager
   *          the project manager to use, cannot be <code>null</code>.
   */
  public ClientController( final BundleContext aBundleContext, final IHost aHost, final ProjectManager aProjectManager )
  {
    this.bundleContext = aBundleContext;
    this.host = aHost;
    this.projectManager = aProjectManager;

    this.dataContainer = new DataContainer( this.projectManager );
    this.evenListeners = new EventListenerList();
    this.actionManager = ActionManagerFactory.createActionManager( this );
  }

  // METHODS

  /**
   * Adds a cursor change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addCursorChangeListener( final DiagramCursorChangeListener aListener )
  {
    this.evenListeners.add( DiagramCursorChangeListener.class, aListener );
  }

  /**
   * Adds the given device controller to this controller, and does this
   * synchronously on the EDT.
   * 
   * @param aDeviceController
   *          the device controller to add, cannot be <code>null</code>.
   */
  public void addDevice( final DeviceController aDeviceController )
  {
    if ( this.mainFrame != null )
    {
      SwingUtilities.invokeLater( new Runnable()
      {
        @Override
        public void run()
        {
          final IManagedAction deviceAction = ClientController.this.mainFrame.addDeviceMenuItem( aDeviceController
              .getName() );

          ClientController.this.actionManager.add( deviceAction );

          // TODO ehm, yes... a hack...
          if ( aDeviceController.getClass().getName().startsWith( "org.sump" ) )
          {
            deviceAction.putValue( Action.SELECTED_KEY, Boolean.TRUE );
            ClientController.this.currentDevCtrl = aDeviceController;
          }

          updateActions();
        }
      } );
    }
  }

  /**
   * Adds the given exporter to this controller, and does this synchronously on
   * the EDT..
   * 
   * @param aExporter
   *          the exporter to add, cannot be <code>null</code>.
   */
  public void addExporter( final Exporter aExporter )
  {
    if ( this.mainFrame != null )
    {
      SwingUtilities.invokeLater( new Runnable()
      {
        @Override
        public void run()
        {
          final IManagedAction exporterAction = ClientController.this.mainFrame.addExportMenuItem( aExporter.getName() );
          ClientController.this.actionManager.add( exporterAction );

          updateActions();
        }
      } );
    }
  }

  /**
   * Adds the menu component of the given provider to this controller, and does
   * this synchronously on the EDT.
   * 
   * @param aProvider
   *          the menu component provider, cannot be <code>null</code>.
   */
  public void addMenu( final ComponentProvider aProvider )
  {
    if ( this.mainFrame != null )
    {
      SwingUtilities.invokeLater( new Runnable()
      {
        @Override
        public void run()
        {
          final JMenu menu = ( JMenu )aProvider.getComponent();
          final JMenuBar menuBar = ClientController.this.mainFrame.getJMenuBar();
          menuBar.add( menu );
          aProvider.addedToContainer();

          menuBar.revalidate();
          menuBar.repaint();
        }
      } );
    }
  }

  /**
   * Adds the given tool to this controller, and does this synchronously on the
   * EDT..
   * 
   * @param aTool
   *          the tool to add, cannot be <code>null</code>.
   */
  public void addTool( final Tool aTool )
  {
    if ( this.mainFrame != null )
    {
      SwingUtilities.invokeLater( new Runnable()
      {
        @Override
        public void run()
        {
          final IManagedAction toolAction = ClientController.this.mainFrame.addToolMenuItem( aTool.getName() );
          ClientController.this.actionManager.add( toolAction );

          updateActions();
        }
      } );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.tools.AnalysisCallback#analysisAborted(java.lang.String)
   */
  @Override
  public void analysisAborted( final String aReason )
  {
    setStatus( "Analysis aborted! " + aReason );

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void analysisComplete( final AcquisitionResult aNewData )
  {
    if ( aNewData != null )
    {
      this.dataContainer.setCapturedData( aNewData );
    }
    if ( this.mainFrame != null )
    {
      repaintMainFrame();
    }

    setStatus( "" );
    updateActions();
  }

  /**
   * @see nl.lxtreme.ols.client.IClientController#cancelCapture()
   */
  public void cancelCapture()
  {
    final DeviceController deviceController = getDeviceController();
    if ( deviceController == null )
    {
      return;
    }

    deviceController.cancel();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureAborted(java.lang.String)
   */
  @Override
  public void captureAborted( final String aReason )
  {
    setStatus( "Capture aborted! " + aReason );
    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void captureComplete( final AcquisitionResult aCapturedData )
  {
    setAcquisitionResult( aCapturedData );

    setStatus( "Capture finished at {0,date,medium} {0,time,medium}.", new Date() );

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public boolean captureData( final Window aParent )
  {
    final DeviceController devCtrl = getDeviceController();
    if ( devCtrl == null )
    {
      return false;
    }

    try
    {
      if ( devCtrl.setupCapture( aParent ) )
      {
        setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", devCtrl.getName(), new Date() );

        devCtrl.captureData( this );
        return true;
      }

      return false;
    }
    catch ( IOException exception )
    {
      captureAborted( "I/O problem: " + exception.getMessage() );

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        exception.printStackTrace();
      }

      return false;
    }
    finally
    {
      updateActions();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.devices.CaptureCallback#captureStarted(int, int,
   *      int)
   */
  @Override
  public synchronized void captureStarted( final int aSampleRate, final int aChannelCount, final int aChannelMask )
  {
    final Runnable runner = new Runnable()
    {
      @Override
      public void run()
      {
        updateActions();
      }
    };

    if ( SwingUtilities.isEventDispatchThread() )
    {
      runner.run();
    }
    else
    {
      SwingUtilities.invokeLater( runner );
    }
  }

  /**
   * {@inheritDoc}
   */
  public void clearAllCursors()
  {
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      this.dataContainer.setCursorPosition( i, null );
    }
    fireCursorChangedEvent( 0, -1 ); // removed...

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public synchronized void clearDeviceController()
  {
    this.currentDevCtrl = null;
  }

  /**
   * {@inheritDoc}
   */
  public void createNewProject()
  {
    this.projectManager.createNewProject();

    if ( this.mainFrame != null )
    {
      this.mainFrame.repaint();
    }

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void exit()
  {
    if ( this.host != null )
    {
      this.host.exit();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void exportTo( final String aExporterName, final File aExportFile ) throws IOException
  {
    if ( this.mainFrame == null )
    {
      return;
    }

    OutputStream writer = null;

    try
    {
      writer = new FileOutputStream( aExportFile );

      final Exporter exporter = getExporter( aExporterName );
      exporter.export( this.dataContainer, this.mainFrame.getDiagramScrollPane(), writer );

      setStatus( "Export to {0} succesful ...", aExporterName );
    }
    finally
    {
      HostUtils.closeResource( writer );
    }
  }

  /**
   * @see nl.lxtreme.ols.client.ActionProvider#getAction(java.lang.String)
   */
  public Action getAction( final String aID )
  {
    return this.actionManager.getAction( aID );
  }

  /**
   * {@inheritDoc}
   */
  public DataContainer getDataContainer()
  {
    return this.dataContainer;
  }

  /**
   * Returns the current device controller.
   * 
   * @return the current device controller, can be <code>null</code>.
   */
  public synchronized DeviceController getDeviceController()
  {
    return this.currentDevCtrl;
  }

  /**
   * Returns all current tools known to the OSGi framework.
   * 
   * @return a collection of tools, never <code>null</code>.
   */
  public final Collection<DeviceController> getDevices()
  {
    final List<DeviceController> tools = new ArrayList<DeviceController>();
    synchronized ( this.bundleContext )
    {
      try
      {
        final ServiceReference[] serviceRefs = this.bundleContext.getAllServiceReferences(
            DeviceController.class.getName(), null );
        for ( ServiceReference serviceRef : serviceRefs )
        {
          tools.add( ( DeviceController )this.bundleContext.getService( serviceRef ) );
        }
      }
      catch ( InvalidSyntaxException exception )
      {
        throw new RuntimeException( exception );
      }
    }
    return tools;
  }

  /**
   * Returns the current diagram settings.
   * 
   * @return the current diagram settings, can be <code>null</code> if there is
   *         no main frame to take the settings from.
   */
  public final DiagramSettings getDiagramSettings()
  {
    final Project currentProject = this.projectManager.getCurrentProject();
    final UserSettings settings = currentProject.getSettings( MutableDiagramSettings.NAME );
    if ( settings instanceof DiagramSettings )
    {
      return ( DiagramSettings )settings;
    }

    // Overwrite the default created user settings object with our own. This
    // should be done implicitly, so make sure we keep the project's change flag
    // in the correct state...
    final MutableDiagramSettings diagramSettings = new MutableDiagramSettings( settings );

    final boolean oldChangedFlag = currentProject.isChanged();
    try
    {
      currentProject.setSettings( diagramSettings );
    }
    finally
    {
      currentProject.setChanged( oldChangedFlag );
    }

    return diagramSettings;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String[] getExportExtensions( final String aExporterName )
  {
    final Exporter exporter = getExporter( aExporterName );
    if ( exporter == null )
    {
      return new String[0];
    }
    return exporter.getFilenameExtentions();
  }

  /**
   * {@inheritDoc}
   */
  public File getProjectFilename()
  {
    return this.projectManager.getCurrentProject().getFilename();
  }

  /**
   * Returns all current tools known to the OSGi framework.
   * 
   * @return a collection of tools, never <code>null</code>.
   */
  public final Collection<Tool> getTools()
  {
    final List<Tool> tools = new ArrayList<Tool>();
    synchronized ( this.bundleContext )
    {
      try
      {
        final ServiceReference[] serviceRefs = this.bundleContext.getAllServiceReferences( Tool.class.getName(), null );
        for ( ServiceReference serviceRef : serviceRefs )
        {
          tools.add( ( Tool )this.bundleContext.getService( serviceRef ) );
        }
      }
      catch ( InvalidSyntaxException exception )
      {
        throw new RuntimeException( exception );
      }
    }
    return tools;
  }

  /**
   * {@inheritDoc}
   */
  public void gotoCursorPosition( final int aCursorIdx )
  {
    if ( ( this.mainFrame != null ) && this.dataContainer.isCursorsEnabled() )
    {
      final Long cursorPosition = this.dataContainer.getCursorPosition( aCursorIdx );
      if ( cursorPosition != null )
      {
        this.mainFrame.gotoPosition( cursorPosition.longValue() );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public void gotoFirstAvailableCursor()
  {
    if ( ( this.mainFrame != null ) && this.dataContainer.isCursorsEnabled() )
    {
      for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
      {
        if ( this.dataContainer.isCursorPositionSet( c ) )
        {
          final Long cursorPosition = this.dataContainer.getCursorPosition( c );
          if ( cursorPosition != null )
          {
            this.mainFrame.gotoPosition( cursorPosition.longValue() );
          }
          break;
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public void gotoLastAvailableCursor()
  {
    if ( ( this.mainFrame != null ) && this.dataContainer.isCursorsEnabled() )
    {
      for ( int c = Ols.MAX_CURSORS - 1; c >= 0; c-- )
      {
        if ( this.dataContainer.isCursorPositionSet( c ) )
        {
          final Long cursorPosition = this.dataContainer.getCursorPosition( c );
          if ( cursorPosition != null )
          {
            this.mainFrame.gotoPosition( cursorPosition.longValue() );
          }
          break;
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public void gotoTriggerPosition()
  {
    if ( ( this.mainFrame != null ) && this.dataContainer.hasTriggerData() )
    {
      final long position = this.dataContainer.getTriggerPosition();
      this.mainFrame.gotoPosition( position );
    }
  }

  /**
   * {@inheritDoc}
   */
  public synchronized boolean isDeviceSelected()
  {
    return this.currentDevCtrl != null;
  }

  /**
   * {@inheritDoc}
   */
  public synchronized boolean isDeviceSetup()
  {
    return isDeviceSelected() && this.currentDevCtrl.isSetup();
  }

  /**
   * {@inheritDoc}
   */
  public boolean isProjectChanged()
  {
    return this.projectManager.getCurrentProject().isChanged();
  }

  /**
   * {@inheritDoc}
   */
  public void openDataFile( final File aFile ) throws IOException
  {
    final FileReader reader = new FileReader( aFile );

    try
    {
      final Project tempProject = this.projectManager.createTemporaryProject();
      OlsDataHelper.read( tempProject, reader );

      setChannelLabels( tempProject.getChannelLabels() );
      setAcquisitionResult( tempProject.getCapturedData() );
      setCursorData( tempProject.getCursorPositions(), tempProject.isCursorsEnabled() );

      setStatus( "Capture data loaded from {0} ...", aFile.getName() );
    }
    finally
    {
      reader.close();

      zoomToFit();

      updateActions();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void openProjectFile( final File aFile ) throws IOException
  {
    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( aFile );

      this.projectManager.loadProject( fis );

      final Project project = this.projectManager.getCurrentProject();
      project.setFilename( aFile );

      zoomToFit();

      setStatus( "Project {0} loaded ...", project.getName() );
    }
    finally
    {
      HostUtils.closeResource( fis );
    }
  }

  /**
   * {@inheritDoc}
   */
  public void removeCursor( final int aCursorIdx )
  {
    if ( this.mainFrame != null )
    {
      this.dataContainer.setCursorPosition( aCursorIdx, null );
      fireCursorChangedEvent( aCursorIdx, -1 ); // removed...
    }

    updateActions();
  }

  /**
   * Removes a cursor change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeCursorChangeListener( final DiagramCursorChangeListener aListener )
  {
    this.evenListeners.remove( DiagramCursorChangeListener.class, aListener );
  }

  /**
   * Removes the given device from the list of devices, and does this
   * synchronously on the EDT.
   * 
   * @param aDeviceController
   *          the device to remove, cannot be <code>null</code>.
   */
  public void removeDevice( final DeviceController aDeviceController )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        if ( ClientController.this.currentDevCtrl == aDeviceController )
        {
          ClientController.this.currentDevCtrl = null;
        }

        if ( ClientController.this.mainFrame != null )
        {
          ClientController.this.mainFrame.removeDeviceMenuItem( aDeviceController.getName() );
        }

        updateActions();
      }
    } );
  }

  /**
   * Removes the given exporter from the list of exporters, and does this
   * synchronously on the EDT.
   * 
   * @param aExporter
   *          the exporter to remove, cannot be <code>null</code>.
   */
  public void removeExporter( final Exporter aExporter )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        if ( ClientController.this.mainFrame != null )
        {
          final IManagedAction exportAction = ClientController.this.mainFrame.removeExportMenuItem( aExporter.getName() );
          if ( exportAction != null )
          {
            ClientController.this.actionManager.remove( exportAction );
          }
        }

        updateActions();
      }
    } );
  }

  /**
   * Removes the menu from the given provider from this controller, and does
   * this synchronously on the EDT.
   * 
   * @param aProvider
   *          the menu component provider, cannot be <code>null</code>.
   */
  public void removeMenu( final ComponentProvider aProvider )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        if ( ClientController.this.mainFrame != null )
        {
          final JMenu menu = ( JMenu )aProvider.getComponent();

          final JMenuBar menuBar = ClientController.this.mainFrame.getJMenuBar();
          menuBar.remove( menu );

          aProvider.removedFromContainer();

          menuBar.revalidate();
          menuBar.repaint();
        }
      }
    } );
  }

  /**
   * Removes the given tool from the list of tools, and does this synchronously
   * on the EDT.
   * 
   * @param aTool
   *          the tool to remove, cannot be <code>null</code>.
   */
  public void removeTool( final Tool aTool )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        if ( ClientController.this.mainFrame != null )
        {
          final IManagedAction toolAction = ClientController.this.mainFrame.removeToolMenuItem( aTool.getName() );
          if ( toolAction != null )
          {
            ClientController.this.actionManager.remove( toolAction );
          }
        }

        updateActions();
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  public boolean repeatCaptureData( final Window aParent )
  {
    final DeviceController devCtrl = getDeviceController();
    if ( devCtrl == null )
    {
      return false;
    }

    try
    {
      setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", devCtrl.getName(), new Date() );

      devCtrl.captureData( this );

      return true;
    }
    catch ( IOException exception )
    {
      captureAborted( "I/O problem: " + exception.getMessage() );

      exception.printStackTrace();

      // Make sure to handle IO-interrupted exceptions properly!
      HostUtils.handleInterruptedException( exception );

      return false;
    }
    finally
    {
      updateActions();
    }
  }

  /**
   * Runs the tool denoted by the given name.
   * 
   * @param aToolName
   *          the name of the tool to run, cannot be <code>null</code>;
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void runTool( final String aToolName, final Window aParent )
  {
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.log( Level.INFO, "Running tool: \"{0}\" ...", aToolName );
    }

    final Tool tool = findToolByName( aToolName );
    if ( tool == null )
    {
      JOptionPane.showMessageDialog( aParent, "No such tool found: " + aToolName, "Error ...",
          JOptionPane.ERROR_MESSAGE );
    }
    else
    {
      final ToolContext context = createToolContext();
      tool.process( aParent, this.dataContainer, context, this );
    }

    updateActions();
  }

  /**
   * @see nl.lxtreme.ols.api.devices.CaptureCallback#samplesCaptured(java.util.List)
   */
  @Override
  public void samplesCaptured( final List<Sample> aSamples )
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.sampleCaptured( aSamples );
    }
    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void saveDataFile( final File aFile ) throws IOException
  {
    final FileWriter writer = new FileWriter( aFile );
    try
    {
      final Project tempProject = this.projectManager.createTemporaryProject();
      tempProject.setCapturedData( this.dataContainer );

      OlsDataHelper.write( tempProject, writer );

      setStatus( "Capture data saved to {0} ...", aFile.getName() );
    }
    finally
    {
      writer.flush();
      writer.close();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void saveProjectFile( final String aName, final File aFile ) throws IOException
  {
    FileOutputStream out = null;
    try
    {
      final Project project = this.projectManager.getCurrentProject();
      project.setFilename( aFile );
      project.setName( aName );

      out = new FileOutputStream( aFile );
      this.projectManager.saveProject( out );

      setStatus( "Project {0} saved ...", aName );
    }
    finally
    {
      HostUtils.closeResource( out );
    }
  }

  /**
   * Sets whether or not cursors are enabled.
   * 
   * @param aState
   *          <code>true</code> if the cursors should be enabled,
   *          <code>false</code> otherwise.
   */
  public void setCursorMode( final boolean aState )
  {
    this.dataContainer.setCursorEnabled( aState );
    // Reflect the change directly on the diagram...
    repaintMainFrame();

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void setCursorPosition( final int aCursorIdx, final Point aLocation )
  {
    // Implicitly enable cursor mode, the user already had made its
    // intensions clear that he want to have this by opening up the
    // context menu anyway...
    setCursorMode( true );

    if ( this.mainFrame != null )
    {
      // Convert the mouse-position to a sample index...
      final long sampleIdx = this.mainFrame.convertMousePositionToSampleIndex( aLocation );

      this.dataContainer.setCursorPosition( aCursorIdx, Long.valueOf( sampleIdx ) );

      fireCursorChangedEvent( aCursorIdx, aLocation.x );
    }

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public synchronized void setDeviceController( final String aDeviceName )
  {
    if ( LOG.isLoggable( Level.INFO ) )
    {
      final String name = ( aDeviceName == null ) ? "no device" : aDeviceName;
      LOG.log( Level.INFO, "Setting current device controller to: \"{0}\" ...", name );
    }

    final Collection<DeviceController> devices = getDevices();
    for ( DeviceController device : devices )
    {
      if ( aDeviceName.equals( device.getName() ) )
      {
        this.currentDevCtrl = device;
      }
    }

    updateActions();
  }

  /**
   * @param aMainFrame
   *          the mainFrame to set
   */
  public void setMainFrame( final MainFrame aMainFrame )
  {
    if ( this.mainFrame != null )
    {
      this.projectManager.removePropertyChangeListener( this.mainFrame );
    }
    if ( aMainFrame != null )
    {
      this.projectManager.addPropertyChangeListener( aMainFrame );
    }

    this.mainFrame = aMainFrame;
  }

  /**
   * {@inheritDoc}
   */
  public final void setStatus( final String aMessage, final Object... aMessageArgs )
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.setStatus( aMessage, aMessageArgs );
    }
  }

  /**
   * Shows the "about OLS" dialog on screen. the parent window to use, can be
   * <code>null</code>.
   */
  public void showAboutBox()
  {
    MainFrame.showAboutBox( this.host.getVersion() );
  }

  /**
   * {@inheritDoc}
   */
  public void showBundlesDialog( final Window aOwner )
  {
    BundlesDialog dialog = new BundlesDialog( aOwner, this.bundleContext );
    if ( dialog.showDialog() )
    {
      dialog.dispose();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void showChannelLabelsDialog( final Window aParent )
  {
    if ( this.mainFrame != null )
    {
      DiagramLabelsDialog dialog = new DiagramLabelsDialog( aParent, this.dataContainer.getChannelLabels() );
      if ( dialog.showDialog() )
      {
        final String[] channelLabels = dialog.getChannelLabels();
        setChannelLabels( channelLabels );
      }

      dialog.dispose();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void showDiagramModeSettingsDialog( final Window aParent )
  {
    if ( this.mainFrame != null )
    {
      ModeSettingsDialog dialog = new ModeSettingsDialog( aParent, getDiagramSettings() );
      if ( dialog.showDialog() )
      {
        final DiagramSettings settings = dialog.getDiagramSettings();
        this.projectManager.getCurrentProject().setSettings( settings );
        diagramSettingsUpdated();
      }

      dialog.dispose();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void showPreferencesDialog( final Window aParent )
  {
    GeneralSettingsDialog dialog = new GeneralSettingsDialog( aParent, getDiagramSettings() );
    if ( dialog.showDialog() )
    {
      final DiagramSettings settings = dialog.getDiagramSettings();
      this.projectManager.getCurrentProject().setSettings( settings );
      diagramSettingsUpdated();
    }

    dialog.dispose();
  }

  /**
   * @see nl.lxtreme.ols.api.ProgressCallback#updateProgress(int)
   */
  @Override
  public void updateProgress( final int aPercentage )
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.setProgress( aPercentage );
    }
  }

  /**
   * {@inheritDoc}
   */
  public void zoomDefault()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.zoomDefault();
    }

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void zoomIn()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.zoomIn();
    }

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void zoomOut()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.zoomOut();
    }

    updateActions();
  }

  /**
   * {@inheritDoc}
   */
  public void zoomToFit()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.zoomToFit();
    }

    updateActions();
  }

  /**
   * Returns the current main frame.
   * 
   * @return the main frame, can be <code>null</code>.
   */
  final MainFrame getMainFrame()
  {
    return this.mainFrame;
  }

  /**
   * Creates the tool context denoting the range of samples that should be
   * analysed by a tool.
   * 
   * @return a tool context, never <code>null</code>.
   */
  private ToolContext createToolContext()
  {
    int startOfDecode = -1;
    int endOfDecode = -1;

    final int dataLength = this.dataContainer.getValues().length;
    if ( this.dataContainer.isCursorsEnabled() )
    {
      if ( this.dataContainer.isCursorPositionSet( 0 ) )
      {
        final Long cursor1 = this.dataContainer.getCursorPosition( 0 );
        startOfDecode = this.dataContainer.getSampleIndex( cursor1.longValue() ) - 1;
      }
      if ( this.dataContainer.isCursorPositionSet( 1 ) )
      {
        final Long cursor2 = this.dataContainer.getCursorPosition( 1 );
        endOfDecode = this.dataContainer.getSampleIndex( cursor2.longValue() ) + 1;
      }
    }
    else
    {
      startOfDecode = 0;
      endOfDecode = dataLength;
    }

    startOfDecode = Math.max( 0, startOfDecode );
    if ( ( endOfDecode < 0 ) || ( endOfDecode >= dataLength ) )
    {
      endOfDecode = dataLength - 1;
    }

    int channels = this.dataContainer.getChannels();
    if ( channels == Ols.NOT_AVAILABLE )
    {
      channels = Ols.MAX_CHANNELS;
    }

    int enabledChannels = this.dataContainer.getEnabledChannels();
    if ( enabledChannels == Ols.NOT_AVAILABLE )
    {
      enabledChannels = NumberUtils.getBitMask( channels );
    }

    return new DefaultToolContext( startOfDecode, endOfDecode, channels, enabledChannels );
  }

  /**
   * Should be called after the diagram settings are changed. This method will
   * cause the main frame to be updated.
   */
  private void diagramSettingsUpdated()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.diagramSettingsUpdated();
      repaintMainFrame();
    }
  }

  /**
   * Searches for the tool with the given name.
   * 
   * @param aToolName
   *          the name of the tool to search for, cannot be <code>null</code>.
   * @return the tool with the given name, can be <code>null</code> if no such
   *         tool can be found.
   */
  private Tool findToolByName( final String aToolName )
  {
    Tool result = null;

    final Collection<Tool> tools = getTools();
    for ( Tool tool : tools )
    {
      if ( aToolName.equals( tool.getName() ) )
      {
        result = tool;
        break;
      }
    }
    return result;
  }

  /**
   * @param aCursorIdx
   * @param aMouseXpos
   */
  private void fireCursorChangedEvent( final int aCursorIdx, final int aMouseXpos )
  {
    final DiagramCursorChangeListener[] listeners = this.evenListeners.getListeners( DiagramCursorChangeListener.class );
    for ( final DiagramCursorChangeListener listener : listeners )
    {
      if ( aMouseXpos >= 0 )
      {
        listener.cursorChanged( aCursorIdx, aMouseXpos );
      }
      else
      {
        listener.cursorRemoved( aCursorIdx );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  private Exporter getExporter( final String aName ) throws IllegalArgumentException
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    try
    {
      final ServiceReference[] serviceRefs = this.bundleContext
          .getAllServiceReferences( Exporter.class.getName(), null );
      final int count = ( serviceRefs == null ) ? 0 : serviceRefs.length;

      for ( int i = 0; i < count; i++ )
      {
        final Exporter exporter = ( Exporter )this.bundleContext.getService( serviceRefs[i] );

        if ( aName.equals( exporter.getName() ) )
        {
          return exporter;
        }
      }

      return null;
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new RuntimeException( "getExporter failed!", exception );
    }
  }

  /**
   * Dispatches a request to repaint the entire main frame.
   */
  private void repaintMainFrame()
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        ClientController.this.mainFrame.repaint();
      }
    } );
  }

  /**
   * Sets the captured data and zooms the view to show all the data.
   * 
   * @param aData
   *          the new captured data to set, cannot be <code>null</code>.
   */
  private void setAcquisitionResult( final AcquisitionResult aData )
  {
    this.dataContainer.setCapturedData( aData );

    if ( this.mainFrame != null )
    {
      this.mainFrame.zoomToFit();
    }
  }

  /**
   * Set the channel labels.
   * 
   * @param aChannelLabels
   *          the channel labels to set, cannot be <code>null</code>.
   */
  private void setChannelLabels( final String[] aChannelLabels )
  {
    if ( aChannelLabels != null )
    {
      this.dataContainer.setChannelLabels( aChannelLabels );
      this.mainFrame.setChannelLabels( aChannelLabels );
    }
  }

  /**
   * @param aCursorData
   *          the cursor positions to set, cannot be <code>null</code>;
   * @param aCursorsEnabled
   *          <code>true</code> if cursors should be enabled, <code>false</code>
   *          if they should be disabled.
   */
  private void setCursorData( final Long[] aCursorData, final boolean aCursorsEnabled )
  {
    this.dataContainer.setCursorEnabled( aCursorsEnabled );
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      this.dataContainer.setCursorPosition( i, aCursorData[i] );
    }
  }

  /**
   * Synchronizes the state of the actions to the current state of this host.
   */
  private void updateActions()
  {
    final boolean deviceControllerSet = this.currentDevCtrl != null;
    final boolean deviceCapturing = deviceControllerSet && this.currentDevCtrl.isCapturing();
    final boolean deviceSetup = deviceControllerSet && !deviceCapturing && this.currentDevCtrl.isSetup();

    getAction( CaptureAction.ID ).setEnabled( deviceControllerSet );
    getAction( CancelCaptureAction.ID ).setEnabled( deviceCapturing );
    getAction( RepeatCaptureAction.ID ).setEnabled( deviceSetup );

    final boolean projectChanged = this.projectManager.getCurrentProject().isChanged();
    final boolean projectSavedBefore = this.projectManager.getCurrentProject().getFilename() != null;
    final boolean dataAvailable = this.dataContainer.hasCapturedData();

    getAction( SaveProjectAction.ID ).setEnabled( projectChanged );
    getAction( SaveProjectAsAction.ID ).setEnabled( projectSavedBefore && projectChanged );
    getAction( SaveDataFileAction.ID ).setEnabled( dataAvailable );

    getAction( ZoomInAction.ID ).setEnabled( dataAvailable );
    getAction( ZoomOutAction.ID ).setEnabled( dataAvailable );
    getAction( ZoomDefaultAction.ID ).setEnabled( dataAvailable );
    getAction( ZoomFitAction.ID ).setEnabled( dataAvailable );

    final boolean triggerEnable = dataAvailable && this.dataContainer.hasTriggerData();
    getAction( GotoTriggerAction.ID ).setEnabled( triggerEnable );

    // Update the cursor actions accordingly...
    getAction( SetCursorModeAction.ID ).setEnabled( dataAvailable );
    getAction( SetCursorModeAction.ID ).putValue( Action.SELECTED_KEY,
        Boolean.valueOf( this.dataContainer.isCursorsEnabled() ) );

    final boolean enableCursors = dataAvailable && this.dataContainer.isCursorsEnabled();

    boolean anyCursorSet = false;
    for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
    {
      final boolean cursorPositionSet = this.dataContainer.isCursorPositionSet( c );
      anyCursorSet |= cursorPositionSet;

      final boolean gotoCursorNEnabled = enableCursors && cursorPositionSet;
      getAction( GotoNthCursorAction.getID( c ) ).setEnabled( gotoCursorNEnabled );

      final Action action = getAction( SetCursorAction.getCursorId( c ) );
      action.setEnabled( dataAvailable );
      action.putValue( Action.SELECTED_KEY, Boolean.valueOf( cursorPositionSet ) );
    }

    getAction( GotoFirstCursorAction.ID ).setEnabled( enableCursors && anyCursorSet );
    getAction( GotoLastCursorAction.ID ).setEnabled( enableCursors && anyCursorSet );

    getAction( ClearCursors.ID ).setEnabled( enableCursors && anyCursorSet );

    // Update the tools...
    final IManagedAction[] toolActions = this.actionManager.getActionByType( RunToolAction.class );
    for ( IManagedAction toolAction : toolActions )
    {
      toolAction.setEnabled( dataAvailable );
    }

    // Update the exporters...
    final IManagedAction[] exportActions = this.actionManager.getActionByType( ExportAction.class );
    for ( IManagedAction exportAction : exportActions )
    {
      exportAction.setEnabled( dataAvailable );
    }
  }
}
