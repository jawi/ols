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
import java.util.List;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
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
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * Denotes a front-end controller for the client.
 */
public final class ClientController implements ActionProvider, AcquisitionProgressListener, AcquisitionStatusListener,
    AcquisitionDataListener, AnnotationListener, ApplicationCallback
{
  // INNER TYPES

  /**
   * 
   */
  final class AccumulatingRepaintingRunnable extends AccumulatingRunnable<Void>
  {
    /**
     * {@inheritDoc}
     */
    @Override
    protected void run( final List<Void> aArgs )
    {
      repaintMainFrame();
    }
  }

  /**
   * Provides a default tool context implementation.
   */
  static final class DefaultToolContext implements ToolContext
  {
    // VARIABLES

    private final DataContainer data;
    private final int startSampleIdx;
    private final int endSampleIdx;

    // CONSTRUCTORS

    /**
     * Creates a new DefaultToolContext instance.
     * 
     * @param aStartSampleIdx
     *          the starting sample index;
     * @param aEndSampleIdx
     *          the ending sample index;
     * @param aData
     *          the acquisition result.
     */
    public DefaultToolContext( final int aStartSampleIdx, final int aEndSampleIdx, final DataContainer aData )
    {
      this.startSampleIdx = aStartSampleIdx;
      this.endSampleIdx = aEndSampleIdx;
      this.data = aData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannels()
    {
      return this.data.getChannels();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getCursorPosition( final int aIndex )
    {
      return this.data.getCursorPosition( aIndex );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AcquisitionResult getData()
    {
      return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getEnabledChannels()
    {
      return this.data.getEnabledChannels();
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

  /**
   * A runnable implementation that accumulates several calls to avoid an
   * avalanche of events on the EDT.
   */
  final class ProgressUpdatingRunnable extends AccumulatingRunnable<Integer>
  {
    /**
     * {@inheritDoc}
     */
    @Override
    protected void run( final List<Integer> aArgs )
    {
      final Integer percentage = aArgs.get( aArgs.size() - 1 );
      setProgressOnEDT( percentage.intValue() );
      updateActionsOnEDT();
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClientController.class.getName() );

  // VARIABLES

  private final OsgiHelper osgiHelper;

  private final BundleContext bundleContext;
  private final IActionManager actionManager;

  private final EventListenerList evenListeners;
  private final ProgressUpdatingRunnable progressAccumulatingRunnable;
  private final AccumulatingRepaintingRunnable repaintAccumulatingRunnable;

  private DataContainer dataContainer;

  private volatile ProjectManager projectManager;
  private volatile DataAcquisitionService dataAcquisitionService;
  private volatile MainFrame mainFrame;
  private volatile String selectedDevice;
  private volatile HostProperties hostProperties;

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
  public ClientController( final BundleContext aBundleContext )
  {
    this.bundleContext = aBundleContext;

    this.osgiHelper = new OsgiHelper( aBundleContext );

    this.evenListeners = new EventListenerList();
    this.actionManager = ActionManagerFactory.createActionManager( this );

    this.progressAccumulatingRunnable = new ProgressUpdatingRunnable();
    this.repaintAccumulatingRunnable = new AccumulatingRepaintingRunnable();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionComplete( final AcquisitionResult aData )
  {
    setAcquisitionResult( aData );

    updateActionsOnEDT();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionEnded( final AcquisitionResultStatus aStatus )
  {
    if ( aStatus.isAborted() )
    {
      setStatusOnEDT( "Capture aborted! {0}", aStatus.getMessage() );
    }
    else if ( aStatus.isFailed() )
    {
      setStatusOnEDT( "Capture failed! {0}", aStatus.getMessage() );
    }
    else
    {
      setStatusOnEDT( "Capture finished at {0,date,medium} {0,time,medium}.", new Date() );
    }

    updateActionsOnEDT();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionInProgress( final int aPercentage )
  {
    this.progressAccumulatingRunnable.add( Integer.valueOf( aPercentage ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionStarted()
  {
    updateActionsOnEDT();
  }

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
      SwingComponentUtils.invokeOnEDT( new Runnable()
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
   * @see nl.lxtreme.ols.client.IClientController#cancelCapture()
   */
  public void cancelCapture()
  {
    final DataAcquisitionService acquisitionService = getDataAcquisitionService();
    if ( acquisitionService != null )
    {
      acquisitionService.cancelAcquisition();
    }

    updateActionsOnEDT();
  }

  /**
   * {@inheritDoc}
   */
  public boolean captureData( final Window aParent )
  {
    final DataAcquisitionService acquisitionService = getDataAcquisitionService();
    final Device device = getDevice();
    if ( ( device == null ) || ( acquisitionService == null ) )
    {
      return false;
    }

    try
    {
      if ( device.setupCapture( aParent ) )
      {
        setStatusOnEDT( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", device.getName(), new Date() );

        acquisitionService.acquireData( device );
        return true;
      }

      return false;
    }
    catch ( final IOException exception )
    {
      setStatusOnEDT( "I/O problem: " + exception.getMessage() );

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        exception.printStackTrace();
      }

      return false;
    }
    finally
    {
      updateActionsOnEDT();
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

    updateActionsOnEDT();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations()
  {
    for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
    {
      clearAnnotations( i );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations( final int aChannelIdx )
  {
    this.dataContainer.clearChannelAnnotations( aChannelIdx );
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

    updateActionsOnEDT();
  }

  /**
   * {@inheritDoc}
   */
  public void exit()
  {
    try
    {
      // Stop the framework bundle; which should stop all other bundles as
      // well; the STOP_TRANSIENT option ensures the bundle is restarted the
      // next time...
      this.bundleContext.getBundle( 0 ).stop( Bundle.STOP_TRANSIENT );
    }
    catch ( final IllegalStateException ex )
    {
      LOG.warning( "Bundle context no longer valid while shutting down client?!" );

      // The bundle context is no longer valid; we're going to exit anyway, so
      // lets ignore this exception for now...
      System.exit( -1 );
    }
    catch ( final BundleException be )
    {
      LOG.warning( "Bundle context no longer valid while shutting down client?!" );

      System.exit( -1 );
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

      setStatusOnEDT( "Export to {0} succesful ...", aExporterName );
    }
    finally
    {
      HostUtils.closeResource( writer );

      updateActionsOnEDT();
    }
  }

  /**
   * @see nl.lxtreme.ols.client.ActionProvider#getAction(java.lang.String)
   */
  @Override
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
  public final Device getDevice()
  {
    if ( this.selectedDevice != null )
    {
      return getDeviceController( this.selectedDevice );
    }

    return null;
  }

  /**
   * Returns all available device controllers.
   * 
   * @return an array of device controller names, never <code>null</code>, but
   *         an empty array is possible.
   */
  public String[] getDeviceNames()
  {
    return getAllServiceNamesFor( Device.class );
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
   * Returns all available exporters.
   * 
   * @return an array of exporter names, never <code>null</code>, but an empty
   *         array is possible.
   */
  public String[] getExporterNames()
  {
    return getAllServiceNamesFor( Exporter.class );
  }

  /**
   * {@inheritDoc}
   */
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
   * Returns the current value of hostProperties.
   * 
   * @return the host properties, can be <code>null</code>.
   */
  public HostProperties getHostProperties()
  {
    return this.hostProperties;
  }

  /**
   * {@inheritDoc}
   */
  public File getProjectFilename()
  {
    return this.projectManager.getCurrentProject().getFilename();
  }

  /**
   * Returns all available tools.
   * 
   * @return an array of tool names, never <code>null</code>, but an empty array
   *         is possible.
   */
  public String[] getToolNames()
  {
    return getAllServiceNamesFor( Tool.class );
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
  @Override
  public final boolean handleAbout()
  {
    showAboutBox();
    return true;
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
   * {@inheritDoc}
   */
  public boolean isDeviceSelected()
  {
    return this.selectedDevice != null;
  }

  /**
   * {@inheritDoc}
   */
  public boolean isDeviceSetup()
  {
    return isDeviceSelected() && getDevice().isSetup();
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
  @Override
  public void onAnnotation( final Annotation<?> aAnnotation )
  {
    if ( aAnnotation instanceof DataAnnotation )
    {
      final DataAnnotation<?> dataAnnotation = ( DataAnnotation<?> )aAnnotation;

      this.dataContainer.addChannelAnnotation( dataAnnotation.getChannel(), dataAnnotation.getStartSampleIndex(),
          dataAnnotation.getEndSampleIndex(), dataAnnotation.getAnnotation() );

      // Accumulate repaint events to avoid an avalanche of events on the EDT...
      this.repaintAccumulatingRunnable.add( ( Void )null );
    }
    else
    {
      this.dataContainer.setChannelLabel( aAnnotation.getChannel(), aAnnotation.toString() );
    }
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

      setStatusOnEDT( "Capture data loaded from {0} ...", aFile.getName() );
    }
    finally
    {
      HostUtils.closeResource( reader );

      zoomToFit();

      updateActionsOnEDT();
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

      setStatusOnEDT( "Project {0} loaded ...", project.getName() );
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

    updateActionsOnEDT();
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
   * Removes the menu from the given provider from this controller, and does
   * this synchronously on the EDT.
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
        final JMenuBar menuBar = getMainMenuBar();
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
   * {@inheritDoc}
   */
  public boolean repeatCaptureData( final Window aParent )
  {
    final DataAcquisitionService acquisitionService = getDataAcquisitionService();
    final Device devCtrl = getDevice();
    if ( ( devCtrl == null ) || ( acquisitionService == null ) )
    {
      return false;
    }

    try
    {
      setStatusOnEDT( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", devCtrl.getName(), new Date() );

      acquisitionService.acquireData( devCtrl );

      return true;
    }
    catch ( final IOException exception )
    {
      setStatusOnEDT( "I/O problem: " + exception.getMessage() );

      exception.printStackTrace();

      // Make sure to handle IO-interrupted exceptions properly!
      HostUtils.handleInterruptedException( exception );

      return false;
    }
    finally
    {
      updateActionsOnEDT();
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

    final Tool<?> tool = getTool( aToolName );
    if ( tool == null )
    {
      JOptionPane.showMessageDialog( aParent, "No such tool found: " + aToolName, "Error ...",
          JOptionPane.ERROR_MESSAGE );
    }
    else
    {
      final ToolContext context = createToolContext();
      tool.invoke( aParent, context );
    }

    updateActionsOnEDT();
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

      setStatusOnEDT( "Capture data saved to {0} ...", aFile.getName() );
    }
    finally
    {
      HostUtils.closeResource( writer );
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

      setStatusOnEDT( "Project {0} saved ...", aName );
    }
    finally
    {
      HostUtils.closeResource( out );
    }
  }

  /**
   * {@inheritDoc}
   */
  public void selectDevice( final String aDeviceName )
  {
    this.selectedDevice = aDeviceName;
    // Make sure the action reflect the current situation...
    updateActionsOnEDT();
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

    updateActionsOnEDT();
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

    updateActionsOnEDT();
  }

  /**
   * @param aMainFrame
   *          the mainFrame to set
   */
  public void setMainFrame( final MainFrame aMainFrame )
  {
    if ( ( this.projectManager != null ) && ( aMainFrame != null ) )
    {
      this.projectManager.addPropertyChangeListener( aMainFrame );
    }

    this.mainFrame = aMainFrame;
  }

  /**
   * Shows the "about OLS" dialog on screen. the parent window to use, can be
   * <code>null</code>.
   */
  public void showAboutBox()
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.showAboutBox();
    }
  }

  /**
   * {@inheritDoc}
   */
  public void showBundlesDialog( final Window aOwner )
  {
    final BundlesDialog dialog = new BundlesDialog( aOwner, this.bundleContext );
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
      final DiagramLabelsDialog dialog = new DiagramLabelsDialog( aParent, this.dataContainer.getChannelLabels() );
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
      final ModeSettingsDialog dialog = new ModeSettingsDialog( aParent, getDiagramSettings() );
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
    final GeneralSettingsDialog dialog = new GeneralSettingsDialog( aParent, getDiagramSettings() );
    if ( dialog.showDialog() )
    {
      final DiagramSettings settings = dialog.getDiagramSettings();
      this.projectManager.getCurrentProject().setSettings( settings );
      diagramSettingsUpdated();
    }

    dialog.dispose();
  }

  /**
   * Called by the dependency manager when this component is about to be
   * started.
   */
  public void start()
  {
    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        // Cause exceptions to be shown in a more user-friendly way...
        JErrorDialog.installSwingExceptionHandler();

        final MainFrame mainFrame = MainFrame.createMainFrame( ClientController.this );
        setMainFrame( mainFrame );

        mainFrame.setVisible( true );

        LOG.info( "Client started ..." );
      }
    } );
  }

  /**
   * Called by the dependency manager when this component is about to be
   * stopped.
   */
  public void stop()
  {
    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        final MainFrame mainFrame = getMainFrame();
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

          setMainFrame( null );
        }

        JErrorDialog.uninstallSwingExceptionHandler();

        LOG.info( "Client stopped ..." );
      }
    } );
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

    updateActionsOnEDT();
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

    updateActionsOnEDT();
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

    updateActionsOnEDT();
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

    updateActionsOnEDT();
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
   * Returns the main menu bar.
   * 
   * @return the main menu bar, can be <code>null</code>.
   */
  final JMenuBar getMainMenuBar()
  {
    JMenuBar result = null;
    if ( this.mainFrame != null )
    {
      result = this.mainFrame.getJMenuBar();
    }
    return result;
  }

  /**
   * @param aProjectManager
   */
  final void removeProjectManager( final ProjectManager aProjectManager )
  {
    if ( ( this.projectManager != null ) && ( this.mainFrame != null ) )
    {
      this.projectManager.removePropertyChangeListener( this.mainFrame );
    }

    this.projectManager = null;
    this.dataContainer = null;
  }

  /**
   * Updates the progress on the EventDispatchThread (EDT).
   */
  final void setProgressOnEDT( final int aPercentage )
  {
    if ( this.mainFrame != null )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          ClientController.this.mainFrame.setProgress( aPercentage );
        }
      } );
    }
  }

  /**
   * Sets projectManager to the given value.
   * 
   * @param aProjectManager
   *          the projectManager to set.
   */
  final void setProjectManager( final ProjectManager aProjectManager )
  {
    if ( ( this.projectManager != null ) && ( this.mainFrame != null ) )
    {
      this.projectManager.removePropertyChangeListener( this.mainFrame );
    }
    this.projectManager = aProjectManager;
    this.dataContainer = new DataContainer( this.projectManager );
  }

  /**
   * @param aMessage
   * @param aMessageArgs
   */
  void setStatusOnEDT( final String aMessage, final Object... aMessageArgs )
  {
    if ( this.mainFrame != null )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          ClientController.this.mainFrame.setStatus( aMessage, aMessageArgs );
        }
      } );
    }
  }

  /**
   * Updates the actions on the EventDispatchThread (EDT).
   */
  final void updateActionsOnEDT()
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final DataAcquisitionService acquisitionService = getDataAcquisitionService();
        final Device device = getDevice();

        final boolean deviceControllerSet = ( device != null );
        final boolean deviceCapturing = ( acquisitionService != null ) && acquisitionService.isAcquiring();
        final boolean deviceSetup = deviceControllerSet && !deviceCapturing && device.isSetup();

        getAction( CaptureAction.ID ).setEnabled( deviceControllerSet );
        getAction( CancelCaptureAction.ID ).setEnabled( deviceCapturing );
        getAction( RepeatCaptureAction.ID ).setEnabled( deviceSetup );

        final boolean projectChanged = ClientController.this.projectManager.getCurrentProject().isChanged();
        final boolean projectSavedBefore = ClientController.this.projectManager.getCurrentProject().getFilename() != null;
        final boolean dataAvailable = ClientController.this.dataContainer.hasCapturedData();

        getAction( SaveProjectAction.ID ).setEnabled( projectChanged );
        getAction( SaveProjectAsAction.ID ).setEnabled( projectSavedBefore && projectChanged );
        getAction( SaveDataFileAction.ID ).setEnabled( dataAvailable );

        getAction( ZoomInAction.ID ).setEnabled( dataAvailable );
        getAction( ZoomOutAction.ID ).setEnabled( dataAvailable );
        getAction( ZoomDefaultAction.ID ).setEnabled( dataAvailable );
        getAction( ZoomFitAction.ID ).setEnabled( dataAvailable );

        final boolean triggerEnable = dataAvailable && ClientController.this.dataContainer.hasTriggerData();
        getAction( GotoTriggerAction.ID ).setEnabled( triggerEnable );

        // Update the cursor actions accordingly...
        getAction( SetCursorModeAction.ID ).setEnabled( dataAvailable );
        getAction( SetCursorModeAction.ID ).putValue( Action.SELECTED_KEY,
            Boolean.valueOf( ClientController.this.dataContainer.isCursorsEnabled() ) );

        final boolean enableCursors = dataAvailable && ClientController.this.dataContainer.isCursorsEnabled();

        boolean anyCursorSet = false;
        for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
        {
          final boolean cursorPositionSet = ClientController.this.dataContainer.isCursorPositionSet( c );
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
        final IManagedAction[] toolActions = ClientController.this.actionManager.getActionByType( RunToolAction.class );
        for ( final IManagedAction toolAction : toolActions )
        {
          toolAction.setEnabled( dataAvailable );
        }

        // Update the exporters...
        final IManagedAction[] exportActions = ClientController.this.actionManager.getActionByType( ExportAction.class );
        for ( final IManagedAction exportAction : exportActions )
        {
          exportAction.setEnabled( dataAvailable );
        }
      }
    } );
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

    return new DefaultToolContext( startOfDecode, endOfDecode, this.dataContainer );
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
   * @param aServiceClass
   * @return
   */
  private String[] getAllServiceNamesFor( final Class<?> aServiceClass )
  {
    final String[] result = this.osgiHelper.getAllServicePropertiesFor( Action.NAME, aServiceClass );
    Arrays.sort( result );
    return result;
  }

  /**
   * Returns the data acquisition service.
   * 
   * @return a data acquisition service, never <code>null</code>.
   */
  private DataAcquisitionService getDataAcquisitionService()
  {
    return this.dataAcquisitionService;
  }

  /**
   * {@inheritDoc}
   */
  private Device getDeviceController( final String aName ) throws IllegalArgumentException
  {
    return this.osgiHelper.getService( Device.class, Action.NAME, aName );
  }

  /**
   * {@inheritDoc}
   */
  private Exporter getExporter( final String aName ) throws IllegalArgumentException
  {
    return this.osgiHelper.getService( Exporter.class, Action.NAME, aName );
  }

  /**
   * {@inheritDoc}
   */
  private Tool<?> getTool( final String aName ) throws IllegalArgumentException
  {
    return this.osgiHelper.getService( Tool.class, Action.NAME, aName );
  }

  /**
   * Dispatches a request to repaint the entire main frame.
   */
  private void repaintMainFrame()
  {
    if ( this.mainFrame != null )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          ClientController.this.mainFrame.repaint();
        }
      } );
    }
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
      this.mainFrame.zoomDefault();
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

}
