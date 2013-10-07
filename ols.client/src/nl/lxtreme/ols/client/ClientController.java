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
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.api.*;
import nl.lxtreme.ols.client.api.Constants;
import nl.lxtreme.ols.client.appcallback.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.export.api.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;

import com.jidesoft.plaf.*;


/**
 * Denotes a front-end controller for the client.
 */
public final class ClientController implements ActionProvider, AcquisitionProgressListener, AcquisitionStatusListener,
    AcquisitionDataListener, ApplicationCallback
{
  // INNER TYPES

  /**
   * Provides a {@link AccumulatingRunnable} that repaints the entire main frame
   * once in a while. This is necessary if a tool produces lots of annotations
   * in a short time-frame, which would otherwise cause the UI to become slow
   * due to the many repaint requests.
   */
  final class AccumulatingRepaintingRunnable extends AccumulatingRunnable<Void>
  {
    /**
     * {@inheritDoc}
     */
    @Override
    protected void run( final Deque<Void> aArgs )
    {
      repaintMainFrame();
    }
  }

  /**
   * Provides an {@link Action} for closing a {@link JOptionPane}.
   */
  static final class CloseOptionPaneAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final JOptionPane optionPane = ( JOptionPane )aEvent.getSource();
      optionPane.setValue( Integer.valueOf( JOptionPane.CLOSED_OPTION ) );
    }
  }

  /**
   * Provides a listener for cursor changes that reflects all changes to their
   * corresponding actions.
   */
  final class CursorActionListener implements ICursorChangeListener
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorAdded( final Cursor aCursor )
    {
      updateActionsOnEDT();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorChanged( final String aPropertyName, final Cursor aOldCursor, final Cursor aNewCursor )
    {
      // Nothing...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorRemoved( final Cursor aOldCursor )
    {
      updateActionsOnEDT();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorsInvisible()
    {
      updateActionsOnEDT();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorsVisible()
    {
      updateActionsOnEDT();
    }
  }

  /**
   * Provides a default tool context implementation.
   */
  final class DefaultToolContext implements ToolContext
  {
    // VARIABLES

    private final SignalDiagramModel model;
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
    public DefaultToolContext( final int aStartSampleIdx, final int aEndSampleIdx, final SignalDiagramModel aModel )
    {
      this.startSampleIdx = aStartSampleIdx;
      this.endSampleIdx = aEndSampleIdx;
      this.model = aModel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addAnnotation( Annotation aAnnotation )
    {
      this.model.addAnnotation( aAnnotation );

      scheduleRepaintEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearAnnotations( int... aChannelIdxs )
    {
      this.model.clearAnnotations( aChannelIdxs );

      scheduleRepaintEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannels()
    {
      return getData().getChannelCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Cursor getCursor( final int aIndex )
    {
      Cursor[] cursors = getData().getCursors();
      if ( aIndex < 0 || aIndex >= cursors.length )
      {
        throw new IllegalArgumentException( "Invalid cursor index!" );
      }
      return cursors[aIndex];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AcquisitionData getData()
    {
      return this.model.getAcquisitionData();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getEnabledChannels()
    {
      return getData().getEnabledChannels();
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
    protected void run( final Deque<Integer> aArgs )
    {
      final Integer percentage = aArgs.getLast();
      setProgressOnEDT( percentage.intValue() );
      updateActionsOnEDT();
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClientController.class.getName() );

  // VARIABLES

  private final ActionManager actionManager;
  private final SignalDiagramController signalDiagramController;

  private final ConcurrentMap<String, Device> devices;
  private final ConcurrentMap<String, Tool<?>> tools;
  private final ConcurrentMap<String, Exporter> exporters;

  private final ProgressUpdatingRunnable progressAccumulatingRunnable;
  private final AccumulatingRepaintingRunnable repaintAccumulatingRunnable;

  private volatile ProjectManager projectManager;
  private volatile DataAcquisitionService dataAcquisitionService;
  private volatile MainFrame mainFrame;
  private volatile UIColorSchemeManager colorSchemeManager;

  private volatile long acquisitionStartTime;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ClientController} instance.
   */
  public ClientController()
  {
    this.devices = new ConcurrentHashMap<String, Device>();
    this.tools = new ConcurrentHashMap<String, Tool<?>>();
    this.exporters = new ConcurrentHashMap<String, Exporter>();

    this.actionManager = new ActionManager();

    this.signalDiagramController = new SignalDiagramController( this.actionManager );

    Runnable runner = new Runnable()
    {
      public void run()
      {
        ClientController.this.signalDiagramController.initialize();

        ActionManagerFactory.fillActionManager( ClientController.this.actionManager, ClientController.this );
      }
    };
    SwingComponentUtils.invokeOnEDT( runner );

    this.progressAccumulatingRunnable = new ProgressUpdatingRunnable();
    this.repaintAccumulatingRunnable = new AccumulatingRepaintingRunnable();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionComplete( final AcquisitionData aData )
  {
    try
    {
      getCurrentProject().setCapturedData( aData );
    }
    catch ( Exception exception )
    {
      exception.printStackTrace();
    }
    finally
    {
      updateActionsOnEDT();
    }
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
      long time = System.currentTimeMillis() - this.acquisitionStartTime;

      setStatusOnEDT( "Capture finished at {0,date,medium} {0,time,medium}, and took {1}.", new Date(),
          Unit.Time.format( time / 1.0e3 ) );
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
    this.acquisitionStartTime = System.currentTimeMillis();

    updateActionsOnEDT();
  }

  /**
   * Adds a given device to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to add, cannot be <code>null</code>.
   */
  public void addDevice( final Device aDevice )
  {
    if ( this.devices.putIfAbsent( aDevice.getName(), aDevice ) == null )
    {
      this.actionManager.add( new SelectDeviceAction( this, aDevice.getName() ) );
    }
  }

  /**
   * Adds a given exporter to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to add, cannot be <code>null</code>.
   */
  public void addExporter( final Exporter aExporter )
  {
    if ( this.exporters.putIfAbsent( aExporter.getName(), aExporter ) == null )
    {
      this.actionManager.add( new ExportAction( this, aExporter.getName() ) );
    }
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
        final JMenuBar menuBar = getMainMenuBar();

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
   * Adds a given tool to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aTool
   *          the tool to add, cannot be <code>null</code>.
   */
  public void addTool( final Tool<?> aTool )
  {
    if ( this.tools.putIfAbsent( aTool.getName(), aTool ) == null )
    {
      this.actionManager.add( new RunToolAction( this, aTool.getName(), aTool.getCategory() ) );
    }
  }

  /**
   * @see nl.lxtreme.ols.client.IClientController#cancelCapture()
   */
  public void cancelCapture()
  {
    final DataAcquisitionService acquisitionService = getDataAcquisitionService();
    final Device device = getDevice();
    if ( ( device == null ) || ( acquisitionService == null ) )
    {
      return;
    }

    try
    {
      acquisitionService.cancelAcquisition( device );
    }
    catch ( final IllegalStateException exception )
    {
      setStatusOnEDT( "No acquisition in progress!" );
    }
    catch ( final IOException exception )
    {
      setStatusOnEDT( "I/O problem: " + exception.getMessage() );
    }
    finally
    {
      updateActionsOnEDT();
    }
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

      return false;
    }
    finally
    {
      updateActionsOnEDT();
    }
  }

  /**
   * Creates a new project, causing all current data to be thrown away.
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

  public void clearAnnotations()
  {
    getSignalDiagramController().getSignalDiagramModel().getAnnotationData().clearAll();

    scheduleRepaintEvent();
  }

  /**
   * Terminates & shuts down the client.
   */
  public void exit()
  {
    // Close the main window ourselves; we should do this explicitly...
    if ( this.mainFrame != null )
    {
      // Do *not* call MainFrame#close() as it will call this method causing an
      // infinite loop...
      this.mainFrame.internalClose();
      this.mainFrame = null;
    }

    try
    {
      // Stop the framework bundle; which should stop all other bundles as
      // well; the STOP_TRANSIENT option ensures the bundle is restarted the
      // next time...
      Bundle thisBundle = getBundle();
      if ( thisBundle != null )
      {
        BundleContext bc = thisBundle.getBundleContext();
        bc.getBundle( 0 ).stop( Bundle.STOP_TRANSIENT );
      }
    }
    catch ( BundleException be )
    {
      LOG.log( Level.WARNING, "Bundle context no longer valid while shutting down client?!", be );
    }
  }

  /**
   * Exports the current data set to a file using an {@link Exporter} with a
   * given name.
   * 
   * @param aExporterName
   *          the name of the exporter to use, cannot be <code>null</code>;
   * @param aExportFile
   *          the file to export the results to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the export.
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
      exporter.export( getCurrentData(), this.mainFrame.getDiagramScrollPane(), writer );

      setStatusOnEDT( "Export to {0} succesful ...", aExporterName );
    }
    finally
    {
      try
      {
        if ( writer != null )
        {
          writer.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }

      updateActionsOnEDT();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Action getAction( final String aID )
  {
    return this.actionManager.getAction( aID );
  }

  /**
   * Provides direct access to the cursor with the given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor, >= 0 && <
   *          {@link OlsConstants#MAX_CURSORS}.
   * @return a cursor, never <code>null</code>.
   */
  public Cursor getCursor( final int aCursorIdx )
  {
    final AcquisitionData currentDataSet = getCurrentData();
    if ( currentDataSet == null )
    {
      return null;
    }
    Cursor[] cursors = currentDataSet.getCursors();
    if ( aCursorIdx < 0 || aCursorIdx >= cursors.length )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    return cursors[aCursorIdx];
  }

  /**
   * Returns the current selected device.
   * 
   * @return the selected device, can be <code>null</code> if no device is
   *         selected.
   */
  public Device getDevice()
  {
    if ( this.mainFrame != null )
    {
      final String selectedDevice = this.mainFrame.getSelectedDeviceName();
      if ( selectedDevice != null )
      {
        return getDevice( selectedDevice );
      }
    }

    return null;
  }

  /**
   * Returns all available devices.
   * 
   * @return an array of device names, never <code>null</code>, but an empty
   *         array is possible.
   */
  public String[] getDeviceNames()
  {
    List<String> result = new ArrayList<String>( this.devices.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * {@inheritDoc}
   */
  public Exporter getExporter( final String aName ) throws IllegalArgumentException
  {
    return this.exporters.get( aName );
  }

  /**
   * Returns all available exporters.
   * 
   * @return an array of exporter names, never <code>null</code>, but an empty
   *         array is possible.
   */
  public String[] getExporterNames()
  {
    List<String> result = new ArrayList<String>( this.exporters.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * Returns the "supported" export extensions for the exporter with the given
   * name.
   * 
   * @param aExporterName
   *          the name of the exporter to get the possible file extensions for,
   *          cannot be <code>null</code>.
   * @return an array of supported file extensions, never <code>null</code>.
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
   * Returns the current project's file name.
   * 
   * @return the project file name, can be <code>null</code> if it is never
   *         saved before.
   */
  public File getProjectFilename()
  {
    return getCurrentProject().getFilename();
  }

  /**
   * Returns the signal diagram controller.
   * 
   * @return a signal diagram controller, never <code>null</code>.
   */
  public SignalDiagramController getSignalDiagramController()
  {
    return this.signalDiagramController;
  }

  /**
   * Returns all available tools.
   * 
   * @return an array of tool names, never <code>null</code>, but an empty array
   *         is possible.
   */
  public String[] getToolNames()
  {
    List<String> result = new ArrayList<String>( this.tools.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * {@inheritDoc}
   */
  public void gotoTriggerPosition()
  {
    final AcquisitionData capturedData = getCurrentData();
    if ( ( capturedData != null ) && capturedData.hasTriggerData() )
    {
      final long position = capturedData.getTriggerPosition();
      this.mainFrame.gotoPosition( 0, position );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleAbout()
  {
    showAboutBox();
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handlePreferences()
  {
    showPreferencesDialog( getMainFrame() );
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleQuit()
  {
    LOG.fine( "Handling quit from app.menu..." );

    exit();
    // On Mac OS, it appears that if we acknowledge this event, the system
    // shuts down our application for us, thereby not calling our stop/shutdown
    // hooks... By returning false, we're not acknowledging the quit action to
    // the system, but instead do it all on our own...
    return false;
  }

  /**
   * Returns whether or not there's captured data to display.
   * 
   * @return <code>true</code> if there's captured data, <code>false</code>
   *         otherwise.
   */
  public boolean hasCapturedData()
  {
    return ( getCurrentData() != null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasPreferences()
  {
    return true;
  }

  /**
   * Runs the tool denoted by the given name.
   * 
   * @param aToolName
   *          the name of the tool to run, cannot be <code>null</code>;
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void invokeTool( final String aToolName, final Window aParent )
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
   * Returns whether or not a device is selected.
   * 
   * @return <code>true</code> if a device is selected, <code>false</code>
   *         otherwise.
   */
  public boolean isDeviceSelected()
  {
    return ( this.mainFrame != null ) && ( this.mainFrame.getSelectedDeviceName() != null );
  }

  /**
   * {@inheritDoc}
   */
  public boolean isDeviceSetup()
  {
    return isDeviceSelected() && getDevice().isSetup();
  }

  /**
   * Returns whether or not the current project is changed.
   * 
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> otherwise.
   */
  public boolean isProjectChanged()
  {
    final Project currentProject = getCurrentProject();
    if ( currentProject == null )
    {
      return false;
    }
    return currentProject.isChanged();
  }

  /**
   * Opens a given file as OLS-data file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openDataFile( final File aFile ) throws IOException
  {
    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( aFile );

      this.projectManager.loadDataFile( fis );

      setStatusOnEDT( "Capture data loaded from {0} ...", aFile.getName() );
    }
    finally
    {
      try
      {
        if ( fis != null )
        {
          fis.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }

      updateActionsOnEDT();
    }
  }

  /**
   * Opens a given file as OLS-project file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openProjectFile( final File aFile ) throws IOException
  {
    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( aFile );

      this.projectManager.loadProject( fis );

      final Project project = getCurrentProject();
      project.setFilename( aFile );

      setStatusOnEDT( "Project {0} loaded ...", project.getName() );
    }
    finally
    {
      try
      {
        if ( fis != null )
        {
          fis.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }
      updateActionsOnEDT();
    }
  }

  /**
   * Removes a given device from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to remove, cannot be <code>null</code>.
   */
  public void removeDevice( final Device aDevice )
  {
    synchronized ( this.devices )
    {
      final String deviceName = aDevice.getName();

      this.devices.remove( deviceName );

      try
      {
        IManagedAction action = this.actionManager.getAction( SelectDeviceAction.getID( deviceName ) );
        this.actionManager.remove( action );
      }
      catch ( IllegalArgumentException exception )
      {
        LOG.log( Level.FINE, "No action for device {}?!", deviceName );
      }
    }

    updateActionsOnEDT();
  }

  /**
   * Removes a given exporter from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to remove, cannot be <code>null</code>.
   */
  public void removeExporter( final Exporter aExporter )
  {
    synchronized ( this.exporters )
    {
      final String exporterName = aExporter.getName();

      this.exporters.remove( exporterName );

      try
      {
        IManagedAction action = this.actionManager.getAction( ExportAction.getID( exporterName ) );
        this.actionManager.remove( action );
      }
      catch ( IllegalArgumentException exception )
      {
        LOG.log( Level.FINE, "No action for exporter {}?!", exporterName );
      }
    }
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
   * Removes a given tool from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aTool
   *          the tool to remove, cannot be <code>null</code>.
   */
  public void removeTool( final Tool<?> aTool )
  {
    synchronized ( this.tools )
    {
      final String toolName = aTool.getName();

      this.tools.remove( toolName );

      try
      {
        IManagedAction action = this.actionManager.getAction( RunToolAction.getID( toolName ) );
        this.actionManager.remove( action );
      }
      catch ( IllegalArgumentException exception )
      {
        LOG.log( Level.FINE, "No action for tool {}?!", toolName );
      }
    }
  }

  /**
   * Restarts a new acquisition with the current device and with its current
   * settings.
   */
  public void repeatCaptureData()
  {
    final DataAcquisitionService acquisitionService = getDataAcquisitionService();
    final Device devCtrl = getDevice();

    if ( ( devCtrl == null ) || ( acquisitionService == null ) )
    {
      return;
    }

    try
    {
      setStatusOnEDT( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", devCtrl.getName(), new Date() );

      acquisitionService.acquireData( devCtrl );
    }
    catch ( final IOException exception )
    {
      setStatusOnEDT( "I/O problem: " + exception.getMessage() );
    }
    finally
    {
      updateActionsOnEDT();
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-data file
   * format.
   * 
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveDataFile( final File aFile ) throws IOException
  {
    FileOutputStream fos = null;
    try
    {
      fos = new FileOutputStream( aFile );

      this.projectManager.saveDataFile( fos );

      setStatusOnEDT( "Capture data saved to {0} ...", aFile.getName() );
    }
    finally
    {
      try
      {
        if ( fos != null )
        {
          fos.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }

      updateActionsOnEDT();
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-project
   * file format.
   * 
   * @param aName
   *          the name of the project to store, cannot be <code>null</code>;
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveProjectFile( final String aName, final File aFile ) throws IOException
  {
    FileOutputStream out = null;
    try
    {
      final Project project = getCurrentProject();
      project.setFilename( aFile );
      project.setName( aName );

      out = new FileOutputStream( aFile );
      this.projectManager.saveProject( out );

      setStatusOnEDT( "Project {0} saved ...", aName );
    }
    finally
    {
      try
      {
        if ( out != null )
        {
          out.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }

      updateActionsOnEDT();
    }
  }

  /**
   * Selects the device with the given name.
   * 
   * @param aDeviceName
   *          the name of the device to select, can be <code>null</code> to
   *          deselect the current selected device.
   */
  public void selectDevice( final String aDeviceName )
  {
    if ( this.mainFrame != null )
    {
      this.mainFrame.setSelectedDeviceName( aDeviceName );
    }
    // Make sure the action reflect the current situation...
    updateActionsOnEDT();
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
   * Shows a dialog with all current bundles.
   * 
   * @param aOwner
   *          the owning window to use, can be <code>null</code>.
   */
  public void showBundlesDialog( final Window aOwner )
  {
    final Bundle thisBundle = getBundle();
    if ( thisBundle != null )
    {
      final BundlesDialog dialog = new BundlesDialog( aOwner, thisBundle.getBundleContext() );
      if ( dialog.showDialog() )
      {
        dialog.dispose();
      }
    }
  }

  /**
   * Shows the global preferences dialog.
   * 
   * @param aParent
   *          the parent window of the dialog, can be <code>null</code>.
   */
  public void showPreferencesDialog( final Window aParent )
  {
    final PreferencesDialog dialog = new PreferencesDialog( aParent, this.colorSchemeManager );

    final Bundle thisBundle = getBundle();
    final DependencyManager dm = new DependencyManager( thisBundle.getBundleContext() );
    final Component comp = dm.createComponent();

    comp.setImplementation( dialog ).add( dm.createServiceDependency() //
        .setService( ConfigurationAdmin.class ) //
        .setInstanceBound( true ) //
        .setRequired( true ) ) //
        .addStateListener( new ComponentStateAdapter()
        {
          @Override
          public void started( final Component aComponent )
          {
            if ( dialog.showDialog() )
            {
              // Update the default settings (if needed)...
              updateDefaultSettings();

              // Ensure all UI-related changes are immediately visible...
              repaintMainFrame();
            }

            // All changes are persisted automatically...
            dm.remove( comp );
          }
        } );

    dm.add( comp );
  }

  /**
   * Called by the dependency manager when this component is about to be
   * started.
   */
  public final void start()
  {
    final String version = getVersion();
    initOSSpecifics( Constants.SHORT_NAME, getVersion() );

    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        // Cause exceptions to be shown in a more user-friendly way...
        JErrorDialog.installSwingExceptionHandler();

        Bundle bundle = FrameworkUtil.getBundle( ClientController.class );
        File dataStorage = bundle.getBundleContext().getDataFile( "" );

        final MainFrame mf = new MainFrame( dataStorage, ClientController.this );
        setMainFrame( mf );

        // ensure that all changes to cursors are reflected in the UI...
        ClientController.this.signalDiagramController.addCursorChangeListener( new CursorActionListener() );
        updateDefaultSettings();

        mf.setTitle( Constants.FULL_NAME );
        mf.setStatus( "{0} v{1} ready ...", Constants.SHORT_NAME, version );
        mf.setVisible( true );

        LOG.info( "Client started ..." );
      }
    } );
  }

  /**
   * Called by the dependency manager when this component is about to be
   * stopped.
   */
  public final void stop()
  {
    this.devices.clear();
    this.tools.clear();
    this.exporters.clear();

    // Make sure we're running on the EDT to ensure the Swing threading model is
    // correctly defined...
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        final MainFrame mf = getMainFrame();
        if ( mf != null )
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
          // release all resources...
          mf.internalClose();
          // Make sure the event listeners are deregistered...
          removeMainFrame( mf );
        }

        JErrorDialog.uninstallSwingExceptionHandler();
      }
    } );

    try
    {
      // Flush all pending EDT events...
      SwingUtilities.invokeAndWait( new Runnable()
      {
        @Override
        public void run()
        {
          LOG.info( "Client stopped ..." );
        }
      } );
    }
    catch ( Exception exception )
    {
      // Ignore; hope for the best...
    }
  }

  /**
   * Returns the current data set.
   * 
   * @return the current data set, never <code>null</code>.
   */
  final AcquisitionData getCurrentData()
  {
    final Project currentProject = getCurrentProject();
    if ( currentProject == null )
    {
      return null;
    }
    return currentProject.getDataSet();
  }

  /**
   * Returns the current project.
   * 
   * @return the current project, never <code>null</code>.
   */
  final Project getCurrentProject()
  {
    if ( this.projectManager == null )
    {
      return null;
    }
    return this.projectManager.getCurrentProject();
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
   * @return the version of this client, never <code>null</code>.
   */
  final String getReportIncidentAddress()
  {
    Dictionary<?, ?> headers = getBundle().getHeaders();
    return ( String )headers.get( "X-ClientIncidentAddress" );
  }

  /**
   * @return the version of this client, never <code>null</code>.
   */
  final String getVersion()
  {
    Dictionary<?, ?> headers = getBundle().getHeaders();
    return ( String )headers.get( "X-ClientVersion" );
  }

  /**
   * @param aMainFrame
   *          the main frame to remove, cannot be <code>null</code>.
   */
  final void removeMainFrame( final MainFrame aMainFrame )
  {
    if ( this.projectManager != null )
    {
      this.projectManager.removePropertyChangeListener( aMainFrame );
    }

    this.mainFrame = null;
  }

  /**
   * Called by the dependency manager when the project manager service is going
   * away.
   * 
   * @param aProjectManager
   *          the old project manager to remove.
   */
  final void removeProjectManager( final ProjectManager aProjectManager )
  {
    if ( this.signalDiagramController != null )
    {
      aProjectManager.removePropertyChangeListener( this.signalDiagramController );
    }
    if ( this.mainFrame != null )
    {
      aProjectManager.removePropertyChangeListener( this.mainFrame );
    }

    this.projectManager = null;
  }

  /**
   * @param aMainFrame
   *          the main frame to set, cannot be <code>null</code>.
   */
  final void setMainFrame( final MainFrame aMainFrame )
  {
    this.mainFrame = aMainFrame;

    if ( this.projectManager != null )
    {
      this.projectManager.addPropertyChangeListener( this.mainFrame );
    }
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
   * Called by the dependency manager when a (new) project manager service
   * becomes available.
   * 
   * @param aProjectManager
   *          the projectManager to set.
   */
  final void setProjectManager( final ProjectManager aProjectManager )
  {
    this.projectManager = aProjectManager;

    if ( this.projectManager != null )
    {
      if ( this.signalDiagramController != null )
      {
        this.projectManager.addPropertyChangeListener( this.signalDiagramController );
      }
      if ( this.mainFrame != null )
      {
        this.projectManager.addPropertyChangeListener( this.mainFrame );
      }
    }
  }

  /**
   * Sets the given message + arguments as status message.
   * 
   * @param aMessage
   *          the message to set;
   * @param aMessageArgs
   *          the (optional) arguments of the message.
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

        final boolean projectChanged = isProjectChanged();
        final boolean projectSavedBefore = !isAnonymousProject();
        final boolean dataAvailable = hasCapturedData();
        final boolean hasTriggerData = hasTriggerData();
        final boolean cursorsEnabled = areCursorsEnabled();
        final boolean enableCursors = dataAvailable && cursorsEnabled;

        getAction( SaveProjectAction.ID ).setEnabled( projectChanged );
        getAction( SaveProjectAsAction.ID ).setEnabled( projectSavedBefore && projectChanged );
        getAction( SaveDataFileAction.ID ).setEnabled( dataAvailable );

        getAction( GotoTriggerAction.ID ).setEnabled( dataAvailable && hasTriggerData );

        // Update the cursor actions accordingly...
        getAction( SetCursorModeAction.ID ).setEnabled( dataAvailable );
        getAction( SetCursorModeAction.ID ).putValue( Action.SELECTED_KEY, Boolean.valueOf( cursorsEnabled ) );

        getAction( SmartJumpAction.getJumpLeftID() ).setEnabled( dataAvailable );
        getAction( SmartJumpAction.getJumpRightID() ).setEnabled( dataAvailable );

        boolean anyCursorSet = false;
        for ( int c = 0; c < OlsConstants.MAX_CURSORS; c++ )
        {
          final boolean cursorPositionSet = isCursorSet( c );
          anyCursorSet |= cursorPositionSet;

          final boolean gotoCursorNEnabled = enableCursors && cursorPositionSet;
          getAction( GotoNthCursorAction.getID( c ) ).setEnabled( gotoCursorNEnabled );
        }

        final boolean snapCursorMode = getSignalDiagramController().getSignalDiagramModel().isSnapCursorMode();
        getAction( SetCursorSnapModeAction.ID ).putValue( Action.SELECTED_KEY, Boolean.valueOf( snapCursorMode ) );

        getAction( GotoFirstCursorAction.ID ).setEnabled( enableCursors && anyCursorSet );
        getAction( GotoLastCursorAction.ID ).setEnabled( enableCursors && anyCursorSet );

        getAction( DeleteAllCursorsAction.ID ).setEnabled( enableCursors && anyCursorSet );
        getAction( RemoveAnnotationsAction.ID ).setEnabled( dataAvailable );

        getAction( SetMeasurementModeAction.ID ).setEnabled( dataAvailable );
        getAction( ShowManagerViewAction.ID ).setEnabled( dataAvailable );

        // Update the tools...
        final IManagedAction[] toolActions = getActionsByType( RunToolAction.class );
        for ( final IManagedAction toolAction : toolActions )
        {
          if ( !ToolCategory.OTHER.equals( ( ( RunToolAction )toolAction ).getCategory() ) )
          {
            toolAction.setEnabled( dataAvailable );
          }
        }

        // Update the exporters...
        final IManagedAction[] exportActions = getActionsByType( ExportAction.class );
        for ( final IManagedAction exportAction : exportActions )
        {
          exportAction.setEnabled( dataAvailable );
        }
      }
    } );
  }

  /**
   * Updates the default settings.
   */
  final void updateDefaultSettings()
  {
    this.signalDiagramController.setDefaultSettings();
  }

  /**
   * Returns whether or not the cursors are enabled.
   * 
   * @return <code>true</code> if cursors are enabled, <code>false</code>
   *         otherwise.
   */
  protected boolean areCursorsEnabled()
  {
    return this.signalDiagramController.getSignalDiagramModel().isCursorMode();
  }

  /**
   * Returns all actions of a given type.
   * 
   * @param aActionType
   *          the type of action to return, cannot be <code>null</code>.
   * @return an array of requested actions, never <code>null</code>.
   */
  protected IManagedAction[] getActionsByType( final Class<? extends IManagedAction> aActionType )
  {
    if ( this.actionManager == null )
    {
      return new IManagedAction[0];
    }
    return this.actionManager.getActionByType( aActionType );
  }

  /**
   * Returns whether or not there is trigger data available.
   * 
   * @return <code>true</code> if there is trigger data available,
   *         <code>false</code> otherwise.
   */
  protected boolean hasTriggerData()
  {
    final AcquisitionData currentDataSet = getCurrentData();
    if ( currentDataSet == null )
    {
      return false;
    }
    return currentDataSet.hasTriggerData();
  }

  /**
   * Returns whether or not the current project is "anonymous", i.e., not yet
   * saved.
   * 
   * @return <code>true</code> if the current project is anonymous,
   *         <code>false</code> otherwise.
   */
  protected boolean isAnonymousProject()
  {
    final Project currentProject = getCurrentProject();
    if ( currentProject == null )
    {
      return false;
    }
    return currentProject.getFilename() == null;
  }

  /**
   * Returns whether or not a cursor with the given index is set.
   * 
   * @param aCursorIdx
   *          the index of the cursor test.
   * @return <code>true</code> if the cursor with the given index is set,
   *         <code>false</code> otherwise.
   */
  protected boolean isCursorSet( final int aCursorIdx )
  {
    final Cursor cursor = getCursor( aCursorIdx );
    if ( cursor == null )
    {
      return false;
    }
    return cursor.isDefined();
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

    final AcquisitionData capturedData = getCurrentData();
    final SignalDiagramModel model = getSignalDiagramController().getSignalDiagramModel();

    if ( capturedData != null )
    {
      Cursor[] cursors = capturedData.getCursors();

      final int dataLength = capturedData.getValues().length;
      if ( areCursorsEnabled() )
      {
        if ( cursors.length > 1 && cursors[0].isDefined() )
        {
          final Cursor cursor1 = cursors[0];
          startOfDecode = capturedData.getSampleIndex( cursor1.getTimestamp() ) - 1;
        }
        if ( cursors.length > 2 && cursors[1].isDefined() )
        {
          final Cursor cursor2 = cursors[1];
          endOfDecode = capturedData.getSampleIndex( cursor2.getTimestamp() ) + 1;
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
    }

    return new DefaultToolContext( startOfDecode, endOfDecode, model );
  }

  private Bundle getBundle()
  {
    return FrameworkUtil.getBundle( getClass() );
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
  private Device getDevice( final String aName ) throws IllegalArgumentException
  {
    return this.devices.get( aName );
  }

  /**
   * {@inheritDoc}
   */
  private Tool<?> getTool( final String aName ) throws IllegalArgumentException
  {
    return this.tools.get( aName );
  }

  /**
   * Initializes the OS-specific stuff.
   * 
   * @param aApplicationName
   *          the name of the application (when this needs to be passed to the
   *          guest OS);
   * @param aVersion
   *          the version of the application.
   * @param aApplicationCallback
   *          the application callback used to report application events on some
   *          platforms (Mac OS), may be <code>null</code>.
   */
  private void initOSSpecifics( final String aApplicationName, String aVersion )
  {
    System.setProperty( "nl.lxtreme.ols.client.version", aVersion );

    // Use the defined email address...
    System.setProperty( JErrorDialog.PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS, getReportIncidentAddress() );

    if ( Activator.isDebugMode() )
    {
      // Install a custom repaint manager that detects whether Swing
      // components are created outside the EDT; if so, it will yield a
      // stack trace to the offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    final String osName = System.getProperty( "os.name" ).toLowerCase();
    if ( ( "mac os x".equals( osName ) || "darwin".equals( osName ) ) )
    {
      // Moves the main menu bar to the screen menu bar location...
      System.setProperty( "apple.laf.useScreenMenuBar", "true" );
      System.setProperty( "apple.awt.textantialiasing", "true" );
      System.setProperty( "apple.awt.graphics.EnableQ2DX", "true" );
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
    else if ( osName.indexOf( "win" ) >= 0 )
    {
      UIManager.put( "Application.useSystemFontSettings", Boolean.TRUE );
      setLookAndFeel();
    }
    else
    {
      UIManager.put( "Application.useSystemFontSettings", Boolean.FALSE );
      setLookAndFeel();
    }

    // Install the JIDE-specific extensions...
    LookAndFeelFactory.installJideExtension();
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
   * Sets the default L&F, assuming <tt>-Dswing.defaultlaf=...</tt> is defined.
   */
  private void setLookAndFeel()
  {
    final UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    // to make sure we always use system class loader
    defaults.put( "ClassLoader", Activator.class.getClassLoader() );

    String lafName = System.getProperty( "swing.defaultlaf", UIManager.getSystemLookAndFeelClassName() );

    try
    {
      UIManager.setLookAndFeel( lafName );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Failed to set look and feel to: " + lafName, exception );
    }
  }

  /**
   * Schedules a repaint event for the entire window.
   */
  void scheduleRepaintEvent()
  {
    // Accumulate repaint events to avoid an avalanche of events on the EDT...
    this.repaintAccumulatingRunnable.add( ( Void )null );
  }
}
