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


import static nl.lxtreme.ols.acquisition.AcquisitionConstants.*;
import static nl.lxtreme.ols.client2.ClientConstants.*;
import static nl.lxtreme.ols.common.OlsConstants.*;
import static nl.lxtreme.ols.tool.api.ToolConstants.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.acquisition.AcquisitionService.DeviceState;
import nl.lxtreme.ols.client2.about.*;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.bundles.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.client2.menu.*;
import nl.lxtreme.ols.client2.platform.*;
import nl.lxtreme.ols.client2.prefs.*;
import nl.lxtreme.ols.client2.project.*;
import nl.lxtreme.ols.client2.usersettings.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.acquisitiondetails.*;
import nl.lxtreme.ols.client2.views.managed.annotations.*;
import nl.lxtreme.ols.client2.views.managed.cursors.*;
import nl.lxtreme.ols.client2.views.managed.measurement.*;
import nl.lxtreme.ols.client2.views.managed.outline.*;
import nl.lxtreme.ols.client2.views.managed.pulsecount.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.Unit.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.export.api.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.event.*;
import org.osgi.service.event.Event;

import com.jidesoft.docking.*;
import com.jidesoft.docking.DockingManager.TabbedPaneCustomizer;
import com.jidesoft.plaf.*;
import com.jidesoft.swing.*;


/**
 * Represents the main client.
 */
public class Client extends DefaultDockableHolder implements ApplicationCallback, Closeable, EventHandler
{
  // INNER TYPES

  /**
   * Denotes the direction in which a smart jump should be performed.
   */
  public static enum JumpDirection
  {
    LEFT, RIGHT;

    public boolean isLeft()
    {
      return this == LEFT;
    }

    public boolean isRight()
    {
      return this == RIGHT;
    }
  }

  /**
   * Denotes the type of jump that should be performed.
   */
  public static enum JumpType
  {
    CURSOR, SIGNAL_EDGE, ANNOTATION;
  }

  /**
   * Provides an {@link Action} for closing a {@link JOptionPane}.
   */
  static final class CloseOptionPaneAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final JOptionPane optionPane = ( JOptionPane )aEvent.getSource();
      optionPane.setValue( Integer.valueOf( JOptionPane.CLOSED_OPTION ) );
    }
  }

  /**
   * A runnable implementation that accumulates several calls to avoid an
   * avalanche of events on the EDT.
   */
  final class StatusUpdatingRunnable extends AccumulatingRunnable<Object>
  {
    @Override
    protected void run( final Deque<Object> aArgs )
    {
      for ( int i = 0, max = aArgs.size(); i < max; i++ )
      {
        Object value = aArgs.pop();
        if ( value instanceof Integer )
        {
          int percentage = ( ( Integer )value ).intValue();
          setProgress( percentage );
        }
        else if ( value instanceof String )
        {
          setStatus( ( String )value );
        }
        else if ( value instanceof Object[] )
        {
          Object[] array = ( Object[] )value;
          if ( array.length > 1 )
          {
            setStatus( array[0].toString(), Arrays.copyOfRange( array, 1, array.length ) );
          }
          else
          {
            setStatus( array[0].toString() );
          }
        }
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final int MODE_SNAP_CURSORS = 1;
  private static final int MODE_MEASUREMENT = 2;

  // VARIABLES

  private final List<ManagedAction> registeredActions;
  private final List<ManagedView> registeredViews;
  private final List<ViewController> viewControllers;
  private final StatusUpdatingRunnable statusUpdater;

  // Injected by Felix DM...
  private volatile EventAdmin eventAdmin;
  private volatile ActionManager actionManager;
  private volatile MenuManager menuManager;
  private volatile AcquisitionService acquisitionService;
  private volatile UserSettingProvider userSettingProvider;
  private volatile ProjectManager projectManager;
  private volatile ViewManager viewManager;

  // Locally managed...
  private volatile Bundle bundle;
  private volatile String selectedDeviceName;
  private volatile int mode;

  private JTabbedPane viewsPane;
  private JTextStatusBar status;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Client} instance.
   */
  public Client()
  {
    // Do not start if we're running headless...
    if ( GraphicsEnvironment.isHeadless() )
    {
      throw new RuntimeException( "Cannot start client: running headless." );
    }

    this.bundle = FrameworkUtil.getBundle( getClass() );

    this.registeredActions = new CopyOnWriteArrayList<ManagedAction>();
    this.registeredViews = new CopyOnWriteArrayList<ManagedView>();
    this.viewControllers = new CopyOnWriteArrayList<ViewController>();
    this.statusUpdater = new StatusUpdatingRunnable();

    this.mode = 0;
  }

  // METHODS

  /**
   * Starts a data acquisition for the current selected device.
   * 
   * @param aParent
   *          the parent window to use for showing the configuration dialog of
   *          the selected device.
   * @throws IOException
   *           in case of I/O problems while communicating with the device;
   * @throws IllegalStateException
   *           in case no device is selected.
   */
  public void acquireData( Window aParent ) throws IllegalStateException, IOException
  {
    if ( !isDeviceSelected() )
    {
      throw new IllegalStateException( "No device selected!" );
    }

    String deviceName = this.selectedDeviceName;

    try
    {
      Map<String, ? extends Serializable> config = this.acquisitionService.configureDevice( aParent, deviceName );
      if ( config != null )
      {
        this.acquisitionService.acquireData( config, deviceName );
      }
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Adds a given view controller to this client.
   * 
   * @param aController
   *          the controller to add, cannot be <code>null</code>.
   */
  public void addViewController( final ViewController aController )
  {
    if ( this.viewControllers.add( aController ) )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          String title = aController.getModel().getSessionName();
          BaseView view = aController.getView();

          viewsPane.add( title, view );
          viewsPane.setSelectedIndex( viewsPane.getTabCount() - 1 );
        }
      } );
    }
  }

  /**
   * @return <code>true</code> if cursors are visible, <code>false</code>
   *         otherwise.
   */
  public boolean areCursorsVisible()
  {
    AcquisitionData data = getAcquiredData();
    if ( data == null )
    {
      return false;
    }
    return data.areCursorsVisible();
  }

  /**
   * Cancels an ongoing acquisition.
   * 
   * @throws IOException
   *           in case of I/O problems while communicating with the device;
   * @throws IllegalStateException
   *           in case no device is selected.
   */
  public void cancelAcquisition() throws IllegalStateException, IOException
  {
    if ( !isDeviceSelected() )
    {
      throw new IllegalStateException( "No device selected!" );
    }

    String deviceName = this.selectedDeviceName;

    try
    {
      this.statusUpdater.add( -1, "Acquisition cancelled for " + deviceName + " ..." );

      this.acquisitionService.cancelAcquisition( deviceName );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * @return <code>true</code> if zooming is supported by the current view,
   *         <code>false</code> otherwise.
   */
  public boolean canZoomView()
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      return viewCtrl.canZoomView();
    }
    return false;
  }

  /**
   * Clears all current annotations for all channels.
   */
  public void clearAnnotations()
  {
    Session session = getCurrentSession();
    if ( session != null )
    {
      session.getAnnotationData().clearAll();
    }
  }

  /**
   * Clears all current annotations for the channel with the given index.
   * 
   * @param aChannelIdx
   *          the index of the channel to clear the annotations for, >= 0.
   */
  public void clearAnnotations( int aChannelIdx )
  {
    Session session = getCurrentSession();
    if ( session != null )
    {
      session.getAnnotationData().clear( aChannelIdx );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * Closes all available sessions.
   */
  public void closeAllSessions()
  {
    for ( ViewController viewCtrl : this.viewControllers )
    {
      viewCtrl.dispose();
    }

    updateManagedState();
  }

  /**
   * Closes the current session, if any is available.
   */
  public void closeCurrentSession()
  {
    Session session = getCurrentSession();
    if ( session != null )
    {
      session.close();
    }
    updateManagedState();
  }

  /**
   * Creates a new tool context for invoking a tool.
   * 
   * @return a new {@link ToolContext} instance, never <code>null</code>.
   */
  public ToolContext createToolContext()
  {
    return new ToolContext()
    {
      @Override
      public void addAnnotation( Annotation aAnnotation )
      {
        Session session = getCurrentSession();
        if ( session != null )
        {
          AnnotationData annotationData = session.getAnnotationData();
          annotationData.add( aAnnotation );
        }
      }

      @Override
      public void clearAnnotations( int... aChannelIdxs )
      {
        Session session = getCurrentSession();
        if ( session != null )
        {
          AnnotationData annotationData = session.getAnnotationData();
          for ( int channelIdx : aChannelIdxs )
          {
            annotationData.clear( channelIdx );
          }
        }
      }

      @Override
      public AcquisitionData getData()
      {
        Session session = getCurrentSession();
        return ( session != null ) ? session.getAcquiredData() : null;
      }
    };
  }

  /**
   * Exports the current acquired data to a file using the given exporter.
   * 
   * @param aExporter
   * @param aFile
   * @throws IOException
   */
  public void exportData( Exporter aExporter, File aFile ) throws IOException
  {
    try
    {
      FileOutputStream os = new FileOutputStream( aFile );

      aExporter.export( getAcquiredData(), getDockingManager().getMainContainer(), os );

      this.statusUpdater.add( String.format( "Export to %s succesful ...", aExporter.getName() ) );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Returns the current acquired data.
   * 
   * @return the current acquired data, can be <code>null</code> if no data is
   *         yet acquired.
   */
  public AcquisitionData getAcquiredData()
  {
    Session session = getCurrentSession();
    if ( session == null )
    {
      return null;
    }
    return session.getAcquiredData();
  }

  /**
   * @return the action manager, never <code>null</code>.
   */
  public ActionManager getActionManager()
  {
    return this.actionManager;
  }

  /**
   * Returns all available channels for the current acquired data.
   * 
   * @return an array with all channels, never <code>null</code>.
   */
  public Channel[] getAllChannels()
  {
    AcquisitionData data = getAcquiredData();
    if ( data == null )
    {
      return new Channel[0];
    }
    return data.getChannels();
  }

  /**
   * Returns all cursors, both defined and undefined.
   * 
   * @return an array with all cursors, never <code>null</code>.
   */
  public Cursor[] getAllCursors()
  {
    AcquisitionData data = getAcquiredData();
    if ( data == null )
    {
      return new Cursor[0];
    }
    return data.getCursors();
  }

  /**
   * Returns all defined cursors.
   * 
   * @return an array with all defined cursors, never <code>null</code>.
   */
  public Cursor[] getDefinedCursors()
  {
    AcquisitionData data = getAcquiredData();
    if ( data == null )
    {
      return new Cursor[0];
    }
    List<Cursor> result = new ArrayList<Cursor>();
    for ( Cursor cursor : data.getCursors() )
    {
      if ( cursor.isDefined() )
      {
        result.add( cursor );
      }
    }
    return result.toArray( new Cursor[result.size()] );
  }

  /**
   * Returns the state of a device.
   * 
   * @return the device state, or <code>null</code> if no device is selected.
   */
  public DeviceState getDeviceState()
  {
    if ( !isDeviceSelected() )
    {
      return null;
    }

    return this.acquisitionService.getState( this.selectedDeviceName );
  }

  /**
   * @return the filename of the current project, can be <code>null</code> in
   *         case the project is not yet saved.
   */
  public File getFile()
  {
    Session session = getCurrentSession();
    if ( session != null )
    {
      return session.getFile();
    }
    return null;
  }

  /**
   * @return the name of the selected device, can be <code>null</code>.
   */
  public String getSelectedDeviceName()
  {
    return this.selectedDeviceName;
  }

  /**
   * @return the number of open sessions, >= 0.
   */
  public int getSessionCount()
  {
    return this.viewControllers.size();
  }

  /**
   * Jumps to the cursor with the given index, if defined.
   * 
   * @param aIndex
   *          the index of the cursor to jump to.
   */
  public void gotoCursor( int aIndex )
  {
    Cursor cursor = null;

    if ( aIndex == Integer.MIN_VALUE )
    {
      // Search for first defined cursor...
      Cursor[] cursors = getDefinedCursors();
      if ( cursors.length > 0 )
      {
        cursor = cursors[0];
      }
    }
    else if ( aIndex == Integer.MAX_VALUE )
    {
      // Search for the last defined cursor...
      Cursor[] cursors = getDefinedCursors();
      if ( cursors.length > 0 )
      {
        cursor = cursors[cursors.length - 1];
      }
    }
    else if ( aIndex >= 0 && aIndex < OlsConstants.MAX_CURSORS )
    {
      // Jump to the given cursor...
      Cursor[] cursors = getAllCursors();
      if ( aIndex <= cursors.length && cursors[aIndex].isDefined() )
      {
        cursor = cursors[aIndex];
      }
    }

    if ( cursor != null )
    {
      scrollToRelativeTimestamp( cursor.getTimestamp() );
    }
  }

  /**
   * Jumps to the first defined cursor.
   */
  public void gotoFirstCursor()
  {
    gotoCursor( Integer.MIN_VALUE );
  }

  /**
   * Jumps to the last defined cursor.
   */
  public void gotoLastCursor()
  {
    gotoCursor( Integer.MAX_VALUE );
  }

  /**
   * Jumps to the trigger position.
   */
  public void gotoTrigger()
  {
    AcquisitionData data = getAcquiredData();
    if ( data != null && data.hasTriggerData() )
    {
      scrollToRelativeTimestamp( data.getTriggerPosition() );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleAbout()
  {
    showAboutBox( this );
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( Event aEvent )
  {
    String topic = aEvent.getTopic();
    // We're in charge of all tool-related events...
    if ( TOPIC_TOOL_STARTED.equals( topic ) )
    {
      String toolName = ( String )aEvent.getProperty( TTS_TOOL_NAME );

      this.statusUpdater.add( 0, new Object[] { "Tool %1$s started on %2$tF %2$tT...", toolName, new Date() } );

      setCursor( java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR ) );
    }
    else if ( TOPIC_TOOL_PROGRESS.equals( topic ) )
    {
      this.statusUpdater.add( ( Integer )aEvent.getProperty( TTP_PROGRESS ) );
    }
    else if ( TOPIC_TOOL_FINISHED.equals( topic ) )
    {
      String toolName = ( String )aEvent.getProperty( TTF_TOOL_NAME );
      Long executionTime = ( Long )aEvent.getProperty( TTF_EXECUTION_TIME );
      Exception exception = ( Exception )aEvent.getProperty( TTF_EXCEPTION );

      if ( exception == null )
      {
        // @formatter:off
        this.statusUpdater.add( 100, new Object[] { "Tool %s completed its task in %s.", toolName, Value.asTime( executionTime, Time.MS ) } );
        // @formatter:on
      }
      else
      {
        // @formatter:off
        this.statusUpdater.add( 100, new Object[] { "Tool %s failed with reason %s.", toolName, exception.getMessage() } );
        // @formatter:on
      }

      this.statusUpdater.add( 0 );

      setCursor( null );
    }
    else
    {
      if ( TOPIC_ACQUISITION_STARTED.equals( topic ) )
      {
        // @formatter:off
        this.statusUpdater.add( 0, new Object[] { "Acquisition started for %1$s on %2$tF %2$tT...", getSelectedDeviceName(), new Date() } );
        // @formatter:on

        updateManagedState();

        setCursor( java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR ) );
      }
      else if ( TOPIC_ACQUISITION_PROGRESS.equals( topic ) )
      {
        Integer percentage = ( Integer )aEvent.getProperty( TAP_PROGRESS );

        this.statusUpdater.add( percentage );
      }
      else if ( TOPIC_ACQUISITION_FINISHED.equals( topic ) )
      {
        Long executionTime = ( Long )aEvent.getProperty( TAC_EXECUTION_TIME );
        Exception exception = ( Exception )aEvent.getProperty( TAC_EXCEPTION );

        if ( exception != null )
        {
          // @formatter:off
          this.statusUpdater.add( 0, new Object[] { "Acquisition failed for %s, possible reason: %s", getSelectedDeviceName(), exception.getMessage() } );
          // @formatter:on
        }
        else
        {
          // @formatter:off
          this.statusUpdater.add( 0, new Object[] { "Acquisition ended for %s and took %s...", getSelectedDeviceName(), Value.asTime( executionTime, Time.MS ) } );
          // @formatter:on
        }

        updateManagedState();

        setCursor( null );
      }

      ViewController viewCtrl = getCurrentViewController();
      if ( viewCtrl != null )
      {
        viewCtrl.handleEvent( topic, aEvent );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handlePreferences()
  {
    showPreferencesDialog( this );
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleQuit()
  {
    handleShutdown();
    // We should always return OSX as to indicate that we're handling the Quit
    // operation ourselves. Otherwise, OSX will simply shutdown our JVM for
    // us...
    return false;
  }

  /**
   * Tries to shutdown this client, by stopping the entire framework. In case of
   * unsaved changes, for which the user does not acknowledge them to lose, this
   * method does nothing.
   */
  public void handleShutdown()
  {
    // TODO handle unsaved changed!
    close();

    if ( this.bundle != null )
    {
      try
      {
        Bundle systemBundle = this.bundle.getBundleContext().getBundle( 0L );
        systemBundle.stop();
        this.bundle = null;
      }
      catch ( BundleException exception )
      {
        // Ignore...
      }
    }
    else
    {
      System.exit( 0 );
    }
  }

  /**
   * Returns whether or not there is acquired data present.
   * 
   * @return <code>true</code> if there is data acquired, <code>false</code>
   *         otherwise.
   */
  public boolean hasAcquiredData()
  {
    return getAcquiredData() != null;
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
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> otherwise.
   */
  public boolean isChanged()
  {
    return false; // XXX
  }

  /**
   * @return <code>true</code> if cursors are to be snapped to the nearest
   *         signal transition, <code>false</code> if they can be placed freely.
   */
  public boolean isCursorSnapMode()
  {
    return ( this.mode & MODE_SNAP_CURSORS ) != 0;
  }

  /**
   * @return <code>true</code> if there is a device selected for acquisition,
   *         <code>false</code> otherwise.
   */
  public boolean isDeviceSelected()
  {
    return this.selectedDeviceName != null && !"".equals( this.selectedDeviceName.trim() );
  }

  /**
   * @return <code>true</code> if the current selected device is properly set
   *         up, <code>false</code> otherwise.
   */
  public boolean isDeviceSetup()
  {
    if ( !isDeviceSelected() )
    {
      return false;
    }
    return this.acquisitionService.isDeviceSetup( this.selectedDeviceName );
  }

  /**
   * @return <code>true</code> if measurement mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isMeasurementMode()
  {
    return ( this.mode & MODE_MEASUREMENT ) != 0;
  }

  /**
   * Opens a given file as data file.
   * 
   * @param aFile
   *          the file to load from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the given file.
   */
  public void openDataFile( File aFile ) throws IOException
  {
    FileReader reader = new FileReader( aFile );
    try
    {
      AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

      OlsDataHelper.read( reader, builder );
      reader.close();

      String name = getProjectName( aFile );

      postEvent( TOPIC_DATA_LOADED, TDL_NAME, name, TDL_FILE, aFile, TDL_DATA, builder.build() );

      this.statusUpdater.add( String.format( "Data file %s opened...", aFile.getName() ) );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Opens a given file as project file.
   * 
   * @param aFile
   *          the file to load from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the given file.
   */
  public void openProjectFile( File aFile ) throws IOException
  {
    try
    {
      this.projectManager.loadProject( aFile );

      this.statusUpdater.add( String.format( "Project file %s opened...", aFile.getName() ) );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Removes all cursors.
   */
  public void removeAllCursors()
  {
    try
    {
      for ( Cursor cursor : getDefinedCursors() )
      {
        cursor.clear();
      }
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Removes a given view controller from this client.
   * 
   * @param aController
   *          the controller to remove, cannot be <code>null</code>.
   */
  public void removeViewController( final ViewController aController )
  {
    if ( this.viewControllers.remove( aController ) )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          viewsPane.remove( aController.getView() );
        }
      } );
    }
  }

  /**
   * Repeats the acquiring of data for the current selected device.
   * 
   * @throws IOException
   *           in case of I/O problems while communicating with the device;
   * @throws IllegalStateException
   *           in case no device is selected or when it is not set up.
   */
  public void repeatAcquireData() throws IllegalStateException, IOException
  {
    if ( !isDeviceSelected() )
    {
      throw new IllegalStateException( "No device selected!" );
    }
    if ( !isDeviceSetup() )
    {
      throw new IllegalStateException( "Device not setup!" );
    }

    String deviceName = this.selectedDeviceName;
    try
    {
      this.acquisitionService.acquireData( deviceName );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Saves the current acquired data to the given file.
   * 
   * @param aFile
   *          the file to save to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the given file.
   */
  public void saveDataFile( File aFile ) throws IOException
  {
    Session session = getCurrentSession();
    if ( session == null )
    {
      return;
    }

    FileWriter writer = new FileWriter( aFile );
    try
    {
      OlsDataHelper.write( writer, session.getAcquiredData() );
      writer.close();
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Saves the current project to the given file.
   * 
   * @param aFile
   *          the file to save to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the given file.
   */
  public void saveProjectFile( File aFile ) throws IOException
  {
    try
    {
      this.projectManager.saveProject( aFile, getCurrentSession() );
    }
    finally
    {
      updateManagedState();
    }
  }

  /**
   * Scrolls the current view so that the given timestamp is visible.
   * 
   * @param aTimestamp
   *          the relative timestamp to scroll to, >= 0.
   */
  public void scrollToRelativeTimestamp( long aTimestamp )
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.scrollToTimestamp( aTimestamp );
    }
  }

  /**
   * Selects a device for acquiring data.
   * 
   * @param aDeviceName
   *          the name of the device to select, can be <code>null</code>.
   */
  public void selectDevice( String aDeviceName )
  {
    this.selectedDeviceName = aDeviceName;
  }

  /**
   * Sets whether or not the cursor snap mode is enabled.
   * 
   * @param aEnabled
   *          <code>true</code> if cursors should be snapped to the nearest
   *          signal transition, <code>false</code> if cursors can be placed
   *          freely.
   */
  public void setCursorSnapMode( boolean aEnabled )
  {
    if ( aEnabled )
    {
      this.mode |= MODE_SNAP_CURSORS;
    }
    else
    {
      this.mode &= ~MODE_SNAP_CURSORS;
    }

    postClientStateChangeEvent( TOPIC_CLIENT_STATE_MODE, "snapCursors", aEnabled );
    updateManagedState();
  }

  /**
   * Sets whether or not cursors are visible on screen.
   * 
   * @param aCursorsVisible
   *          <code>true</code> if cursors should become visible,
   *          <code>false</code> if cursors should become invisible.
   */
  public void setCursorsVisible( boolean aCursorsVisible )
  {
    AcquisitionData data = getAcquiredData();
    if ( data != null )
    {
      data.setCursorsVisible( aCursorsVisible );
    }

    postClientStateChangeEvent( TOPIC_CLIENT_STATE_MODE, "cursorsVisible", aCursorsVisible );
    updateManagedState();
    repaint();
  }

  /**
   * Sets whether or not measurement mode is enabled.
   * 
   * @param aEnabled
   *          <code>true</code> if measurement mode is to be enabled,
   *          <code>false</code> if measurement mode is disabled.
   */
  public void setMeasurementMode( boolean aEnabled )
  {
    if ( aEnabled )
    {
      this.mode |= MODE_MEASUREMENT;
    }
    else
    {
      this.mode &= ~MODE_MEASUREMENT;
    }

    postClientStateChangeEvent( TOPIC_CLIENT_STATE_MODE, "measurementMode", aEnabled );
    updateManagedState();
    repaint();
  }

  /**
   * Shows additional information about this client.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showAboutBox( Window aParent )
  {
    AboutBox aboutBox = new AboutBox( ClientConstants.SHORT_NAME, getVersion() );
    aboutBox.showDialog();
  }

  /**
   * Shows a dialog with the current set of bundles.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showBundlesDialog( Window aParent )
  {
    BundlesDialog bundlesDialog = new BundlesDialog( aParent );
    bundlesDialog.showDialog();
  }

  /**
   * Configures the channel groups for the current acquired data.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showChannelGroupManagerDialog( Window aParent )
  {
    // TODO
  }

  /**
   * Shows the preferences dialog for this client.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showPreferencesDialog( Window aParent )
  {
    PreferencesDialog preferencesDialog = new PreferencesDialog( aParent );
    preferencesDialog.showDialog();
  }

  /**
   * Performs a "smart" jump in a given direction.
   * 
   * @param aType
   *          what kind of jump to perform;
   * @param aDirection
   *          in what direction to jump.
   */
  public void smartJump( JumpType aType, JumpDirection aDirection )
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.smartJump( aType, aDirection );
    }
  }

  /**
   * Updates the state of all managed actions and views.
   */
  public void updateManagedState()
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        actionManager.updateState( Client.this );
      }
    } );
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.zoomAll();

      updateManagedState();
      repaint();
    }
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.zoomIn();

      updateManagedState();
      repaint();
    }
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.zoomOriginal();

      updateManagedState();
      repaint();
    }
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    ViewController viewCtrl = getCurrentViewController();
    if ( viewCtrl != null )
    {
      viewCtrl.zoomOut();

      updateManagedState();
      repaint();
    }
  }

  final void postClientStateChangeEvent( String aTopic )
  {
    postEvent( aTopic, CS_CONTROLLER, getCurrentViewController() );
  }

  final void postClientStateChangeEvent( String aTopic, String aKey, Object aValue )
  {
    postEvent( aTopic, CS_CONTROLLER, getCurrentViewController(), aKey, aValue );
  }

  /**
   * Register all client-specific actions.
   */
  final void registerManagedActions()
  {
    // File menu
    registerAction( new OpenAction() );
    registerAction( new SaveAction() );
    registerAction( new SaveAsAction() );

    // Capture menu
    registerAction( new AcquireDataAction() );
    registerAction( new RepeatAcquisitionAction() );
    registerAction( new CancelAcquisitionAction() );

    // Diagram menu
    registerAction( new ZoomInAction() );
    registerAction( new ZoomOutAction() );
    registerAction( new ZoomOriginalAction() );
    registerAction( new ZoomAllAction() );
    registerAction( new GotoTriggerAction() );
    registerAction( new GotoFirstCursorAction() );
    registerAction( new GotoLastCursorAction() );
    registerAction( new DeleteAllCursorsAction() );
    registerAction( new SetCursorsVisibleAction() );
    registerAction( new SetCursorSnapModeAction() );
    registerAction( new RemoveAnnotationsAction() );
    registerAction( new ShowChannelGroupDialogAction() );
    for ( int i = 0; i < OlsConstants.MAX_CURSORS; i++ )
    {
      registerAction( new GotoNthCursorAction( i ) );
    }

    // Tool menu
    registerAction( new SetMeasurementModeAction() );

    // Help menu
    registerAction( new ShowBundlesAction() );

    registerAction( new SmartJumpAction( JumpDirection.LEFT ) );
    registerAction( new SmartJumpAction( JumpDirection.RIGHT ) );

    if ( !Platform.isMacOS() )
    {
      // File menu
      registerAction( new ExitAction() );

      // Edit menu
      registerAction( new ShowPreferencesDialogAction() );

      // Help menu
      registerAction( new ShowAboutBoxAction() );
    }
  }

  /**
   * Register all managed views.
   */
  final void registerManagedViews()
  {
    registerView( new CursorDetailsView() );
    registerView( new AcquisitionDetailsView() );
    registerView( new MeasurementView() );
    registerView( new PulseCountView() );
    registerView( new ChannelOutlineView() );
    registerView( new AnnotationsView() );
  }

  /**
   * Updates the progress bar to the given percentage.
   * 
   * @param aPercentage
   *          the percentage to set, &gt;= 0 && &lt;= 100. If given a value &lt;
   *          0, the progress bar will be set in indeterminate mode.
   */
  final void setProgress( final int aPercentage )
  {
    this.status.setProgressBarIndeterminate( aPercentage < 0 );
    this.status.setProgress( aPercentage );
  }

  /**
   * Sets the status text to a given (formatted) message.
   * 
   * @param aMessage
   * @param aArguments
   */
  final void setStatus( final String aMessage, final Object... aArguments )
  {
    this.status.setText( String.format( aMessage, aArguments ) );
  }

  /**
   * Starts this client, should be called from the EDT.
   */
  final void startClient()
  {
    System.setProperty( "nl.lxtreme.ols.client.version", getVersion() );

    // Use the defined email address...
    System.setProperty( JErrorDialog.PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS, getReportIncidentAddress() );

    if ( Boolean.getBoolean( "nl.lxtreme.ols.client.debug" ) )
    {
      // Install a custom repaint manager that detects whether Swing
      // components are created outside the EDT; if so, it will yield a
      // stack trace to the offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    if ( Platform.isMacOS() )
    {
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
    else if ( Platform.isWindows() )
    {
      // Windows platform...
      UIManager.put( "Application.useSystemFontSettings", Boolean.TRUE );
      setLookAndFeel();
    }
    else
    {
      // All other platforms...
      UIManager.put( "Application.useSystemFontSettings", Boolean.FALSE );
      setLookAndFeel();
    }

    // Install the JIDE-specific extensions...
    LookAndFeelFactory.installJideExtension();

    setJMenuBar( this.menuManager.getMenuBar() );

    // Let the host platform determine where this diagram should be displayed;
    // gives it more or less a native feel...
    setLocationByPlatform( true );

    setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE );
    setSize( 1024, 600 );

    // Add the window icon...
    setIconImages( internalGetIconImages() );

    this.status = new JTextStatusBar();

    DockingManager dm = getDockingManager();
    dm.setAutoDocking( true );
    dm.setUseGlassPaneEnabled( true );
    dm.setDockedFramesResizable( true );
    dm.setFloatingFramesResizable( true );

    dm.setEasyTabDock( true );
    dm.setFloatingContainerType( DockingManager.FLOATING_CONTAINER_TYPE_DIALOG );
    dm.setInitSplitPriority( DockingManager.SPLIT_EAST_SOUTH_WEST_NORTH );
    dm.setDoubleClickAction( DockingManager.DOUBLE_CLICK_TO_AUTOHIDE );
    dm.setOutlineMode( DockingManager.HW_OUTLINE_MODE );

    dm.setGroupAllowedOnSidePane( true );
    dm.setHidable( false );
    dm.setShowContextMenu( true );
    dm.setShowGripper( false );
    dm.setShowWorkspace( true );
    dm.setShowTitleOnOutline( false );
    dm.setShowTitleBar( true );
    dm.setShowDividerGripper( true );
    dm.setSidebarRollover( true );
    dm.setUsePref( false );

    dm.setTabbedPaneCustomizer( new TabbedPaneCustomizer()
    {
      @Override
      public void customize( final JideTabbedPane aTabbedPane )
      {
        aTabbedPane.setShowIconsOnTab( false );
        aTabbedPane.setShowCloseButtonOnTab( false );
        aTabbedPane.setUseDefaultShowIconsOnTab( false );
        aTabbedPane.setUseDefaultShowCloseButtonOnTab( false );
      }
    } );

    this.viewsPane = new JTabbedPane();
    this.viewsPane.addMouseListener( new MouseAdapter()
    {
      public void mouseClicked( MouseEvent aEvent )
      {
        JTabbedPane pane = ( JTabbedPane )aEvent.getComponent();
        int idx = pane.getSelectedIndex();
        if ( idx >= 0 && idx < pane.getTabCount() )
        {
          Rectangle rect = pane.getUI().getTabBounds( pane, idx );
          if ( rect != null && rect.contains( aEvent.getPoint() ) && aEvent.getClickCount() == 2 )
          {
            getCurrentViewController().editSessionName();
          }
        }
      }
    } );
    this.viewsPane.addChangeListener( new ChangeListener()
    {
      @Override
      public void stateChanged( ChangeEvent aEvent )
      {
        postClientStateChangeEvent( TOPIC_CLIENT_STATE_VIEW_CHANGED );

        updateManagedState();
      }
    } );
    // Issue #208: ensure we can use the close buttons...
    addWindowListener( new WindowAdapter()
    {
      @Override
      public void windowClosing( WindowEvent aEvent )
      {
        // In case the user does not acknowledge the shut down, we do not close
        // the window...
        handleShutdown();
      }
    } );

    Workspace workspace = dm.getWorkspace();
    workspace.setAcceptDockableFrame( false );
    workspace.add( this.viewsPane );

    Container contentPane = getContentPane();
    contentPane.add( this.menuManager.getToolBar(), BorderLayout.PAGE_START );
    contentPane.add( dm.getMainContainer(), BorderLayout.CENTER );
    contentPane.add( this.status, BorderLayout.PAGE_END );

    // Load user settings...
    UserSettings settings = this.userSettingProvider.getSettings( getClass().getName() );
    loadUserSettings( settings );

    dm.resetToDefault();

    dm.activateWorkspace();

    updateManagedState();
    // Lastly, make ourselve visible on screen...
    setVisible( true );
  }

  /**
   * Stops this client, should be called from the EDT.
   */
  final void stopClient()
  {
    // Save user settings...
    UserSettings settings = this.userSettingProvider.getSettings( getClass().getName() );
    saveUserSettings( settings );

    // Closes this window...
    close();
  }

  final void unregisterClientActions()
  {
    for ( ManagedAction action : this.registeredActions )
    {
      this.actionManager.remove( action );
    }
    this.registeredActions.clear();
  }

  /**
   * Called by Felix DM when starting this component.
   * 
   * @param aComponent
   *          the component definition, never <code>null</code>.
   */
  protected void start( Component aComponent ) throws Exception
  {
    Platform.installApplicationCallback( this );

    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        registerManagedActions();
        registerManagedViews();

        startClient();
      }
    } );
  }

  /**
   * Called by Felix DM when stopping this component.
   * 
   * @param aComponent
   *          the component definition, never <code>null</code>.
   */
  protected void stop( Component aComponent ) throws Exception
  {
    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        unregisterClientActions();

        stopClient();
      }
    } );
  }

  /**
   * @return the current session, if available.
   */
  private Session getCurrentSession()
  {
    ViewController viewController = getCurrentViewController();
    if ( viewController != null )
    {
      ViewModel model = viewController.getModel();

      return model.getSession();
    }

    return null;
  }

  /**
   * @return the current view controller, if available.
   */
  private ViewController getCurrentViewController()
  {
    int idx = this.viewsPane.getSelectedIndex();
    if ( idx < 0 || idx >= this.viewControllers.size() )
    {
      return null;
    }

    return this.viewControllers.get( idx );
  }

  private String getProjectName( File aFile )
  {
    return SwingComponentUtils.stripFileExtension( aFile )[0];
  }

  /**
   * @return the version of this client, never <code>null</code>.
   */
  private String getReportIncidentAddress()
  {
    Dictionary<?, ?> headers = this.bundle.getHeaders();
    return ( String )headers.get( "X-ClientIncidentAddress" );
  }

  /**
   * @return the version of this client, never <code>null</code>.
   */
  private String getVersion()
  {
    Dictionary<?, ?> headers = this.bundle.getHeaders();
    return ( String )headers.get( "X-ClientVersion" );
  }

  /**
   * Creates a list of icon images that are used to decorate this frame.
   * 
   * @return a list of images, never <code>null</code>.
   */
  private List<? extends Image> internalGetIconImages()
  {
    final Image windowIcon16x16 = IconFactory.createImage( IconLocator.WINDOW_ICON_16x16 );
    final Image windowIcon32x32 = IconFactory.createImage( IconLocator.WINDOW_ICON_32x32 );
    final Image windowIcon48x48 = IconFactory.createImage( IconLocator.WINDOW_ICON_48x48 );
    final Image windowIcon64x64 = IconFactory.createImage( IconLocator.WINDOW_ICON_64x64 );
    final Image windowIcon256x256 = IconFactory.createImage( IconLocator.WINDOW_ICON_256x256 );
    return Arrays.asList( windowIcon16x16, windowIcon32x32, windowIcon48x48, windowIcon64x64, windowIcon256x256 );
  }

  /**
   * @param aSettings
   *          the settings for this client to load from, cannot be
   *          <code>null</code>.
   */
  private void loadUserSettings( UserSettings aSettings )
  {
    this.mode = aSettings.getInt( "mode", 0 );
    this.selectedDeviceName = aSettings.get( "selectedDevice", "OpenBench LogicSniffer" );
  }

  /**
   * @param aTopic
   * @param aProperties
   */
  private void postEvent( String aTopic, Object... aProperties )
  {
    Map<Object, Object> props = new HashMap<Object, Object>();
    for ( int i = 0; i < aProperties.length; i += 2 )
    {
      props.put( aProperties[i], aProperties[i + 1] );
    }

    this.eventAdmin.postEvent( new Event( aTopic, props ) );
  }

  /**
   * Registers a new {@link ManagedAction}.
   * 
   * @param aAction
   *          the action to register, cannot be <code>null</code>.
   */
  private void registerAction( ManagedAction aAction )
  {
    this.registeredActions.add( this.actionManager.add( aAction ) );
  }

  /**
   * Registers a new {@link ManagedView}.
   * 
   * @param aView
   *          the view to register, cannot be <code>null</code>.
   */
  private void registerView( ManagedView aView )
  {
    if ( this.registeredViews.add( this.viewManager.add( aView ) ) )
    {
      DockableFrame frame = new DockableFrame();

      aView.initialize( frame );

      getDockingManager().addFrame( frame );
    }
  }

  /**
   * @param settings
   *          the settings for this client to save to, cannot be
   *          <code>null</code>.
   */
  private void saveUserSettings( UserSettings settings )
  {
    settings.putInt( "mode", this.mode );
    settings.put( "selectedDevice", this.selectedDeviceName );
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
      System.err.printf( "[WARNING] Failed to set look and feel to '%s', possible cause: %s.%n", lafName,
          exception.getMessage() );
    }
  }
}
