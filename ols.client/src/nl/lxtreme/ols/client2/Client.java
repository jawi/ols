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
import java.io.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.client.api.Constants;
import nl.lxtreme.ols.client2.about.*;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.bundles.*;
import nl.lxtreme.ols.client2.colorscheme.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.client2.menu.*;
import nl.lxtreme.ols.client2.prefs.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;

import com.jidesoft.docking.*;
import com.jidesoft.docking.DockingManager.TabbedPaneCustomizer;
import com.jidesoft.swing.*;


/**
 * Represents the main client.
 */
public class Client extends DefaultDockableHolder implements Closeable, AcquisitionStatusListener,
    AcquisitionProgressListener
{
  // INNER TYPES

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
      setProgress( percentage.intValue() );
      updateActions();
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final int MODE_SNAP_CURSORS = 1;
  private static final int MODE_MEASUREMENT = 2;

  // VARIABLES

  private final List<ManagedAction> registeredActions;
  private final ProgressUpdatingRunnable progressUpdater;
  // Injected by Felix DM...
  private volatile ActionManager actionManager;
  private volatile MenuManager menuManager;
  private volatile SessionProvider sessionProvider;
  private volatile DataAcquisitionService acquisitionService;
  private volatile ColorSchemeProvider colorSchemeProvider;
  // Locally managed...
  private volatile String selectedDeviceName;
  private volatile int mode;

  private JTextStatusBar status;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Client} instance.
   */
  public Client()
  {
    this.registeredActions = new ArrayList<ManagedAction>();
    this.progressUpdater = new ProgressUpdatingRunnable();

    this.mode = 0;
  }

  // METHODS

  /**
   * Returns whether the current host's operating system is Mac OS X.
   * 
   * @return <code>true</code> if running on Mac OS X, <code>false</code>
   *         otherwise.
   */
  public static boolean isMacOS()
  {
    final String osName = System.getProperty( "os.name" );
    return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
  }

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

    Map<String, ? extends Serializable> config = this.acquisitionService.configureDevice( aParent, deviceName );
    if ( config != null )
    {
      this.acquisitionService.acquireData( config, deviceName );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionEnded( AcquisitionResultStatus aStatus )
  {
    if ( aStatus.isAborted() )
    {
      setStatus( "Acquisition aborted..." );
    }
    else if ( aStatus.isFailed() )
    {
      setStatus( "Acquisition failed for %s, possible reason: %s", getSelectedDeviceName(), aStatus.getMessage() );
    }
    else
    {
      setStatus( "Acquisition ended for %s and took %s...", getSelectedDeviceName(),
          Unit.Time.format( aStatus.getTime() / 1.0e3 ) );
    }

    setProgress( 100 );
    updateActions();
    setProgress( 0 );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionInProgress( int aPercentage )
  {
    this.progressUpdater.add( Integer.valueOf( aPercentage ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionStarted()
  {
    setStatus( "Acquisition started for %s", getSelectedDeviceName() );
    setProgress( 0 );
    updateActions();
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
    this.acquisitionService.cancelAcquisition( this.selectedDeviceName );
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
   * Configures the channel groups for the current acquired data.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void configureChannelGroups( Window aParent )
  {
    // TODO
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
        AnnotationData annotationData = getCurrentSession().getAnnotationData();
        annotationData.add( aAnnotation );
      }

      @Override
      public void clearAnnotations( int... aChannelIdxs )
      {
        AnnotationData annotationData = getCurrentSession().getAnnotationData();
        for ( int channelIdx : aChannelIdxs )
        {
          annotationData.clear( channelIdx );
        }
      }

      @Override
      public AcquisitionData getData()
      {
        return getCurrentSession().getAcquiredData();
      }
    };
  }

  /**
   * Exits this client.
   */
  public void exit()
  {
    close();

    Bundle thisBundle = FrameworkUtil.getBundle( getClass() );
    if ( thisBundle != null )
    {
      try
      {
        Bundle systemBundle = thisBundle.getBundleContext().getBundle( 0L );
        systemBundle.stop();
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
   * @return the filename of the current project, can be <code>null</code> in
   *         case the project is not yet saved.
   */
  public File getProjectFileName()
  {
    return null; // TODO
  }

  /**
   * @return the name of the selected device, can be <code>null</code>.
   */
  public String getSelectedDeviceName()
  {
    return this.selectedDeviceName;
  }

  /**
   * @return the version of this client, never <code>null</code>.
   */
  public String getVersion()
  {
    Dictionary<?, ?> headers = FrameworkUtil.getBundle( getClass() ).getHeaders();
    return ( String )headers.get( "X-ClientVersion" );
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
   * Tests whether or not an acquisition is in progress.
   * 
   * @return <code>true</code> if there is an acquisition in progress,
   *         <code>false</code> otherwise.
   */
  public boolean isAcquiring()
  {
    if ( !isDeviceSelected() )
    {
      return false;
    }
    return this.acquisitionService.isAcquiring( this.selectedDeviceName );
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

  public boolean isProjectChanged()
  {
    return false; // TODO
  }

  public void newProject()
  {
    // TODO
  }

  public void openDataFile( File aFile ) throws IOException
  {
    // TODO
  }

  public void openProjectFile( File aFile ) throws IOException
  {
    // TODO
  }

  /**
   * Removes all cursors.
   */
  public void removeAllCursors()
  {
    for ( Cursor cursor : getDefinedCursors() )
    {
      cursor.clear();
    }
    updateActions();
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
    this.acquisitionService.acquireData( this.selectedDeviceName );
  }

  public void saveDataFile( File aFile ) throws IOException
  {
    // TODO
  }

  public void saveProjectFile( String aProjectName, File aFile ) throws IOException
  {
    // TODO
  }

  /**
   * Scrolls the current view so that the given timestamp is visible.
   * 
   * @param aTimestamp
   *          the relative timestamp to scroll to, >= 0.
   */
  public void scrollToRelativeTimestamp( long aTimestamp )
  {
    // TODO
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
    updateActions();
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
    updateActions();
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
    updateActions();
  }

  /**
   * Updates the progress bar to the given percentage.
   * 
   * @param aPercentage
   *          the percentage to set, >= 0 && <= 100.
   */
  public void setProgress( final int aPercentage )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        status.setProgress( aPercentage );
      }
    } );
  }

  /**
   * Sets the status text to a given (formatted) message.
   * 
   * @param aMessage
   * @param aArguments
   */
  public void setStatus( final String aMessage, final Object... aArguments )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        status.setText( String.format( aMessage, aArguments ) );
      }
    } );
  }

  /**
   * Shows additional information about this client.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showAboutBox( Window aParent )
  {
    new AboutBox( Constants.SHORT_NAME, getVersion() ).showDialog();
  }

  /**
   * Shows a dialog with the current set of bundles.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showBundlesDialog( Window aParent )
  {
    new BundlesDialog( aParent ).showDialog();
  }

  /**
   * Shows the preferences dialog for this client.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void showPreferencesDialog( Window aParent )
  {
    if ( new PreferencesDialog( aParent, this.colorSchemeProvider ).showDialog() )
    {
      invalidate();
      repaint();
    }
  }

  /**
   * Updates the state of all actions.
   */
  public void updateActions()
  {
    this.actionManager.updateState( this );
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    // TODO
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    // TODO
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    // TODO
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    // TODO
  }

  /**
   * Register all client-specific actions.
   */
  final void registerClientActions()
  {
    // File menu
    registerAction( new NewProjectAction() );
    registerAction( new OpenProjectAction() );
    registerAction( new SaveProjectAction() );
    registerAction( new SaveProjectAsAction() );
    registerAction( new OpenDataFileAction() );
    registerAction( new SaveDataFileAction() );

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
    registerAction( new ConfigureChannelGroupsAction() );
    for ( int i = 0; i < OlsConstants.MAX_CURSORS; i++ )
    {
      registerAction( new GotoNthCursorAction( i ) );
    }

    // Tool menu
    registerAction( new SetMeasurementModeAction() );

    // Help menu
    registerAction( new ShowBundlesAction() );

    if ( !isMacOS() )
    {
      // File menu
      registerAction( new ExitAction() );

      // Edit menu
      registerAction( new ShowPreferencesDialogAction() );

      // Help menu
      registerAction( new HelpAboutAction() );
    }
  }

  /**
   * Starts this client.
   */
  final void startClient()
  {
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

    Workspace workspace = dm.getWorkspace();
    workspace.setAcceptDockableFrame( false );
    // workspace.add( new ZoomCapableScrollPane( signalDiagramController ) );

    Container contentPane = getContentPane();
    // contentPane.add( tools, BorderLayout.PAGE_START );
    contentPane.add( dm.getMainContainer(), BorderLayout.CENTER );
    contentPane.add( this.status, BorderLayout.PAGE_END );

    dm.resetToDefault();

    dm.activateWorkspace();

    updateActions();
    // Lastly, make ourselve visible on screen...
    setVisible( true );
  }

  /**
   * Stops this client.
   */
  final void stopClient()
  {
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
    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        registerClientActions();

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
    Session[] sessions = this.sessionProvider.getSessions();
    if ( sessions.length < 1 )
    {
      return null;
    }
    return sessions[sessions.length - 1];
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
   * Registers a new {@link ManagedAction}.
   * 
   * @param aAction
   *          the action to register, cannot be <code>null</code>.
   */
  private void registerAction( ManagedAction aAction )
  {
    this.registeredActions.add( this.actionManager.add( aAction ) );
  }
}
