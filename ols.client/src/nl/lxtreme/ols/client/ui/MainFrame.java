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
package nl.lxtreme.ols.client.ui;


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.client.ui.action.manager.*;
import nl.lxtreme.ols.client.ui.icons.*;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.toolwindow.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.WindowManager.Configurable;
import nl.lxtreme.ols.util.swing.WindowManager.UserSettings;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;

import com.jidesoft.docking.*;
import com.jidesoft.docking.DockingManager.TabbedPaneCustomizer;
import com.jidesoft.docking.event.*;
import com.jidesoft.swing.*;


/**
 * Denotes the frame of the main UI.
 */
public final class MainFrame extends DefaultDockableHolder implements PropertyChangeListener, Configurable
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
      repaint( 50L );
    }
  }

  /**
   * Provides a {@link AccumulatingRunnable} that repaints the entire main frame
   * once in a while. This is necessary if a tool produces lots of annotations
   * in a short time-frame, which would otherwise cause the UI to become slow
   * due to the many repaint requests.
   */
  final class AccumulatingUpdateActionsRunnable extends AccumulatingRunnable<Void>
  {
    /**
     * {@inheritDoc}
     */
    @Override
    protected void run( final Deque<Void> aArgs )
    {
      updateActionStates();
    }
  }

  /**
   * Provides a listener for cursor changes that reflects all changes to their
   * corresponding actions.
   */
  final class CursorActionListener implements IMarkerChangeListener
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorsInvisible()
    {
      scheduleActionsUpdate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorsVisible()
    {
      scheduleActionsUpdate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void markerAdded( final Marker aCursor )
    {
      scheduleActionsUpdate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void markerChanged( final String aPropertyName, final Marker aNewCursor )
    {
      // Nothing...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void markerMoved( final long aOldTimestamp, final long aNewTimestamp )
    {
      // Nothing...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void markerRemoved( final Marker aOldCursor )
    {
      scheduleActionsUpdate();
    }
  }

  /**
   * Listens to window-close events for our main frame, explicitly invoking code
   * to close it on all platforms.
   */
  static class MainFrameListener extends WindowAdapter
  {
    /**
     * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
     */
    @Override
    public void windowClosing( final WindowEvent aEvent )
    {
      final MainFrame mainFrame = ( MainFrame )aEvent.getSource();
      mainFrame.close();
    }
  }

  /**
   * Provides a {@link MouseWheelListener} that adapts its events to our own
   * {@link ZoomController}.
   */
  static class MouseWheelZoomAdapter implements MouseWheelListener
  {
    // VARIABLES

    private final SignalDiagramController controller;
    private final ZoomController zoomController;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MouseWheelZoomAdapter} instance.
     * 
     * @param aZoomController
     *          the zoom controller to adapt, cannot be <code>null</code>.
     */
    public MouseWheelZoomAdapter( final ZoomController aZoomController, final SignalDiagramController aController )
    {
      this.zoomController = aZoomController;
      this.controller = aController;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseWheelMoved( final MouseWheelEvent aEvent )
    {
      // Convert to the component under the mouse-cursor...
      Component view = this.controller.getSignalDiagram();
      Point newPoint = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

      // Dispatch the actual zooming to the zoom controller...
      this.zoomController.zoom( aEvent.getWheelRotation(), newPoint );

      aEvent.consume();
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

      doSetProgress( Math.max( 0, Math.min( 100, percentage.intValue() ) ) );
    }
  }

  /**
   * Provides a custom scrollpane that intercepts all {@link MouseWheelEvent}s
   * in order to differentiate between zoom-events and non-zooming events.
   * <p>
   * Idea based on: <a
   * href="http://tips4java.wordpress.com/2010/01/10/mouse-wheel-controller/"
   * >this blog posting</a>.
   * </p>
   */
  final class ZoomCapableScrollPane extends JScrollPane implements MouseWheelListener
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final MouseWheelZoomAdapter zoomAdapter;

    private volatile List<MouseWheelListener> originalListeners;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ZoomCapableScrollPane} instance.
     * 
     * @param aController
     *          the signal diagram controller to use, cannot be
     *          <code>null</code>.
     */
    public ZoomCapableScrollPane( final SignalDiagramController aController )
    {
      super( aController.getSignalDiagram() );

      this.zoomAdapter = new MouseWheelZoomAdapter( aController.getZoomController(), aController );

      setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
      setVerticalScrollBarPolicy( ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED );

      updateUI();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void addMouseWheelListener( final MouseWheelListener aListener )
    {
      lazyInitListeners();
      this.originalListeners.add( aListener );
      super.addMouseWheelListener( aListener );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseWheelMoved( final MouseWheelEvent aEvent )
    {
      // Intercept all events and check whether we've hit a zooming event...
      if ( isZoomEvent( aEvent ) )
      {
        this.zoomAdapter.mouseWheelMoved( aEvent );
      }
      else
      {
        // Not a zoom-event; just redispatch it to all "original" listeners...
        MouseWheelListener[] listeners;
        synchronized ( this.originalListeners )
        {
          listeners = this.originalListeners.toArray( new MouseWheelListener[this.originalListeners.size()] );
        }

        for ( MouseWheelListener listener : listeners )
        {
          listener.mouseWheelMoved( aEvent );
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void removeMouseWheelListener( final MouseWheelListener aListener )
    {
      lazyInitListeners();
      this.originalListeners.remove( aListener );
      super.removeMouseWheelListener( aListener );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setUI( final ScrollPaneUI aNewUI )
    {
      super.setUI( aNewUI );

      boolean installAdapter = true;

      synchronized ( this.originalListeners )
      {
        lazyInitListeners();

        this.originalListeners.clear();

        final MouseWheelListener[] listeners = getListeners( MouseWheelListener.class );
        for ( MouseWheelListener listener : listeners )
        {
          if ( listener == this )
          {
            installAdapter = false;
          }
          else
          {
            removeMouseWheelListener( listener );
            this.originalListeners.add( listener );
          }
        }
      }

      if ( installAdapter )
      {
        super.addMouseWheelListener( this );
      }
    }

    /**
     * @return the input modifier to distinguish between scroll events and zoom
     *         events.
     */
    private int getMouseWheelZoomModifier()
    {
      if ( Platform.isMacOS() )
      {
        return InputEvent.META_DOWN_MASK;
      }

      return InputEvent.CTRL_DOWN_MASK;
    }

    /**
     * @return <code>true</code> if the default mouse-wheel behavior is to zoom,
     *         <code>false</code> if the default mouse-wheel behavior is to
     *         scroll.
     */
    private boolean isMouseWheelZoomDefault()
    {
      return UIManager.getBoolean( UIManagerKeys.MOUSEWHEEL_ZOOM_DEFAULT );
    }

    /**
     * Tests whether the given {@link MouseWheelEvent} is a zooming event.
     * 
     * @param aEvent
     *          the {@link MouseWheelEvent} to test, may be <code>null</code>.
     * @return <code>true</code> if the given {@link MouseWheelEvent} is
     *         actually to be regarded as a zooming event, <code>false</code> if
     *         not.
     */
    private boolean isZoomEvent( final MouseWheelEvent aEvent )
    {
      if ( aEvent == null )
      {
        return false;
      }

      boolean invert = isMouseWheelZoomDefault();

      final int modifier = getMouseWheelZoomModifier();
      final int result = ( aEvent.getModifiersEx() & modifier );
      return invert ? ( result == 0 ) : ( result != 0 );
    }

    /**
     * Lazily initializes the listeners.
     */
    private void lazyInitListeners()
    {
      if ( this.originalListeners == null )
      {
        this.originalListeners = new ArrayList<MouseWheelListener>();
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ActionManager actionManager;
  private final DeviceController deviceController;
  private final ProgressUpdatingRunnable progressAccumulatingRunnable;
  private final AccumulatingRepaintingRunnable repaintAccumulatingRunnable;
  private final AccumulatingUpdateActionsRunnable updateActionsRunnable;
  private final JTextStatusBar status;

  private volatile boolean wasHidden = false;

  private volatile File dataStorage;
  private AcquisitionDetailsView captureDetails;

  private MarkerDetailsView cursorDetails;
  private MeasurementView measurementDetails;
  private AnnotationOverview annotationOverview;

  // CONSTRUCTORS

  /**
   * Creates a new {@link MainFrame} instance.
   */
  public MainFrame( final ActionManager aActionManager, final DeviceController aDeviceController )
  {
    super();

    this.actionManager = aActionManager;
    this.deviceController = aDeviceController;

    this.progressAccumulatingRunnable = new ProgressUpdatingRunnable();
    this.repaintAccumulatingRunnable = new AccumulatingRepaintingRunnable();
    this.updateActionsRunnable = new AccumulatingUpdateActionsRunnable();

    // Let the host platform determine where this diagram should be
    // displayed; gives it more or less a native feel...
    setLocationByPlatform( true );

    setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE );
    setSize( 1024, 600 );

    // Add the window icon...
    setIconImages( internalGetIconImages() );

    this.status = new JTextStatusBar();

    setContentPane( new JPanel( new BorderLayout() ) );

    // Support closing of this window on Windows/Linux platforms...
    addWindowListener( new MainFrameListener() );
  }

  // METHODS

  /**
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    // final String propertyName = aEvent.getPropertyName();
    // if ( "project".equals( propertyName ) )
    // {
    // Project project = ( Project )aEvent.getNewValue();
    //
    // updateWindowDecorations( project );
    // }
    // else if ( "capturedData".equals( propertyName ) )
    // {
    // updateWindowDecorations( this.controller.getCurrentProject() );
    // }

    scheduleActionsUpdate();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    // Detour: make sure the controller does this, so the actions are correctly
    // synchronized; make sure the OLS device is selected by default...
    this.deviceController.setSelectedDeviceName( aSettings.get( "selectedDevice", "OpenBench LogicSniffer" ) );
  }

  /**
   * @param aToolWindow
   */
  public final void registerToolWindow( final IToolWindow aToolWindow )
  {
    boolean defaultVisible = UIManager.getBoolean( UIManagerKeys.SHOW_TOOL_WINDOWS_DEFAULT );

    DockableFrame frame = new DockableFrame( aToolWindow.getName() );
    frame.getContentPane().add( ( Component )aToolWindow );
    frame.setDefaultEscapeAction( DockableFrame.ESCAPE_ACTION_DO_NOTING );
    frame.setVisible( defaultVisible );
    frame.getDockableAction().setEnabled( false );

    DockContext context = frame.getContext();
    context.setInitMode( DockContext.STATE_FRAMEDOCKED );
    context.setInitSide( DockContext.DOCK_SIDE_EAST );

    getDockingManager().addFrame( frame );
  }

  /**
   * Schedules an update of the actions.
   */
  public void scheduleActionsUpdate()
  {
    this.updateActionsRunnable.add( ( Void )null );
  }

  /**
   * Schedules a progress update.
   * 
   * @param aPercentage
   *          the percentage to set, cannot be <code>null</code>.
   */
  public final void scheduleProgressUpdate( final Integer aPercentage )
  {
    this.progressAccumulatingRunnable.add( aPercentage );
  }

  /**
   * Schedules a repaint of this entire component.
   */
  public final void scheduleRepaint()
  {
    this.repaintAccumulatingRunnable.add( ( Void )null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    String selectedDevice = this.deviceController.getSelectedDeviceName();
    if ( selectedDevice == null )
    {
      // We cannot put null values into the settings!
      selectedDevice = "";
    }
    aSettings.put( "selectedDevice", selectedDevice );
  }

  /**
   * Closes this main frame.
   * 
   * @param aExitApp
   *          <code>true</code> if the application should exit,
   *          <code>false</code> if it should keep running.
   */
  final void close()
  {
    File dataFile = new File( this.dataStorage, "dock.settings" );

    getDockingManager().saveLayoutDataToFile( dataFile.getAbsolutePath() );

    setVisible( false );
    dispose();
  }

  /**
   * Called by {@link Client} on the EDT when that component is stopped by Felix
   * DM.
   */
  final void destroy()
  {
    // Safety guard: also loop through all unclosed frames and close them
    // as well...
    final Window[] openWindows = Window.getWindows();
    for ( Window window : openWindows )
    {
      window.setVisible( false );
      window.dispose();
    }

    if ( this.dataStorage == null )
    {
      // Don't do anything when there's no file...
      return;
    }

    JErrorDialog.uninstallSwingExceptionHandler();
  }

  /**
   * Updates the progress bar to the given percentage.
   * 
   * @param aPercentage
   *          the percentage to set, >= 0 && <= 100.
   */
  final void doSetProgress( final int aPercentage )
  {
    this.status.setProgress( aPercentage );
  }

  /**
   * Sets the status bar message to the message given.
   * 
   * @param aMessage
   *          the message to set as status text;
   * @param aMessageArgs
   *          the (optional) message arguments.
   */
  final void doSetStatus( final String aMessage )
  {
    this.status.setText( aMessage );
    this.status.setProgress( 0 );
  }

  /**
   * Called by {@link Client} on the EDT when that component is started by Felix
   * DM.
   * 
   * @param aContext
   *          the bundle context to use;
   * @param aController
   *          the {@link SignalDiagramController} to use.
   */
  final void initialize( final BundleContext aContext, final SignalDiagramController aController )
  {
    final MenuBarFactory menuBarFactory = new MenuBarFactory();

    this.dataStorage = aContext.getDataFile( "" );

    // ensure that all changes to cursors are reflected in the UI...
    aController.addCursorChangeListener( new CursorActionListener() );
    aController.setDefaultSettings();

    // Ensure we've got a proper signal diagram to display...
    SignalDiagramComponent signalDiagram = new SignalDiagramComponent( aController );
    aController.setSignalDiagram( signalDiagram );

    setTitle( Platform.getFullName() );
    setJMenuBar( menuBarFactory.createMenuBar( this ) );

    DockingManager dm = getDockingManager();
    dm.setAutoDocking( true );
    dm.setUseGlassPaneEnabled( true );
    dm.setDockedFramesResizable( true );
    dm.setEasyTabDock( true );
    dm.setFloatingContainerType( DockingManager.FLOATING_CONTAINER_TYPE_WINDOW );
    dm.setGroupAllowedOnSidePane( true );
    dm.setHidable( false );
    dm.setInitSplitPriority( DockingManager.SPLIT_WEST_SOUTH_EAST_NORTH );
    dm.setLayoutDirectory( this.dataStorage.getName() );
    dm.setOutlineMode( DockingManager.HW_OUTLINE_MODE );
    dm.setShowContextMenu( true );
    dm.setShowGripper( false );
    dm.setShowWorkspace( true );
    dm.setSidebarRollover( true );
    dm.setUsePref( false );

    dm.addDockableFrameListener( new DockableFrameAdapter()
    {
      @Override
      public void dockableFrameFloating( final DockableFrameEvent aEvent )
      {
        // Show title bar for each floating dockable frame; this way, it can be
        // identified properly, and provides access to the context menu...
        aEvent.getDockableFrame().setShowTitleBar( true );
      }

      @Override
      public void dockableFrameDocked( final DockableFrameEvent aEvent )
      {
        // Hide the title bar for docked framed...
        aEvent.getDockableFrame().setShowTitleBar( false );
      }
    } );

    dm.setTabbedPaneCustomizer( new TabbedPaneCustomizer()
    {
      @Override
      public void customize( final JideTabbedPane tabbedPane )
      {
        tabbedPane.setShowIconsOnTab( false );
        tabbedPane.setShowCloseButton( false );
        tabbedPane.setUseDefaultShowIconsOnTab( false );
        tabbedPane.setUseDefaultShowCloseButtonOnTab( false );
      }
    } );

    // Start the layout of the docking frames...
    dm.loadLayoutData();

    this.cursorDetails = MarkerDetailsView.create( aController );
    registerToolWindow( this.cursorDetails );

    this.captureDetails = AcquisitionDetailsView.create( aController );
    registerToolWindow( this.captureDetails );

    this.measurementDetails = MeasurementView.create( aController );
    registerToolWindow( this.measurementDetails );

    this.annotationOverview = AnnotationOverview.create( aController );
    registerToolWindow( this.annotationOverview );

    Workspace workspace = dm.getWorkspace();
    workspace.add( new ZoomCapableScrollPane( aController ) );

    dm.resetToDefault();

    Container contentPane = getContentPane();
    contentPane.add( menuBarFactory.createToolBar(), BorderLayout.PAGE_START );
    contentPane.add( dm.getMainContainer(), BorderLayout.CENTER );
    contentPane.add( this.status, BorderLayout.PAGE_END );

    // Finalize the layout of the docking frames...
    File dataFile = new File( this.dataStorage, "dock.settings" );
    if ( ( this.dataStorage != null ) && dataFile.exists() )
    {
      dm.loadLayoutDataFromFile( dataFile.getAbsolutePath() );
    }
  }

  /**
   * Updates the states of the various actions.
   */
  void updateActionStates()
  {
    this.actionManager.updateActionStates();
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
   * Updates the title and any other window decorations for the current running
   * platform.
   * 
   * @param aProject
   *          the project to take the current properties from, can be
   *          <code>null</code>.
   */
  private void updateWindowDecorations( final String aProjectName, final boolean aChanged )
  {
    String title = Platform.getFullName();
    if ( ( aProjectName != null ) && !"".equals( aProjectName.trim() ) )
    {
      // Denote the project file in the title of the main window...
      title = title.concat( " :: " ).concat( aProjectName );
    }
    setTitle( title );

    getRootPane().putClientProperty( "Window.documentModified", Boolean.valueOf( aChanged ) );
  }
}
