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
import java.beans.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.cursor.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.client.signaldisplay.view.toolwindow.*;
import nl.lxtreme.ols.host.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.Configurable;
import nl.lxtreme.ols.util.swing.component.*;

import org.noos.xing.mydoggy.*;
import org.noos.xing.mydoggy.plaf.*;
import org.osgi.framework.*;


/**
 * Denotes the frame of the main UI.
 */
public final class MainFrame extends JFrame implements PropertyChangeListener, Configurable
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
  final class CursorActionListener implements ICursorChangeListener
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorAdded( final CursorElement aCursor )
    {
      scheduleActionsUpdate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorChanged( final String aPropertyName, final CursorElement aNewCursor )
    {
      // Nothing...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorMoved( final long aOldTimestamp, final long aNewTimestamp )
    {
      // Nothing...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cursorRemoved( final CursorElement aOldCursor )
    {
      scheduleActionsUpdate();
    }

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

    private final ZoomController zoomController;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MouseWheelZoomAdapter} instance.
     * 
     * @param aZoomController
     *          the zoom controller to adapt, cannot be <code>null</code>.
     */
    public MouseWheelZoomAdapter( final ZoomController aZoomController )
    {
      this.zoomController = aZoomController;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseWheelMoved( final MouseWheelEvent aEvent )
    {
      // Convert to the component under the mouse-cursor...
      final JComponent destination = SwingComponentUtils.getDeepestComponentAt( aEvent );
      final Point newPoint = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), destination );

      final int r = aEvent.getWheelRotation();
      if ( r < 0 )
      {
        this.zoomController.zoomIn( newPoint );
      }
      else if ( r > 0 )
      {
        this.zoomController.zoomOut( newPoint );
      }

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

      this.zoomAdapter = new MouseWheelZoomAdapter( aController.getZoomController() );

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
      if ( isMacOS() )
      {
        return InputEvent.META_DOWN_MASK;
      }

      return InputEvent.CTRL_DOWN_MASK;
    }

    /**
     * Returns whether the current host's operating system is Mac OS X.
     * 
     * @return <code>true</code> if running on Mac OS X, <code>false</code>
     *         otherwise.
     */
    private boolean isMacOS()
    {
      final String osName = System.getProperty( "os.name" );
      return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
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

  private static final long serialVersionUID = 1L;

  // CONSTANTS

  public static final String TW_ACQUISITION = AcquisitionDetailsView.ID;

  public static final String TW_MEASURE = MeasurementView.ID;
  public static final String TW_CURSORS = CursorDetailsView.ID;
  public static final String GROUP_DEFAULT = "Default";

  private final ActionManager actionManager;

  // VARIABLES

  private final DeviceController deviceController;
  private final HostProperties hostProperties;
  private final AtomicReference<MyDoggyToolWindowManager> windowManagerRef;
  private final ProgressUpdatingRunnable progressAccumulatingRunnable;
  private final AccumulatingRepaintingRunnable repaintAccumulatingRunnable;
  private final AccumulatingUpdateActionsRunnable updateActionsRunnable;
  private final JTextStatusBar status;

  private volatile boolean wasHidden = false;

  private volatile File dataStorage;
  private AcquisitionDetailsView captureDetails;

  private CursorDetailsView cursorDetails;
  private MeasurementView measurementDetails;
  private AnnotationOverview annotationOverview;

  /**
   * Creates a new {@link MainFrame} instance.
   */
  public MainFrame( final ActionManager aActionManager, final DeviceController aDeviceController,
      final HostProperties aHostProperties )
  {
    this.actionManager = aActionManager;
    this.deviceController = aDeviceController;
    this.hostProperties = aHostProperties;

    this.windowManagerRef = new AtomicReference<MyDoggyToolWindowManager>();
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

  // CONSTRUCTORS

  /**
   * @param aWindow
   */
  private static void tweakToolWindow( final ToolWindow aWindow )
  {
    RepresentativeAnchorDescriptor<?> anchorDesc = aWindow.getRepresentativeAnchorDescriptor();
    anchorDesc.setTitle( aWindow.getTitle() );
    anchorDesc.setPreviewEnabled( false );
    if ( aWindow.getIcon() != null )
    {
      anchorDesc.setIcon( aWindow.getIcon() );
    }

    final ToolWindowType[] types = ToolWindowType.values();
    for ( ToolWindowType type : types )
    {
      ToolWindowTypeDescriptor desc = aWindow.getTypeDescriptor( type );
      desc.setAnimating( false );
      desc.setAutoHide( false );
      desc.setEnabled( true );
      desc.setHideRepresentativeButtonOnVisible( false );
      desc.setIdVisibleOnTitleBar( false );
      desc.setTitleBarButtonsVisible( false );
      desc.setTitleBarVisible( false );
    }

    DockedTypeDescriptor desc = ( DockedTypeDescriptor )aWindow.getTypeDescriptor( ToolWindowType.DOCKED );
    desc.setPopupMenuEnabled( false );

    aWindow.setAvailable( true );
    aWindow.setHideOnZeroTabs( false );
    aWindow.setVisible( UIManager.getBoolean( UIManagerKeys.SHOW_TOOL_WINDOWS_DEFAULT ) );
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
   * @param aGroupName
   */
  public final void registerToolWindow( final IToolWindow aToolWindow, final String aGroupName )
  {
    MyDoggyToolWindowManager wm = getManager();

    ToolWindow tw = wm.registerToolWindow( aToolWindow.getId(), aToolWindow.getName(), aToolWindow.getIcon(),
        ( java.awt.Component )aToolWindow, ToolWindowAnchor.RIGHT );

    final ToolWindowGroup group = wm.getToolWindowGroup( aGroupName );
    group.setImplicit( false );
    group.addToolWindow( tw );

    tweakToolWindow( tw );
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
    setVisible( false );
    dispose();
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
   * @return the current tool window manager, cannot be <code>null</code>.
   */
  final MyDoggyToolWindowManager getManager()
  {
    return this.windowManagerRef.get();
  }

  /**
   * Called by Felix DM when starting this component.
   */
  final void startOnEDT( final BundleContext aContext, final SignalDiagramController aController )
  {
    // ensure that all changes to cursors are reflected in the UI...
    aController.addCursorChangeListener( new CursorActionListener() );
    aController.setDefaultSettings();

    // Ensure we've got a proper signal diagram to display...
    SignalDiagramComponent signalDiagram = new SignalDiagramComponent( aController );
    aController.setSignalDiagram( signalDiagram );

    final MyDoggyToolWindowManager wm = new MyDoggyToolWindowManager( Locale.getDefault(),
        MyDoggyToolWindowManager.class.getClassLoader() );
    wm.setDockableMainContentMode( false );

    // First one wins...
    this.windowManagerRef.compareAndSet( null, wm );

    this.cursorDetails = CursorDetailsView.create( aController );
    registerToolWindow( this.cursorDetails, GROUP_DEFAULT );

    this.captureDetails = AcquisitionDetailsView.create( aController );
    registerToolWindow( this.captureDetails, GROUP_DEFAULT );

    this.measurementDetails = MeasurementView.create( aController );
    registerToolWindow( this.measurementDetails, GROUP_DEFAULT );

    this.annotationOverview = AnnotationOverview.create( aController );
    registerToolWindow( this.annotationOverview, GROUP_DEFAULT );

    wm.setMainContent( new ZoomCapableScrollPane( aController ) );

    final MenuBarFactory menuBarFactory = new MenuBarFactory();

    setJMenuBar( menuBarFactory.createMenuBar() );

    Container contentPane = getContentPane();
    contentPane.add( menuBarFactory.createToolBar(), BorderLayout.PAGE_START );
    contentPane.add( wm, BorderLayout.CENTER );
    contentPane.add( this.status, BorderLayout.PAGE_END );

    setTitle( this.hostProperties.getFullName() );
    doSetStatus( String.format( "%s v%s ready ...", this.hostProperties.getShortName(),
        this.hostProperties.getVersion() ) );

    this.dataStorage = aContext.getDataFile( "" );

    File dataFile = new File( this.dataStorage, "dock.settings" );
    if ( ( this.dataStorage != null ) && dataFile.exists() )
    {
      FileInputStream fis = null;

      try
      {
        fis = new FileInputStream( dataFile );
        getManager().getPersistenceDelegate().apply( fis );
      }
      catch ( FileNotFoundException exception )
      {
        // Ignore; we shouldn't be here anyways due to dataFile.exists...
      }
      finally
      {
        IOUtil.closeResource( fis );
      }
    }

    // Make main frame visible on screen...
    setVisible( true );
  }

  /**
   * @param aContext
   */
  final void stopOnEDT()
  {
    setVisible( false );
    dispose();

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

    File dataFile = new File( this.dataStorage, "dock.settings" );
    FileOutputStream fos = null;

    try
    {
      fos = new FileOutputStream( dataFile );
      getManager().getPersistenceDelegate().save( fos );
    }
    catch ( FileNotFoundException exception )
    {
      // Ignore; we shouldn't be here anyways due to dataFile.exists...
    }
    finally
    {
      IOUtil.closeResource( fos );
    }

    JErrorDialog.uninstallSwingExceptionHandler();
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
  private void updateWindowDecorations( final Project aProject )
  {
    String title = this.hostProperties.getFullName();
    if ( ( aProject != null ) && ( aProject.getName() != null ) && !"".equals( aProject.getName().trim() ) )
    {
      // Denote the project file in the title of the main window...
      title = title.concat( " :: " ).concat( aProject.getName() );
    }
    setTitle( title );

    getRootPane().putClientProperty( "Window.documentModified", Boolean.valueOf( aProject.isChanged() ) );
  }
}
