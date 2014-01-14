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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.ClientConstants.*;
import static nl.lxtreme.ols.client2.views.UIMgr.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client2.Client.JumpDirection;
import nl.lxtreme.ols.client2.Client.JumpType;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.platform.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.MeasurementInfoBuilder.MeasurementInfo;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.event.Event;


/**
 * Provides a waveform view of {@link AcquisitionData}, in which data is shown
 * graphically as rows.
 */
public class WaveformView extends BaseView
{
  // INNER TYPES

  /**
   * Handles mouse events from the various components.
   */
  final class MouseEventHandler extends MouseAdapter
  {
    // VARIABLES

    private volatile Point lastClickPosition = null;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;

      Component source = aEvent.getComponent();
      int clicks = aEvent.getClickCount();

      if ( source == mainComponent || source == timelineComponent )
      {
        MouseEvent event = convertEvent( aEvent );
        Point point = event.getPoint();

        if ( clicks == 2 )
        {
          Cursor hoveredCursor = model.getSelectedCursor();

          boolean dblClickZoom = UIManager.getBoolean( DOUBLE_CLICK_ZOOM_DEFAULT );
          boolean ctrlDown = Platform.isMacOS() ? aEvent.isMetaDown() : aEvent.isControlDown();

          boolean cursorPlaceEvent = !dblClickZoom ^ ctrlDown;
          boolean zoomEvent = dblClickZoom ^ ctrlDown;

          if ( model.areCursorsVisible() && ( hoveredCursor != null ) )
          {
            editCursorProperties( hoveredCursor );
            // Consume the event to stop further processing...
            aEvent.consume();
          }
          else if ( cursorPlaceEvent )
          {
            // Add next available cursor...
            Cursor cursor = model.getNextAvailableCursor();
            if ( cursor == null )
            {
              Platform.beep();
            }
            else
            {
              long timestamp = model.findCursorTimestamp( point );

              cursor.setTimestamp( timestamp );

              // Make the cursor visible on screen...
              repaintAccumulator.add( cursor );

              // Notify all listeners that something has changed...
              postCursorChangedEvent( cursor );
            }

            // Consume the event to stop further processing...
            aEvent.consume();
          }
          else if ( zoomEvent )
          {
            // Regular zoom event...
            int rotation = ( aEvent.isAltDown() || aEvent.isShiftDown() ) ? 1 : -1;

            zoom( rotation, point );

            // Consume the event to stop further processing...
            aEvent.consume();
          }
        }
        else if ( clicks == 1 )
        {
          if ( model.isMeasurementMode() )
          {
            boolean frozen = model.isMeasurementFrozen();
            if ( aEvent.getButton() == MouseEvent.BUTTON1 )
            {
              model.setMeasurementFrozen( !frozen );
            }

            MeasurementInfo measurementInfo = updateMeasurementInfo( model, point );

            // Fire an event to the interested listeners...
            postMeasurementInfoEvent( measurementInfo, model.isMeasurementFrozen() );
          }
        }
      }
      else if ( source == labelComponent )
      {
        if ( clicks == 2 )
        {
          WaveformElement hoveredElement = model.getSelectedElement();
          if ( hoveredElement != null )
          {
            editElementProperties( hoveredElement );
            // Consume the event to stop further processing...
            aEvent.consume();
          }
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseDragged( MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;
      Cursor movingCursor = model.getSelectedCursor();
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      // Update the selected element...
      updateSelectedElement( model, point );

      if ( model.areCursorsVisible() && ( movingCursor != null ) )
      {
        Cursor oldCursor = movingCursor.clone();
        long oldTimestamp = oldCursor.getTimestamp();

        long newTimestamp = model.findCursorTimestamp( point );
        movingCursor.setTimestamp( newTimestamp );

        repaintCursor( oldTimestamp );
        repaintCursor( newTimestamp );

        // Fire an event to the interested listeners...
        postCursorChangedEvent( movingCursor );

        // We're done with the given event...
        aEvent.consume();
        return;
      }

      boolean button1down = ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0;
      boolean horizontalOnly = ( aEvent.getModifiersEx() & InputEvent.ALT_DOWN_MASK ) != 0;
      boolean verticalOnly = horizontalOnly && ( ( aEvent.getModifiersEx() & InputEvent.SHIFT_DOWN_MASK ) != 0 );

      if ( button1down )
      {
        if ( this.lastClickPosition == null )
        {
          this.lastClickPosition = new Point( point );
        }
        else
        {
          Point _clickPos = this.lastClickPosition;

          JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, aEvent.getComponent() );
          if ( scrollPane != null )
          {
            int dx = aEvent.getX() - _clickPos.x;
            int dy = aEvent.getY() - _clickPos.y;

            JViewport viewPort = scrollPane.getViewport();
            Point scrollPosition = viewPort.getViewPosition();
            int newX = scrollPosition.x;
            if ( !verticalOnly )
            {
              newX -= dx;
            }
            int newY = scrollPosition.y;
            if ( verticalOnly || !horizontalOnly )
            {
              newY -= dy;
            }

            int diagramWidth = mainComponent.getWidth();
            int viewportWidth = viewPort.getWidth();
            int maxX = diagramWidth - viewportWidth - 1;
            scrollPosition.x = Math.max( 0, Math.min( maxX, newX ) );

            int diagramHeight = mainComponent.getHeight();
            int viewportHeight = viewPort.getHeight();
            int maxY = diagramHeight - viewportHeight;
            scrollPosition.y = Math.max( 0, Math.min( maxY, newY ) );

            viewPort.setViewPosition( scrollPosition );
          }
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseMoved( MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      updateSelectedElement( model, point );

      Cursor newCursor = updateSelectedCursor( model, point );

      setCursor( null );
      if ( model.areCursorsVisible() && ( newCursor != null ) )
      {
        setCursor( CURSOR_MOVE_CURSOR );
      }

      if ( model.isMeasurementMode() && !model.isMeasurementFrozen() )
      {
        MeasurementInfo measurementInfo = updateMeasurementInfo( model, point );

        // Fire an event to the interested listeners...
        postMeasurementInfoEvent( measurementInfo, model.isMeasurementFrozen() );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mousePressed( MouseEvent aEvent )
    {
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      if ( ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0 )
      {
        this.lastClickPosition = point;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseReleased( MouseEvent aEvent )
    {
      setCursor( null );
    }

    private MouseEvent convertEvent( final MouseEvent aEvent )
    {
      JComponent view = SwingComponentUtils.getDeepestComponentAt( aEvent );
      return SwingUtilities.convertMouseEvent( aEvent.getComponent(), aEvent, view );
    }

    /**
     * @param aCursor
     *          the changed cursor, cannot be <code>null</code>.
     */
    private void postCursorChangedEvent( Cursor aCursor )
    {
      postEvent( TOPIC_CLIENT_STATE.concat( "/CURSOR_CHANGED" ), "cursor", aCursor.clone(), "controller", controller );
    }

    /**
     * @param aInfo
     *          the measurement information, cannot be <code>null</code>.
     */
    private void postMeasurementInfoEvent( MeasurementInfo aInfo, boolean aFrozen )
    {
      postEvent( TOPIC_MEASUREMENTS, "measurement", aInfo, "frozen", aFrozen );
    }

    private MeasurementInfo updateMeasurementInfo( WaveformModel aModel, Point aPoint )
    {
      MeasurementInfo oldMeasurementInfo = aModel.getMeasurementInfo();

      WaveformElement element = aModel.getSelectedElement();
      if ( element == null || !Type.CHANNEL.equals( element.getType() ) )
      {
        return oldMeasurementInfo;
      }

      MeasurementInfo measurementInfo = aModel.getMeasurementInfo( aPoint, element );

      if ( oldMeasurementInfo != null )
      {
        repaintAccumulator.add( oldMeasurementInfo );
      }

      aModel.setMeasurementInfo( measurementInfo );

      if ( measurementInfo != null )
      {
        repaintAccumulator.add( measurementInfo );
      }

      return measurementInfo;
    }

    private Cursor updateSelectedCursor( WaveformModel aModel, Point aPoint )
    {
      Cursor oldCursor = aModel.getSelectedCursor();
      Cursor newCursor = aModel.findCursor( aPoint );

      aModel.setSelectedCursor( newCursor );

      // Repaint the affected areas
      if ( newCursor != null )
      {
        repaintAccumulator.add( newCursor );
      }
      if ( oldCursor != null )
      {
        repaintAccumulator.add( oldCursor );
      }

      return newCursor;
    }

    private void updateSelectedElement( WaveformModel aModel, Point aPoint )
    {
      WaveformElement oldElement = aModel.getSelectedElement();
      WaveformElement newElement = aModel.findWaveformElement( aPoint );

      aModel.setSelectedElement( newElement );

      // Repaint the affected areas
      if ( newElement != null )
      {
        repaintAccumulator.add( newElement );
      }
      if ( oldElement != null )
      {
        repaintAccumulator.add( oldElement );
      }
    }
  }

  /**
   * A runnable implementation that accumulates several calls to avoid an
   * avalanche of events on the EDT.
   */
  private class RepaintAccumulator extends AccumulatingRunnable<Object>
  {
    @Override
    protected void run( Deque<Object> aArgs )
    {
      for ( int i = 0, max = aArgs.size(); i < max; i++ )
      {
        Object obj = aArgs.pop();
        if ( obj instanceof WaveformElement )
        {
          repaintWaveformLabel( ( WaveformElement )obj );
        }
        else if ( obj instanceof Annotation )
        {
          repaintAnnotation( ( Annotation )obj );
        }
        else if ( obj instanceof Cursor )
        {
          repaintCursor( ( Cursor )obj );
        }
        else if ( obj instanceof MeasurementInfo )
        {
          repaintMeasurementInformation( ( MeasurementInfo )obj );
        }
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  static final java.awt.Cursor CURSOR_WAIT = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR );
  static final java.awt.Cursor CURSOR_HOVER = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.HAND_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_TIMESTAMP = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.E_RESIZE_CURSOR );

  // VARIABLES

  private WaveformTimelineComponent timelineComponent;
  private WaveformLabelComponent labelComponent;
  private WaveformViewComponent mainComponent;
  private ZoomController zoomController;

  private RepaintAccumulator repaintAccumulator;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  public WaveformView( ViewController aController, ViewModel aModel )
  {
    super( aController, aModel );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    revalidatePreferredSizes();

    super.addNotify();
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canZoom()
  {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public double getDisplayedInterval()
  {
    AcquisitionData data = this.model.getData();
    double width = this.mainComponent.getVisibleRect().width;

    return width / ( getZoomFactor() * data.getSampleRate() );
  }

  /**
   * Returns the main view component.
   * 
   * @return a view component, can only be <code>null</code> if this view itself
   *         is not initialized.
   */
  public WaveformViewComponent getViewComponent()
  {
    return this.mainComponent;
  }

  /**
   * @return a zoom factor, >= 0.0.
   */
  @Override
  public double getZoomFactor()
  {
    return this.zoomController.getFactor();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( String aTopic, Event aEvent )
  {
    if ( aTopic.startsWith( TOPIC_ANNOTATIONS ) )
    {
      if ( aTopic.endsWith( "/ADD" ) )
      {
        Annotation annotation = ( Annotation )aEvent.getProperty( "annotation" );
        if ( annotation != null )
        {
          this.repaintAccumulator.add( annotation );
        }
      }
      else if ( aTopic.endsWith( "/CLEAR" ) )
      {
        Channel channel = ( Channel )aEvent.getProperty( "channel" );
        WaveformElement element = ( ( WaveformModel )this.model ).getWaveformElement( channel );
        if ( element != null )
        {
          this.repaintAccumulator.add( element );
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void initialize()
  {
    MouseEventHandler mouseListener = new MouseEventHandler();

    this.repaintAccumulator = new RepaintAccumulator();

    this.mainComponent = new WaveformViewComponent( ( WaveformModel )this.model );
    this.mainComponent.addMouseListener( mouseListener );
    this.mainComponent.addMouseMotionListener( mouseListener );
    this.timelineComponent = new WaveformTimelineComponent( ( WaveformModel )this.model );
    this.timelineComponent.addMouseListener( mouseListener );
    this.timelineComponent.addMouseMotionListener( mouseListener );
    this.labelComponent = new WaveformLabelComponent( ( WaveformModel )this.model );
    this.labelComponent.addMouseListener( mouseListener );
    this.labelComponent.addMouseMotionListener( mouseListener );

    this.zoomController = new ZoomController( this.controller );

    JPopupMenu popup = new JPopupMenu();
    popup.addPopupMenuListener( new PopupMenuListener()
    {
      @Override
      public void popupMenuCanceled( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        popup.removeAll();
      }

      @Override
      public void popupMenuWillBecomeInvisible( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        popup.removeAll();
      }

      @Override
      public void popupMenuWillBecomeVisible( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        JComponent invoker = ( JComponent )popup.getInvoker();
        populatePopup( popup, invoker );
      }
    } );
    setComponentPopupMenu( popup );

    JScrollPane scrollPane = new JScrollPane( this.mainComponent );
    scrollPane.setColumnHeaderView( this.timelineComponent );
    scrollPane.setRowHeaderView( this.labelComponent );
    scrollPane.setCorner( ScrollPaneConstants.UPPER_LEFT_CORNER, new WaveformCornerComponent() );
    scrollPane.setInheritsPopupMenu( true );

    add( scrollPane, BorderLayout.CENTER );

    registerKeyBindings();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void scrollToTimestamp( long aTimestamp )
  {
    final Rectangle visibleRect = this.mainComponent.getVisibleRect();

    Rectangle rect = new Rectangle();
    rect.width = visibleRect.width;
    rect.height = visibleRect.height;
    rect.x = ( int )( ( getZoomFactor() * aTimestamp ) - rect.getCenterX() );
    rect.y = visibleRect.y;

    this.mainComponent.scrollRectToVisible( rect );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void smartJump( JumpType aType, JumpDirection aDirection )
  {
    Rectangle viewSize = this.mainComponent.getVisibleRect();
    Point refPoint = new Point( ( int )Math.round( viewSize.getCenterX() ), 0 );

    long jumpTo = ( ( WaveformModel )this.model ).smartJump( aType, refPoint, aDirection );
    if ( jumpTo >= 0 )
    {
      scrollToTimestamp( jumpTo );
    }
  }

  /**
   * Zooms in the direction denoted by the given rotation using the given
   * coordinate as center location.
   * 
   * @param aRotation
   * @param aLocation
   */
  public void zoom( int aRotation, Point aLocation )
  {
    this.zoomController.zoom( aRotation, aLocation );
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    this.zoomController.zoomAll();
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    this.zoomController.zoomIn();
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    this.zoomController.zoomOriginal();
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    this.zoomController.zoomOut();
  }

  /**
   * Edits the properties of the given cursor.
   * 
   * @param aCursor
   *          the cursor to edit, cannot be <code>null</code>.
   */
  final void editCursorProperties( Cursor aCursor )
  {
    Window parent = getCurrentWindow();
    // Determine a suitable location to place the editor dialog...
    int locationX = ( ( WaveformModel )this.model ).timestampToCoordinate( aCursor.getTimestamp() );
    Point loc = SwingUtilities.convertPoint( this.mainComponent, locationX, 40, parent );

    EditCursorDialog dialog = new EditCursorDialog( parent, aCursor );
    dialog.setLocation( loc );
    if ( dialog.showDialog() )
    {
      aCursor.setColor( dialog.getColor() );
      aCursor.setLabel( dialog.getLabel() );

      repaintCursor( aCursor );
    }
  }

  /**
   * Edits the properties of the given waveform element.
   * 
   * @param aElement
   *          the element to edit, cannot be <code>null</code>.
   */
  final void editElementProperties( WaveformElement aElement )
  {
    Window parent = getCurrentWindow();
    // Determine a suitable location to place the editor dialog...
    int locationY = aElement.getYposition() + ( aElement.getHeight() / 2 ) + 10;
    Point loc = SwingUtilities.convertPoint( this.mainComponent, this.labelComponent.getWidth(), locationY, parent );

    EditWaveformPropertiesDialog dialog = new EditWaveformPropertiesDialog( parent, aElement );
    dialog.setLocation( loc );
    if ( dialog.showDialog() )
    {
      aElement.setLabel( dialog.getLabel() );
      aElement.setColor( dialog.getColor() );
      aElement.setHeight( dialog.getElementHeight() );

      if ( Type.CHANNEL.equals( aElement.getType() ) )
      {
        aElement.setSignalHeight( dialog.getSignalHeight() );
        aElement.setAlignment( dialog.getAlignment() );
      }

      revalidatePreferredSizes();
    }
  }

  /**
   * Populates the popup according to the context of the given invoker.
   * 
   * @param aPopup
   * @param aInvoker
   */
  final void populatePopup( JPopupMenu aPopup, JComponent aInvoker )
  {
    if ( aInvoker instanceof WaveformViewComponent )
    {
      aPopup.add( new JMenuItem( "Edit session title..." ) );
    }
    else if ( aInvoker instanceof WaveformTimelineComponent )
    {

    }
    else if ( aInvoker instanceof WaveformLabelComponent )
    {

    }
    else
    {
      System.out.println( "Unknown invoker: " + aInvoker );
    }
  }

  final void repaintAnnotation( Annotation aAnnotation )
  {
    WaveformModel model = ( WaveformModel )this.model;

    Channel channel = aAnnotation.getChannel();
    WaveformElement element = model.getWaveformElement( channel );
    if ( element == null )
    {
      return;
    }

    if ( aAnnotation instanceof DataAnnotation )
    {
      DataAnnotation ann = ( DataAnnotation )aAnnotation;
      long start = ann.getStartTimestamp();
      long end = ann.getEndTimestamp();

      Rectangle rect = new Rectangle();
      rect.x = model.timestampToCoordinate( start );
      rect.y = element.getYposition();
      rect.width = model.timestampToCoordinate( end ) - rect.x;
      rect.height = element.getHeight();

      this.mainComponent.repaint( rect );
    }
    else if ( aAnnotation instanceof LabelAnnotation )
    {
      repaintWaveformLabel( element );
    }
  }

  /**
   * Repaints the area taken up by the given cursor on screen.
   * 
   * @param aCursor
   *          the cursor to repaint, cannot be <code>null</code>.
   */
  final void repaintCursor( Cursor aCursor )
  {
    repaintCursor( aCursor.getTimestamp() );
  }

  /**
   * Repaints the area taken up by the given cursor on screen.
   * 
   * @param aCursorTimestamp
   *          the cursor timestamp to repaint, cannot be <code>null</code>.
   */
  final void repaintCursor( long aCursorTimestamp )
  {
    WaveformModel model = ( WaveformModel )this.model;

    this.timelineComponent.repaint(); // TODO

    Rectangle rect = this.mainComponent.getVisibleRect();
    rect.x = model.timestampToCoordinate( aCursorTimestamp ) - 1;
    rect.width = 2;

    this.mainComponent.repaint( rect );
  }

  /**
   * Repaints the area taken up by the given measurement information on screen.
   * 
   * @param aMeasurementInfo
   *          the measurement information to repaint, cannot be
   *          <code>null</code>.
   */
  final void repaintMeasurementInformation( MeasurementInfo aMeasurementInfo )
  {
    Rectangle rect = aMeasurementInfo.getRectangle();
    this.mainComponent.repaint( rect.x - 1, rect.y - 1, rect.width + 2, rect.height + 2 );
  }

  /**
   * Repaints the area taken up by the given waveform element on screen.
   * 
   * @param aElement
   *          the waveform element to repaint, cannot be <code>null</code>.
   */
  final void repaintWaveformElement( WaveformElement aElement )
  {
    Rectangle rect = this.mainComponent.getVisibleRect();
    rect.y = aElement.getYposition();
    rect.height = aElement.getHeight();
    this.mainComponent.repaint( rect );
  }

  /**
   * Repaints the area taken up by the label of the given waveform element on
   * screen.
   * 
   * @param aElement
   *          the waveform element to repaint, cannot be <code>null</code>.
   */
  final void repaintWaveformLabel( WaveformElement aElement )
  {
    Rectangle rect = this.labelComponent.getVisibleRect();
    rect.y = aElement.getYposition();
    rect.height = aElement.getHeight();
    this.labelComponent.repaint( rect );

    WaveformModel model = ( WaveformModel )this.model;
    int groupIdx = aElement.getGroupIndex();
    for ( WaveformElement element : model.getWaveformElements() )
    {
      if ( Type.GROUP.equals( element.getType() ) && groupIdx == element.getGroupIndex() )
      {
        rect.y = element.getYposition();
        rect.height = element.getHeight();
        this.labelComponent.repaint( rect );
      }
    }
  }

  /**
   * Registers all key bindings this component supports.
   */
  private void registerKeyBindings()
  {
    ActionManager actionManager = this.controller.getActionManager();

    // Wrap the original zoom-actions so that we can update its state upon
    // each invocation...
    Action zoomInAction = actionManager.getAction( ZoomInAction.ID );
    Action zoomOutAction = actionManager.getAction( ZoomOutAction.ID );
    Action zoomAllAction = actionManager.getAction( ZoomAllAction.ID );
    Action zoomOriginalAction = actionManager.getAction( ZoomOriginalAction.ID );

    registerKeyBinding( this, '+', zoomInAction );
    registerKeyBinding( this, '=', zoomInAction );
    registerKeyBinding( this, '-', zoomOutAction );
    registerKeyBinding( this, '_', zoomOutAction );
    registerKeyBinding( this, '[', zoomAllAction );
    registerKeyBinding( this, ']', zoomOriginalAction );

    int[] modifiers = { InputEvent.SHIFT_DOWN_MASK, InputEvent.ALT_DOWN_MASK, InputEvent.CTRL_DOWN_MASK,
        InputEvent.META_DOWN_MASK };

    Action smartJumpLeftAction = actionManager.getAction( SmartJumpAction.getJumpLeftID() );

    int[] smartJumpLeftKeys = { KeyEvent.VK_LEFT, KeyEvent.VK_KP_LEFT };
    for ( int key : smartJumpLeftKeys )
    {
      for ( int modifier : modifiers )
      {
        registerKeyBinding( this, KeyStroke.getKeyStroke( key, modifier ), smartJumpLeftAction );
      }
    }

    Action smartJumpRightAction = actionManager.getAction( SmartJumpAction.getJumpRightID() );

    int[] smartJumpRightKeys = { KeyEvent.VK_RIGHT, KeyEvent.VK_KP_RIGHT };
    for ( int key : smartJumpRightKeys )
    {
      for ( int modifier : modifiers )
      {
        registerKeyBinding( this, KeyStroke.getKeyStroke( key, modifier ), smartJumpRightAction );
      }
    }
  }

  /**
   * Recalculates all preferred sizes and revalidates all components.
   */
  private void revalidatePreferredSizes()
  {
    WaveformModel model = ( WaveformModel )this.model;

    int labelWidth = this.labelComponent.getPreferredWidth();
    int timelineHeight = getInt( TIMELINE_HEIGHT, 40 );

    int mainWidth = model.calculateScreenWidth();
    int mainHeight = model.calculateScreenHeight();

    this.labelComponent.setPreferredSize( new Dimension( labelWidth, mainHeight ) );
    this.labelComponent.revalidate();

    this.mainComponent.setPreferredSize( new Dimension( mainWidth, mainHeight ) );
    this.mainComponent.revalidate();

    this.timelineComponent.setPreferredSize( new Dimension( mainWidth, timelineHeight ) );
    this.timelineComponent.revalidate();
  }
}
