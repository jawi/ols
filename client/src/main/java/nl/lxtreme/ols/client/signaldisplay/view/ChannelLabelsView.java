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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.action.*;
import nl.lxtreme.ols.client.signaldisplay.dnd.*;
import nl.lxtreme.ols.client.signaldisplay.dnd.DragAndDropTargetController.DragAndDropHandler;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a view for the channel labels.
 */
public class ChannelLabelsView extends AbstractViewLayer
{
  // INNER TYPES

  /**
   * Provides a mouse-event hand
   */
  static final class ChannelLabelMouseHandler extends AbstractMouseHandler
  {
    // CONSTRUCTORS

    /**
     * Creates a new ChannelLabelsView.MouseHandler instance.
     */
    public ChannelLabelMouseHandler( final SignalDiagramController aController )
    {
      super( aController );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      // Ensure the focus is moved to the main signal diagram component...
      getViewComponent().requestFocusInWindow();

      if ( !aEvent.isConsumed() && ( aEvent.getClickCount() == 2 ) )
      {
        final IUIElement element = findSignalElement( aEvent.getPoint() );
        if ( element != null )
        {
          ActionEvent stubEvent = new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" );
          new EditSignalElementPropertiesAction( this.controller, element, aEvent.getLocationOnScreen() )
          .actionPerformed( stubEvent );

          // Do not process this event any further...
          aEvent.consume();
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mousePressed( final MouseEvent aEvent )
    {
      if ( handlePopupTrigger( aEvent ) )
      {
        aEvent.consume();
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseReleased( final MouseEvent aEvent )
    {
      if ( handlePopupTrigger( aEvent ) )
      {
        aEvent.consume();
      }
    }

    /**
     * Creates the context-sensitive popup menu for channel labels.
     *
     * @param aRelativePoint
     *          the current mouse location to show the popup menu, cannot be
     *          <code>null</code>.
     * @param aLocationOnScreen
     *          the location on screen, cannot be <code>null</code>.
     * @return a popup menu, can be <code>null</code> if the given mouse point
     *         is not above a channel.
     */
    private JPopupMenu createChannelLabelPopup( final Point aRelativePoint, final Point aLocationOnScreen )
    {
      return this.popupHelper.createChannelLabelPopup( aRelativePoint, aLocationOnScreen );
    }

    /**
     * Finds the UI-element under the given point.
     *
     * @param aPoint
     *          the coordinate of the potential channel, cannot be
     *          <code>null</code>.
     * @return the channel index, or -1 if not found.
     */
    private IUIElement findSignalElement( final Point aPoint )
    {
      return this.controller.getViewModel().findUIElement( aPoint );
    }

    /**
     * Handles the given event if it represents a popup trigger.
     *
     * @param aEvent
     *          the mouse event to handle as popup trigger, cannot be
     *          <code>null</code>.
     * @return <code>true</code> if the event represented a popup trigger,
     *         <code>false</code> otherwise.
     */
    private boolean handlePopupTrigger( final MouseEvent aEvent )
    {
      boolean result = false;
      if ( aEvent.isPopupTrigger() )
      {
        JPopupMenu contextMenu = createChannelLabelPopup( aEvent.getPoint(), aEvent.getLocationOnScreen() );
        if ( contextMenu != null )
        {
          contextMenu.show( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
          result = true;
        }
      }

      return result;
    }
  }

  /**
   * Provides an mouse event listener to allow some of the functionality (such
   * as DnD and cursor dragging) of this component to be controlled with the
   * mouse.
   */
  static final class DragAndDropListener implements DragGestureListener, DragSourceMotionListener, DragSourceListener
  {
    // VARIABLES

    // See #dragGestureRecognized for details about why this image exists...
    private final BufferedImage stubImage;

    // CONSTRUCTORS

    /**
     * Creates a new DragAndDropListener instance.
     */
    public DragAndDropListener()
    {
      this.stubImage = new BufferedImage( 1, 1, BufferedImage.TYPE_INT_ARGB );
    }

    // METHODS

    /**
     * @param aDropRow
     * @return
     */
    private static Point createChannelDropPoint( final Point aPoint, final ChannelLabelsView aView,
        final Component aTargetComponent )
    {
      final ChannelLabelsViewModel model = aView.getModel();
      final int offset = model.findUIElementVirtualOffset( aPoint );

      final Point dropPoint = new Point( 0, offset );

      SwingUtilities.convertPointToScreen( dropPoint, aView );
      SwingUtilities.convertPointFromScreen( dropPoint, aTargetComponent );

      return dropPoint;
    }

    /**
     * @param aComponent
     * @return
     */
    private static GhostGlassPane getGlassPane( final Component aComponent )
    {
      return ( GhostGlassPane )SwingUtilities.getRootPane( aComponent ).getGlassPane();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragDropEnd( final DragSourceDropEvent aEvent )
    {
      if ( !DragAndDropLock.releaseLock( this ) )
      {
        return;
      }

      final GhostGlassPane glassPane = getGlassPane( aEvent.getDragSourceContext().getComponent() );
      glassPane.setVisible( false );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragEnter( final DragSourceDragEvent aEvent )
    {
      // NO-op
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragExit( final DragSourceEvent aEvent )
    {
      // NO-op
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragGestureRecognized( final DragGestureEvent aEvent )
    {
      if ( DragAndDropLock.isLocked( this ) || !DragAndDropLock.obtainLock( this ) )
      {
        return;
      }

      final Point coordinate = ( Point )aEvent.getDragOrigin().clone();

      final ChannelLabelsView sourceComponent = ( ChannelLabelsView )aEvent.getComponent();
      final ChannelLabelsViewModel model = sourceComponent.getModel();

      final IUIElement element = model.findUIElement( coordinate );
      if ( ( element == null ) || ( element instanceof ElementGroup ) )
      {
        DragAndDropLock.releaseLock( this );
        return;
      }

      final GhostGlassPane glassPane = getGlassPane( sourceComponent );

      final Point dropPoint = createChannelDropPoint( coordinate, sourceComponent, glassPane );

      final SignalElementInsertionPointRenderer renderer = new SignalElementInsertionPointRenderer( model, element );

      glassPane.setRenderer( renderer, dropPoint, coordinate );
      glassPane.setVisible( true );
      glassPane.repaintPartially();

      // Use this version of the startDrag method as to avoid a potential error
      // on MacOS: without the explicit image, it will try to create one from
      // the component itself (= this SignalView), which can be as wide as
      // Integer.MAX_VALUE pixels. This can cause a numeric overflow in the
      // image routines causing a NegativeArraySizeException. By giving it
      // explicitly an image, it will use that one instead. This is not a
      // problem for this component, as the dragged cursor will be drawn on the
      // glasspane, not by the DnD routines of Java...
      aEvent.startDrag( DragSource.DefaultMoveDrop, this.stubImage, new Point( 0, 0 ),
          new ChannelTransferable( element ), null /* dsl */);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragMouseMoved( final DragSourceDragEvent aEvent )
    {
      final Point coordinate = aEvent.getLocation();
      if ( !DragAndDropLock.isLocked( this ) || ( coordinate == null ) )
      {
        return;
      }

      final DragSourceContext dragSourceContext = aEvent.getDragSourceContext();

      final ChannelLabelsView sourceComponent = ( ChannelLabelsView )dragSourceContext.getComponent();
      final GhostGlassPane glassPane = getGlassPane( sourceComponent );

      SwingUtilities.convertPointFromScreen( coordinate, sourceComponent );

      final Point dropPoint = createChannelDropPoint( coordinate, sourceComponent, glassPane );

      glassPane.updateRenderer( dropPoint, coordinate );
      glassPane.repaintPartially();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dragOver( final DragSourceDragEvent aEvent )
    {
      // NO-op
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dropActionChanged( final DragSourceDragEvent aEvent )
    {
      // NO-op
    }
  }

  /**
   * Provides the D&D drop handler for accepting dropped channels.
   */
  final class DropHandler implements DragAndDropHandler
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean acceptDrop( final SignalDiagramController aController, final DropTargetDropEvent aEvent )
    {
      boolean accepted = false;

      try
      {
        final Transferable transferable = aEvent.getTransferable();

        final SignalElement movedElement = ( SignalElement )transferable
            .getTransferData( ChannelTransferable.CHANNEL_FLAVOR );
        if ( movedElement != null )
        {
          final ChannelLabelsViewModel model = getModel();

          final IUIElement insertElement = model.findUIElement( aEvent.getLocation() );
          if ( accepted = model.acceptDrop( movedElement, insertElement ) )
          {
            // Move the channel rows...
            model.moveSignalElement( movedElement, insertElement );
          }
        }
      }
      catch ( Exception exception )
      {
        LOG.log( Level.WARNING, "Getting transfer data failed!", exception );
      }

      return accepted;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDropAction()
    {
      return DnDConstants.ACTION_MOVE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataFlavor getFlavor()
    {
      return ChannelTransferable.CHANNEL_FLAVOR;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  static final Logger LOG = Logger.getLogger( ChannelLabelsView.class.getName() );

  // VARIABLES

  private final ChannelLabelsViewModel model;

  private final DropHandler dropHandler;
  private final DragAndDropListener dndListener;
  private final DragGestureRecognizer dragGestureRecognizer;

  private final ChannelLabelMouseHandler mouseHandler;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelLabelsView} instance.
   */
  private ChannelLabelsView( final SignalDiagramController aController )
  {
    super( aController );

    this.model = new ChannelLabelsViewModel( aController );

    this.dropHandler = new DropHandler();
    this.dndListener = new DragAndDropListener();

    final DragSource dragSource = DragSource.getDefaultDragSource();
    dragSource.addDragSourceMotionListener( this.dndListener );
    dragSource.addDragSourceListener( this.dndListener );

    this.dragGestureRecognizer = dragSource.createDefaultDragGestureRecognizer( this, DnDConstants.ACTION_MOVE,
        this.dndListener );

    this.mouseHandler = new ChannelLabelMouseHandler( aController );

    updateUI();
  }

  // METHODS

  /**
   * Creates a new {@link ChannelLabelsView} instance.
   *
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   * @return a new {@link ChannelLabelsView} instance, never <code>null</code>.
   */
  public static ChannelLabelsView create( final SignalDiagramController aController )
  {
    ChannelLabelsView result = new ChannelLabelsView( aController );
    return result;
  }

  /**
   * Installs all listeners and the support for DnD.
   *
   * @see javax.swing.JComponent#addNotify()
   */
  @Override
  public void addNotify()
  {
    final DragAndDropTargetController dndTargetController = getDnDTargetController();
    dndTargetController.addHandler( this.dropHandler );

    setDropTarget( new DropTarget( this, dndTargetController ) );

    addMouseListener( this.mouseHandler );

    super.addNotify();
  }

  /**
   * Returns the current value of model.
   *
   * @return the model
   */
  public ChannelLabelsViewModel getModel()
  {
    return this.model;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeNotify()
  {
    getDnDTargetController().removeHandler( this.dropHandler );

    DragSource dragSource = this.dragGestureRecognizer.getDragSource();
    if ( dragSource != null )
    {
      dragSource.removeDragSourceListener( this.dndListener );
      dragSource.removeDragSourceMotionListener( this.dndListener );
    }

    removeMouseListener( this.mouseHandler );

    super.removeNotify();
  }

  /**
   * Overridden in order to set a custom UI, which not only paints this diagram,
   * but also can be used to manage the various settings, such as colors,
   * height, and so on.
   *
   * @see javax.swing.JComponent#updateUI()
   */
  @Override
  public final void updateUI()
  {
    setUI( new ChannelLabelsUI() );
  }

  /**
   * {@inheritDoc}
   * <p>
   * For some strange reason it is not possible to do this from our own
   * {@link ChannelLabelMouseHandler}. Probably due to the fact that this
   * component is also DnD-enabled...
   * </p>
   */
  @Override
  protected void processMouseMotionEvent( final MouseEvent aEvent )
  {
    if ( !aEvent.isConsumed() )
    {
      final MouseEvent event = convertEvent( aEvent );
      final Point point = event.getPoint();

      getController().setSelectedChannel( point );
    }

    super.processMouseMotionEvent( aEvent );
  }

  /**
   * @param aEvent
   * @return
   */
  private MouseEvent convertEvent( final MouseEvent aEvent )
  {
    final JComponent view = SwingComponentUtils.getDeepestComponentAt( aEvent );
    return SwingUtilities.convertMouseEvent( aEvent.getComponent(), aEvent, view );
  }
}
