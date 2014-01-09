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
package nl.lxtreme.ols.client.signaldisplay;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.beans.*;

import javax.swing.*;

import nl.lxtreme.ols.api.acquisition.AcquisitionResult;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.devices.Device;
import nl.lxtreme.ols.client.Activator;
import nl.lxtreme.ols.client.ClientController;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomEvent;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomListener;
import nl.lxtreme.ols.client.signaldisplay.dnd.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.SignalElementType;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides the main component controller for the signal diagram component.
 */
public final class SignalDiagramController implements ZoomListener, PropertyChangeListener
{
  // VARIABLES

  private final DragAndDropTargetController dndTargetController;
  private final IActionManager actionManager;

  private SignalDiagramModel signalDiagramModel;
  private SignalDiagramComponent signalDiagram;
  private final ClientController clientController;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalDiagramController} instance.
   * 
   * @param aActionManager
   *          the action manager to use, cannot be <code>null</code>.
   */
  public SignalDiagramController( final IActionManager aActionManager, final ClientController clientController )
  {
    this.actionManager = aActionManager;
    this.clientController = clientController;
    this.dndTargetController = new DragAndDropTargetController( this );
  }

  // METHODS

  /**
   * Adds a channel change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addChannelChangeListener( final ISignalElementChangeListener aListener )
  {
    getSignalDiagramModel().getSignalElementManager().addChannelChangeListener( aListener );
  }

  /**
   * Adds a cursor change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addCursorChangeListener( final ICursorChangeListener aListener )
  {
    getSignalDiagramModel().addCursorChangeListener( aListener );
  }

  /**
   * Adds a data model change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addDataModelChangeListener( final IDataModelChangeListener aListener )
  {
    getSignalDiagramModel().addDataModelChangeListener( aListener );
  }

  /**
   * Adds a measurement listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addMeasurementListener( final IMeasurementListener aListener )
  {
    getSignalDiagramModel().addMeasurementListener( aListener );
  }

  /**
   * Adds a property change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    getSignalDiagramModel().addPropertyChangeListener( aListener );
  }

  /**
   * Returns the current value of actionManager.
   * 
   * @return the actionManager
   */
  public final IActionManager getActionManager()
  {
    return this.actionManager;
  }

  /**
   * Returns the set of defined cursors, never <code>null</code>.
   * 
   * @return all defined cursors, never <code>null</code>.
   */
  public Cursor[] getDefinedCursors()
  {
    return getSignalDiagramModel().getDefinedCursors();
  }

  /**
   * @return the dndTargetController
   */
  public final DragAndDropTargetController getDndTargetController()
  {
    return this.dndTargetController;
  }

  /**
   * @return the signal diagram component, never <code>null</code>.
   */
  public final SignalDiagramComponent getSignalDiagram()
  {
    return this.signalDiagram;
  }

  /**
   * @return the signal diagram model, never <code>null</code>.
   */
  public final SignalDiagramModel getSignalDiagramModel()
  {
    return this.signalDiagramModel;
  }

  /**
   * Returns the signal element type that is underneat the given coordinate.
   * 
   * @param aPoint
   *          the coordinate to determine what signal element is underneat, may
   *          be <code>null</code>.
   * @return <code>null</code> if no coordinate is given, or no signal element
   *         type could be determined. Otherwise, the signal element type.
   */
  public SignalElementType getSignalHoverType( final Point aPoint )
  {
    final IUIElement element = getSignalDiagramModel().findUIElement( aPoint );
    if ( element instanceof SignalElement )
    {
      return ( ( SignalElement )element ).getType();
    }
    return null;
  }

  /**
   * Returns the zoom controller of this diagram.
   * 
   * @return the zoom controller, never <code>null</code>.
   */
  public ZoomController getZoomController()
  {
    return this.signalDiagramModel.getZoomController();
  }

  /**
   * Factory method to create a new {@link SignalDiagramController} instance.
   * 
   * @param aActionManager
   *          the action manager to use for the controller instance, cannot be
   *          <code>null</code>.
   * @return a new {@link SignalDiagramController} instance, never
   *         <code>null</code>.
   */
  public void initialize()
  {
    final SignalDiagramModel model = new SignalDiagramModel( this );
    setSignalDiagramModel( model );

    // Register our controller as listener for zooming events...
    model.getZoomController().addZoomListener( this );

    final SignalDiagramComponent diagram = new SignalDiagramComponent( this );
    setSignalDiagram( diagram );
  }

  /**
   * Returns whether the cursor denoted by the given index is defined.
   * 
   * @param aCursorIdx
   *          the index of the cursor to check.
   * @return <code>true</code> if the cursor with the given index is defined,
   *         <code>false</code> otherwise.
   */
  public boolean isCursorDefined( final int aCursorIdx )
  {
    return getSignalDiagramModel().isCursorDefined( aCursorIdx );
  }

  /**
   * Returns whether or not the cursor mode is enabled.
   * 
   * @return <code>true</code> if the cursor mode is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isCursorMode()
  {
    return getSignalDiagramModel().isCursorMode();
  }

  /**
   * Drags a cursor with a given index to a given point, possibly snapping to a
   * signal edge.
   * 
   * @param aCursorIdx
   *          the cursor index to move, should be &gt;= 0 && &lt; 10;
   * @param aPoint
   *          the new point of the cursor. In case of snapping, this point
   *          should match a signal edge, cannot be <code>null</code>.
   */
  public void moveCursor( final int aCursorIdx, final Point aPoint )
  {
    final long newCursorTimestamp = locationToTimestamp( aPoint );

    getSignalDiagramModel().setCursor( aCursorIdx, newCursorTimestamp );
  }

  /**
   * Called when the zoom-level is changed, or restored.
   * <p>
   * In case the dead-center zooming does not work, make sure that:
   * </p>
   * <ul>
   * <li>the view of the viewport is updated <b>directly</b> with respect to its
   * dimensions and location;
   * <li>the viewport is revalidated <b>before</b> the new location is set;
   * <li>the zoom controller calculates the correct width and height of the view
   * using the <b>new</b> zoom factor;
   * <li>the center point is in the correct coordinate space;
   * <li>only the width of the component is updated;
   * <li>the zoom level is <em>restored</em> after changing the window
   * dimensions;
   * <li>the timeline and channel labels follow the width/height of the signal
   * view.
   * </ul>
   * 
   * @see http://stackoverflow.com/questions/115103
   */
  @Override
  public void notifyZoomChange( final ZoomEvent aEvent )
  {
    final boolean dataAvailable = getSignalDiagramModel().hasData();

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final ZoomController zoomCtrl = aEvent.getZoomController();

        // Update the zoom action's state...
        Action zoomInAction = getActionManager().getAction( ZoomInAction.ID );
        zoomInAction.setEnabled( dataAvailable && zoomCtrl.canZoomIn() );

        Action zoomOutAction = getActionManager().getAction( ZoomOutAction.ID );
        zoomOutAction.setEnabled( dataAvailable && zoomCtrl.canZoomOut() );

        Action zoomAllAction = getActionManager().getAction( ZoomAllAction.ID );
        zoomAllAction.setEnabled( dataAvailable && !zoomCtrl.isZoomAll() );

        Action zoomOriginalAction = getActionManager().getAction( ZoomOriginalAction.ID );
        zoomOriginalAction.setEnabled( dataAvailable && !zoomCtrl.isZoomDefault() );

        // Idea based on <http://stackoverflow.com/questions/115103>
        JScrollPane scrollPane = SwingComponentUtils.getAncestorOfClass( JScrollPane.class, getSignalDiagram() );
        if ( scrollPane == null )
        {
          // Nothing to do...
          return;
        }

        JViewport viewport = scrollPane.getViewport();
        JViewport timelineViewport = scrollPane.getColumnHeader();
        Component view = viewport.getView();

        if ( Activator.isDebugMode() )
        {
          System.out.printf( "Handling %s.%n", aEvent );
        }

        view.setPreferredSize( aEvent.getDimension() );

        // Make sure to make the viewport aware of the new dimensions of the
        // view, this needs to be done *before* the view is set to its new
        // location...
        viewport.doLayout();

        view.setLocation( aEvent.getLocation() );

        // Layout the timeline as well, as it needs probably be repainted as
        // well, since the view itself is changed...
        timelineViewport.doLayout();
        timelineViewport.repaint();
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    String name = aEvent.getPropertyName();
    if ( "project".equals( name ) )
    {
      Project project = ( Project )aEvent.getNewValue();
      setDataModel( project.getDataSet() );
    }
    else if ( "capturedData".equals( name ) )
    {
      DataSet dataSet = ( DataSet )aEvent.getNewValue();
      setDataModel( dataSet );

      // Make sure the view is updated accordingly...
      getZoomController().restoreZoomLevel();
    }
  }

  /**
   * Revalidates the various components and repaints the entire component.
   * <p>
   * SLOW METHOD: USE WITH CARE!
   * </p>
   */
  public void revalidateAll()
  {
    this.signalDiagram.revalidateAll();
    this.signalDiagram.repaintAll();
  }

  /**
   * Removes a channel change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeChannelChangeListener( final ISignalElementChangeListener aListener )
  {
    getSignalDiagramModel().getSignalElementManager().removeChannelChangeListener( aListener );
  }

  /**
   * Removes the cursor denoted by the given index. If the cursor with the given
   * index is <em>undefined</em> this method does nothing (not even call event
   * listeners!).
   * 
   * @param aCursorIdx
   *          the index of the cursor to remove.
   */
  public void removeCursor( final int aCursorIdx )
  {
    getSignalDiagramModel().removeCursor( aCursorIdx );
  }

  /**
   * Removes a cursor change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeCursorChangeListener( final ICursorChangeListener aListener )
  {
    getSignalDiagramModel().removeCursorChangeListener( aListener );
  }

  /**
   * Removes a data model change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeDataModelChangeListener( final IDataModelChangeListener aListener )
  {
    getSignalDiagramModel().removeDataModelChangeListener( aListener );
  }

  /**
   * Removes the given measurement listener from the list of listeners.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeMeasurementListener( final IMeasurementListener aListener )
  {
    getSignalDiagramModel().removeMeasurementListener( aListener );
  }

  /**
   * Removes a property change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    getSignalDiagramModel().removePropertyChangeListener( aListener );
  }

  /**
   * Jumps to a given timestamp in the diagram.
   * 
   * @param aTimestamp
   *          the time stamp to jump to.
   */
  public void scrollToTimestamp( final long aTimestamp )
  {
    getSignalDiagram().scrollToTimestamp( aTimestamp );
  }

  /**
   * Enables or disables the cursor mode, which in effect, Ttrns the visibility
   * of all cursors either on or off.
   * <p>
   * This method does <em>not</em> modify any cursor, only whether they are
   * displayed or not!
   * </p>
   * 
   * @param aVisible
   *          <code>true</code> if the cursors should be made visible,
   *          <code>false</code> if the cursors should be made invisible.
   */
  public void setCursorMode( final boolean aVisible )
  {
    getSignalDiagramModel().setCursorMode( aVisible );
  }

  /**
   * Sets the data model for this controller.
   * 
   * @param aDataSet
   *          the data set to set, cannot be <code>null</code>.
   */
  public void setDataModel( final DataSet aDataSet )
  {
    getSignalDiagramModel().setDataModel( aDataSet );

    // will update the view and show the signals...
    revalidateAll();

    // optionally center the view on the trigger moment...
    boolean autoCenterOnTrigger = UIManager.getBoolean( UIManagerKeys.AUTO_CENTER_TO_TRIGGER_AFTER_CAPTURE );
    final AcquisitionResult capturedData = aDataSet.getCapturedData();

    // Issue #181
    if ( autoCenterOnTrigger && ( capturedData != null ) && capturedData.hasTriggerData() )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        public void run()
        {
          scrollToTimestamp( capturedData.getTriggerPosition() );
        }
      } );
    }
  }

  /**
   * Starts this component.
   */
  public void setDefaultSettings()
  {
    // Set the correct defaults...
    final boolean snapCursorsDefault = UIManager.getBoolean( UIManagerKeys.SNAP_CURSORS_DEFAULT );
    final boolean snapCursorsValue = getSignalDiagramModel().isSnapCursorMode();

    if ( snapCursorsValue ^ snapCursorsDefault )
    {
      setSnapModeEnabled( snapCursorsDefault );
    }
  }

  /**
   * Enables or disables the measurement mode.
   * 
   * @param aEnabled
   *          <code>true</code> to enable the measurement mode,
   *          <code>false</code> to disable this mode.
   */
  public void setMeasurementMode( final boolean aEnabled )
  {
    getSignalDiagramModel().setMeasurementMode( aEnabled );
  }

  /**
   * @param aPosition
   */
  public void setSelectedChannel( final Point aPosition )
  {
    SignalDiagramModel model = getSignalDiagramModel();

    final IUIElement element = model.findUIElement( aPosition );
    if ( !( element instanceof SignalElement ) )
    {
      return;
    }

    SignalElement signalElement = ( SignalElement )element;
    int oldIndex = model.getSelectedChannelIndex();
    int newIndex = -1;

    if ( signalElement.isDigitalSignal() )
    {
      // Update the selected channel index...
      newIndex = signalElement.getChannel().getIndex();
    }
    if ( oldIndex != newIndex )
    {
      model.setSelectedChannelIndex( newIndex );
    }

    ChannelLabelsView channelLabelsView = getChannelLabelsView();
    if ( channelLabelsView != null )
    {
      Rectangle rect;
      int width = channelLabelsView.getWidth();

      // Repaint the affected areas
      if ( signalElement != null )
      {
        rect = new Rectangle( 0, signalElement.getYposition(), width, signalElement.getHeight() );
        channelLabelsView.repaint( rect );

        ElementGroup signalGroup = signalElement.getGroup();
        rect = new Rectangle( 0, signalGroup.getYposition(), width, signalGroup.getHeight() );
        channelLabelsView.repaint( rect );
      }

      SignalElement currentElement = model.getSignalElementManager().getDigitalSignalByChannelIndex( oldIndex );
      if ( currentElement != null )
      {
        rect = new Rectangle( 0, currentElement.getYposition(), width, currentElement.getHeight() );
        channelLabelsView.repaint( rect );

        ElementGroup currentGroup = currentElement.getGroup();
        rect = new Rectangle( 0, currentGroup.getYposition(), width, currentGroup.getHeight() );
        channelLabelsView.repaint( rect );
      }
    }
  }

  /**
   * Disables the cursor "snap" mode.
   * 
   * @param aSnapMode
   *          <code>true</code> if the snap mode should be enabled,
   *          <code>false</code> otherwise.
   */
  public void setSnapModeEnabled( final boolean aSnapMode )
  {
    getSignalDiagramModel().setSnapCursorMode( aSnapMode );
  }

  /**
   * @param aChannelIndex
   * @param aType
   * @param aDirection
   */
  public void smartJump( final int aChannelIndex, final SmartJumpAction.JumpType aType,
      final SmartJumpAction.JumpDirection aDirection )
  {
    SmartJumpHelper jumpHelper = new SmartJumpHelper( this, aDirection, aType );

    long timestamp = jumpHelper.getSmartJumpPosition( aChannelIndex );
    if ( timestamp >= 0 )
    {
      scrollToTimestamp( timestamp );
    }
  }

  /**
   * @param aComponent
   */
  final void setSignalDiagram( final SignalDiagramComponent aComponent )
  {
    this.signalDiagram = aComponent;
  }

  /**
   * Sets signalDiagramModel to the given value.
   * 
   * @param aSignalDiagramModel
   *          the signalDiagramModel to set.
   */
  final void setSignalDiagramModel( final SignalDiagramModel aSignalDiagramModel )
  {
    this.signalDiagramModel = aSignalDiagramModel;
  }

  /**
   * @return
   */
  public ChannelLabelsView getChannelLabelsView()
  {
    JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, getSignalDiagram() );
    if ( scrollPane != null )
    {
      return ( ChannelLabelsView )scrollPane.getRowHeader().getView();
    }
    return null;
  }

  /**
   * @param aPoint
   * @return
   */
  private long locationToTimestamp( final Point aPoint )
  {
    return this.signalDiagram.getModel().locationToTimestamp( aPoint );
  }
  
  public Device getDevice()
  {
	return this.clientController.getDevice();  
  }
}
