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


import java.awt.*;
import java.beans.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomEvent;
import nl.lxtreme.ols.client.signaldisplay.dnd.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides the main component controller for the signal diagram component.
 */
public final class SignalDiagramController implements PropertyChangeListener
{
  // VARIABLES

  private final DragAndDropTargetController dndTargetController;
  private final IActionManager actionManager;

  private SignalDiagramComponent signalDiagram;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalDiagramController} instance.
   * 
   * @param aActionManager
   *          the action manager to use, cannot be <code>null</code>.
   */
  public SignalDiagramController( final IActionManager aActionManager )
  {
    this.actionManager = aActionManager;
    this.dndTargetController = new DragAndDropTargetController( this );
  }

  // METHODS

  /**
   * Adds a channel change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addChannelChangeListener( final IChannelChangeListener aListener )
  {
    getSignalDiagramModel().getChannelGroupManager().addChannelChangeListener( aListener );
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
   * @return the signalDiagram
   */
  public final SignalDiagramComponent getSignalDiagram()
  {
    return this.signalDiagram;
  }

  /**
   * @return
   */
  public SignalDiagramModel getSignalDiagramModel()
  {
    if ( this.signalDiagram == null )
    {
      return null;
    }
    return this.signalDiagram.getModel();
  }

  /**
   * Returns the zoom controller of this diagram.
   * 
   * @return the zoom controller, never <code>null</code>.
   */
  public ZoomController getZoomController()
  {
    return this.signalDiagram.getZoomController();
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
    }
  }

  /**
   * Recalculates the dimensions of the various components and repaints the
   * entire component.
   * <p>
   * SLOW METHOD: USE WITH CARE!
   * </p>
   */
  public void recalculateDimensions()
  {
    this.signalDiagram.recalculateDimensions();
    this.signalDiagram.repaint( 25L );
  }

  /**
   * Removes a channel change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeChannelChangeListener( final IChannelChangeListener aListener )
  {
    getSignalDiagramModel().getChannelGroupManager().removeChannelChangeListener( aListener );
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
   * Disables the cursor "snap" mode.
   * 
   * @param aSnapMode
   *          <code>true</code> if the snap mode should be enabled,
   *          <code>false</code> otherwise.
   */
  public void setSnapModeEnabled( final boolean aSnapMode )
  {
    getSignalDiagramModel().setSnapCursor( aSnapMode );
  }

  /**
   * Callback method that should be called when the current zoom-factor is
   * changed.
   */
  final void notifyZoomChange( final ZoomEvent aEvent )
  {
    final boolean dataAvailable = this.signalDiagram.getModel().hasData();

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        Action zoomInAction = SignalDiagramController.this.actionManager.getAction( ZoomInAction.ID );
        zoomInAction.setEnabled( dataAvailable && aEvent.canZoomIn() );

        Action zoomOutAction = SignalDiagramController.this.actionManager.getAction( ZoomOutAction.ID );
        zoomOutAction.setEnabled( dataAvailable && aEvent.canZoomOut() );

        Action zoomAllAction = SignalDiagramController.this.actionManager.getAction( ZoomAllAction.ID );
        zoomAllAction.setEnabled( dataAvailable && !aEvent.isZoomAll() );

        Action zoomOriginalAction = SignalDiagramController.this.actionManager.getAction( ZoomOriginalAction.ID );
        zoomOriginalAction.setEnabled( dataAvailable && !aEvent.isZoomOriginal() );

        SignalDiagramController.this.signalDiagram.recalculateDimensions();
      }
    } );
  }

  /**
   * @param aComponent
   */
  final void setSignalDiagram( final SignalDiagramComponent aComponent )
  {
    this.signalDiagram = aComponent;
  }

  /**
   * @param aPoint
   * @return
   */
  private long locationToTimestamp( final Point aPoint )
  {
    return this.signalDiagram.getModel().locationToTimestamp( aPoint );
  }
}
