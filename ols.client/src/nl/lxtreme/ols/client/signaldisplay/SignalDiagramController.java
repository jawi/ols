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

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomEvent;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomListener;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.client.signaldisplay.view.channellabels.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides the main component controller for the signal diagram component.
 */
public class SignalDiagramController implements ZoomListener, PropertyChangeListener
{
  // VARIABLES

  private final DragAndDropTargetController dndTargetController;

  private final SignalDiagramModel signalDiagramModel;
  private final ZoomController zoomController;
  private SignalDiagramComponent signalDiagram;

  // Injected by Felix DM...
  private volatile ActionManager actionManager;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalDiagramController} instance.
   */
  public SignalDiagramController()
  {
    this.dndTargetController = new DragAndDropTargetController( this );

    this.zoomController = new ZoomController( this );
    this.signalDiagramModel = new SignalDiagramModel( this );
    // Register our controller as listener for zooming events...
    this.zoomController.addZoomListener( this );
  }

  // METHODS

  /**
   * Adds an annotation data change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addAnnotationDataChangedListener( final IAnnotationDataChangedListener aListener )
  {
    getSignalDiagramModel().addAnnotationDataChangedListener( aListener );
  }

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
  public void addCursorChangeListener( final IMarkerChangeListener aListener )
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
   * Called when annotations are cleared.
   * 
   * @param aChannelIdx
   *          the channel index of the channel whose annotations are cleared, or
   *          <code>null</code> if all annotations are cleared.
   */
  public void clearAnnotations( final Integer aChannelIdx )
  {
    getSignalDiagramModel().fireAnnotationDataClearedEvent( aChannelIdx );
  }

  /**
   * @return the current annotation data, never <code>null</code>.
   */
  public final AnnotationHelper getAnnotationsHelper()
  {
    return getSignalDiagramModel().getAnnotationHelper();
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
    final SignalElement element = getSignalDiagramModel().findSignalElement( aPoint );
    return ( element != null ) ? element.getType() : null;
  }

  /**
   * Returns the zoom controller of this diagram.
   * 
   * @return the zoom controller, never <code>null</code>.
   */
  public final ZoomController getZoomController()
  {
    return this.zoomController;
  }

  /**
   * Handles the given annotation for further processing.
   * 
   * @param aAnnotation
   *          the annotation to handle, cannot be <code>null</code>.
   */
  public void handleAnnotation( final Annotation aAnnotation )
  {
    // For label annotations we should set the channel label...
    if ( aAnnotation instanceof LabelAnnotation )
    {
      LabelAnnotation label = ( LabelAnnotation )aAnnotation;
      SignalElement channel = getSignalDiagramModel().getSignalElement( label.getChannelIndex() );
      channel.setLabel( label.getData() );
    }
  }

  /**
   * @return <code>true</code> if in measurement mode, <code>false</code>
   *         otherwise.
   */
  public boolean isMeasurementMode()
  {
    return getSignalDiagramModel().isMeasurementMode();
  }

  /**
   * @return <code>true</code> if the snap cursor mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isSnapCursorMode()
  {
    return getSignalDiagramModel().isSnapCursorMode();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void notifyZoomChange( final ZoomEvent aEvent )
  {
    final boolean dataAvailable = getSignalDiagramModel().hasData();
    final ActionManager am = this.actionManager;

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        // Update the zoom action's state... TODO move this logic to the
        // Actions!!!
        Action zoomInAction = am.getAction( ZoomInAction.ID );
        zoomInAction.setEnabled( dataAvailable && aEvent.canZoomIn() );

        Action zoomOutAction = am.getAction( ZoomOutAction.ID );
        zoomOutAction.setEnabled( dataAvailable && aEvent.canZoomOut() );

        Action zoomAllAction = am.getAction( ZoomAllAction.ID );
        zoomAllAction.setEnabled( dataAvailable && !aEvent.isZoomAll() );

        Action zoomOriginalAction = am.getAction( ZoomOriginalAction.ID );
        zoomOriginalAction.setEnabled( dataAvailable && !aEvent.isZoomOriginal() );

        // Update the main component's state...
        final SignalDiagramComponent diagram = getSignalDiagram();

        // Recalculate all dimensions...
        diagram.calculateDimensions();
        // Notify that everything needs to be revalidated as well...
        diagram.revalidateAll();
        // Issue #100: in case the factor is changed, we need to repaint all
        // components...
        diagram.repaintAll();
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
    System.out.println( "PropertyChanged: " + name ); // XXX
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
    this.signalDiagram.calculateDimensions();
    this.signalDiagram.revalidateAll();
    this.signalDiagram.repaintAll();
  }

  /**
   * Removes an annotation data change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeAnnotationDataChangedListener( final IAnnotationDataChangedListener aListener )
  {
    getSignalDiagramModel().removeAnnotationDataChangedListener( aListener );
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
   * Removes a cursor change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeCursorChangeListener( final IMarkerChangeListener aListener )
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
  public void scrollToTimestamp( final Long aTimestamp )
  {
    if ( aTimestamp != null )
    {
      getSignalDiagram().scrollToTimestamp( aTimestamp.longValue() );
    }
  }

  /**
   * Sets the data model for this controller.
   * 
   * @param aData
   *          the data set to set, cannot be <code>null</code>.
   */
  public void setAcquisitionData( final AcquisitionData aData )
  {
    getSignalDiagramModel().setAcquisitionData( aData );

    recalculateDimensions();
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
   * Starts this component.
   */
  public void setDefaultSettings()
  {
    // Set the correct defaults...
    setSnapModeEnabled( UIManager.getBoolean( UIManagerKeys.SNAP_CURSORS_DEFAULT ) );
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

    final SignalElement signalElement = model.findSignalElement( aPosition );
    final SignalElement currentElement = model.getSelectedChannel();

    int currentIndex = -1;
    if ( ( currentElement != null ) && currentElement.isDigitalSignal() )
    {
      currentIndex = currentElement.getChannel().getIndex();
    }

    int newIndex = -1;
    if ( ( signalElement != null ) && signalElement.isDigitalSignal() )
    {
      newIndex = signalElement.getChannel().getIndex();
    }

    if ( currentIndex != newIndex )
    {
      model.setSelectedChannelIndex( newIndex );

      ChannelLabelsView channelLabelsView = getChannelLabelsView();
      if ( channelLabelsView != null )
      {
        int width = channelLabelsView.getWidth();

        // Repaint the affected areas
        if ( signalElement != null )
        {
          Rectangle rect1 = new Rectangle( 0, signalElement.getYposition(), width, signalElement.getHeight() );
          channelLabelsView.repaint( rect1 );
        }

        if ( currentElement != null )
        {
          Rectangle rect2 = new Rectangle( 0, currentElement.getYposition(), width, currentElement.getHeight() );
          channelLabelsView.repaint( rect2 );
        }
      }
    }
  }

  /**
   * Sets signalDiagram to the given value.
   * 
   * @param aSignalDiagram
   *          the signalDiagram to set.
   */
  public void setSignalDiagram( final SignalDiagramComponent aSignalDiagram )
  {
    this.signalDiagram = aSignalDiagram;
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
   * Notifies all listeners that the annotation data is changed.
   */
  public void updateAnnotations()
  {
    // Notify all listeners that something has happened...
    getSignalDiagramModel().fireAnnotationDataChangedEvent();
  }

  /**
   * @return the composition for Felix DM to inject the members of.
   */
  final Object[] getComposition()
  {
    return new Object[] { this, this.signalDiagramModel, this.zoomController };
  }

  /**
   * @return
   */
  private ChannelLabelsView getChannelLabelsView()
  {
    JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, getSignalDiagram() );
    if ( scrollPane != null )
    {
      return ( ChannelLabelsView )scrollPane.getRowHeader().getView();
    }
    return null;
  }
}
