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
package nl.lxtreme.ols.client2.views;


import static nl.lxtreme.ols.client2.ClientConstants.*;
import nl.lxtreme.ols.client2.Client.JumpDirection;
import nl.lxtreme.ols.client2.Client.JumpType;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.views.state.*;
import nl.lxtreme.ols.client2.views.waveform.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.osgi.service.event.*;


/**
 * Provides the main controller for the view, which either creates a
 * {@link StateView} or an {@link WaveformView}.
 */
public class ViewController
{
  // VARIABLES

  private final Session session;
  // Injected by Felix DM...
  private volatile ActionManager actionManager;

  private BaseView view;
  private ViewModel model;

  // CONSTRUCTORS

  /**
   * Creates a new, uninitialized, {@link ViewController} instance.
   */
  public ViewController( Session aSession )
  {
    this.session = aSession;
  }

  // METHODS

  /**
   * Adds a given listener to the list of marker change listeners.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addMarkerChangeListener( IMarkerChangeListener aListener )
  {
    this.model.addMarkerChangeListener( aListener );
  }

  /**
   * Returns the current value of actionManager.
   * 
   * @return the actionManager
   */
  public ActionManager getActionManager()
  {
    return this.actionManager;
  }

  /**
   * Returns the current model.
   * 
   * @return the view model, cannot be <code>null</code>.
   */
  public ViewModel getModel()
  {
    return this.model;
  }

  /**
   * Returns the current view.
   * 
   * @return the actual view, cannot be <code>null</code>.
   */
  public BaseView getView()
  {
    return this.view;
  }

  /**
   * {@inheritDoc}
   */
  public void handleEvent( String aTopic, Event aEvent )
  {
    if ( aTopic.startsWith( TOPIC_CLIENT_STATE ) )
    {
      // Client state changed...
      this.model.handleEvent( aTopic, aEvent );
    }
    else if ( aTopic.startsWith( TOPIC_ANNOTATIONS ) )
    {
      // Annotation added or removed...
      this.view.handleEvent( aTopic, aEvent );
    }
  }

  /**
   * Removes a given listener from the list of marker change listeners.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeMarkerChangeListener( IMarkerChangeListener aListener )
  {
    this.model.removeMarkerChangeListener( aListener );
  }

  /**
   * Repaints the <em>entire</em> view, which is a rather heavy operation and
   * should be used with care.
   */
  public void repaintAll()
  {
    this.view.repaint( 50L );
  }

  /**
   * Tells the view to scroll to the given timestamp.
   * 
   * @param timestamp
   *          the timestamp to scroll to; if &lt; 0, the view will scroll to the
   *          beginning; if {@value Long#MAX_VALUE}, the view will scroll to the
   *          end.
   */
  public void scrollToTimestamp( long timestamp )
  {
    this.view.scrollToTimestamp( timestamp );
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
    this.view.smartJump( aType, aDirection );
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    this.view.zoomAll();
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    this.view.zoomIn();
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    this.view.zoomOriginal();
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    this.view.zoomOut();
  }

  /**
   * Sets the model for this controller.
   * 
   * @param aModel
   *          the model to set, cannot be <code>null</code>.
   */
  final void setModel( ViewModel aModel )
  {
    if ( aModel == null )
    {
      throw new IllegalArgumentException( "Model cannot be null!" );
    }
    this.model = aModel;
  }

  /**
   * Sets the view for this controller.
   * 
   * @param aView
   *          the view to set, cannot be <code>null</code>.
   */
  final void setView( BaseView aView )
  {
    if ( aView == null )
    {
      throw new IllegalArgumentException( "View cannot be null!" );
    }
    this.view = aView;
  }

  /**
   * Starts this controller, its view and model. Should be called from the
   * Event-Dispatch thread (EDT).
   */
  final void startController()
  {
    ViewModel model;
    BaseView view;

    AcquisitionData data = this.session.getAcquiredData();
    if ( data.hasTimingData() )
    {
      model = new WaveformModel( this, this.session );
      view = new WaveformView( this, model );
    }
    else
    {
      model = new ViewModel( this.session );
      view = new StateView( this, model );
    }

    setView( view );
    setModel( model );

    this.view.initialize();
    this.model.initialize();
  }

  /**
   * Initializes this controller, its view and model.
   * <p>
   * This method is called by Felix DM upon starting of this component.
   * </p>
   */
  protected void start( Component aComponent )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        startController();
      }
    } );
  }
}
