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
package nl.lxtreme.ols.client.view;


import javax.swing.*;

import nl.lxtreme.ols.client.view.state.*;
import nl.lxtreme.ols.client.view.waveform.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides the main controller for the view, which either creates a
 * {@link StateView} or an {@link WaveformView}.
 */
public class ViewController
{
  // VARIABLES

  private BaseView view;
  private ViewModel model;

  // CONSTRUCTORS

  /**
   * Creates a new, uninitialized, {@link ViewController} instance.
   */
  public ViewController()
  {
    // Nop
  }

  // METHODS

  private static BaseView createView( ViewController aController, ViewModel aModel )
  {
    BaseView view;
    if ( aModel.hasTimingData() )
    {
      view = new WaveformView( aController, aModel );
    }
    else
    {
      view = new StateView( aController, aModel );
    }
    return view;
  }

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
   * Initializes this controller, its view and model. Should be called from the
   * Event-Dispatch thread (EDT).
   */
  public void initialize( AcquisitionData aData )
  {
    if ( !SwingUtilities.isEventDispatchThread() )
    {
      throw new IllegalStateException( "Can only be called from EDT!?" );
    }

    ViewModel model = new ViewModel( aData );
    BaseView view = createView( this, model );

    setView( view );
    setModel( model );

    this.model.initialize();
    this.view.initialize();
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
}
