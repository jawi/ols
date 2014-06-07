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

import java.util.*;

import javax.swing.*;

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
  private volatile EventAdmin eventAdmin;
  // Locally managed...
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
   * @return <code>true</code> if zooming is supported by the contained view,
   *         <code>false</code> otherwise.
   */
  public boolean canZoomView()
  {
    return this.view.canZoom();
  }

  /**
   * Disposes this view controller and all of its resources.
   * <p>
   * After calling this method, one can no longer use this controller, nor its
   * model or its view!
   * </p>
   */
  public void dispose()
  {
    if ( this.model != null )
    {
      this.model.getSession().close();
    }

    this.model = null;
    this.view = null;
  }

  /**
   * Shows an editor to edit the session name, and updates the model and view
   * accordingly.
   */
  public void editSessionName()
  {
    String newName = this.view.showSessionNameDialog();
    if ( newName != null )
    {
      JTabbedPane tabbedPane = SwingComponentUtils.getAncestorOfClass( JTabbedPane.class, this.view );
      if ( tabbedPane != null )
      {
        int idx = tabbedPane.getSelectedIndex();
        if ( idx >= 0 && idx < tabbedPane.getTabCount() )
        {
          tabbedPane.setTitleAt( idx, newName );
        }
      }
      this.model.setSessionName( newName );
    }
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
    if ( aTopic.startsWith( TOPIC_ANNOTATIONS ) )
    {
      // Annotation added or removed...
      this.view.handleEvent( aTopic, aEvent );
    }
  }

  /**
   * Moves a given channel to a given group and index.
   * 
   * @param aChannel
   *          the channel to move;
   * @param aGroup
   *          the (new) group to move the channel to;
   * @param aChildIndex
   *          the (new) index of the channel in the given group.
   */
  public void moveElement( Channel aChannel, ChannelGroup aGroup, int aChildIndex )
  {
    if ( aChannel.getGroup() != aGroup || aChannel.getIndex() != aChildIndex )
    {
      this.model.moveElement( aChannel, aGroup, aChildIndex );
    }
  }

  /**
   * Posts an asynchronous event.
   * 
   * @param aTopic
   *          the topic on which to post;
   * @param aProperties
   *          the event properties.
   */
  public void postEvent( String aTopic, Object... aProperties )
  {
    Map<Object, Object> props = new HashMap<Object, Object>();
    for ( int i = 0; i < aProperties.length; i += 2 )
    {
      props.put( aProperties[i], aProperties[i + 1] );
    }

    this.eventAdmin.postEvent( new Event( aTopic, props ) );
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
   * Sets the selected cursor to the one given.
   * 
   * @param aCursor
   *          the selected cursor, can be <code>null</code>.
   */
  public void setSelectedCursor( Cursor aCursor )
  {
    Cursor oldCursor = this.model.getSelectedCursor();
    if ( oldCursor != null )
    {
      this.view.repaintCursor( oldCursor );
    }
    this.model.setSelectedCursor( aCursor );
    this.view.repaintCursor( aCursor );
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
  protected void start( Component aComponent ) throws Exception
  {
    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        startController();
      }
    } );
  }
}
