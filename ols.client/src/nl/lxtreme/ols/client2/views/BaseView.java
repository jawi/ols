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


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.Client.JumpDirection;
import nl.lxtreme.ols.client2.Client.JumpType;
import nl.lxtreme.ols.client2.views.state.*;
import nl.lxtreme.ols.client2.views.waveform.*;
import nl.lxtreme.ols.common.Unit.Time;
import nl.lxtreme.ols.common.Unit.Value;
import nl.lxtreme.ols.common.acquisition.Cursor;

import org.osgi.service.event.Event;


/**
 * Base class for both {@link StateView} and {@link WaveformView}.
 */
public abstract class BaseView extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  protected final ViewController controller;
  protected final ViewModel model;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BaseView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  protected BaseView( ViewController aController, ViewModel aModel )
  {
    this.controller = aController;
    this.model = aModel;

    setLayout( new BorderLayout() );
  }

  // METHODS

  /**
   * @return <code>true</code> if zooming is supported by this view,
   *         <code>false</code> otherwise.
   */
  public boolean canZoom()
  {
    return false;
  }

  /**
   * Returns the interval displayed by the current view.
   * 
   * @return a interval.
   */
  public abstract double getDisplayedInterval();

  /**
   * @return a zoom factor of 1.0 (always).
   */
  public double getZoomFactor()
  {
    return 1.0;
  }

  /**
   * {@inheritDoc}
   */
  public void handleEvent( String aTopic, Event aEvent )
  {
    // Nop
  }

  /**
   * Initializes this view.
   */
  public abstract void initialize();

  /**
   * {@inheritDoc}
   */
  @Override
  public final void paint( Graphics aGraphics )
  {
    setCursor( java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR ) );
    try
    {
      if ( Boolean.getBoolean( "nl.lxtreme.ols.client.debug" ) )
      {
        long startTime = System.currentTimeMillis();
        try
        {
          super.paint( aGraphics );
        }
        finally
        {
          long endTime = System.currentTimeMillis();

          // @formatter:off
          System.out.printf( "%s rendering time = %s.%n", getClass().getSimpleName(), Value.asTime( endTime - startTime, Time.MS ) );
          // @formatter:on
        }
      }
      else
      {
        super.paint( aGraphics );
      }
    }
    finally
    {
      setCursor( null );
    }
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
    // Nop
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
    // Nop
  }

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    // Nop
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    // Nop
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    // Nop
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    // Nop
  }

  /**
   * Posts an asynchronous event.
   * 
   * @param aTopic
   *          the topic on which to post;
   * @param aProperties
   *          the event properties.
   */
  protected final void postEvent( String aTopic, Object... aProperties )
  {
    this.controller.postEvent( aTopic, aProperties );
  }

  /**
   * Repaints the area taken up by the given cursor on screen.
   * 
   * @param aCursor
   *          the cursor to repaint, cannot be <code>null</code>.
   */
  protected void repaintCursor( Cursor aCursor )
  {
    // Nop
  }
}
