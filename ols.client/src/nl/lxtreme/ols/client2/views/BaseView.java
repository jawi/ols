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

import nl.lxtreme.ols.client2.views.state.*;
import nl.lxtreme.ols.client2.views.waveform.*;
import nl.lxtreme.ols.common.*;


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
   * Initializes this view.
   */
  public abstract void initialize();

  /**
   * {@inheritDoc}
   */
  @Override
  public final void paint( Graphics aGraphics )
  {
    if ( Boolean.getBoolean( "nl.lxtreme.ols.client.debug" ) )
    {
      long startTime = System.nanoTime();
      try
      {
        super.paint( aGraphics );
      }
      finally
      {
        long endTime = System.nanoTime();
        double renderTime = ( endTime - startTime ) / 1.0e9;

        System.out.printf( "%s rendering time = %s.%n", getClass().getSimpleName(), Unit.Time.format( renderTime ) );
      }
    }
    else
    {
      super.paint( aGraphics );
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
    // TODO
  }
}
