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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.base;


import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.osgi.*;

import org.osgi.framework.*;


/**
 * 
 */
public class ToolProgressListenerServiceTracker implements ToolProgressListener
{
  // VARIABLES

  private final WhiteboardHelper<ToolProgressListener> toolProgressListenerHelper;

  // CONSTRUCTORS

  /**
   * Creates a new ToolProgressListenerServiceTracker instance.
   * 
   * @param aContext
   */
  public ToolProgressListenerServiceTracker( final BundleContext aContext )
  {
    this.toolProgressListenerHelper = new WhiteboardHelper<ToolProgressListener>( aContext, ToolProgressListener.class );
  }

  // METHODS

  /**
   * Closes this progress listener.
   */
  public void close()
  {
    try
    {
      this.toolProgressListenerHelper.close();
    }
    catch ( IllegalStateException exception )
    {
      // Ignore; bundle context probably is incorrect...
    }
  }

  /**
   * Opens this progress listener for business.
   */
  public void open()
  {
    this.toolProgressListenerHelper.open( true /* trackAllServices */);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setProgress( final int aPercentage )
  {
    this.toolProgressListenerHelper.accept( new WhiteboardHelper.Visitor<ToolProgressListener>()
    {
      @Override
      public void visit( final ToolProgressListener aService )
      {
        aService.setProgress( aPercentage );
      }
    } );
  }
}
