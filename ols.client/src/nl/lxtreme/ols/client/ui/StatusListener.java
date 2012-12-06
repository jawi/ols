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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui;


import java.text.*;

import nl.lxtreme.ols.tool.api.*;


/**
 * Provides an abstraction for setting the status message and/or progress of the
 * main client.
 */
public interface StatusListener extends ToolProgressListener
{
  // METHODS

  /**
   * Sets the status message.
   * 
   * @param aMessage
   *          the message to display, can be parameterized by using the same
   *          syntax as used in {@link MessageFormat};
   * @param aParameters
   *          the optional message parameters.
   */
  void setStatus( String aMessage, Object... aParameters );

  /**
   * Reports the progress of something as a percentage.
   * 
   * @param aPercentage
   *          the percentage to report, >= 0 && <= 100.
   */
  @Override
  public void setProgress( int aPercentage );

}
