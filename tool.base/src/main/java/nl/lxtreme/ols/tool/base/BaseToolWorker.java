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


import javax.swing.*;

import nl.lxtreme.ols.api.*;


/**
 * Provides a base class for tools wishing to do their processing in the background.
 * 
 * @see SwingWorker
 */
public abstract class BaseToolWorker<T> extends SwingWorker<T, Integer>
{
  // VARIABLES

  private final CapturedData data;

  // CONSTRUCTORS

  /**
   * Creates a new BaseToolWorker instance.
   * 
   * @param aData
   *          the captured data to process, can be <code>null</code>.
   */
  public BaseToolWorker( final CapturedData aData )
  {
    this.data = aData;
  }

  // METHODS

  /**
   * Returns the captured data.
   * 
   * @return the captured data, can be <code>null</code>.
   */
  protected final CapturedData getData()
  {
    return this.data;
  }
}

/* EOF */
