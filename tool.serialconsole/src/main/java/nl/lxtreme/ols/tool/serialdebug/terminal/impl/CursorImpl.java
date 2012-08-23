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
package nl.lxtreme.ols.tool.serialdebug.terminal.impl;


import nl.lxtreme.ols.tool.serialdebug.terminal.*;


/**
 * Provides an implementation of {@link ICursor}.
 */
public class CursorImpl implements ICursor
{
  // VARIABLES

  private int blinkRate;
  private boolean visible;
  private int x;
  private int y;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorImpl} instance.
   */
  public CursorImpl()
  {
    this.blinkRate = 500;
    this.visible = true;
    this.x = 0;
    this.y = 0;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int getBlinkRate()
  {
    return this.blinkRate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getX()
  {
    return this.x;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getY()
  {
    return this.y;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible()
  {
    return this.visible;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setBlinkRate( final int aRate )
  {
    if ( aRate < 0 )
    {
      throw new IllegalArgumentException( "Invalid blink rate!" );
    }
    this.blinkRate = aRate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setVisible( final boolean aVisible )
  {
    this.visible = aVisible;
  }

  /**
   * {@inheritDoc}
   */
  final void setPosition( final int aX, final int aY )
  {
    this.x = aX;
    this.y = aY;
  }
}
