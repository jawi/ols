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
package nl.lxtreme.ols.tool.linedecoder.impl;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.linedecoder.*;


/**
 * 
 */
public class LineDecoderToolContextImpl implements LineDecoderToolContext
{
  // VARIABLES

  private final ToolContext toolContext;
  private final int[] lines;
  private final boolean inverted;
  private final boolean recoverClock;
  private final int clockSpeed;

  // CONSTRUCTORS

  /**
   * Creates a new LineDecoderToolContextImpl instance.
   */
  public LineDecoderToolContextImpl( final ToolContext aContext, final int[] aLines, final boolean aInverted,
      final boolean aRecoverClock, final int aClockSpeed )
  {
    this.toolContext = aContext;
    this.lines = aLines;
    this.inverted = aInverted;
    this.recoverClock = aRecoverClock;
    this.clockSpeed = aClockSpeed;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int getChannels()
  {
    return this.toolContext.getChannels();
  }

  /**
   * Returns the current value of clockSpeed.
   * 
   * @return the clockSpeed
   */
  public int getClockSpeed()
  {
    return this.clockSpeed;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor getCursor( final int aSelectedIndex )
  {
    return this.toolContext.getCursor( aSelectedIndex );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult getData()
  {
    return this.toolContext.getData();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getEnabledChannels()
  {
    return this.toolContext.getEnabledChannels();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getEndSampleIndex()
  {
    return this.toolContext.getEndSampleIndex();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLength()
  {
    return this.toolContext.getLength();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int[] getLineChannels()
  {
    return this.lines;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getStartSampleIndex()
  {
    return this.toolContext.getStartSampleIndex();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isInverted()
  {
    return this.inverted;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isRecoverClock()
  {
    return this.recoverClock;
  }
}
