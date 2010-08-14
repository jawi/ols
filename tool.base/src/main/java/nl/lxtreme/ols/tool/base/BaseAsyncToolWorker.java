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

import nl.lxtreme.ols.api.data.*;


/**
 * Provides a base class for tools wishing to do their processing in the
 * background.
 * <p>
 * For convenience, this base class provides direct access to all captured data.
 * </p>
 * 
 * @see SwingWorker
 * @see CapturedData
 */
public abstract class BaseAsyncToolWorker<T> extends SwingWorker<T, Integer> implements CapturedData
{
  // VARIABLES

  private final AnnotatedData data;

  // CONSTRUCTORS

  /**
   * Creates a new BaseToolWorker instance.
   * 
   * @param aData
   *          the captured data to process, can be <code>null</code>.
   */
  public BaseAsyncToolWorker( final AnnotatedData aData )
  {
    this.data = aData;
  }

  // METHODS

  /**
   * calculate the time offset
   * 
   * @param aTime
   *          absolute sample number
   * @return time relative to data
   */
  public final long calculateTime( final long aTime )
  {
    return this.data.calculateTime( aTime );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getAbsoluteLength()
   */
  @Override
  public final long getAbsoluteLength()
  {
    return this.data.getAbsoluteLength();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannels()
   */
  @Override
  public int getChannels()
  {
    return this.data.getChannels();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getDataAt(long)
   */
  @Override
  public int getDataAt( final long aAbs )
  {
    return this.data.getDataAt( aAbs );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getEnabledChannels()
   */
  @Override
  public int getEnabledChannels()
  {
    return this.data.getEnabledChannels();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleIndex(long)
   */
  @Override
  public int getSampleIndex( final long aAbs )
  {
    return this.data.getSampleIndex( aAbs );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleRate()
   */
  @Override
  public int getSampleRate()
  {
    return this.data.getSampleRate();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTimestamps()
   */
  @Override
  public long[] getTimestamps()
  {
    return this.data.getTimestamps();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTriggerPosition()
   */
  @Override
  public long getTriggerPosition()
  {
    return this.data.getTriggerPosition();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getValues()
   */
  @Override
  public int[] getValues()
  {
    return this.data.getValues();
  }

  /**
   * Returns whether there is captured data to process.
   * 
   * @return <code>true</code> if there is captured data to process,
   *         <code>false</code> otherwise.
   */
  public final boolean hasCapturedData()
  {
    return this.data.hasCapturedData();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTimingData()
   */
  @Override
  public boolean hasTimingData()
  {
    return this.data.hasTimingData();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTriggerData()
   */
  @Override
  public boolean hasTriggerData()
  {
    return this.data.hasTriggerData();
  }

  /**
   * Updates the captured data.
   * 
   * @param aCapturedData
   *          the captured data to update, cannot be <code>null</code> nor be
   *          "this".
   */
  protected final void setCapturedData( final CapturedData aCapturedData )
  {
    if ( aCapturedData == this )
    {
      throw new IllegalArgumentException( "Invalid captured data instance!" );
    }
    this.data.setCapturedData( aCapturedData );
  }

}

/* EOF */
