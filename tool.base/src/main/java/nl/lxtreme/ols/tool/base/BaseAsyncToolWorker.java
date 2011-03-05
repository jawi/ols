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
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a base class for tools wishing to do their processing in the
 * background.
 * <p>
 * Implementors of this class should implement {@link #doInBackground()} and do
 * the actual work. During this process, you can use {@link #setProgress(int)}
 * and {@link #publish(Object...)} to communicate with the GUI. See JavaDoc on
 * SwingWorker for more information on this.
 * </p>
 * <p>
 * For convenience, this base class provides direct access to all captured data.
 * </p>
 * 
 * @see SwingWorker
 * @see CapturedData
 */
public abstract class BaseAsyncToolWorker<T> extends SwingWorker<T, Integer> implements AcquisitionResult
{
  // VARIABLES

  private final DataContainer data;
  private final ToolContext context;

  // CONSTRUCTORS

  /**
   * Creates a new BaseToolWorker instance.
   * 
   * @param aData
   *          the data container to work with, could be <code>null</code>.
   */
  public BaseAsyncToolWorker( final DataContainer aData, final ToolContext aContext )
  {
    this.data = aData;
    this.context = aContext;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#addChannelAnnotation(int, long,
   *      long, Object)
   */
  public final void addChannelAnnotation( final int aChannelIdx, final int aStartIdx, final int aEndIdx,
      final Object aData )
  {
    this.data.addChannelAnnotation( aChannelIdx, aStartIdx, aEndIdx, aData );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#clearChannelAnnotations(int)
   */
  public final void clearChannelAnnotations( final int aChannelIdx )
  {
    this.data.clearChannelAnnotations( aChannelIdx );
  }

  /**
   * Returns whether or not this worker contains any data to work on.
   * 
   * @return <code>true</code> if there is any data to process by this worker,
   *         <code>false</code> otherwise.
   */
  public final boolean containsData()
  {
    return ( this.data != null ) && this.data.hasCapturedData();
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
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannelLabel(int)
   */
  public final String getChannelLabel( final int aChannelIdx )
  {
    return this.data.getChannelLabel( aChannelIdx );
  }

  /**
   * Returns the channel label for a channel with the given index, using the
   * given default label if the channel's label is not set.
   * 
   * @param aChannelIdx
   *          the index of the channel to return the label for, >= 0 && < 32;
   * @param aDefault
   *          the default label to use in case the channel label is not set.
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannelLabel(int)
   */
  public final String getChannelLabel( final int aChannelIdx, final String aDefault )
  {
    String result = getChannelLabel( aChannelIdx );
    if ( ( result == null ) || result.trim().isEmpty() )
    {
      result = aDefault;
    }
    return result;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannels()
   */
  @Override
  public final int getChannels()
  {
    return this.data.getChannels();
  }

  /**
   * Returns the context in which this tool worker should to its job.
   * 
   * @return the context, never <code>null</code>.
   */
  public ToolContext getContext()
  {
    return this.context;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getCursorPosition(int)
   */
  public final Long getCursorPosition( final int aCursorIdx ) throws IllegalArgumentException
  {
    return this.data.getCursorPosition( aCursorIdx );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getEnabledChannels()
   */
  @Override
  public final int getEnabledChannels()
  {
    return this.data.getEnabledChannels();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleIndex(long)
   */
  @Override
  public final int getSampleIndex( final long aAbs )
  {
    return this.data.getSampleIndex( aAbs );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleRate()
   */
  @Override
  public final int getSampleRate()
  {
    return this.data.getSampleRate();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTimestamps()
   */
  @Override
  public final long[] getTimestamps()
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
  public final int[] getValues()
  {
    return this.data.getValues();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasCapturedData()
   */
  public final boolean hasCapturedData()
  {
    return this.data.hasCapturedData();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTimingData()
   */
  @Override
  public final boolean hasTimingData()
  {
    return this.data.hasTimingData();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTriggerData()
   */
  @Override
  public final boolean hasTriggerData()
  {
    return this.data.hasTriggerData();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#isChannelLabelSet(int)
   */
  public final boolean isChannelLabelSet( final int aChannelIdx )
  {
    return this.data.isChannelLabelSet( aChannelIdx );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#isCursorPositionSet(int)
   */
  public final boolean isCursorPositionSet( final int aCursorIdx )
  {
    return this.data.isCursorPositionSet( aCursorIdx );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#isCursorsEnabled()
   */
  public final boolean isCursorsEnabled()
  {
    return this.data.isCursorsEnabled();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#setChannelLabel(int, String)
   */
  public final void setChannelLabel( final int aChannelIdx, final String aLabel )
  {
    this.data.setChannelLabel( aChannelIdx, aLabel );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#setChannelLabel(int, String)
   */
  public final String updateChannelLabel( final int aChannelIdx, final String aLabel )
  {
    final String label = getChannelLabel( aChannelIdx, aLabel );
    setChannelLabel( aChannelIdx, label );
    return label;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#setCapturedData(CapturedData)
   */
  protected final void setCapturedData( final AcquisitionResult aCapturedData )
  {
    if ( aCapturedData == this )
    {
      throw new IllegalArgumentException( "Invalid captured data instance!" );
    }
    this.data.setCapturedData( aCapturedData );
  }
}

/* EOF */
