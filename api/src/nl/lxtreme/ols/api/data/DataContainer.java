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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.api.data;


import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.project.*;


/**
 * Provides a container for captured data in which the data can be annotated
 * with "any" kind of information, such as cursors, protocol decoding
 * information, and so on.
 * <p>
 * Data files will start with a header containing meta data marked by lines
 * starting with ";". The actual readout values will follow after the header. A
 * value is a logic level transition of one channel. The associated timestamp
 * since sample start (start has timestamp 0) is stored, too after a @
 * character. This is called compressed format. The handling of the data within
 * the class is the same. A value is 32bits long. The value is encoded in hex
 * and each value is followed by a new line.
 * 
 * @deprecated
 */
@Deprecated
public final class DataContainer implements AcquisitionResult
{
  // VARIABLES

  /** The individual annotations. */
  private final Map<Integer, ChannelAnnotations> annotations;
  private final ProjectManager projectManager;

  // CONSTRUCTORS

  /**
   * Creates a new DataContainer instance.
   * 
   * @param aProjectManager
   *          the project manager to use for this container, cannot be
   *          <code>null</code>.
   */
  public DataContainer( final ProjectManager aProjectManager )
  {
    if ( aProjectManager == null )
    {
      throw new IllegalArgumentException( "Project manager cannot be null!" );
    }
    this.projectManager = aProjectManager;

    this.annotations = new HashMap<Integer, ChannelAnnotations>();
  }

  // METHODS

  /**
   * Adds a channel annotation for the channel with the given index.
   * 
   * @param aChannelIdx
   *          the index of channel to remove all annotations for, >=0 && < 32.
   * @param aStartIdx
   *          the start index;
   * @param aEndIdx
   *          the end index;
   * @param aData
   *          the data.
   */
  public void addChannelAnnotation( final int aChannelIdx, final long aStartTimestamp, final long aEndTimestamp,
      final Object aData )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    ChannelAnnotations annotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( annotations == null )
    {
      annotations = new ChannelAnnotations( aChannelIdx );
      this.annotations.put( Integer.valueOf( aChannelIdx ), annotations );
    }
    annotations.addAnnotation( aStartTimestamp, aEndTimestamp, aData );
  }

  /**
   * Calculates the time value corresponding to the given sample index.
   * 
   * @param aSampleIndex
   *          the sample index to get the time value for, >= 0.
   * @return the time value, in seconds.
   */
  public double calculateTime( final int aSampleIndex )
  {
    double timestamp = calculateTimeOffset( getTimestamps()[aSampleIndex] );
    return ( timestamp / getSampleRate() );
  }

  /**
   * Clears <em>all</em> channel annotations for the channel with the given
   * index.
   * 
   * @param aChannelIdx
   *          the index of channel to remove all annotations for, >=0 && < 32.
   */
  public void clearChannelAnnotations( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    this.annotations.remove( Integer.valueOf( aChannelIdx ) );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getAbsoluteLength()
   */
  @Override
  public long getAbsoluteLength()
  {
    return hasCapturedData() ? getAcquisitionData().getAbsoluteLength() : Ols.NOT_AVAILABLE;
  }

  /**
   * Returns all channel labels.
   * 
   * @return an array of all channel's label, never <code>null</code>.
   */
  public Channel[] getAllChannels()
  {
    return getCurrentDataSet().getChannels();
  }

  /**
   * Returns the number of channel blocks that are available in the data.
   * 
   * @return a block count, >= 0 && < {@value nl.lxtreme.ols.api.Ols#MAX_BLOCKS}
   *         .
   */
  public int getBlockCount()
  {
    return ( int )Math.min( Ols.MAX_BLOCKS, Math.ceil( getChannels() / ( double )Ols.CHANNELS_PER_BLOCK ) );
  }

  /**
   * Returns the channel annotations.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0 &&
   *          < 32.
   * @param aTimeIndex
   *          the time index to get the channel annotation for, >= 0.
   * @return the channel annotations, can be <code>null</code>.
   */
  public ChannelAnnotation getChannelAnnotation( final int aChannelIdx, final int aTimeIndex )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }

    final ChannelAnnotations channelAnnotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( channelAnnotations == null )
    {
      return null;
    }
    return channelAnnotations.getAnnotation( aTimeIndex );
  }

  /**
   * Returns the channel annotations.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0 &&
   *          < 32.
   * @param aStartIdx
   *          the start time/sample index to retrieve the channel annotations
   *          for, >= 0;
   * @param aEndIdx
   *          the end time/sample index to retrieve the channel annotations for,
   *          >= 0.
   * @return the channel annotations, can be <code>null</code>.
   */
  public Iterator<ChannelAnnotation> getChannelAnnotations( final int aChannelIdx, final int aStartIdx,
      final int aEndIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }

    final ChannelAnnotations channelAnnotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( channelAnnotations == null )
    {
      return Collections.<ChannelAnnotation> emptyList().iterator();
    }
    return channelAnnotations.getAnnotations( aStartIdx, aEndIdx );
  }

  /**
   * Returns the channel label.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the label for, >= 0 && <
   *          {@value nl.lxtreme.ols.api.Ols#MAX_CHANNELS}.
   * @return the channel's label, can be <code>null</code>.
   */
  public String getChannelLabel( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    return getAllChannels()[aChannelIdx].getLabel();
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannels()
   */
  @Override
  public int getChannels()
  {
    return hasCapturedData() ? getAcquisitionData().getChannels() : Ols.NOT_AVAILABLE;
  }

  /**
   * Returns the number of channels available in the block with the given block
   * number.
   * <p>
   * It is assumed that only the last block can contain less than
   * {@value nl.lxtreme.ols.api.Ols#CHANNELS_PER_BLOCK} channels. All preceeding
   * blocks (if available) are considered to be "complete" blocks.
   * </p>
   * 
   * @param aBlockNr
   *          the block number, >= 0 && <
   *          {@value nl.lxtreme.ols.api.Ols#MAX_BLOCKS}.
   * @return the number of channels for the given block, >= 0 && <
   *         {@link #getBlockCount()}.
   * @throws IllegalArgumentException
   *           in case the given block number was invalid.
   */
  public int getChannelsForBlock( final int aBlockNr )
  {
    final int blockCount = getBlockCount();
    if ( ( aBlockNr < 0 ) || ( aBlockNr >= blockCount ) )
    {
      throw new IllegalArgumentException( "Invalid block number: " + aBlockNr + "!" );
    }

    int result = Ols.CHANNELS_PER_BLOCK;
    if ( aBlockNr == ( blockCount - 1 ) )
    {
      final int remainder = getChannels() % Ols.CHANNELS_PER_BLOCK;
      if ( remainder != 0 )
      {
        result = remainder;
      }
    }

    return result;
  }

  /**
   * Get position of a cursor.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, should be >= 0 and < 10.
   * @return a cursor position, or Long.MIN_VALUE if not set.
   * @throws IllegalArgumentException
   *           in case an invalid cursor index was given.
   */
  public Long getCursorPosition( final int aCursorIdx ) throws IllegalArgumentException
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > ( Ols.MAX_CURSORS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index: " + aCursorIdx + "! Should be between 0 and "
          + Ols.MAX_CURSORS );
    }
    final Cursor[] cursorPositions = getCursors();
    if ( ( cursorPositions == null ) || ( cursorPositions[aCursorIdx] == null ) )
    {
      return null;
    }

    return Long.valueOf( cursorPositions[aCursorIdx].getTimestamp() );
  }

  /**
   * Returns the (absolute) time value for the cursor indicated by the given
   * index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to return as time, should be >= 0 and <
   *          10.
   * @return the time value (in seconds), or -1.0 if the cursor is not
   *         available.
   */
  public Double getCursorTimeValue( final int aCursorIdx )
  {
    if ( !isCursorPositionSet( aCursorIdx ) )
    {
      return null;
    }

    Long cursorPos = getCursorPosition( aCursorIdx );
    return Double.valueOf( calculateTimeOffset( cursorPos.longValue() )
        / ( double )getAcquisitionData().getSampleRate() );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getEnabledChannels()
   */
  @Override
  public int getEnabledChannels()
  {
    return hasCapturedData() ? getAcquisitionData().getEnabledChannels() : Ols.NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleIndex(long)
   */
  @Override
  public int getSampleIndex( final long aAbs )
  {
    return hasCapturedData() ? getAcquisitionData().getSampleIndex( aAbs ) : Ols.NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleRate()
   */
  @Override
  public int getSampleRate()
  {
    return hasCapturedData() ? getAcquisitionData().getSampleRate() : Ols.NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTimestamps()
   */
  @Override
  public long[] getTimestamps()
  {
    return hasCapturedData() ? getAcquisitionData().getTimestamps() : new long[0];
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTriggerPosition()
   */
  @Override
  public long getTriggerPosition()
  {
    return hasCapturedData() && hasTriggerData() ? getAcquisitionData().getTriggerPosition() : Ols.NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getValues()
   */
  @Override
  public int[] getValues()
  {
    return hasCapturedData() ? getAcquisitionData().getValues() : new int[0];
  }

  /**
   * Returns whether any captured data is available.
   * 
   * @return <code>true</code> if there is captured data, <code>false</code>
   *         otherwise.
   */
  public boolean hasCapturedData()
  {
    return getAcquisitionData() != null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTimingData()
   */
  @Override
  public boolean hasTimingData()
  {
    return hasCapturedData() ? getAcquisitionData().hasTimingData() : false;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTriggerData()
   */
  @Override
  public boolean hasTriggerData()
  {
    return hasCapturedData() ? getAcquisitionData().hasTriggerData() : false;
  }

  /**
   * Returns whether a channel label is set or not.
   * 
   * @param aChannelIdx
   *          the channel index to check whether its label is set, >= 0 && < 32.
   * @return <code>true</code> if there a non-empty label set for the given
   *         channel index, <code>false</code> otherwise.
   */
  public boolean isChannelLabelSet( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    final Channel channel = getAllChannels()[aChannelIdx];
    return channel.hasName();
  }

  /**
   * Returns whether or not the cursor with the given index is set.
   * 
   * @param aCursorIdx
   *          the index of the cursor to check, should be >= 0 and < 10.
   * @return <code>true</code> if the cursor with the given index is set,
   *         <code>false</code> otherwise.
   */
  public boolean isCursorPositionSet( final int aCursorIdx )
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > ( Ols.MAX_CURSORS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index: " + aCursorIdx + "! Should be between 0 and "
          + Ols.MAX_CURSORS );
    }
    final Cursor[] cursorPositions = getCursors();
    if ( cursorPositions == null )
    {
      return false;
    }

    return ( cursorPositions[aCursorIdx] != null ) && ( cursorPositions[aCursorIdx].isDefined() );
  }

  /**
   * Sets the captured data.
   * 
   * @param aData
   *          the captured data to set, may be <code>null</code>.
   */
  public void setCapturedData( final AcquisitionResult aData )
  {
    this.projectManager.getCurrentProject().setCapturedData( aData );
    this.annotations.clear();
  }

  /**
   * @param aChannelIdx
   *          the index of the channel to set the label for, >= 0 && < 32;
   * @param aAnnotations
   *          the annotation for the given channel, cannot be <code>null</code>.
   */
  public void setChannelAnnotations( final int aChannelIdx, final ChannelAnnotations aAnnotations )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    this.annotations.put( Integer.valueOf( aChannelIdx ), aAnnotations );
  }

  /**
   * Sets the channel label.
   * 
   * @param aChannelIdx
   *          the index of the channel to set the label for, >= 0 && < 32;
   * @param aLabel
   *          the label to set, may be <code>null</code>.
   */
  public void setChannelLabel( final int aChannelIdx, final String aLabel )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > ( Ols.MAX_CHANNELS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + Ols.MAX_CHANNELS );
    }
    getAllChannels()[aChannelIdx].setLabel( aLabel );
  }

  /**
   * Sets all channel labels directly.
   * 
   * @param aLabels
   *          the array of labels to set, cannot be <code>null</code>.
   */
  public void setChannelLabels( final String[] aLabels )
  {
    if ( aLabels.length != Ols.MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Invalid channel labels! Should have exact " + Ols.MAX_CHANNELS + " items!" );
    }
    for ( int i = 0; i < aLabels.length; i++ )
    {
      getAllChannels()[i].setLabel( aLabels[i] );
    }
  }

  /**
   * Sets a cursor position.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, should be >= 0 and < 10;
   * @param aCursorPosition
   *          the actual cursor position to set.
   * @throws IllegalArgumentException
   *           in case an invalid cursor index was given.
   */
  public void setCursorPosition( final int aCursorIdx, final Long aCursorPosition ) throws IllegalArgumentException
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > ( Ols.MAX_CURSORS - 1 ) ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index! Should be between 0 and " + Ols.MAX_CURSORS );
    }
    final Cursor[] cursorPositions = getCursors();
    if ( cursorPositions != null )
    {
      if ( ( aCursorPosition == null ) || ( aCursorPosition.longValue() == Long.MIN_VALUE ) )
      {
        cursorPositions[aCursorIdx].clear();
      }
      else
      {
        cursorPositions[aCursorIdx].setTimestamp( aCursorPosition.longValue() );
      }
    }
  }

  /**
   * Calculates the time offset
   * 
   * @param aTime
   *          the absolute sample number, >= 0.
   * @return time relative to data
   */
  protected long calculateTimeOffset( final long aTime )
  {
    final AcquisitionResult capturedData = getAcquisitionData();
    if ( capturedData.hasTriggerData() )
    {
      return aTime - capturedData.getTriggerPosition();
    }

    return aTime;
  }

  /**
   * Returns the current acquisition results.
   * 
   * @return the captured data, never <code>null</code>.
   */
  private AcquisitionResult getAcquisitionData()
  {
    return getCurrentDataSet().getCapturedData();
  }

  /**
   * @return
   */
  private DataSet getCurrentDataSet()
  {
    return this.projectManager.getCurrentProject().getDataSet();
  }

  /**
   * @return the cursorPositions
   */
  private Cursor[] getCursors()
  {
    return getCurrentDataSet().getCursors();
  }
}
