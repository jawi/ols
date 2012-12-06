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
package nl.lxtreme.ols.common.acquisition;


import static nl.lxtreme.ols.common.Ols.*;

import java.util.*;

import nl.lxtreme.ols.common.*;


/**
 * Provides a convenient way to create new instances of {@link AcquisitionData}.
 */
public final class AcquisitionDataBuilder
{
  // INNER TYPES

  /**
   * Provides an implementation of {@link AcquisitionData}.
   */
  static final class AcquisitionDataImpl implements AcquisitionData
  {
    // VARIABLES

    private final int[] values;
    private final long[] timestamps;
    private final long triggerPosition;
    private final int sampleRate;
    private final int channelCount;
    private final int enabledChannels;
    private final long absoluteLength;
    private final Channel[] channels;
    private final Cursor[] cursors;
    private boolean cursorsVisible;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AcquisitionDataImpl} instance.
     */
    AcquisitionDataImpl( final int[] aValues, final long[] aTimestamps, final long aTriggerPosition,
        final int aSampleRate, final int aChannelCount, final int aEnabledChannels, final long aAbsoluteLength,
        final Cursor[] aCursors, final boolean aCursorsVisible )
    {
      this.values = aValues;
      this.timestamps = aTimestamps;

      if ( this.values.length != this.timestamps.length )
      {
        throw new IllegalArgumentException( "Values and timestamps size mismatch!" );
      }

      this.triggerPosition = aTriggerPosition;
      this.sampleRate = aSampleRate;
      this.channelCount = aChannelCount;
      this.enabledChannels = aEnabledChannels;
      this.absoluteLength = aAbsoluteLength;

      this.cursors = Arrays.copyOf( aCursors, Ols.MAX_CURSORS );
      this.cursorsVisible = aCursorsVisible;

      List<Channel> _channels = new ArrayList<Channel>( this.channelCount );

      for ( int i = 0; i < Ols.MAX_CHANNELS; i++ )
      {
        final int mask = ( 1 << i );
        if ( ( this.enabledChannels & mask ) != 0 )
        {
          _channels.add( new ChannelImpl( i, true /* enabled */) );
        }
      }

      this.channels = _channels.toArray( new Channel[_channels.size()] );
    }

    // METHODS

    /**
     * Provides a binary search for arrays of long-values.
     * <p>
     * This implementation is directly copied from the JDK
     * {@link Arrays#binarySearch(long[], long)} implementation, slightly
     * modified to only perform a single comparison-action.
     * </p>
     * 
     * @param aArray
     *          the array of long values to search in;
     * @param aFromIndex
     *          the from index to search from;
     * @param aToIndex
     *          the to index to search up and until;
     * @param aKey
     *          the value to search for.
     * @return the index of the given key, which is either the greatest index of
     *         the value less or equal to the given key.
     * @see Arrays#binarySearch(long[], long)
     */
    static final int binarySearch( final long[] aArray, final int aFromIndex, final int aToIndex, final Long aKey )
    {
      int mid = -1;
      int low = aFromIndex;
      int high = aToIndex - 1;

      while ( low <= high )
      {
        mid = ( low + high ) >>> 1;
        final Long midVal = Long.valueOf( aArray[mid] );

        final int c = aKey.compareTo( midVal );
        if ( c > 0 )
        {
          low = mid + 1;
        }
        else if ( c < 0 )
        {
          high = mid - 1;
        }
        else
        {
          return mid; // key found
        }
      }

      if ( mid < 0 )
      {
        return low;
      }

      // Determine the insertion point, avoid crossing the array boundaries...
      if ( mid < ( aToIndex - 1 ) )
      {
        // If the searched value is greater than the value of the found index,
        // insert it after this value, otherwise before it (= the last
        // return)...
        if ( aKey.longValue() > aArray[mid] )
        {
          return mid + 1;
        }
      }

      return mid;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final long getAbsoluteLength()
    {
      return this.absoluteLength;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Channel[] getChannels()
    {
      return this.channels;
    }

    @Override
    public int getChannelCount()
    {
      return this.channelCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Cursor[] getCursors()
    {
      return this.cursors;
    }

    @Override
    public int getEnabledChannels()
    {
      return this.enabledChannels;
    }

    @Override
    public int getSampleIndex( final long abs )
    {
      return binarySearch( this.timestamps, 0, this.timestamps.length, Long.valueOf( abs ) );
    }

    @Override
    public int getSampleRate()
    {
      return this.sampleRate;
    }

    @Override
    public long[] getTimestamps()
    {
      return this.timestamps;
    }

    @Override
    public long getTriggerPosition()
    {
      return this.triggerPosition;
    }

    @Override
    public int[] getValues()
    {
      return this.values;
    }

    @Override
    public boolean hasTimingData()
    {
      return ( this.sampleRate != NOT_AVAILABLE );
    }

    @Override
    public boolean hasTriggerData()
    {
      return ( this.triggerPosition != NOT_AVAILABLE );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isCursorsVisible()
    {
      return this.cursorsVisible;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setCursorsVisible( final boolean aVisible )
    {
      this.cursorsVisible = aVisible;
    }
  }

  /**
   * Provides a default implementation of {@link Channel}.
   */
  static final class ChannelImpl implements Channel
  {
    // VARIABLES

    private final int index;
    private final int mask;

    private String label;
    private boolean enabled;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelImpl} instance.
     * 
     * @param aIndex
     *          the index of the channel to represent;
     * @param aEnabled
     *          <code>true</code> if the channel is enabled, <code>false</code>
     *          otherwise.
     */
    public ChannelImpl( final int aIndex, final boolean aEnabled )
    {
      this.index = aIndex;
      this.enabled = aEnabled;
      this.mask = 1 << aIndex;
      this.label = getDefaultLabel( aIndex );
    }

    // METHODS

    /**
     * Returns the default label for a channel.
     * 
     * @param aIndex
     *          the index of the channel to create the label for.
     * @return a default label, never <code>null</code>.
     */
    private static String getDefaultLabel( final int aIndex )
    {
      return String.format( "Channel %d", Integer.valueOf( aIndex ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Channel aOther )
    {
      return getIndex() - aOther.getIndex();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( ( aObject == null ) || !( aObject instanceof ChannelImpl ) )
      {
        return false;
      }

      ChannelImpl other = ( ChannelImpl )aObject;
      if ( this.index != other.index )
      {
        return false;
      }
      if ( this.mask != other.mask )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getIndex()
    {
      return this.index;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLabel()
    {
      return this.label;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMask()
    {
      return this.mask;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + this.index;
      result = ( prime * result ) + this.mask;
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasName()
    {
      return ( this.label != null ) && !"".equals( this.label.trim() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEnabled()
    {
      return this.enabled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setEnabled( final boolean aEnabled )
    {
      this.enabled = aEnabled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLabel( final String aName )
    {
      if ( ( aName == null ) || "".equals( aName.trim() ) )
      {
        this.label = getDefaultLabel( this.index );
      }
      else
      {
        this.label = aName.trim();
      }
    }
  }

  /**
   * Provides a default implementation of {@link Cursor}.
   */
  static final class CursorImpl implements Cursor
  {
    // CONSTANTS

    private static final long UNDEFINED = -1L;

    // VARIABLES

    private final int index;

    private String label;
    private long timestamp;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CursorImpl} instance.
     */
    public CursorImpl( final Cursor aCursor )
    {
      this.index = aCursor.getIndex();
      this.timestamp = aCursor.isDefined() ? aCursor.getTimestamp() : UNDEFINED;
      this.label = aCursor.getLabel();
    }

    /**
     * Creates a new {@link CursorImpl} instance.
     */
    public CursorImpl( final int aIndex )
    {
      this.index = aIndex;
      this.timestamp = UNDEFINED;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear()
    {
      this.timestamp = UNDEFINED;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Cursor aOther )
    {
      return getIndex() - aOther.getIndex();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( ( aObject == null ) || !( aObject instanceof CursorImpl ) )
      {
        return false;
      }

      CursorImpl other = ( CursorImpl )aObject;
      if ( this.index != other.index )
      {
        return false;
      }
      if ( this.timestamp != other.timestamp )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getIndex()
    {
      return this.index;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLabel()
    {
      return this.label;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getTimestamp()
    {
      if ( this.timestamp < 0L )
      {
        throw new IllegalStateException( "Undefined cursor!" );
      }
      return this.timestamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + this.index;
      result = ( prime * result ) + ( int )( this.timestamp ^ ( this.timestamp >>> 32 ) );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasLabel()
    {
      return ( this.label != null ) && !"".equals( this.label.trim() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isDefined()
    {
      return this.timestamp >= 0L;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLabel( final String aLabel )
    {
      this.label = aLabel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setTimestamp( final long aTimestamp )
    {
      this.timestamp = aTimestamp;
    }
  }

  /**
   * Small container used to keep a sample and its timestamp together.
   */
  static final class Sample implements Comparable<Sample>
  {
    // VARIABLES

    final long timestamp;
    final int value;

    // CONSTRUCTORS

    /**
     * Creates a new {@link Sample} instance.
     */
    public Sample( final long aTimestamp, final int aValue )
    {
      this.timestamp = aTimestamp;
      this.value = aValue;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Sample aSample )
    {
      int result = ( this.timestamp < aSample.timestamp ) ? -1 : ( ( this.timestamp == aSample.timestamp ) ? 0 : 1 );
      if ( result == 0 )
      {
        result = ( this.value - aSample.value );
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( ( aObject == null ) || !( aObject instanceof Sample ) )
      {
        return false;
      }

      final Sample other = ( Sample )aObject;
      return ( this.timestamp == other.timestamp ) && ( this.value == other.value );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + ( int )( this.timestamp ^ ( this.timestamp >>> 32 ) );
      result = ( prime * result ) + this.value;
      return result;
    }
  }

  // VARIABLES

  private long absoluteLength;
  private int channelCount;
  private int enabledChannelMask;
  private final SortedSet<Sample> sampleData;
  private final Cursor[] cursors;
  private int sampleRate;
  private long triggerPosition;
  private boolean cursorsVisible;

  // CONSTRUCTORS

  /**
   * Creates a new, empty, {@link AcquisitionDataBuilder} instance.
   */
  public AcquisitionDataBuilder()
  {
    this.sampleData = new TreeSet<Sample>();
    this.cursors = new Cursor[Ols.MAX_CURSORS];
    this.absoluteLength = NOT_AVAILABLE;
    this.channelCount = 0;
    this.enabledChannelMask = 0;
    this.triggerPosition = NOT_AVAILABLE;
    this.sampleRate = NOT_AVAILABLE; // state values
    this.cursorsVisible = true; // by default

    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      this.cursors[i] = new CursorImpl( i );
    }
  }

  /**
   * Creates a new {@link AcquisitionDataBuilder} instance initialized with the
   * given {@link AcquisitionData}.
   * 
   * @param aData
   *          the acquisition data to initialize this builder with;
   * @param aIncludeSamples
   *          <code>true</code> to also copy the sample data of the given
   *          {@link AcquisitionData}, <code>false</code> to copy only the basic
   *          data.
   */
  public AcquisitionDataBuilder( final AcquisitionData aData, final boolean aIncludeSamples )
  {
    this.sampleData = new TreeSet<Sample>();
    this.cursors = new Cursor[Ols.MAX_CURSORS];
    this.absoluteLength = aData.getAbsoluteLength();
    this.channelCount = aData.getChannelCount();
    this.enabledChannelMask = aData.getEnabledChannels();
    this.triggerPosition = aData.getTriggerPosition();
    this.sampleRate = aData.getSampleRate();
    this.cursorsVisible = true; // by default

    final Cursor[] _cursors = aData.getCursors();
    for ( int i = 0; i < _cursors.length; i++ )
    {
      // Create real copies of the cursors to make them independent...
      this.cursors[i] = new CursorImpl( _cursors[i] );
    }

    if ( aIncludeSamples )
    {
      int[] values = aData.getValues();
      long[] timestamps = aData.getTimestamps();

      for ( int i = 0; i < values.length; i++ )
      {
        addSample( timestamps[i], values[i] );
      }
    }
  }

  // METHODS

  /**
   * Adds a new sample to this builder.
   * 
   * @param aTimestamp
   *          the absolute time stamp of the sample, >= 0;
   * @param aValue
   *          the sample value.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given timestamp was negative.
   */
  public AcquisitionDataBuilder addSample( final long aTimestamp, final int aValue )
  {
    if ( aTimestamp < 0 )
    {
      throw new IllegalArgumentException( "Timestamp cannot be negative!" );
    }
    this.sampleData.add( new Sample( aTimestamp, aValue ) );
    return this;
  }

  /**
   * Builds the acquisition data instance.
   * 
   * @return a new {@link AcquisitionData} instance, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case of problems building the {@link AcquisitionData}, for
   *           example, due to incomplete or invalid values supplied to this
   *           builder.
   */
  public AcquisitionData build()
  {
    if ( this.channelCount == 0 )
    {
      throw new IllegalArgumentException( "No channel count defined!" );
    }
    if ( this.enabledChannelMask == 0 )
    {
      throw new IllegalArgumentException( "No channel mask is defined!" );
    }

    int[] values;
    long[] timestamps;

    if ( !this.sampleData.isEmpty() )
    {
      int sampleCount = 1;

      Sample sample;
      List<Sample> sampleData = new ArrayList<Sample>( this.sampleData );

      // Pass 1: count number of unique samples...
      int oldValue = sampleData.get( 0 ).value;
      for ( int i = 1; i < sampleData.size(); i++ )
      {
        sample = sampleData.get( i );
        int value = sample.value;
        if ( oldValue != value )
        {
          sampleCount++;
          oldValue = value;
        }
      }

      // Pass 2: create data structures...
      values = new int[sampleCount];
      timestamps = new long[values.length];

      sample = sampleData.get( 0 );
      oldValue = sample.value;

      values[0] = oldValue;
      timestamps[0] = sample.timestamp;

      for ( int i = 1, j = 1; i < sampleData.size(); i++ )
      {
        sample = sampleData.get( i );
        int value = sample.value;
        if ( oldValue != value )
        {
          values[j] = ( sample.value & this.enabledChannelMask );
          timestamps[j] = sample.timestamp;
          j++;
        }
        oldValue = value;
      }
    }
    else
    {
      // Empty set.
      values = new int[] { 0 };
      timestamps = new long[] { 0L };
    }

    // Ensure we've got an absolute length available...
    long absLength = ( this.absoluteLength == NOT_AVAILABLE ) ? timestamps[timestamps.length - 1] + 1L
        : this.absoluteLength;

    return new AcquisitionDataImpl( values, timestamps, this.triggerPosition, this.sampleRate, this.channelCount,
        this.enabledChannelMask, absLength, this.cursors, this.cursorsVisible );
  }

  /**
   * Clears all sample rate information in this builder, effectively marking the
   * sample data as state-values instead of time-based values.
   * 
   * @return this builder.
   */
  public AcquisitionDataBuilder clearSampleRate()
  {
    this.sampleRate = NOT_AVAILABLE;
    return this;
  }

  /**
   * Clears all trigger information in this builder.
   * 
   * @return this builder.
   */
  public AcquisitionDataBuilder clearTrigger()
  {
    this.triggerPosition = NOT_AVAILABLE;
    return this;
  }

  /**
   * Sets the absolute length of the sample data.
   * 
   * @param aAbsoluteLength
   *          the absolute length, >= 0.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given timestamp was negative.
   */
  public AcquisitionDataBuilder setAbsoluteLength( final long aAbsoluteLength )
  {
    if ( aAbsoluteLength < 0 )
    {
      throw new IllegalArgumentException( "Timestamp cannot be negative!" );
    }
    this.absoluteLength = aAbsoluteLength;
    return this;
  }

  /**
   * Sets the number of channels, or sample width.
   * 
   * @param aChannelCount
   *          the channel count, > 0 && <= 32.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given channel count was zero or negative.
   */
  public AcquisitionDataBuilder setChannelCount( final int aChannelCount )
  {
    if ( aChannelCount <= 0 )
    {
      throw new IllegalArgumentException( "Channel count cannot be less than one!" );
    }
    if ( aChannelCount > MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Channel count cannot be more than " + MAX_CHANNELS + "!" );
    }
    this.channelCount = aChannelCount;
    this.enabledChannelMask = ( int )( ( 1L << aChannelCount ) - 1L );
    return this;
  }

  /**
   * Sets the label for a particular cursor.
   * 
   * @param aIndex
   *          the index of the cursor to set, >= 0 && < {@value Ols#MAX_CURSORS}
   *          ;
   * @param aLabel
   *          the label of the cursor to set.
   * @throws IllegalArgumentException
   *           in case of an invalid cursor index.
   */
  public AcquisitionDataBuilder setCursorLabel( final int aIndex, final String aLabel )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    this.cursors[aIndex].setLabel( aLabel );
    return this;
  }

  /**
   * Sets whether or not cursors are visible.
   * 
   * @param aCursorsVisible
   * @return this builder.
   */
  public AcquisitionDataBuilder setCursorsVisible( final boolean aCursorsVisible )
  {
    this.cursorsVisible = aCursorsVisible;
    return this;
  }

  /**
   * Sets the timestamp for a particular cursor.
   * 
   * @param aIndex
   *          the index of the cursor to set, >= 0 && < {@value Ols#MAX_CURSORS}
   *          ;
   * @param aTimestamp
   *          the timestamp of the cursor to set.
   * @throws IllegalArgumentException
   *           in case of an invalid cursor index.
   */
  public AcquisitionDataBuilder setCursorTimestamp( final int aIndex, final long aTimestamp )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    this.cursors[aIndex].setTimestamp( aTimestamp );
    return this;
  }

  /**
   * Sets the enabled channel mask.
   * 
   * @param aMask
   *          the mask denoting which channels are enabled, != 0.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given mask was zero.
   */
  public AcquisitionDataBuilder setEnabledChannelMask( final int aMask )
  {
    if ( aMask == 0 )
    {
      throw new IllegalArgumentException( "Mask cannot be zero!" );
    }
    this.enabledChannelMask = aMask;
    return this;
  }

  /**
   * Sets the sample rate.
   * 
   * @param aSampleRate
   *          the sample rate (in Hertz) to set, >= 0.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given sample rate was negative.
   */
  public AcquisitionDataBuilder setSampleRate( final int aSampleRate )
  {
    if ( aSampleRate < 0 )
    {
      throw new IllegalArgumentException( "Sample rate cannot be negative!" );
    }
    this.sampleRate = aSampleRate;
    return this;
  }

  /**
   * Sets the trigger position.
   * 
   * @param aTriggerPosition
   *          the trigger position to set, >= 0.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case the given trigger position was negative.
   */
  public AcquisitionDataBuilder setTriggerPosition( final long aTriggerPosition )
  {
    if ( aTriggerPosition < 0 )
    {
      throw new IllegalArgumentException( "Trigger position cannot be negative!" );
    }
    this.triggerPosition = aTriggerPosition;
    return this;
  }
}
