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
package nl.lxtreme.ols.common.acquisition;


import static nl.lxtreme.ols.common.OlsConstants.*;

import java.util.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.ChannelBuilder.ChannelImpl;
import nl.lxtreme.ols.common.acquisition.ChannelGroupBuilder.ChannelGroupImpl;
import nl.lxtreme.ols.common.acquisition.CursorBuilder.CursorImpl;


/**
 * Provides a convenient way to create new instances of {@link AcquisitionData}.
 */
public final class AcquisitionDataBuilder
{
  // INNER TYPES

  /**
   * Whether or not to include annotations when applying a data template.
   */
  public static enum IncludeAnnotations
  {
    YES, NO;
  }

  /**
   * Whether or not to include sample data when applying a data template.
   */
  public static enum IncludeSamples
  {
    YES, NO;
  }

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
    private final ChannelGroup[] channelGroups;
    private final Cursor[] cursors;
    private boolean cursorsVisible;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AcquisitionDataImpl} instance.
     */
    AcquisitionDataImpl( final int[] aValues, final long[] aTimestamps, final long aTriggerPosition,
        final int aSampleRate, final int aChannelCount, final int aEnabledChannels, final long aAbsoluteLength,
        final Cursor[] aCursors, final boolean aCursorsVisible, final Channel[] aChannels,
        final ChannelGroupImpl[] aChannelGroups )
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

      this.cursors = Arrays.copyOf( aCursors, OlsConstants.MAX_CURSORS );
      this.cursorsVisible = aCursorsVisible;

      this.channels = Arrays.copyOf( aChannels, aChannels.length );
      this.channelGroups = Arrays.copyOf( aChannelGroups, aChannelGroups.length );
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
    public boolean areCursorsVisible()
    {
      return this.cursorsVisible;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final long getAbsoluteLength()
    {
      return this.absoluteLength;
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
    public ChannelGroup[] getChannelGroups()
    {
      return this.channelGroups;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Channel[] getChannels()
    {
      return this.channels;
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
    public void setCursorsVisible( final boolean aVisible )
    {
      this.cursorsVisible = aVisible;
    }
  }

  /**
   * Container for keeping channel group-related information together.
   */
  static final class ChannelGroupInfo
  {
    // VARIABLES

    String name;
    final List<Integer> channelIndices;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelGroupInfo} instance.
     */
    public ChannelGroupInfo( String aName )
    {
      this.channelIndices = new ArrayList<Integer>();
      this.name = aName;
    }

    // METHODS

    void addChannelIndex( int aChannelIdx )
    {
      this.channelIndices.add( Integer.valueOf( aChannelIdx ) );
    }

    boolean containsChannel( int aChannelIdx )
    {
      return this.channelIndices.contains( Integer.valueOf( aChannelIdx ) );
    }

    void copyChannelIndices( ChannelGroup aChannelGroup )
    {
      this.channelIndices.clear();
      for ( Channel c : aChannelGroup.getChannels() )
      {
        this.channelIndices.add( Integer.valueOf( c.getIndex() ) );
      }
    }
  }

  /**
   * Container for keeping channel-related information together.
   */
  static final class ChannelInfo
  {
    // VARIABLES

    String label;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelInfo} instance.
     */
    public ChannelInfo( String aName )
    {
      this.label = aName;
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
     * Performs a comparison on the value only.
     * 
     * @param aSample
     *          the sample to compare the value of, cannot be <code>null</code>.
     * @return 0 if the values matched, -1 if this value is less then the given
     *         sample value, or 1 if this value is more than the given sample
     *         value.
     */
    public int compareValueTo( final Sample aSample )
    {
      return ( this.value - aSample.value );
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

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return String.format( "%08x@%d", this.value, this.timestamp );
    }
  }

  // VARIABLES

  private long lastSeenTimestamp;
  private long absoluteLength;
  private int channelCount;
  private int enabledChannelMask;
  private final SortedSet<Sample> sampleData;
  private final SortedSet<ChannelImpl> channels;
  private final SortedSet<ChannelGroupImpl> channelGroups;
  private final SortedSet<CursorImpl> cursors;
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
    this.channels = new TreeSet<ChannelImpl>();
    this.channelGroups = new TreeSet<ChannelGroupImpl>();
    this.cursors = new TreeSet<CursorImpl>();
    this.absoluteLength = NOT_AVAILABLE;
    this.lastSeenTimestamp = NOT_AVAILABLE;
    this.enabledChannelMask = NOT_AVAILABLE;
    this.triggerPosition = NOT_AVAILABLE;
    this.sampleRate = NOT_AVAILABLE; // state values
    this.channelCount = 0;
    this.cursorsVisible = true; // by default
  }

  // METHODS

  /**
   * Adds a new channel to this builder.
   * 
   * @return a new {@link ChannelBuilder} instance to define the channel, never
   *         <code>null</code>.
   * @throws IllegalArgumentException
   *           in case a channel with the given index already exists.
   */
  public AcquisitionDataBuilder add( ChannelBuilder aBuilder )
  {
    add( aBuilder.build( this ) );
    return this;
  }

  /**
   * Adds a new channel group to this builder.
   * 
   * @return a new {@link ChannelGroupBuilder} instance to define the channel
   *         group, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case a channel group with the given index already exists.
   */
  public AcquisitionDataBuilder add( ChannelGroupBuilder aBuilder )
  {
    add( aBuilder.build( this ) );
    return this;
  }

  /**
   * Adds a new cursor to this builder.
   * 
   * @return a new {@link CursorBuilder} instance to define the cursor, never
   *         <code>null</code>.
   * @throws IllegalArgumentException
   *           in case a cursor with the given index already exists.
   */
  public AcquisitionDataBuilder add( CursorBuilder aBuilder )
  {
    add( aBuilder.build( this ) );
    return this;
  }

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

    Sample sample = new Sample( aTimestamp, aValue );

    if ( this.sampleData.add( sample ) )
    {
      // Keep track of the last seen timestamp, for determining the absolute
      // length (in case it is not defined)...
      this.lastSeenTimestamp = this.sampleData.last().timestamp;
    }

    return this;
  }

  /**
   * Applies the given data as template for this builder.
   * 
   * @param aData
   *          the acquisition data to apply as template;
   * @param aIncludeSamples
   *          whether or not to include the sample data of the given acquisition
   *          data;
   * @param aIncludeAnnotations
   *          whether or not to include the annotation data of the given
   *          acquisition data.
   * @return this builder.
   */
  public AcquisitionDataBuilder applyTemplate( AcquisitionData aData, IncludeSamples aIncludeSamples,
      IncludeAnnotations aIncludeAnnotations )
  {
    setAbsoluteLength( aData.getAbsoluteLength() ).setChannelCount( aData.getChannelCount() )
        .setCursorsVisible( aData.areCursorsVisible() ).setEnabledChannelMask( aData.getEnabledChannels() )
        .setSampleRate( aData.getSampleRate() ).setTriggerPosition( aData.getTriggerPosition() );

    for ( Cursor _cursor : aData.getCursors() )
    {
      // Create real copies of the cursors to make them independent...
      CursorBuilder builder = createCursor() //
          .setIndex( _cursor.getIndex() ) //
          .setColor( _cursor.getColor() ) //
          .setLabel( _cursor.getLabel() );
      if ( _cursor.isDefined() )
      {
        builder.setTimestamp( _cursor.getTimestamp() );
      }

      add( builder );
    }

    // Copy channel names...
    for ( Channel c : aData.getChannels() )
    {
      add( createChannel() //
          .setColor( c.getColor() ) //
          .setEnabled( c.isEnabled() ) //
          .setIndex( c.getIndex() ) //
          .setLabel( c.getLabel() ) );

      if ( aIncludeAnnotations == IncludeAnnotations.YES )
      {
        // channelDef.copyAnnotations( c ); XXX
      }
    }

    // Copy channel groups...
    for ( ChannelGroup cg : aData.getChannelGroups() )
    {
      add( createChannelGroup() //
          .setIndex( cg.getIndex() ) //
          .setColor( cg.getColor() ) //
          .setName( cg.getName() ) //
          .addChannel( cg.getChannels() ) //
          .setChannelCount( cg.getChannels().length ) );
    }

    if ( aIncludeSamples == IncludeSamples.YES )
    {
      int[] values = aData.getValues();
      long[] timestamps = aData.getTimestamps();

      for ( int i = 0; i < values.length; i++ )
      {
        addSample( timestamps[i], values[i] );
      }
    }

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

    // Ensure a consistent model...
    ensureChannelGroupPresent();
    ensureConsistentModel();

    // Ensure we've got an absolute length available...
    long absLength;
    if ( this.absoluteLength == NOT_AVAILABLE )
    {
      absLength = this.lastSeenTimestamp;
    }
    else
    {
      absLength = Math.max( this.absoluteLength, this.lastSeenTimestamp );
    }

    final int[] values;
    final long[] timestamps;

    if ( !this.sampleData.isEmpty() )
    {
      Iterator<Sample> iter = this.sampleData.iterator();
      Sample lastSample;

      int sampleCount = 1;
      int lastValue = iter.next().value & this.enabledChannelMask;
      while ( iter.hasNext() )
      {
        lastSample = iter.next();
        int value = ( lastSample.value & this.enabledChannelMask );
        if ( lastValue != value )
        {
          sampleCount++;
          lastValue = value;
        }
      }

      // Issue #167: make sure the absolute length is *always* present...
      boolean addExtraSample = ( this.lastSeenTimestamp != absLength ) || ( sampleCount < 2 );
      if ( addExtraSample )
      {
        sampleCount++;
      }

      values = new int[sampleCount];
      timestamps = new long[values.length];

      iter = this.sampleData.iterator();
      lastSample = iter.next();

      values[0] = lastSample.value & this.enabledChannelMask;
      timestamps[0] = lastSample.timestamp;
      lastValue = values[0];

      int i = 1;
      while ( iter.hasNext() )
      {
        lastSample = iter.next();
        int value = ( lastSample.value & this.enabledChannelMask );

        if ( lastValue != value )
        {
          values[i] = value;
          timestamps[i] = lastSample.timestamp;
          lastValue = value;
          i++;
        }
        else if ( i > 1 )
        {
          timestamps[i - 1] = lastSample.timestamp;
        }
      }

      // Issue #167: make sure the absolute length is *always* present...
      if ( addExtraSample )
      {
        values[sampleCount - 1] = values[sampleCount - 2];
        timestamps[sampleCount - 1] = absLength;
      }
    }
    else
    {
      // Empty set.
      values = new int[] { 0 };
      timestamps = new long[] { 0L };
      absLength = 0L;
    }

    CursorImpl[] _cursors = new CursorImpl[OlsConstants.MAX_CURSORS];
    for ( CursorImpl cursor : this.cursors )
    {
      _cursors[cursor.getIndex()] = cursor;
    }
    for ( int i = 0; i < _cursors.length; i++ )
    {
      if ( _cursors[i] == null )
      {
        _cursors[i] = new CursorBuilder().setIndex( i ).build( this );
      }
    }

    Channel[] _channels = this.channels.toArray( new Channel[this.channels.size()] );
    ChannelGroupImpl[] _channelGroups = this.channelGroups.toArray( new ChannelGroupImpl[this.channelGroups.size()] );

    AcquisitionDataImpl result = new AcquisitionDataImpl( values, timestamps, this.triggerPosition, this.sampleRate,
        this.channelCount, this.enabledChannelMask, absLength, _cursors, this.cursorsVisible, _channels, _channelGroups );

    for ( int i = 0; i < _cursors.length; i++ )
    {
      _cursors[i].setAcquisitionData( result );
    }

    return result;
  }

  /**
   * Clears the absolute length in this builder.
   * 
   * @return this builder.
   */
  public AcquisitionDataBuilder clearAbsoluteLength()
  {
    this.absoluteLength = NOT_AVAILABLE;
    return this;
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
   * @return a new {@link ChannelBuilder} to define a new channel, never
   *         <code>null</code>.
   * @see #add(ChannelBuilder)
   */
  public ChannelBuilder createChannel()
  {
    return new ChannelBuilder();
  }

  /**
   * @return a new {@link ChannelGroupBuilder} to define a new channel group,
   *         never <code>null</code>.
   * @see #add(ChannelGroupBuilder)
   */
  public ChannelGroupBuilder createChannelGroup()
  {
    return new ChannelGroupBuilder();
  }

  /**
   * @return a new {@link CursorBuilder} to define a new cursor, never
   *         <code>null</code>.
   * @see #add(ChannelBuilder)
   */
  public CursorBuilder createCursor()
  {
    return new CursorBuilder();
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
   */
  public AcquisitionDataBuilder setSampleRate( final int aSampleRate )
  {
    if ( aSampleRate >= 0 )
    {
      this.sampleRate = aSampleRate;
    }
    return this;
  }

  /**
   * Sets the trigger position.
   * 
   * @param aTriggerPosition
   *          the trigger position to set, >= 0.
   * @return this builder.
   */
  public AcquisitionDataBuilder setTriggerPosition( final long aTriggerPosition )
  {
    if ( aTriggerPosition >= 0 )
    {
      this.triggerPosition = aTriggerPosition;
    }
    return this;
  }

  ChannelGroupImpl add( ChannelGroupImpl aGroup )
  {
    for ( ChannelGroupImpl g : this.channelGroups )
    {
      if ( g.getIndex() == aGroup.getIndex() )
      {
        throw new IllegalArgumentException( "Channel group with index " + aGroup.getIndex() + " already defined!" );
      }
    }
    this.channelGroups.add( aGroup );
    return aGroup;
  }

  ChannelImpl add( ChannelImpl aChannel )
  {
    for ( ChannelImpl c : this.channels )
    {
      if ( c.getIndex() == aChannel.getIndex() )
      {
        throw new IllegalArgumentException( "Channel with index " + aChannel.getIndex() + " already defined!" );
      }
    }
    this.channels.add( aChannel );
    return aChannel;
  }

  CursorImpl add( CursorImpl aCursor )
  {
    for ( CursorImpl c : this.cursors )
    {
      if ( c.getIndex() == aCursor.getIndex() )
      {
        throw new IllegalArgumentException( "Cursor with index " + aCursor.getIndex() + " already defined!" );
      }
    }
    this.cursors.add( aCursor );
    return aCursor;
  }

  ChannelImpl getChannel( int aIndex )
  {
    for ( ChannelImpl channel : this.channels )
    {
      if ( channel.getIndex() == aIndex )
      {
        return channel;
      }
    }
    return null;
  }

  int getSampleRate()
  {
    return this.sampleRate;
  }

  long getTriggerPosition()
  {
    return this.triggerPosition;
  }

  /**
   * Ensures that at least one channel group is present containing all channels.
   */
  private void ensureChannelGroupPresent()
  {
    if ( this.channelGroups.isEmpty() )
    {
      // Add all channels to one channel group, which indirectly will define the
      // channels as well...
      int channelCount = 0;
      int groupCount = 0;

      ChannelGroupBuilder cgBuilder = null;
      for ( int i = 0; channelCount < this.channelCount && i < OlsConstants.MAX_CHANNELS; i++ )
      {
        int mask = ( 1 << i );
        if ( ( this.enabledChannelMask & mask ) != 0 )
        {
          if ( cgBuilder == null )
          {
            cgBuilder = createChannelGroup().setIndex( groupCount++ );
          }

          cgBuilder.addChannel( i );
          channelCount++;
        }

        if ( ( cgBuilder != null ) && ( channelCount % OlsConstants.CHANNELS_PER_BLOCK ) == 0 )
        {
          add( cgBuilder );
          cgBuilder = null;
        }
      }

      if ( cgBuilder != null )
      {
        add( cgBuilder );
      }
    }
  }

  private void ensureConsistentModel()
  {
    int realChannelCount = 0;
    List<ChannelImpl> remove = new ArrayList<ChannelImpl>();

    for ( ChannelImpl ch : this.channels )
    {
      if ( ( this.enabledChannelMask & ch.getMask() ) != 0 )
      {
        // Channel is present...
        realChannelCount++;
      }
      else
      {
        // Channel is masked out, remove it...
        remove.add( ch );
      }
    }

    for ( ChannelImpl ch : remove )
    {
      if ( this.channels.remove( ch ) )
      {
        ch.removeFromGroup();
      }
    }

    if ( realChannelCount == 0 )
    {
      throw new IllegalArgumentException( "No channel count defined!" );
    }

    this.channelCount = realChannelCount;
  }
}
