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

import java.awt.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.List;

import nl.lxtreme.ols.common.*;


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
        final ChannelGroup[] aChannelGroups )
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
   * Provides a default implementation of {@link ChannelGroup}.
   */
  static final class ChannelGroupImpl implements ChannelGroup
  {
    // VARIABLES

    private final int index;
    private String name;
    private final List<Channel> channels;

    /**
     * Creates a new {@link ChannelGroupImpl} instance.
     */
    public ChannelGroupImpl( int aIndex, String aName, List<Channel> aChannels )
    {
      this.index = aIndex;
      this.name = aName;
      this.channels = new ArrayList<Channel>( aChannels );
    }

    // METHODS

    public void addChannel( Channel aChannel )
    {
      this.channels.add( aChannel );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( ChannelGroup aGroup )
    {
      return this.index - aGroup.getIndex();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( aObject == null || getClass() != aObject.getClass() )
      {
        return false;
      }

      ChannelGroupImpl other = ( ChannelGroupImpl )aObject;
      if ( this.index != other.index )
        return false;
      if ( this.name == null )
      {
        if ( other.name != null )
          return false;
      }
      else if ( !this.name.equals( other.name ) )
        return false;
      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Channel[] getChannels()
    {
      return this.channels.toArray( new Channel[this.channels.size()] );
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
    public String getName()
    {
      return this.name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + this.index;
      result = prime * result + ( ( this.name == null ) ? 0 : this.name.hashCode() );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setName( String aName )
    {
      if ( aName == null )
      {
        throw new IllegalArgumentException( "Name cannot be null!" );
      }
      this.name = aName;
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
    private Color color;

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
    public Cursor clone()
    {
      try
      {
        CursorImpl clone = ( CursorImpl )super.clone();
        clone.label = this.label;
        clone.timestamp = this.timestamp;
        return clone;
      }
      catch ( CloneNotSupportedException exception )
      {
        throw new RuntimeException( "Clone contract broken?!" );
      }
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
    public Color getColor()
    {
      return this.color;
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
    public boolean inArea( long aTimestamp, double aDelta )
    {
      if ( !isDefined() )
      {
        return false;
      }

      final double min = this.timestamp - aDelta;
      final double max = this.timestamp + aDelta;

      return ( ( aTimestamp >= min ) && ( aTimestamp <= max ) );
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
    public void setColor( Color aColor )
    {
      this.color = aColor;
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
  private final Map<Integer, ChannelInfo> channelDefs;
  private final Map<Integer, ChannelGroupInfo> channelGroupDefs;
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
    this.channelDefs = new HashMap<Integer, ChannelInfo>();
    this.channelGroupDefs = new HashMap<Integer, ChannelGroupInfo>();
    this.cursors = new Cursor[OlsConstants.MAX_CURSORS];
    this.absoluteLength = NOT_AVAILABLE;
    this.lastSeenTimestamp = NOT_AVAILABLE;
    this.enabledChannelMask = NOT_AVAILABLE;
    this.triggerPosition = NOT_AVAILABLE;
    this.sampleRate = NOT_AVAILABLE; // state values
    this.channelCount = 0;
    this.cursorsVisible = true; // by default

    for ( int i = 0; i < OlsConstants.MAX_CURSORS; i++ )
    {
      this.cursors[i] = new CursorImpl( i );
    }
  }

  // METHODS

  /**
   * Adds a new channel group to this builder.
   * 
   * @param aIndex
   *          the index of the channel group to add (zero-based);
   * @param aName
   *          the name of the new channel group, cannot be <code>null</code>.
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case a channel group with the given index already exists.
   */
  public AcquisitionDataBuilder addChannelGroup( final int aIndex, final String aName )
  {
    Integer idx = Integer.valueOf( aIndex );
    if ( this.channelGroupDefs.containsKey( idx ) )
    {
      throw new IllegalArgumentException( "Channel group with index " + aIndex + " already defined!" );
    }
    this.channelGroupDefs.put( idx, new ChannelGroupInfo( aName ) );
    return this;
  }

  /**
   * Adds a channel to a group.
   * 
   * @param aChannelIndex
   *          the channel index to add to a group (zero-based);
   * @param aGroupIndex
   *          the group index to add the channel to (zero-based).
   * @return this builder.
   * @throws IllegalArgumentException
   *           in case no channel, or channel group with the given index exists.
   */
  public AcquisitionDataBuilder addChannelToGroup( final int aChannelIndex, final int aGroupIndex )
  {
    for ( ChannelGroupInfo _cgDef : this.channelGroupDefs.values() )
    {
      if ( _cgDef.containsChannel( aChannelIndex ) )
      {
        throw new IllegalArgumentException( "Channel already contained in group!" );
      }
    }

    ChannelGroupInfo channelGroupDef = this.channelGroupDefs.get( Integer.valueOf( aGroupIndex ) );
    if ( channelGroupDef == null )
    {
      throw new IllegalArgumentException( "No such channel group defined with index #" + aGroupIndex );
    }

    channelGroupDef.addChannelIndex( aChannelIndex );
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
    // Keep track of the last seen timestamp, for determining the absolute
    // length (in case it is not defined)...
    this.lastSeenTimestamp = Math.max( aTimestamp, this.lastSeenTimestamp );

    this.sampleData.add( sample );

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

    final Cursor[] _cursors = aData.getCursors();
    for ( int i = 0; i < _cursors.length; i++ )
    {
      // Create real copies of the cursors to make them independent...
      this.cursors[i] = new CursorImpl( _cursors[i] );
    }

    // Copy channel names...
    for ( Channel c : aData.getChannels() )
    {
      Integer chIdx = Integer.valueOf( c.getIndex() );

      ChannelInfo channelDef = this.channelDefs.get( chIdx );
      if ( channelDef == null )
      {
        channelDef = new ChannelInfo( c.getLabel() );
        this.channelDefs.put( chIdx, channelDef );
      }

      if ( aIncludeAnnotations == IncludeAnnotations.YES )
      {
        // channelDef.copyAnnotations( c ); XXX
      }
    }

    // Copy channel groups...
    for ( ChannelGroup cg : aData.getChannelGroups() )
    {
      Integer cgIndex = Integer.valueOf( cg.getIndex() );

      ChannelGroupInfo cgDef = this.channelGroupDefs.get( cgIndex );
      if ( cgDef == null )
      {
        cgDef = new ChannelGroupInfo( cg.getName() );
        this.channelGroupDefs.put( cgIndex, cgDef );
      }

      cgDef.copyChannelIndices( cg );
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
      int sampleCount = this.sampleData.size();

      // Issue #167: make sure the absolute length is *always* present...
      boolean addExtraSample = ( this.lastSeenTimestamp != absLength ) || ( sampleCount < 2 );
      if ( addExtraSample )
      {
        sampleCount++;
      }

      values = new int[sampleCount];
      timestamps = new long[values.length];

      int i = 0;
      for ( Sample sample : this.sampleData )
      {
        values[i] = ( sample.value & this.enabledChannelMask );
        timestamps[i] = sample.timestamp;
        i++;
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

    LinkedHashMap<Integer, Channel> channelIndex = createChannels();
    List<ChannelGroup> channelGroups = createChannelGroups( channelIndex );

    Channel[] _channels = channelIndex.values().toArray( new Channel[channelIndex.size()] );
    ChannelGroup[] _channelGroups = channelGroups.toArray( new ChannelGroup[channelGroups.size()] );

    return new AcquisitionDataImpl( values, timestamps, this.triggerPosition, this.sampleRate, this.channelCount,
        this.enabledChannelMask, absLength, this.cursors, this.cursorsVisible, _channels, _channelGroups );
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
   * Sets the label for a particular channel.
   * 
   * @param aIndex
   *          the index of the channel to set, >= 0 && <
   *          {@value OlsConstants#MAX_CHANNELS};
   * @param aLabel
   *          the label of the channel to set.
   * @throws IllegalArgumentException
   *           in case of an invalid channel index.
   */
  public AcquisitionDataBuilder setChannelLabel( final int aIndex, final String aLabel )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= OlsConstants.MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid channel index!" );
    }

    ChannelInfo chDef = this.channelDefs.get( Integer.valueOf( aIndex ) );
    if ( chDef == null )
    {
      chDef = new ChannelInfo( aLabel );
      this.channelDefs.put( Integer.valueOf( aIndex ), chDef );
    }
    else
    {
      chDef.label = aLabel;
    }
    return this;
  }

  /**
   * Sets the label for a particular cursor.
   * 
   * @param aIndex
   *          the index of the cursor to set, >= 0 && <
   *          {@value OlsConstants#MAX_CURSORS};
   * @param aLabel
   *          the label of the cursor to set.
   * @throws IllegalArgumentException
   *           in case of an invalid cursor index.
   */
  public AcquisitionDataBuilder setCursorLabel( final int aIndex, final String aLabel )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= OlsConstants.MAX_CURSORS ) )
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
   *          the index of the cursor to set, >= 0 && <
   *          {@value OlsConstants#MAX_CURSORS} ;
   * @param aTimestamp
   *          the timestamp of the cursor to set.
   * @throws IllegalArgumentException
   *           in case of an invalid cursor index.
   */
  public AcquisitionDataBuilder setCursorTimestamp( final int aIndex, final long aTimestamp )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= OlsConstants.MAX_CURSORS ) )
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

  /**
   * @param aChannelIndex
   * @return
   */
  private List<ChannelGroup> createChannelGroups( LinkedHashMap<Integer, Channel> aChannelIndex )
  {
    // Use defaults for the situation when no channel groups are defined...
    ensureChannelGroupsAreDefined( aChannelIndex );

    int cgSize = this.channelGroupDefs.size();
    List<ChannelGroup> channelGroups = new ArrayList<ChannelGroup>( cgSize );
    List<Channel> allChannels = new ArrayList<Channel>( aChannelIndex.values() );

    for ( Entry<Integer, ChannelGroupInfo> entry : this.channelGroupDefs.entrySet() )
    {
      int groupIdx = entry.getKey().intValue();
      ChannelGroupInfo groupDef = entry.getValue();

      List<Integer> channelIndices = groupDef.channelIndices;
      List<Channel> channelRefs = new ArrayList<Channel>( channelIndices.size() );
      for ( Integer channelIdx : channelIndices )
      {
        Channel channel = aChannelIndex.get( channelIdx );
        if ( channel == null )
        {
          // probably masked out...
          continue;
        }
        channelRefs.add( channel );
        allChannels.remove( channel );
      }

      channelGroups.add( new ChannelGroupImpl( groupIdx, groupDef.name, channelRefs ) );
    }

    if ( !allChannels.isEmpty() )
    {
      // Add all remaining channels...
      Integer groupIdx = Integer.valueOf( channelGroups.size() );
      String name = String.format( "Group %d", groupIdx );

      channelGroups.add( new ChannelGroupImpl( groupIdx.intValue(), name, allChannels ) );
    }

    return channelGroups;
  }

  /**
   * @return
   */
  private LinkedHashMap<Integer, Channel> createChannels()
  {
    LinkedHashMap<Integer, Channel> chIndex = new LinkedHashMap<Integer, Channel>( this.channelCount );
    // The enabledChannels only tells us _which_ channels are to be enabled,
    // but we still need to keep track of how many channels we have...
    for ( int i = 0, _channelCount = 0; ( _channelCount < this.channelCount ) && ( i < OlsConstants.MAX_CHANNELS ); i++ )
    {
      final int mask = ( 1 << i );
      if ( ( this.enabledChannelMask & mask ) != 0 )
      {
        Integer idx = Integer.valueOf( i );
        ChannelImpl channelImpl = new ChannelImpl( i, true /* enabled */);

        ChannelInfo chDef = this.channelDefs.get( idx );
        if ( chDef != null )
        {
          channelImpl.setLabel( chDef.label );
          // channelImpl.addAnnotations( chDef.annotations ); XXX
        }

        chIndex.put( idx, channelImpl );
        _channelCount++;
      }
    }
    return chIndex;
  }

  /**
   * @param aChannelIndex
   */
  private void ensureChannelGroupsAreDefined( LinkedHashMap<Integer, Channel> aChannelIndex )
  {
    if ( this.channelGroupDefs.isEmpty() )
    {
      List<Channel> channels = new ArrayList<Channel>( aChannelIndex.values() );

      int groupCount = Math.max( 1, channels.size() / OlsConstants.CHANNELS_PER_BLOCK );
      for ( int g = 0, groupOffset = 0; g < groupCount; g++, groupOffset += OlsConstants.CHANNELS_PER_BLOCK )
      {
        Integer groupIdx = Integer.valueOf( g );
        ChannelGroupInfo cgDef = new ChannelGroupInfo( String.format( "Group %d", groupIdx ) );

        int from = groupOffset;
        int to = Math.min( this.channelCount, groupOffset + OlsConstants.CHANNELS_PER_BLOCK );
        for ( Channel ch : channels.subList( from, to ) )
        {
          cgDef.channelIndices.add( Integer.valueOf( ch.getIndex() ) );
        }

        this.channelGroupDefs.put( groupIdx, cgDef );
      }
    }
  }
}
