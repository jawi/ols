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
package nl.lxtreme.ols.test.data.project;


import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * 
 */
public class StubDataSet implements DataSet
{
  // VARIABLES

  private AcquisitionResult capturedData;
  private Channel[] channels;
  private final Cursor[] cursors;
  private boolean cursorsEnabled;

  // CONSTRUCTORS

  /**
   * Creates a new StubDataSet instance.
   */
  public StubDataSet()
  {
    this.cursors = new Cursor[Ols.MAX_CURSORS];
    for ( int i = 0; i < this.cursors.length; i++ )
    {
      this.cursors[i] = new StubCursor( i );
    }
  }

  // METHODS

  /**
   * Asserts the given absolute lengths is defined in the captured data.
   * 
   * @param aAbsLength
   *          the absolute length that is expected.
   */
  public void assertAbsoluteLength( final long aAbsLength )
  {
    assertNotNull( this.capturedData );

    final long absLength = this.capturedData.getAbsoluteLength();
    assertEquals( aAbsLength, absLength );
  }

  /**
   * Asserts the channel group with the given index is disabled in the captured
   * data.
   * 
   * @param aGroupIdx
   *          the group index.
   */
  public void assertChannelGroupDisabled( final int aGroupIdx )
  {
    assertNotNull( this.capturedData );

    assertTrue( ( this.capturedData.getEnabledChannels() & ( 0xFFL << ( aGroupIdx * 8 ) ) ) == 0 );
  }

  /**
   * Asserts the channel group with the given index is enabled in the captured
   * data.
   * 
   * @param aGroupIdx
   *          the group index.
   */
  public void assertChannelGroupEnabled( final int aGroupIdx )
  {
    assertNotNull( this.capturedData );

    assertTrue( ( this.capturedData.getEnabledChannels() & ( 0xFFL << ( aGroupIdx * 8 ) ) ) != 0 );
  }

  /**
   * Asserts the cursor with the given index occur in the captured data.
   * 
   * @param aCursorIdx
   *          the index of the cursor to assert;
   * @param aCursorValue
   *          the expected value of the cursor to assert.
   */
  public void assertCursorSet( final int aCursorIdx, final long aCursorValue )
  {
    assertNotNull( this.cursors );
    assertTrue( this.cursors.length > aCursorIdx );
    assertTrue( this.cursors[aCursorIdx].isDefined() );
    assertEquals( aCursorValue, this.cursors[aCursorIdx].getTimestamp() );
  }

  /**
   * Asserts the cursor with the given index does NOT occur in the captured
   * data.
   * 
   * @param aCursorIdx
   *          the index of the cursor that should be unset.
   */
  public void assertCursorUnset( final int aCursorIdx )
  {
    assertNotNull( this.cursors );
    assertTrue( this.cursors.length > aCursorIdx );
    assertFalse( this.cursors[aCursorIdx].isDefined() );
  }

  /**
   * Asserts the given timestamps occur in the captured data.
   * 
   * @param aTimestamps
   *          the timestamps that are to be expected, starting at the first
   *          timestamp.
   */
  public void assertTimeStamps( final long... aTimestamps )
  {
    assertNotNull( this.capturedData );

    final long[] timestamps = this.capturedData.getTimestamps();
    assertArrayEquals( aTimestamps, timestamps );
  }

  /**
   * Asserts the given values occur in the captured data.
   * 
   * @param aValues
   *          the sample values that are to be expected, starting at the first
   *          value.
   */
  public void assertValues( final int... aValues )
  {
    assertNotNull( this.capturedData );

    final int[] values = this.capturedData.getValues();
    assertArrayEquals( aValues, values );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult getCapturedData()
  {
    return this.capturedData;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel getChannel( final int aIndex )
  {
    return this.channels[aIndex];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel[] getChannels()
  {
    List<Channel> result = new ArrayList<Channel>();
    // Issue #244 - only return non-null channels...
    for ( Channel chan : this.channels )
    {
      if ( chan != null )
      {
        result.add( chan );
      }
    }
    return result.toArray( size -> new Channel[size] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor getCursor( final int aIndex )
  {
    return this.cursors[aIndex];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor[] getCursors()
  {
    return this.cursors;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isCursorsEnabled()
  {
    return this.cursorsEnabled;
  }

  /**
   * Sets capturedData to the given value.
   * 
   * @param aCapturedData
   *          the capturedData to set.
   */
  public void setCapturedData( final AcquisitionResult aCapturedData )
  {
    this.capturedData = aCapturedData;

    int count = aCapturedData.getChannels();
    int mask = aCapturedData.getEnabledChannels();

    this.channels = new Channel[count];
    for ( int i = 0, j = 0; ( j < count ) && ( i < Ols.MAX_CHANNELS ); i++ )
    {
      final int chMask = ( 1 << i );
      // Issue #99: demultiplex the channels to the right group...
      if ( ( mask & chMask ) == 0 )
      {
        continue;
      }
      this.channels[j++] = new StubChannel( i );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setCursorsEnabled( final boolean aEnabled )
  {
    this.cursorsEnabled = aEnabled;
  }
}
