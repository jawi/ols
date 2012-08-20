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
package nl.lxtreme.ols.tool.serialdebug;


import java.util.*;
import java.util.concurrent.atomic.*;


/**
 * Provides a, thread-safe, mini character buffer which can append a list of
 * characters, or remove a number of characters from the front.
 */
class CharBuffer implements CharSequence
{
  // INNER TYPES

  /**
   * Contains the state of the char buffer itself.
   */
  private static class CharBufferState
  {
    // VARIABLES

    final Integer[] chars;
    final int appendPos;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CharBufferState} instance.
     */
    public CharBufferState( final Integer[] aChars, final int aAppendPos )
    {
      this.chars = aChars;
      this.appendPos = aAppendPos;
    }
  }

  // VARIABLES

  private final AtomicReference<CharBufferState> stateRef;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CharBuffer} instance.
   */
  public CharBuffer()
  {
    this.stateRef = new AtomicReference<CharBufferState>( new CharBufferState( new Integer[10], 0 ) );
  }

  /**
   * Creates a new {@link CharBuffer} instance.
   */
  public CharBuffer( final Integer... aInitialValue )
  {
    Integer[] initialValue = Arrays.copyOf( aInitialValue, aInitialValue.length + 10 );
    int appendPos = aInitialValue.length;
    this.stateRef = new AtomicReference<CharBufferState>( new CharBufferState( initialValue, appendPos ) );
  }

  // METHODS

  /**
   * Appends a given list of characters to this buffer.
   * 
   * @param aChars
   *          the list with characters to add, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given list was <code>null</code>.
   */
  public void append( final List<Integer> aChars )
  {
    if ( aChars == null )
    {
      throw new IllegalArgumentException( "Chars cannot be null!" );
    }

    CharBufferState state = this.stateRef.get();
    Integer[] chars = state.chars;
    int appendPos = state.appendPos;

    if ( ( appendPos + aChars.size() ) >= chars.length )
    {
      // Enlarge array...
      chars = Arrays.copyOf( chars, chars.length + aChars.size() + 10 );
    }
    for ( int i = 0; i < aChars.size(); i++ )
    {
      chars[appendPos++] = aChars.get( i );
    }

    this.stateRef.compareAndSet( state, new CharBufferState( chars, appendPos ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public char charAt( final int aIndex )
  {
    CharBufferState state = this.stateRef.get();
    Integer[] chars = state.chars;
    int appendPos = state.appendPos;

    if ( ( aIndex < 0 ) || ( aIndex >= appendPos ) )
    {
      throw new IndexOutOfBoundsException();
    }

    final Integer integer = chars[aIndex];
    return ( char )( ( integer == null ) ? 0 : integer.intValue() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int length()
  {
    CharBufferState state = this.stateRef.get();
    return state.appendPos;
  }

  /**
   * Removes all characters until the given position.
   * 
   * @param aPosition
   *          the position until which the characters should be removed.
   */
  public void removeUntil( final int aPosition )
  {
    CharBufferState state = this.stateRef.get();
    int appendPos = state.appendPos;

    if ( ( aPosition < 0 ) || ( aPosition > appendPos ) )
    {
      throw new IndexOutOfBoundsException();
    }

    if ( aPosition == 0 )
    {
      // Nothing to do...
      return;
    }
    else if ( aPosition >= ( appendPos - 1 ) )
    {
      // Remove all...
      this.stateRef.compareAndSet( state, new CharBufferState( new Integer[10], 0 ) );
    }
    else
    {
      // Remove otherwise...
      final Integer[] oldArray = state.chars;
      final int newSize = oldArray.length - aPosition;
      final Integer[] newArray = new Integer[newSize];
      final int newAppendPos = appendPos - aPosition;
      System.arraycopy( oldArray, aPosition, newArray, 0, newSize );

      this.stateRef.compareAndSet( state, new CharBufferState( newArray, newAppendPos ) );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CharSequence subSequence( final int aStart, final int aEnd )
  {
    throw new UnsupportedOperationException();
  }
}
