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
package nl.lxtreme.ols.device.sump;


import java.util.*;


/**
 * 
 */
final class SampleCountIterator implements Iterator<Integer>
{
  // VARIABLES

  private final boolean countForward;
  private final int max;
  private final int start;
  private final int end;

  private volatile int idx;

  /**
   * Creates a new {@link SampleCountIterator} instance.
   * 
   * @param aCountForward
   *          <code>true</code> to count in a forward manner;
   * @param aMax
   *          the maximum number to count to.
   */
  public SampleCountIterator( final boolean aCountForward, final int aMax )
  {
    this.countForward = aCountForward;
    this.max = aMax;
    this.start = this.countForward ? 0 : this.max;
    this.end = this.countForward ? this.max : 0;

    this.idx = this.start;
  }

  @Override
  public boolean hasNext()
  {
    return this.countForward ? ( this.idx < this.end ) : ( this.idx > this.end );
  }

  @Override
  public Integer next()
  {
    Integer result = Integer.valueOf( this.idx );
    this.idx += this.countForward ? 1 : -1;
    return result;
  }

  @Override
  public void remove()
  {
    throw new UnsupportedOperationException();
  }
}
