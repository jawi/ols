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
package nl.lxtreme.ols.tool.state;


import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * 
 */
public class StateAnalysisWorker extends BaseToolWorker<CapturedData>
{
  // VARIABLES

  private int number;
  private int level;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public StateAnalysisWorker( final CapturedData aData )
  {
    super( aData );
  }

  // METHODS

  /**
   * Sets the level to the given value.
   * 
   * @param aLevel
   *          the level to set, cannot be <code>null</code>.
   */
  public void setLevel( final int aLevel )
  {
    this.level = aLevel;
  }

  /**
   * Sets the number to the given value.
   * 
   * @param aNumber
   *          the number to set, cannot be <code>null</code>.
   */
  public void setNumber( final int aNumber )
  {
    this.number = aNumber;
  }

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected CapturedData doInBackground() throws Exception
  {
    final CapturedData data = getData();

    // obtain data from captured data
    int[] values = data.values;
    long triggerPosition = data.triggerPosition;

    // calculate new sample array size
    int last = values[0] & 1 << this.number;
    int size = 0;
    for ( int value : values )
    {
      int current = value & 1 << this.number;
      if ( ( last == this.level ) && ( current != this.level ) )
      {
        size++;
      }
      last = current;
    }

    // convert captured data
    last = values[0] & 1 << this.number;
    int pos = 0;
    int newTrigger = -1;
    int[] newValues = new int[size];
    for ( int i = 0; i < values.length; i++ )
    {
      int current = values[i] & 1 << this.number;
      if ( ( last == this.level ) && ( current != this.level ) )
      {
        newValues[pos++] = values[i - 1];
      }
      if ( triggerPosition == i )
      {
        newTrigger = pos;
      }
      last = current;
    }

    return new CapturedData( newValues, newTrigger, CapturedData.NOT_AVAILABLE, data.channels, data.enabledChannels );
  }
}
