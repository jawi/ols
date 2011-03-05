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


import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * 
 */
public class StateAnalysisWorker extends BaseAsyncToolWorker<AcquisitionResult>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( StateAnalysisWorker.class.getName() );

  // VARIABLES

  private int number;
  private int level;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public StateAnalysisWorker( final DataContainer aData, final ToolContext aContext )
  {
    super( aData, aContext );
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
   * Convert captured data from timing data to state data using the given
   * channel as clock.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected CapturedData doInBackground() throws Exception
  {
    // obtain data from captured data
    final int[] values = getValues();
    final long triggerPosition = getTriggerPosition();

    final int maskValue = 1 << this.number;

    int size = 0;
    int last = ( values[0] & maskValue ) >> this.number;

    // calculate new sample array size
    for ( final int value : values )
    {
      final int current = ( value & maskValue ) >> this.number;
      if ( ( last == this.level ) && ( current != this.level ) )
      {
        size++;
      }
      last = current;
    }

    if ( size <= 0 )
    {
      LOG.log( Level.WARNING, "No state changes found in data; aborting analysis..." );
      throw new IllegalStateException( "No state changes found!" );
    }

    // convert captured data
    last = values[0] & maskValue;
    int pos = 0;
    int newTrigger = -1;

    final int[] newValues = new int[size];
    for ( int i = 0; i < values.length; i++ )
    {
      final int current = ( values[i] & maskValue ) >> this.number;
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

    final CapturedData newCapturedData = new CapturedData( newValues, newTrigger, Ols.NOT_AVAILABLE,
        getChannels(), getEnabledChannels() );
    setCapturedData( newCapturedData );

    return newCapturedData;
  }
}
