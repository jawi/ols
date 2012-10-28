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
package nl.lxtreme.ols.tool.state;


import java.util.concurrent.*;
import java.util.logging.*;

import aQute.bnd.annotation.metatype.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * 
 */
public class StateAnalysisTask implements Callable<Void>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( StateAnalysisTask.class.getName() );

  // VARIABLES

  private final ToolContext context;

  private final int number;
  private final int level;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public StateAnalysisTask( final ToolContext aContext, final Configuration aConfiguration )
  {
    this.context = aContext;

    StateConfig config = Configurable.createConfigurable( StateConfig.class, aConfiguration.asMap() );

    this.number = config.channelIdx();
    this.level = ( Edge.RISING == config.channelEdge() ) ? 0 : 1;
  }

  // METHODS

  /**
   * Convert captured data from timing data to state data using the given
   * channel as clock.
   */
  @Override
  public Void call() throws ToolException
  {
    final AcquisitionData data = this.context.getAcquisitionData();

    // obtain data from captured data
    final int[] values = data.getValues();
    final long triggerPosition = data.getTriggerPosition();

    final int maskValue = 1 << this.number;

    final AcquisitionDataBuilder builder = new AcquisitionDataBuilder( data, false /* aIncludeSamples */);
    // We're returning state data instead of time-based data...
    builder.clearSampleRate();

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
      throw new ToolException( "No state changes found!" );
    }

    // convert captured data
    last = values[0] & maskValue;
    for ( int i = 0, pos = 0; i < values.length; i++ )
    {
      final int current = ( values[i] & maskValue ) >> this.number;
      if ( ( last == this.level ) && ( current != this.level ) )
      {
        builder.addSample( pos, values[i - 1] );
      }
      if ( triggerPosition == i )
      {
        builder.setTriggerPosition( pos );
      }
      last = current;
    }

    // Update the acquisition data with our calculated state data...
    this.context.getSession().setAcquisitionData( builder.build() );

    return null;
  }
}
