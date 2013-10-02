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


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Converts acquisition data with timing information to state data, taking one
 * channel as clock.
 */
public class StateAnalysisTask implements ToolTask<AcquisitionData>
{
  // VARIABLES

  private final ToolContext context;

  private int clockLine;
  private Edge sampleEdge;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public StateAnalysisTask( final ToolContext aContext )
  {
    this.context = aContext;
  }

  // METHODS

  /**
   * Converts acquisition data from timing data to state data using the given
   * channel as clock.
   */
  @Override
  public AcquisitionData call() throws Exception
  {
    AcquisitionData data = this.context.getData();

    if ( !data.hasTimingData() )
    {
      throw new IllegalStateException( "Cannot convert state data to state data!" );
    }

    // obtain data from captured data
    int[] values = data.getValues();
    long triggerPosition = data.getTriggerPosition();

    int clockMask = 1 << this.clockLine;
    int dataMask = ~clockMask;

    // one line is the clock, which is no longer necessary to show (it is always
    // zero)
    int newChannelCount = data.getChannelCount() - 1;
    // mask out the clock line...
    int newEnabledChannels = ( int )( ( ( 1L << data.getChannelCount() ) - 1L ) ) & dataMask;

    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( newChannelCount );
    builder.setEnabledChannelMask( newEnabledChannels );

    // convert captured data
    int last = values[0] & clockMask;
    int pos = 0;

    for ( int i = 0; i < values.length; i++ )
    {
      int current = values[i] & clockMask;
      Edge edge = Edge.toEdge( last, current );

      if ( edge == this.sampleEdge )
      {
        builder.addSample( pos, values[i - 1] & dataMask );
        pos++;
      }

      if ( triggerPosition == i )
      {
        builder.setTriggerPosition( Math.max( 0, pos - 1 ) );
      }

      last = current;
    }

    return builder.build();
  }

  /**
   * @param aEdge
   *          the level to set, cannot be <code>null</code>.
   */
  public void setSampleEdge( final Edge aEdge )
  {
    this.sampleEdge = aEdge;
  }

  /**
   * @param aClockLine
   *          the channel number of the clock, >= 0.
   */
  public void setNumber( final int aClockLine )
  {
    this.clockLine = aClockLine;
  }
}
