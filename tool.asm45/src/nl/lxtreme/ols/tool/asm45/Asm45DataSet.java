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
package nl.lxtreme.ols.tool.asm45;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * @author Ansgar Kueckes
 */
public final class Asm45DataSet extends BaseDataSet<Asm45Data>
{
  // CONSTANTS

  // VARIABLES

  private int decodedWords;
  private int triggerEvent;
  private double last_timing = -1;

  // CONSTRUCTORS

  /**
   * 
   */
  public Asm45DataSet( final int aStartSampleIdx, final int aStopSampleIdx, final AcquisitionResult aData )
  {
    super( aStartSampleIdx, aStopSampleIdx, aData );

    this.decodedWords = 0;
    this.triggerEvent = 0;
  }

  // METHODS

  /**
   * @return the number of decoded bytes, >= 0.
   */
  public int getDecodedWordCount()
  {
    return this.decodedWords;
  }

  /**
   * @return the total time for all samples.
   */
  public String getTotalTime()
  {
    final double startTime = getTime( getStartOfDecode() );
    final double endTime = getTime( getEndOfDecode() );
    return String.format( "%.2f ms", Double.valueOf( ( endTime - startTime ) * 1000000.0 ) );
  }

  /**
   * @return the number of the trigger event, >= 0.
   */
  public int getTriggerEvent()
  {
    return this.triggerEvent;
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  public void reportEvent( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aClocks, final int aBlock, final int aAddress, final int aValue, final boolean aBusGrant,
      final String aType, final String aEvent )
  {
    final int idx = size();
    final double current_timing = getTime( aStartSampleIdx );
    if ( ( this.last_timing < 0 ) && ( current_timing >= 0 ) )
    {
      this.triggerEvent = this.decodedWords;
    }
    this.last_timing = current_timing;
    this.decodedWords++;
    addData( new Asm45Data( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aClocks, aBlock, aAddress, aValue,
        aBusGrant, aType, aEvent ) );
  }
}

/* EOF */
