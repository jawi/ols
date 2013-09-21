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
package nl.lxtreme.ols.tool.dmx512;


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.uart.*;


/**
 * Provides a special purpose asynchronous serial-data decoder for DMX512.
 */
public class DMX512SerialDataDecoder extends AsyncSerialDataDecoder
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512SerialDataDecoder} instance.
   * 
   * @param aConfiguration
   *          the serial configuration to use, cannot be <code>null</code>;
   * @param aContext
   *          the tool context to use, cannot be <code>null</code>.
   */
  public DMX512SerialDataDecoder( final SerialConfiguration aConfiguration, final ToolContext aContext )
  {
    super( aConfiguration, aContext );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected long findStartBit( final int aChannelIdx, final Edge aEdge, final long aStartOfDecode,
      final long aEndOfDecode )
  {
    int sampleRate = this.dataSet.getSampleRate();

    long time = super.findEdge( aChannelIdx, aEdge, aStartOfDecode, aEndOfDecode );
    if ( time < 0 )
    {
      // No start bit found...
      return time;
    }

    // Find the time until signal inverts again...
    long time2 = super.findEdge( aChannelIdx, aEdge.invert(), time, aEndOfDecode );

    // We do not have to know the length of the mark-after-break exactly, as we
    // can deduce this also from the fact that each frame has at least two
    // transitions (start and stop-bits) which always fall inside the total
    // frame size...
    if ( ( time2 - time ) > this.configuration.getFrameSize( sampleRate ) )
    {
      // Mark-after-break found; search again for the next start-bit...
      long time3 = super.findEdge( aChannelIdx, aEdge, time2, aEndOfDecode );

      SerialDecoderCallback callback = getCallback();
      if ( callback != null )
      {
        // The first part is the start-before-break...
        callback.onEvent( aChannelIdx, DMX512DataSet.EVENT_SBB, time, time2 );
        // The second part is the mark-after-break...
        callback.onEvent( aChannelIdx, DMX512DataSet.EVENT_MAB, time2, time3 );
      }

      // Continue the decoding after the MaB...
      time = time3;
    }

    return time;
  }
}
