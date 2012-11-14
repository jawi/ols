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
package nl.lxtreme.ols.export.csv;


import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.export.*;


/**
 * Provides a CSV exporter for exporting the acquisition data in a
 * comma-separated format.
 */
public class CsvExporter implements Exporter
{
  // VARIABLES

  private final char colSeparator;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CsvExporter} instance.
   */
  public CsvExporter()
  {
    this.colSeparator = ',';
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void export( final AcquisitionData aData, final JComponent aComponent, final OutputStream aStream )
      throws IOException
  {
    final PrintStream stream = new PrintStream( aStream );

    try
    {
      // Write header row...
      writeHeaderRow( stream, createHeaderRowValues( aData ) );

      final int sampleRate = aData.getSampleRate();
      final int[] values = aData.getValues();
      final long[] timestamps = aData.getTimestamps();
      final long triggerPos = aData.getTriggerPosition();

      // Write data...
      for ( int i = 0; i < values.length; i++ )
      {
        // Write data row...
        writeDataRow( stream, timestamps[i], triggerPos, sampleRate, values[i], aData.getChannelCount(),
            aData.getEnabledChannels() );
      }
    }
    finally
    {
      stream.flush();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String[] getFilenameExtentions()
  {
    return new String[] { "csv" };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "CSV File";
  }

  /**
   * @param aChannelCount
   * @return
   */
  private void createChannelHeaders( final List<String> aList, final AcquisitionData aData )
  {
    final int insertIdx = aList.size();
    for ( int i = 0; i < aData.getChannelCount(); i++ )
    {
      String label = aData.getChannel( i ).getLabel();
      if ( label == null )
      {
        label = String.format( "Ch.%d", Integer.valueOf( i ) );
      }
      // Causes the channels to be listed in "inverse" order, MSB first...
      aList.add( insertIdx, label );
    }
  }

  /**
   * @param aData
   * @return
   */
  private String[] createHeaderRowValues( final AcquisitionData aData )
  {
    final long triggerPos = aData.getTriggerPosition();
    final int sampleRate = aData.getSampleRate();

    List<String> result = new ArrayList<String>();
    if ( sampleRate > 0 )
    {
      result.add( "timestamp (abs)" );
    }
    else
    {
      result.add( "state (abs)" );
    }

    if ( triggerPos > 0 )
    {
      if ( sampleRate > 0 )
      {
        result.add( "timestamp (rel)" );
      }
      else
      {
        result.add( "state (rel)" );
      }
    }

    if ( sampleRate > 0 )
    {
      result.add( "sample rate (Hz)" );
    }

    createChannelHeaders( result, aData );

    return result.toArray( new String[result.size()] );
  }

  /**
   * @param aValue
   * @return
   */
  private String quote( final String aValue )
  {
    return String.format( "\"%s\"", aValue );
  }

  /**
   * @param aStream
   *          the output stream to write the data values to;
   * @param aAbsTime
   * @param aTriggerPos
   * @param aValue
   * @param aChannels
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeDataRow( final PrintStream aStream, final long aAbsTime, final long aTriggerPos,
      final int aSampleRate, final int aValue, final int aChannelCount, final int aEnabledChannels ) throws IOException
  {
    aStream.print( aAbsTime );

    if ( aTriggerPos > 0 )
    {
      aStream.print( this.colSeparator );
      aStream.print( aAbsTime - aTriggerPos );
    }
    if ( aSampleRate > 0 )
    {
      aStream.print( this.colSeparator );
      aStream.print( aSampleRate );
    }

    if ( aEnabledChannels != 0 )
    {
      for ( int i = Ols.MAX_CHANNELS - 1; i >= 0; i-- )
      {
        final int mask = ( 1 << i );
        final boolean enabled = ( ( aEnabledChannels & mask ) != 0 );

        if ( enabled )
        {
          aStream.print( this.colSeparator );

          int v = ( aValue & mask ) >> i;
          aStream.print( v );
        }
      }
    }

    aStream.println();
  }

  /**
   * @param aStream
   *          the output stream to write the headers to;
   * @param aHeaders
   *          the header values to write.
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeHeaderRow( final PrintStream aStream, final String[] aHeaders ) throws IOException
  {
    for ( int i = 0; i < aHeaders.length; i++ )
    {
      if ( i > 0 )
      {
        aStream.print( this.colSeparator );
      }
      aStream.print( quote( aHeaders[i] ) );
    }

    aStream.println();
  }
}
