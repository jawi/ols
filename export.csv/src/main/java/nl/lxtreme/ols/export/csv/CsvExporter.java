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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;


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
  public void export( final DataSet aDataSet, final JComponent aComponent, final OutputStream aStream )
      throws IOException
  {
    final PrintStream stream = new PrintStream( aStream );

    try
    {
      // Write header row...
      writeHeaderRow( stream, createHeaderRowValues( aDataSet ) );

      final Channel[] channels = aDataSet.getChannels();

      final AcquisitionResult capturedData = aDataSet.getCapturedData();
      final int sampleRate = capturedData.getSampleRate();
      final int[] values = capturedData.getValues();
      final long[] timestamps = capturedData.getTimestamps();
      final long triggerPos = capturedData.getTriggerPosition();

      // Write data...
      for ( int i = 0; i < values.length; i++ )
      {
        // Write data row...
        writeDataRow( stream, timestamps[i], triggerPos, sampleRate, values[i], channels );
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
  private void createChannelHeaders( final List<String> aList, final Channel[] aChannels )
  {
    final int insertIdx = aList.size();
    for ( int i = 0; i < aChannels.length; i++ )
    {
      String label = aChannels[i].getLabel();
      if ( label == null )
      {
        label = String.format( "Ch.%d", Integer.valueOf( i ) );
      }
      // Causes the channels to be listed in "inverse" order, MSB first...
      aList.add( insertIdx, label );
    }
  }

  /**
   * @param aDataSet
   * @return
   */
  private String[] createHeaderRowValues( final DataSet aDataSet )
  {
    final Channel[] channels = aDataSet.getChannels();

    final AcquisitionResult capturedData = aDataSet.getCapturedData();
    final long triggerPos = capturedData.getTriggerPosition();
    final int sampleRate = capturedData.getSampleRate();

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

    createChannelHeaders( result, channels );

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
      final int aSampleRate, final int aValue, final Channel[] aChannels ) throws IOException
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

    if ( aChannels != null )
    {
      int l = aChannels.length;
      for ( int i = 0; i < l; i++ )
      {
        Channel channel = aChannels[l - i - 1];
        int v = ( aValue & channel.getMask() ) >> channel.getIndex();
        aStream.print( this.colSeparator );
        aStream.print( v );
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
