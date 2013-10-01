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
package nl.lxtreme.ols.export.vcd;


import static nl.lxtreme.ols.export.vcd.ValueChangeDumpHelper.*;

import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.export.api.*;


/**
 * Provides a exporter for the "value change dump" format, as specified in IEEE
 * Std 1364-2001.
 */
public class ValueChangeDumpExporter implements Exporter
{
  // CONSTANTS

  private static final String ID = "OLS Java Client";
  private static final String VERSION = "VCD exporter v1.1";

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void export( AcquisitionData aDataSet, JComponent aComponent, OutputStream aStream ) throws IOException
  {
    final PrintWriter writer = new PrintWriter( aStream );
    try
    {
      final double timescale = getTimebase( aDataSet.getSampleRate() );

      writePreamble( writer, aDataSet, timescale );
      writeVariableDump( writer, aDataSet );
      writeDataDump( writer, aDataSet, timescale );
    }
    finally
    {
      writer.flush();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getFilenameExtentions()
   */
  @Override
  public String[] getFilenameExtentions()
  {
    return new String[] { "vcd" };
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getName()
   */
  @Override
  public String getName()
  {
    return "Value Change Dump";
  }

  /**
   * @param aWriter
   * @param aData
   * @param aTimebase
   */
  protected void writeDataDump( final PrintWriter aWriter, final AcquisitionData aData, final double aTimebase )
  {
    final int[] values = aData.getValues();
    final long[] timestamps = aData.getTimestamps();
    final int channelCount = aData.getChannelCount();
    final int channelMask = aData.getEnabledChannels();

    int oldValue = -1;
    for ( int i = 0, size = values.length; i < size; i++ )
    {
      final int value = values[i];
      final long timestamp = timestamps[i];

      final int time = ( int )( timestamp / ( aData.getSampleRate() * aTimebase ) );

      if ( ( i == 0 ) || ( oldValue != value ) )
      {
        writeTime( aWriter, time );
        writeVariableData( aWriter, channelCount, channelMask, value, oldValue, ( i == 0 ) );
      }

      oldValue = value;
    }

    final int time = ( int )( aData.getAbsoluteLength() / ( aData.getSampleRate() * aTimebase ) );
    writeTime( aWriter, time );
  }

  /**
   * @param aWriter
   * @param aContainer
   * @param aTimescale
   */
  protected void writePreamble( final PrintWriter aWriter, final AcquisitionData aData, final double aTimescale )
  {
    writeDeclaration( aWriter, "comment", ID );
    writeDate( aWriter );
    writeDeclaration( aWriter, "version", VERSION );
    writeTimescale( aWriter, aTimescale );
    writeDeclaration( aWriter, "scope", "module logic" );
    writeVariableDefinitions( aWriter, aData );
    writeDeclaration( aWriter, "upscope" );
    writeDeclaration( aWriter, "enddefinitions" );
  }

  /**
   * @param aWriter
   * @param aIndex
   */
  protected void writeSingleVariableDefinition( final PrintWriter aWriter, final int aIndex )
  {
    aWriter.printf( "x%s", getIdentifier( aIndex ) ).println();
  }

  /**
   * @param aWriter
   * @param aTimebase
   */
  protected void writeTime( final PrintWriter aWriter, final long aTimebase )
  {
    ValueChangeDumpHelper.writeTime( aWriter, aTimebase );
  }

  /**
   * @param aWriter
   *          the writer to write the variable data to;
   * @param aChannelCount
   *          the total channel count;
   * @param aChannelMask
   *          the enabled channel mask;
   * @param aValue
   *          the value to write.
   */
  protected void writeVariableData( final PrintWriter aWriter, final int aChannelCount, final int aChannelMask,
      final int aValue, final int aOldValue, final boolean aAllBits )
  {
    int value = aValue;
    int oldValue = aOldValue;
    int mask = aChannelMask;
    for ( int i = 0; i < aChannelCount; i++ )
    {
      if ( ( mask & ( 1 << i ) ) == 0 )
      {
        continue;
      }

      final int bitValue = ( value & 1 );
      final int oldBitValue = ( oldValue & 1 );

      if ( aAllBits || ( bitValue != oldBitValue ) )
      {
        aWriter.printf( "%d%s", Integer.valueOf( bitValue ), getIdentifier( i ) ).println();
      }

      value >>= 1;
      oldValue >>= 1;
    }
  }

  /**
   * Writes down all variable definitions.
   * 
   * @param aWriter
   *          the print writer to write to, cannot be <code>null</code>;
   * @param aContainer
   *          the data container to take the channel information from, cannot be
   *          <code>null</code>.
   */
  protected void writeVariableDefinitions( final PrintWriter aWriter, final AcquisitionData aData )
  {
    final int channelMask = aData.getEnabledChannels();
    final Channel[] channelLabels = aData.getChannels();

    for ( int i = 0; i < channelLabels.length; i++ )
    {
      if ( ( channelMask & ( 1 << i ) ) == 0 )
      {
        continue;
      }

      String label = channelLabels[i].getLabel();
      if ( label == null || "".equals( label.trim() ) )
      {
        label = "channel" + i;
      }

      writeVariable( aWriter, i, label );
    }
  }

  /**
   * @param aWriter
   * @param aContainer
   */
  protected void writeVariableDump( final PrintWriter aWriter, final AcquisitionData aData )
  {
    final int channelCount = aData.getChannelCount();
    final int channelMask = aData.getEnabledChannels();

    writeOpenDeclaration( aWriter, "dumpvars" );

    for ( int i = 0; i < channelCount; i++ )
    {
      if ( ( channelMask & ( 1 << i ) ) == 0 )
      {
        continue;
      }

      writeSingleVariableDefinition( aWriter, i );
    }

    writeCloseDeclaration( aWriter );
  }
}
