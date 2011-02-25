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
package nl.lxtreme.ols.export.vcd;


import static nl.lxtreme.ols.export.vcd.ValueChangeDumpHelper.*;

import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a exporter for the "value change dump" format, as specified in IEEE
 * Std 1364-2001.
 */
public class ValueChangeDumpExporter implements Exporter
{
  // CONSTANTS

  private static final String ID = "OLS Java Client";
  private static final String VERSION = "VCD exporter v1";

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#export(nl.lxtreme.ols.api.data.DataContainer,
   *      javax.swing.JComponent, java.io.Writer)
   */
  @Override
  public void export( final DataContainer aContainer, final JComponent aComponent, final OutputStream aStream )
      throws IOException
  {
    final PrintWriter writer = new PrintWriter( aStream );
    try
    {
      final double timescale = getTimebase( aContainer.getSampleRate() );

      writePreamble( writer, aContainer, timescale );
      writeVariableDump( writer, aContainer );
      writeDataDump( writer, aContainer, timescale );
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
   * @param aContainer
   * @param aTimebase
   */
  private void writeDataDump( final PrintWriter aWriter, final DataContainer aContainer, final double aTimebase )
  {
    final int[] values = aContainer.getValues();
    final long[] timestamps = aContainer.getTimestamps();
    final int channelCount = aContainer.getChannels();

    for ( int i = 0, size = values.length; i < size; i++ )
    {
      final int value = values[i];
      final long timestamp = timestamps[i];

      final int time = ( int )( ( timestamp / ( double )aContainer.getSampleRate() ) / aTimebase );

      writeTime( aWriter, time );
      writeVariableData( aWriter, value, channelCount );
    }
  }

  /**
   * @param aWriter
   * @param aContainer
   * @param aTimescale
   */
  private void writePreamble( final PrintWriter aWriter, final DataContainer aContainer, final double aTimescale )
  {
    writeDeclaration( aWriter, "comment", ID );
    writeDate( aWriter );
    writeDeclaration( aWriter, "version", VERSION );
    writeTimescale( aWriter, aTimescale );
    writeDeclaration( aWriter, "scope", "module logic" );
    writeVariableDefinitions( aWriter, aContainer );
    writeDeclaration( aWriter, "upscope" );
    writeDeclaration( aWriter, "enddefinitions" );
  }

  /**
   * @param aWriter
   * @param aValue
   * @param aChannelCount
   */
  private void writeVariableData( final PrintWriter aWriter, final int aValue, final int aChannelCount )
  {
    int value = aValue;
    for ( int i = 0; i < aChannelCount; i++ )
    {
      final char id = ( char )( '#' + i );
      aWriter.printf( "%d%c", Integer.valueOf( value & 1 ), Character.valueOf( id ) ).println();
      value >>= 1;
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
  private void writeVariableDefinitions( final PrintWriter aWriter, final DataContainer aContainer )
  {
    for ( int i = 0; i < aContainer.getChannels(); i++ )
    {
      String label = aContainer.getChannelLabel( i );
      if ( StringUtils.isEmpty( label ) )
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
  private void writeVariableDump( final PrintWriter aWriter, final DataContainer aContainer )
  {
    writeOpenDeclaration( aWriter, "dumpvars" );

    for ( int i = 0; i < aContainer.getChannels(); i++ )
    {
      aWriter.printf( "x%c", Integer.valueOf( '#' + i ) ).println();
    }

    writeCloseDeclaration( aWriter );
  }
}
