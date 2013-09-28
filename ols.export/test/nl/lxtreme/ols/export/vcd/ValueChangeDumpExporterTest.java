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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.export.vcd;


import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.*;

import org.junit.*;
import org.junit.rules.*;


/**
 * Provides test cases for {@link ValueChangeDumpExporter}.
 */
public class ValueChangeDumpExporterTest
{
  // VARIABLES

  private JComponent component;
  private OutputStream nullOutputStream;
  private ValueChangeDumpExporter exporter;

  @Rule
  public TemporaryFolder folder = new TemporaryFolder();

  // METHODS

  /**
   * 
   */
  @Before
  public void setUp()
  {
    this.component = mock( JComponent.class );
    this.nullOutputStream = mock( OutputStream.class );
    this.exporter = spy( new ValueChangeDumpExporter() );
  }

  /**
   * Test method for
   * {@link ValueChangeDumpExporter#export(DataSet, JComponent, OutputStream)} .
   * <p>
   * This method tests that exporting a datadump with only 8 channels works as
   * expected.
   * </p>
   */
  @Test
  public void testExport16ChannelDataDumpOk() throws IOException
  {
    DataSet dataSet = createStubDataSet( 16 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 16 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 16 ) ).writeVariableData( any( PrintWriter.class ), eq( 16 ), eq( 65535 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 17 ) ).writeTime( any( PrintWriter.class ), anyLong() );
  }

  /**
   * Test method for
   * {@link ValueChangeDumpExporter#export(DataSet, JComponent, OutputStream)} .
   * <p>
   * This method tests that exporting a datadump with only 8 channels works as
   * expected.
   * </p>
   */
  @Test
  public void testExport8ChannelDataDumpOk() throws IOException
  {
    DataSet dataSet = createStubDataSet( 8 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 8 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 8 ) ).writeVariableData( any( PrintWriter.class ), eq( 8 ), eq( 255 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 9 ) ).writeTime( any( PrintWriter.class ), anyLong() );
  }

  /**
   * Test method for
   * {@link ValueChangeDumpExporter#export(DataSet, JComponent, OutputStream)} .
   * <p>
   * This method tests that exporting a datadump with only 8 channels works as
   * expected.
   * </p>
   */
  @Test
  public void testExportDataDumpOk() throws IOException
  {
    DataSet dataSet = createStubDataSet( 8 );

    File file = this.folder.newFile( "dump.vcd" );
    FileOutputStream fos = new FileOutputStream( file );

    try
    {
      this.exporter.export( dataSet, this.component, fos );
    }
    finally
    {
      fos.close();
    }
  }

  /**
   * Test method for
   * {@link ValueChangeDumpExporter#export(DataSet, JComponent, OutputStream)} .
   * <p>
   * This method tests that exporting a datadump with only 1 channel works as
   * expected.
   * </p>
   */
  @Test
  public void testExportSingleChannelDataDumpOk() throws IOException
  {
    DataSet dataSet = createStubDataSet( 1 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 1 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 1 ) ).writeVariableData( any( PrintWriter.class ), eq( 1 ), eq( 1 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 2 ) ).writeTime( any( PrintWriter.class ), anyLong() );
  }

  private DataSet createStubDataSet( int aChannelCount )
  {
    int aSize = aChannelCount;
    List<Integer> values = new ArrayList<Integer>( aSize );
    List<Long> timestamps = new ArrayList<Long>( aSize );

    int value = 0;
    int mask = ( 1 << aChannelCount ) - 1;
    for ( int i = 0; i < aSize; i++ )
    {
      values.add( Integer.valueOf( value & mask ) );
      timestamps.add( Long.valueOf( value ) );
      value++;
    }

    CapturedData capData = new CapturedData( values, timestamps, -1, 100, aChannelCount, mask, value - 1 );

    Channel[] channels = new Channel[aChannelCount];
    for ( int i = 0; i < channels.length; i++ )
    {
      channels[i] = mock( Channel.class );
      when( channels[i].getLabel() ).thenReturn( "label" + i );
    }

    DataSet result = mock( DataSet.class );
    when( result.getCapturedData() ).thenReturn( capData );
    when( result.getChannels() ).thenReturn( channels );

    return result;
  }
}
