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

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.util.*;

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
    DataSet dataSet = DataTestUtils.createStubDataSet( 16 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 16 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 2 ) ).writeVariableData( any( PrintWriter.class ), eq( 16 ), eq( 65535 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 3 ) ).writeTime( any( PrintWriter.class ), anyLong() );
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
    DataSet dataSet = DataTestUtils.createStubDataSet( 8 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 8 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 4 ) ).writeVariableData( any( PrintWriter.class ), eq( 8 ), eq( 255 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 5 ) ).writeTime( any( PrintWriter.class ), anyLong() );
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
    DataSet dataSet = DataTestUtils.createStubDataSet( 8 );

    File file = this.folder.newFile( "dump.vcd" );
    FileOutputStream fos = new FileOutputStream( file );

    try
    {
      this.exporter.export( dataSet, this.component, fos );
    }
    finally
    {
      HostUtils.closeResource( fos );
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
    DataSet dataSet = DataTestUtils.createStubDataSet( 1 );

    this.exporter.export( dataSet, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( dataSet ) );
    verify( this.exporter, times( 1 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 16 ) ).writeVariableData( any( PrintWriter.class ), eq( 1 ), eq( 1 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 17 ) ).writeTime( any( PrintWriter.class ), anyLong() );
  }
}
