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

import junit.framework.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;


/**
 * Provides test cases for {@link ValueChangeDumpExporter}.
 */
public class ValueChangeDumpExporterTest extends TestCase
{
  // VARIABLES

  private JComponent component;
  private OutputStream nullOutputStream;
  private ValueChangeDumpExporter exporter;

  // METHODS

  /**
   * Test method for
   * {@link ValueChangeDumpExporter#export(DataSet, JComponent, OutputStream)} .
   * <p>
   * This method tests that exporting a datadump with only 8 channels works as
   * expected.
   * </p>
   */
  public void testExport16ChannelDataDumpOk() throws IOException
  {
    AcquisitionData data = DataTestUtils.generateAcquisitionData( 16 );

    this.exporter.export( data, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( data ) );
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
  public void testExport8ChannelDataDumpOk() throws IOException
  {
    AcquisitionData data = DataTestUtils.generateAcquisitionData( 8 );

    this.exporter.export( data, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( data ) );
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
  public void testExportDataDumpOk() throws IOException
  {
    AcquisitionData data = DataTestUtils.generateAcquisitionData( 8 );

    ByteArrayOutputStream os = new ByteArrayOutputStream( 1024 );

    try
    {
      this.exporter.export( data, this.component, os );
    }
    finally
    {
      os.close();
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
  public void testExportSingleChannelDataDumpOk() throws IOException
  {
    AcquisitionData data = DataTestUtils.generateAcquisitionData( 1 );

    this.exporter.export( data, this.component, this.nullOutputStream );

    verify( this.exporter ).writeVariableDump( any( PrintWriter.class ), eq( data ) );
    verify( this.exporter, times( 1 ) ).writeSingleVariableDefinition( any( PrintWriter.class ), anyInt() );
    verify( this.exporter, times( 16 ) ).writeVariableData( any( PrintWriter.class ), eq( 1 ), eq( 1 ), anyInt(),
        anyInt(), anyBoolean() );
    verify( this.exporter, times( 17 ) ).writeTime( any( PrintWriter.class ), anyLong() );
  }

  /**
   * 
   */
  @Override
  protected void setUp()
  {
    this.component = mock( JComponent.class );
    this.nullOutputStream = mock( OutputStream.class );
    this.exporter = spy( new ValueChangeDumpExporter() );
  }
}
