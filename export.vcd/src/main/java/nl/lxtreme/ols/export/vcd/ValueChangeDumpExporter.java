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
import java.text.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;


/**
 * Provides a exporter for the "value change dump" format, as specified in IEEE
 * Std 1364-2001.
 */
public class ValueChangeDumpExporter implements Exporter
{
  // CONSTANTS

  private static final String VERSION = "VCD exporter v1";

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#export(nl.lxtreme.ols.api.data.DataContainer,
   *      java.io.Writer)
   */
  @Override
  public void export( final DataContainer aContainer, final Writer aWriter ) throws IOException
  {
    final PrintWriter writer = new PrintWriter( aWriter );
    try
    {
      final double timescale = getTimebase( aContainer.getSampleRate() );

      writeDeclaration( writer, "date", DateFormat.getDateTimeInstance().format( new Date() ) );
      writeDeclaration( writer, "version", VERSION );
      writeDeclaration( writer, "timescale", getTimescale( timescale ) );
      writeDeclaration( writer, "scope", "module logic" );
      writer.println( "$var wire 8 # data $end" );
      writer.println( "$var wire 1 $ data_valid $end" );
      writer.println( "$var wire 1 % en $end" );
      writer.println( "$var wire 1 & rx_en $end" );
      writer.println( "$var wire 1 ' tx_en $end" );
      writer.println( "$var wire 1 ( empty $end" );
      writer.println( "$var wire 1 ) underrun $end" );
      writeDeclaration( writer, "upscope" );
      writeDeclaration( writer, "enddefinitions" );
      writeOpenDeclaration( writer, "dumpvars" );
      writer.println( "bxxxxxxxx #\n" + "x$\n" + "0%\n" + "x&\n" + "x'\n" + "1(\n" + "0)\n" );
      writeCloseDeclaration( writer );
      writeTime( writer, 0 );
      writer.println( "b10000001 #\n" + "0$\n" + "1%\n" + "0&\n" + "1'\n" + "0(\n" + "0)\n" );
      writeTime( writer, 2211 );
      writer.println( "0'\n" + "#2296\n" + "b0 #\n" + "1$\n" + "#2302\n" + "0$\n" + "#2303\n" );
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
}
