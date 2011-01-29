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


import java.io.*;
import java.text.*;
import java.util.*;

import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class ValueChangeDumpHelper
{
  // CONSTRUCTORS

  /**
   * Creates a new ValueChangeDumpHelper instance. Never used.
   */
  private ValueChangeDumpHelper()
  {
    super();
  }

  // METHODS

  /**
   * Returns the timebase for the given sample rate.
   * <p>
   * The timebase can only be in values of 1, 10 or 100, with units ranging from
   * femto-seconds to seconds.
   * </p>
   * 
   * @param aSampleRate
   *          the sample rate to convert to a timebase.
   * @return a timebase.
   */
  public static final double getTimebase( final long aSampleRate )
  {
    double result = 1.0 / aSampleRate;

    final double[] unitVals = { 1.0, 1.0e-3, 1.0e-6, 1.0e-9, 1.0e-12, 1.0e-15 };

    int i = 0;
    for ( ; i < unitVals.length; i++ )
    {
      if ( result >= unitVals[i] )
      {
        break;
      }
    }
    i = Math.min( i, unitVals.length - 1 );

    result /= unitVals[i];

    return normalizeTimebase( result ) * unitVals[i];
  }

  /**
   * @param aWriter
   * @param aKeyword
   * @param aValues
   */
  public static final void writeCloseDeclaration( final PrintWriter aWriter )
  {
    aWriter.println( "$end" );
  }

  /**
   * @param aWriter
   */
  public static final void writeDate( final PrintWriter aWriter )
  {
    writeDeclaration( aWriter, "date", DateFormat.getDateTimeInstance().format( new Date() ) );
  }

  /**
   * @param aWriter
   * @param aKeyword
   * @param aValues
   */
  public static final void writeDeclaration( final PrintWriter aWriter, final String aKeyword, final String... aValues )
  {
    aWriter.printf( "$%s ", aKeyword );
    for ( String value : aValues )
    {
      aWriter.println();
      aWriter.printf( "  %s", value );
    }
    if ( ( aValues != null ) && ( aValues.length > 0 ) )
    {
      aWriter.println();
    }
    writeCloseDeclaration( aWriter );
  }

  /**
   * @param aWriter
   * @param aKeyword
   * @param aValues
   */
  public static final void writeOpenDeclaration( final PrintWriter aWriter, final String aKeyword )
  {
    aWriter.printf( "$%s", aKeyword ).println();
  }

  /**
   * @param aWriter
   * @param aTimebase
   */
  public static final void writeTime( final PrintWriter aWriter, final long aTimebase )
  {
    aWriter.printf( "#%d", Long.valueOf( aTimebase ) ).println();
  }

  /**
   * @param aWriter
   * @param aTimebase
   */
  public static final void writeTimescale( final PrintWriter aWriter, final double aTimebase )
  {
    writeDeclaration( aWriter, "timescale", getTimescale( aTimebase ) );
  }

  /**
   * @param aWriter
   * @param aIndex
   * @param aLabel
   */
  public static final void writeVariable( final PrintWriter aWriter, final int aIndex, final String aLabel )
  {
    aWriter.printf( "$var wire 1 %c %s $end", Integer.valueOf( '#' + aIndex ), aLabel ).println();
  }

  /**
   * @param aTimebase
   * @return
   */
  private static final String getTimescale( final double aTimebase )
  {
    return DisplayUtils.displayTime( aTimebase, 0, " " );
  }

  /**
   * @param aTime
   * @return
   */
  private static final int normalizeTimebase( final double aTime )
  {
    if ( aTime > 100 )
    {
      return 100;
    }
    else if ( aTime > 10 )
    {
      return 10;
    }
    return 1;
  }
}
