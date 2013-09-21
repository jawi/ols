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


import java.io.*;
import java.text.*;
import java.util.*;


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
   * @param aIndex
   * @return
   */
  public static final String getIdentifier( final int aIndex )
  {
    if ( aIndex < 0 )
    {
      throw new IllegalArgumentException( "Index cannot be negative!" );
    }
    if ( aIndex > 90 )
    {
      throw new IllegalArgumentException( "More than 90 identifiers are not supported!" );
    }
    int id = '!' + aIndex;
    return String.format( "%c", Integer.valueOf( id ) );
  }

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
    if ( aSampleRate < 0 )
    {
      throw new IllegalArgumentException( "Negative sample rates are not supported!" );
    }
    // The simple case: a sample rate of 0 yields a timebase of 0
    if ( aSampleRate == 0L )
    {
      return 0.0;
    }

    final double[] unitVals = { 1.0, 1.0e-3, 1.0e-6, 1.0e-9, 1.0e-12, 1.0e-15, 1.0e-18 };

    final double period = 1.0 / aSampleRate;

    int i = 0;
    for ( ; i < unitVals.length; i++ )
    {
      if ( period >= unitVals[i] )
      {
        break;
      }
    }
    i = Math.max( 0, Math.min( i, unitVals.length - 1 ) );

    return normalizeTimebase( 1.0 / ( aSampleRate * unitVals[i] ) ) * unitVals[i];
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
    if ( aValues.length > 0 )
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
    aWriter.printf( "$var wire 1 %s %s $end", getIdentifier( aIndex ), aLabel ).println();
  }

  /**
   * @param aTimebase
   * @return
   */
  protected static final String getTimescale( final double aTimebase )
  {
    if ( aTimebase < 0 )
    {
      throw new IllegalArgumentException( "Negative timebases are not supported!" );
    }
    // The simple case: a timebase of 0 yields a value of "0 s".
    if ( aTimebase == 0.0 )
    {
      return "0 s";
    }

    final String[] unitStrs = { "s", "ms", "us", "ns", "ps", "fs", "as" };
    final double[] unitVals = { 1.0, 1.0e-3, 1.0e-6, 1.0e-9, 1.0e-12, 1.0e-15, 1.0e-18 };

    double absTime = Math.abs( aTimebase );

    int i = 0;
    for ( ; i < unitVals.length; i++ )
    {
      if ( absTime >= unitVals[i] )
      {
        break;
      }
    }
    i = Math.min( i, unitVals.length - 1 );

    return String.format( "%.0f %s", Double.valueOf( aTimebase / unitVals[i] ), unitStrs[i] );
  }

  /**
   * @param aTime
   * @return
   */
  private static final int normalizeTimebase( final double aTime )
  {
    if ( Double.isInfinite( aTime ) || Double.isNaN( aTime ) )
    {
      return 0;
    }

    if ( aTime >= 100.0 )
    {
      return 100;
    }
    else if ( aTime >= 10.0 )
    {
      return 10;
    }
    return 1;
  }
}
