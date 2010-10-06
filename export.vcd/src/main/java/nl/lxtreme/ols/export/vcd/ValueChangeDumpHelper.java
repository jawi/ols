/**
 * 
 */
package nl.lxtreme.ols.export.vcd;


import java.io.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class ValueChangeDumpHelper
{
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
   * @param aTimebase
   * @return
   */
  public static final String getTimescale( final double aTimebase )
  {
    return DisplayUtils.displayTime( aTimebase, 0, " " );
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
    aWriter.printf( "#%d", aTimebase ).println();
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
