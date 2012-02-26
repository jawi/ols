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
package nl.lxtreme.ols.util;


/**
 * 
 */
public final class DisplayUtils
{
  // INNER TYPES

  /**
   * Represents a frequency that can be displayed and has a scale factor.
   */
  public static enum Frequencies
  {
    HZ( "Hz", 1.0 ), //
    KHZ( "kHz", 1.0e3 ), //
    MHZ( "MHz", 1.0e6 ), //
    GHZ( "GHz", 1.0e9 ), //
    THZ( "THz", 1.0e12 ); //

    private String displayName;
    private double factor;

    /**
     * Creates a new DisplayUtils.Frequencies instance.
     */
    private Frequencies( final String aDisplayName, final double aFactor )
    {
      this.displayName = aDisplayName;
      this.factor = aFactor;
    }

    /**
     * Returns the current value of displayName.
     * 
     * @return the displayName
     */
    public String getDisplayName()
    {
      return this.displayName;
    }

    /**
     * Returns the current value of factor.
     * 
     * @return the factor
     */
    public double getFactor()
    {
      return this.factor;
    }
  }

  /**
   * Represents a unit of time that can be displayed has a scale factor.
   */
  public static enum UnitOfTime
  {
    // CONSTANTS

    S( "", 1.0 ), //
    MS( "m", 1.0e-3 ), //
    US( "\u03BC", 1.0e-6 ), //
    NS( "n", 1.0e-9 ), //
    PS( "p", 1.0e-12 ), //
    FS( "f", 1.0e-15 ); //

    /**
     * Constant used to determine whether we should show "0.000 s".
     */
    public static final double ZERO_TIME_THRESHOLD = 1.0e-16;

    private String displayName;
    private double factor;

    /**
     * Creates a new DisplayUtils.UnitOfTime instance.
     */
    private UnitOfTime( final String aPrefix, final double aFactor )
    {
      this.displayName = aPrefix.concat( "s" );
      this.factor = aFactor;
    }

    /**
     * Converts a given time value (as double representation, in seconds) to a
     * more suitable unit of time.
     * 
     * @param aTimeValue
     *          the time value (in seconds) to return the unit of time for.
     * @return a {@link UnitOfTime}, never <code>null</code>.
     */
    public static UnitOfTime valueOf( final double aTimeValue )
    {
      double absTime = Math.abs( aTimeValue );
      final UnitOfTime[] values = values();

      int i = 0;
      if ( absTime > ZERO_TIME_THRESHOLD )
      {
        for ( ; i < values.length; i++ )
        {
          if ( absTime >= values[i].getFactor() )
          {
            break;
          }
        }
      }

      return values[Math.min( i, values.length - 1 )];
    }

    /**
     * Returns the current value of displayName.
     * 
     * @return the displayName
     */
    public String getDisplayName()
    {
      return this.displayName;
    }

    /**
     * Returns the current value of factor.
     * 
     * @return the factor
     */
    public double getFactor()
    {
      return this.factor;
    }

    /**
     * Returns the predecessor of this unit of time.
     * 
     * @return the predecessor of this unit of time, which is a factor
     *         <em>larger</em> than this unit of time. Can be <code>null</code>
     *         if no greater unit of time is defined.
     */
    public UnitOfTime predecessor()
    {
      int i = ordinal();
      final UnitOfTime[] values = values();
      if ( i < 1 )
      {
        return null;
      }
      return values[i - 1];
    }

    /**
     * Returns the successor of this unit of time.
     * 
     * @return the successor of this unit of time, which is a factor
     *         <em>smaller</em> than this unit of time. Can be <code>null</code>
     *         if no smaller unit of time is defined.
     */
    public UnitOfTime successor()
    {
      int i = ordinal();
      final UnitOfTime[] values = values();
      if ( i >= ( values.length - 1 ) )
      {
        return null;
      }
      return values[i + 1];
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new DisplayUtils instance (never used).
   */
  private DisplayUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Converts a given frequency (in Hertz, Hz) to something more readable for
   * the user, like "10.0 kHz".
   * 
   * @param aFrequency
   *          the frequency (in Hz) to convert to a display value.
   * @return the display representation of the given frequency, never
   *         <code>null</code>.
   */
  public static String displayFrequency( final double aFrequency )
  {
    final String[] unitStrs = { "Hz", "kHz", "MHz", "GHz", "THz" };
    final double[] unitVals = { 1.0, 1.0e3, 1.0e6, 1.0e9, 1.0e12 };

    int i = unitVals.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( aFrequency >= unitVals[i] )
      {
        break;
      }
    }
    i = Math.max( i, 0 );

    return String.format( "%.3f %s", Double.valueOf( aFrequency / unitVals[i] ), unitStrs[i] );
  }

  /**
   * Returns a percentage as string value.
   * 
   * @param aPercentage
   *          the percentage, >= 0.0 and <= 1.0.
   * @return a percentage display value, like "59.3%", neer <code>null</code>.
   */
  public static String displayPercentage( final double aPercentage )
  {
    return String.format( "%.1f%%", Double.valueOf( aPercentage * 100.0 ) );
  }

  /**
   * Converts a given time first by scaling it down with a given scale, and
   * returns it as something readable for the user.
   * 
   * @param aTime
   *          the time to convert to a given display value.
   * @param aScale
   *          the scale factor to apply before converting the given time to a
   *          display representation.
   * @return the display representation of the given (scaled) time, never
   *         <code>null</code>.
   */
  public static String displayScaledTime( final double aTime, final double aScale )
  {
    return displayTime( aTime / aScale );
  }

  /**
   * Converts a given size (in bytes) to something more readable for the user,
   * like "10K". The unit conversion is <em>always</em> done in binary (units of
   * 1024).
   * 
   * @param aSize
   *          the size (in bytes) to convert to a display value.
   * @return the display representation of the given size, never
   *         <code>null</code>.
   */
  public static String displaySize( final double aSize )
  {
    final String[] unitStrs = { "", "k", "M", "G", "T" };
    final double[] unitVals = { 1.0, 1024.0, 1048576.0, 1073741824.0, 1099511627776.0 };

    int i = unitVals.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( aSize >= unitVals[i] )
      {
        break;
      }
    }
    i = Math.max( i, 0 );

    return String.format( "%d%s", Integer.valueOf( ( int )( aSize / unitVals[i] ) ), unitStrs[i] );
  }

  /**
   * Converts a given time (in seconds) to something more readable for the user,
   * like "1.000 ms" (always a precision of three).
   * 
   * @param aTime
   *          the time (in seconds) to convert to a given display value.
   * @return the display representation of the given time, never
   *         <code>null</code>.
   */
  public static String displayTime( final double aTime )
  {
    return displayTime( aTime, 3, " " );
  }

  /**
   * Converts a given time (in seconds) to something more readable for the user,
   * like "1.000 ms".
   * 
   * @param aTime
   *          the time (in seconds) to convert to a given display value;
   * @param aPrecision
   *          the precision of the returned string (decimals after the
   *          decimal-separator), should be >= 0 && <= 6.
   * @return the display representation of the given time, never
   *         <code>null</code>.
   */
  public static String displayTime( final double aTime, final int aPrecision, final String aSeparator )
  {
    if ( ( aPrecision < 0 ) || ( aPrecision > 6 ) )
    {
      throw new IllegalArgumentException( "Precision cannot be less than zero or greater than six." );
    }
    if ( aSeparator == null )
    {
      throw new IllegalArgumentException( "Separator cannot be null!" );
    }

    final UnitOfTime unitOfTime = UnitOfTime.valueOf( aTime );

    final String format = String.format( "%%.%df%s%%s", Integer.valueOf( aPrecision ), aSeparator );
    return String.format( format, Double.valueOf( aTime / unitOfTime.getFactor() ), unitOfTime.getDisplayName() );
  }

  /**
   * Returns the ordinal representation (in English) of the given value.
   * 
   * @param aValue
   *          the value to get the ordinal value for, >= 0 && < 40.
   * @return a ordinal number representation of the given value, like "1st".
   */
  public static String getOrdinalNumber( final int aValue )
  {
    String suffix = "";
    if ( ( aValue == 1 ) || ( aValue == 21 ) || ( aValue == 31 ) )
    {
      suffix = "st";
    }
    else if ( ( aValue == 2 ) || ( aValue == 22 ) )
    {
      suffix = "nd";
    }
    else if ( ( aValue == 3 ) || ( aValue == 23 ) )
    {
      suffix = "rd";
    }
    else if ( ( aValue >= 0 ) && ( aValue < 40 ) )
    {
      suffix = "th";
    }
    return String.format( "%d%s", Integer.valueOf( aValue ), suffix );
  }
}

/* EOF */
