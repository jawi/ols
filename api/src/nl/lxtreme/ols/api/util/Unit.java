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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.api.util;


import java.awt.*;
import java.util.logging.*;

import javax.swing.*;


/**
 * Provides a common base class for all unit-related utility classes.
 */
public final class Unit
{
  // INNER TYPES

  /**
   * Represents a frequency unit that has a displayable representation and a
   * scale factor to convert it from/to Hertz.
   */
  public static enum Frequency
  {
    // CONSTANTS

    /** millihertz. */
    MiHZ( "m", 1.0e-3 ),
    /** hertz. */
    HZ( "", 1.0 ),
    /** kilohertz. */
    KHZ( "k", 1.0e3 ),
    /** megahertz. */
    MHZ( "M", 1.0e6 ),
    /** gigahertz. */
    GHZ( "G", 1.0e9 ),
    /** terahertz. */
    THZ( "T", 1.0e12 );

    /** Constant used to determine whether we should show "0Hz". */
    public static final double ZERO_THRESHOLD = 1.0e-5;
    /** All units are in Hertz. */
    private static final String BASE_UNIT = "Hz";

    // VARIABLES

    private final String displayName;
    private final double factor;

    // CONSTRUCTORS

    /**
     * Creates a new {@link Frequency} instance.
     */
    private Frequency( final String aPrefix, final double aFactor )
    {
      this.displayName = aPrefix.concat( BASE_UNIT );
      this.factor = aFactor;
    }

    // METHODS

    /**
     * Convenience method to directly get a displayable represention of a given
     * frequency.
     * <p>
     * This method does the same as calling:
     * <code>toUnit( aFrequency ).format( aFrequency, 3 );</code>.
     * </p>
     * 
     * @param aFrequency
     *          the frequency to get a displayable representation for.
     * @return a string representation of the given frequency, never
     *         <code>null</code>.
     */
    public static String format( final double aFrequency )
    {
      return toUnit( aFrequency ).format( aFrequency, 3 );
    }

    /**
     * Converts a given period to a {@link FrequencyUnit} instance.
     * 
     * @param aFrequency
     *          the frequency to convert to a {@link FrequencyUnit}.
     * @return a {@link FrequencyUnit} instance, never <code>null</code>.
     */
    public static Frequency toUnit( final double aFrequency )
    {
      final Frequency[] freqs = values();
      final double frequency = Math.abs( aFrequency );

      int i = freqs.length - 1;
      for ( ; i >= 0; i-- )
      {
        if ( frequency >= freqs[i].getFactor() )
        {
          break;
        }
      }

      return freqs[Math.max( i, 0 )];
    }

    /**
     * Returns the given frequency as string representation using this frequency
     * unit's display name.
     * 
     * @param aFrequency
     *          the frequency to convert to a string representation;
     * @param aScale
     *          the scale (= number of digits after decimal separator) to use in
     *          the string representation.
     * @return a string representation of the given frequency, like "1.234kHz",
     *         never <code>null</code>.
     */
    public String format( final double aFrequency, final int aScale )
    {
      // For *very* small frequencies, we simply always yield zero...
      if ( Math.abs( aFrequency ) < ZERO_THRESHOLD )
      {
        return Unit.format( 0.0, aScale, HZ.getDisplayName() );
      }

      return Unit.format( aFrequency / getFactor(), aScale, getDisplayName() );
    }

    /**
     * Returns display name of this frequency unit, like "Hz" or "MHz".
     * 
     * @return a display name, never <code>null</code>.
     */
    public String getDisplayName()
    {
      return this.displayName;
    }

    /**
     * Returns the scale factor to get from Hertz to this frequency unit.
     * 
     * @return a scale factor, >= 1.0.
     */
    public double getFactor()
    {
      return this.factor;
    }
  }

  /**
   * Represents a size unit (SI-standard) that has a displayable representation
   * and has a scale factor.
   */
  public static enum SizeSI
  {
    // CONSTANTS

    /** bytes. */
    B( "", 1.0 ),
    /** kilobytes. */
    KB( "k", 1024.0 ),
    /** megabytes. */
    MB( "M", 1048576.0 ),
    /** gigabytes. */
    GB( "G", 1073741824.0 ),
    /** terabytes. */
    TB( "T", 1099511627776.0 );

    /** Constant used to determine whether we should show "0B". */
    public static final double ZERO_THRESHOLD = 1.0e-1;
    /** All units are in bytes. */
    private static final String BASE_UNIT = "B";

    // VARIABLES

    private final String displayName;
    private final double factor;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SizeSI} instance.
     */
    private SizeSI( final String aPrefix, final double aFactor )
    {
      this.displayName = aPrefix.concat( BASE_UNIT );
      this.factor = aFactor;
    }

    // METHODS

    /**
     * Convenience method to directly get a displayable represention of a given
     * size.
     * <p>
     * This method does the same as calling:
     * <code>toUnit( aSize ).format( aSize, 2 );</code>.
     * </p>
     * 
     * @param aSize
     *          the size to get a displayable representation for.
     * @return a string representation of the given size, never
     *         <code>null</code>.
     */
    public static String format( final double aSize )
    {
      return toUnit( aSize ).format( aSize, 2 );
    }

    /**
     * Returns a {@link SizeSI} instance usable for representing the given size.
     * 
     * @param aSize
     *          the size to convert to a {@link SizeSI} instance.
     * @return a {@link SizeSI} instance, never <code>null</code>.
     */
    public static SizeSI toUnit( final double aSize )
    {
      SizeSI[] sizes = values();
      double size = Math.abs( aSize );

      int i = sizes.length - 1;
      for ( ; i >= 0; i-- )
      {
        if ( size >= sizes[i].getFactor() )
        {
          break;
        }
      }

      return sizes[Math.max( i, 0 )];
    }

    /**
     * Returns the given size as string representation using this size unit's
     * display name.
     * 
     * @param aSize
     *          the size to convert to a string representation;
     * @param aScale
     *          the scale (= number of digits after decimal separator) to use in
     *          the string representation.
     * @return a string representation of the given size, like "1.44MB", never
     *         <code>null</code>.
     */
    public String format( final double aSize, final int aScale )
    {
      // For *very* small sizes, we simply always yield zero...
      if ( Math.abs( aSize ) < ZERO_THRESHOLD )
      {
        return Unit.format( 0.0, aScale, B.getDisplayName() );
      }

      return Unit.format( aSize / getFactor(), aScale, getDisplayName() );
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
   * Represents a unit of time, in seconds, that can be displayed has a scale
   * factor.
   */
  public static enum Time
  {
    // CONSTANTS

    /** seconds. */
    S( "", 1.0 ),
    /** milliseconds. */
    MS( "m", 1.0e-3 ),
    /** microseconds. */
    US( "\u03BC", 1.0e-6 ),
    /** nanoseconds. */
    NS( "n", 1.0e-9 ),
    /** picoseconds. */
    PS( "p", 1.0e-12 ),
    /** femtoseconds. */
    FS( "f", 1.0e-15 );

    /** Constant used to determine whether we should show "0.000 s". */
    public static final double ZERO_THRESHOLD = 1.0e-16;
    /** All units are in seconds. */
    private static final String BASE_UNIT = "s";

    // VARIABLES

    private final String displayName;
    private final double factor;

    // CONSTRUCTORS

    /**
     * Creates a new DisplayUtils.UnitOfTime instance.
     */
    private Time( final String aPrefix, final double aFactor )
    {
      this.displayName = aPrefix.concat( BASE_UNIT );
      this.factor = aFactor;
    }

    // METHODS

    /**
     * Convenience method to directly get a displayable represention of a given
     * time value.
     * <p>
     * This method does the same as calling:
     * <code>toUnit( aTime ).format( aTime, 3 );</code>.
     * </p>
     * 
     * @param aTime
     *          the time value (in seconds) to get a displayable representation
     *          for.
     * @return a string representation of the given time, never
     *         <code>null</code>.
     */
    public static String format( final double aTime )
    {
      return toUnit( aTime ).format( aTime, 2 );
    }

    /**
     * Converts a given time value (as double representation, in seconds) to a
     * more suitable unit of time.
     * 
     * @param aTimeValue
     *          the time value (in seconds) to return the unit of time for.
     * @return a {@link Time}, never <code>null</code>.
     */
    public static Time toUnit( final double aTimeValue )
    {
      double absTime = Math.abs( aTimeValue );
      final Time[] values = values();

      int i = 0;
      for ( ; i < values.length; i++ )
      {
        if ( absTime >= values[i].getFactor() )
        {
          break;
        }
      }

      return values[Math.min( i, values.length - 1 )];
    }

    /**
     * Returns the given time as string representation using this time unit's
     * display name and a fixed number of digits after the decimal separator.
     * 
     * @param aTime
     *          the time (in seconds) to convert to a string representation;
     * @param aScale
     *          the scale (= number of digits after decimal separator) to use in
     *          the string representation.
     * @return a string representation of the given time, like "1.453ms", never
     *         <code>null</code>.
     */
    public String format( final double aTime, final int aScale )
    {
      // For *very* small sizes, we simply always yield zero...
      if ( Math.abs( aTime ) < ZERO_THRESHOLD )
      {
        return Unit.format( 0.0, aScale, S.getDisplayName() );
      }

      return Unit.format( aTime / getFactor(), aScale, getDisplayName() );
    }

    /**
     * Returns the given time as string representation using this time unit's
     * display name and the least number of digits after the decimal separator.
     * 
     * @param aTime
     *          the time (in seconds) to convert to a string representation;
     * @param aScale
     *          the scale (= number of digits after decimal separator) to use in
     *          the string representation.
     * @return a string representation of the given time, like "1.453ms", never
     *         <code>null</code>.
     */
    public String formatHumanReadable( final double aTime )
    {
      // For *very* small sizes, we simply always yield zero...
      if ( Math.abs( aTime ) < ZERO_THRESHOLD )
      {
        return Unit.format( 0.0, 1, S.getDisplayName() );
      }

      final Double time = Double.valueOf( aTime / getFactor() );
      String formattedTime = String.format( "%.9f", time );
      formattedTime = formattedTime.replaceAll( "(\\d)0+$", "$1" );

      return Unit.format( formattedTime, getDisplayName() );
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
    public Time predecessor()
    {
      final int i = ordinal();
      return ( i < 1 ) ? null : values()[i - 1];
    }

    /**
     * Returns the successor of this unit of time.
     * 
     * @return the successor of this unit of time, which is a factor
     *         <em>smaller</em> than this unit of time. Can be <code>null</code>
     *         if no smaller unit of time is defined.
     */
    public Time successor()
    {
      final Time[] values = values();
      int i = ordinal();
      if ( i >= ( values.length - 1 ) )
      {
        return null;
      }
      return values[i + 1];
    }
  }

  // CONSTANTS

  /** Plain space. */
  private static final char SPACE = ' ';
  /** Non-breaking space (wide). */
  private static final char NB = '\u00a0';
  /** Thin space. */
  private static final char THIN = '\u2009';
  /** Hair space. */
  private static final char HAIR = '\u200a';

  /** The separator to use between a value and a unit. */
  private static final Character SEPARATOR;

  /**
   * Calculate the "optimal" separator to use for separating a value and a unit.
   */
  static
  {
    Logger logger = Logger.getLogger( Unit.class.getName() );

    char sep;
    if ( GraphicsEnvironment.isHeadless() )
    {
      // Cannot determine the optimal result, use a sensible default...
      logger.fine( "Using SPACE (headless fallback)..." );

      sep = SPACE;
    }
    else
    {
      Font font = UIManager.getFont( "Label.font" );

      if ( font.canDisplay( HAIR ) )
      {
        logger.fine( "Using HAIR..." );

        sep = HAIR;
      }
      else if ( font.canDisplay( THIN ) )
      {
        logger.fine( "Using THIN..." );

        sep = THIN;
      }
      else if ( font.canDisplay( NB ) )
      {
        logger.fine( "Using NB..." );

        sep = NB;
      }
      else
      {
        logger.fine( "Using SPACE (fallback)..." );

        sep = SPACE;
      }
    }
    
    SEPARATOR = Character.valueOf( sep );
  }

  // METHODS

  /**
   * Formats a given value with a given scale and adding a given unit.
   */
  static String format( double aValue, int aScale, String aUnit )
  {
    if ( Double.isNaN( aValue ) || Double.isInfinite( aValue ) )
    {
      return "-";
    }

    String formatSpec = String.format( "%%.%df%c%%s", Integer.valueOf( aScale ), SEPARATOR );
    return String.format( formatSpec, Double.valueOf( aValue ), aUnit );
  }

  /**
   * Formats a given value with a given value and adding a given unit.
   */
  static String format( String aValue, String aUnit )
  {
    return String.format( "%s%c%s", aValue, SEPARATOR, aUnit );
  }
}
