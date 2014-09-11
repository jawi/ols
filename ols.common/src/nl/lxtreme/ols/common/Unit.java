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
package nl.lxtreme.ols.common;


import static java.util.FormattableFlags.*;

import java.awt.*;
import java.util.*;

import javax.swing.*;

import org.slf4j.*;


/**
 * Provides a common base class for all unit-related utility classes.
 */
public interface Unit
{
  // INNER TYPES

  /**
   * Represents a frequency unit that has a displayable representation and a
   * scale factor to convert it from/to Hertz.
   */
  public static enum Frequency implements Unit
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
     * Converts a given period to a {@link FrequencyUnit} instance.
     * 
     * @param aFrequency
     *          the frequency to convert to a {@link FrequencyUnit}.
     * @return a {@link FrequencyUnit} instance, never <code>null</code>.
     */
    public static Frequency toUnit( double aFrequency )
    {
      final Frequency[] freqs = values();
      final double frequency = Math.abs( aFrequency );

      if ( frequency < ZERO_THRESHOLD )
      {
        // Normalize zero to the base unit...
        return HZ;
      }

      int i = freqs.length - 1;
      for ( ; i >= 0; i-- )
      {
        if ( frequency >= freqs[i].getScale() )
        {
          break;
        }
      }

      return freqs[Math.max( i, 0 )];
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
    public double getScale()
    {
      return this.factor;
    }
  }

  /**
   * Represents a size unit (SI-standard) that has a displayable representation
   * and has a scale factor.
   */
  public static enum SizeSI implements Unit
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
    public static final double ZERO_THRESHOLD = 1.0e-2;
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

      if ( size < ZERO_THRESHOLD )
      {
        // Normalize zero to the base unit...
        return B;
      }

      int i = sizes.length - 1;
      for ( ; i >= 0; i-- )
      {
        if ( size >= sizes[i].getScale() )
        {
          break;
        }
      }

      return sizes[Math.max( i, 0 )];
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
    public double getScale()
    {
      return this.factor;
    }
  }

  /**
   * Represents a unit of time, in seconds, that can be displayed has a scale
   * factor.
   */
  public static enum Time implements Unit
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
     * Converts a given time value (as double representation, in seconds) to a
     * more suitable unit of time.
     * 
     * @param aTimeValue
     *          the time value (in seconds) to return the unit of time for.
     * @return a {@link Time}, never <code>null</code>.
     */
    public static Time toUnit( double aTimeValue )
    {
      final Time[] values = values();
      double absTime = Math.abs( aTimeValue );

      if ( absTime < ZERO_THRESHOLD )
      {
        // Normalize zero to the base unit...
        return S;
      }

      int i = 0;
      for ( ; i < values.length; i++ )
      {
        if ( absTime >= values[i].getScale() )
        {
          break;
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
    public double getScale()
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

  /**
   * Represents a value in a certain {@link Unit}.
   */
  public static class Value implements Formattable
  {
    // CONSTANTS

    /** Plain space. */
    private static final char SPACE = ' ';
    /** Non-breaking space (wide). */
    private static final char NB = '\u00a0';
    /** Thin space. */
    private static final char THIN = '\u2009';
    /** Hair space. */
    private static final char HAIR = '\u200a';

    // VARIABLES

    private final Unit unit;
    private final double value;

    // CONSTRUCTORS

    /**
     * Creates a new {@link Value} instance.
     */
    public Value( double aValue, Unit aUnit )
    {
      if ( aUnit == null )
      {
        throw new IllegalArgumentException( "Unit cannot be null!" );
      }
      this.value = aValue;
      this.unit = aUnit;
    }

    // METHODS

    /**
     * Returns a given value as a {@link Value} and determines the correct
     * {@link Frequency} unit.
     * 
     * @param aValue
     *          the value to convert to a {@link Frequency} unit, in Hertz.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asFrequency( Number aValue )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      Frequency actualUnit = Frequency.toUnit( value );
      return new Value( value / actualUnit.getScale(), actualUnit );
    }

    /**
     * Returns a given value as a {@link Value} with a given {@link Frequency}
     * unit.
     * 
     * @param aValue
     *          the value to convert to a {@link Frequency} unit.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asFrequency( Number aValue, Frequency aUnit )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      return new Value( value, aUnit );
    }

    /**
     * Returns a given value as a {@link Value} and detects the correct
     * {@link SizeSI} unit.
     * 
     * @param aValue
     *          the value to convert to a {@link SizeSI} unit.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asSizeSI( Number aValue )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      SizeSI actualUnit = SizeSI.toUnit( value );
      return new Value( value / actualUnit.getScale(), actualUnit );
    }

    /**
     * Returns a given value as a {@link Value} with a given {@link SizeSI}
     * unit.
     * 
     * @param aValue
     *          the value to convert to a {@link SizeSI} unit.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asSizeSI( Number aValue, SizeSI aUnit )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      return new Value( value, aUnit );
    }

    /**
     * Returns a given value as a {@link Value} and detects the correct
     * {@link Time} unit.
     * 
     * @param aValue
     *          the value to convert to a {@link Time} unit.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asTime( Number aValue )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      Time actualUnit = Time.toUnit( value );
      return new Value( value / actualUnit.getScale(), actualUnit );
    }

    /**
     * Returns a given value as a {@link Value} with a given {@link Time} unit.
     * 
     * @param aValue
     *          the value to convert to a {@link Time} unit.
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public static Value asTime( Number aValue, Time aUnit )
    {
      if ( aValue == null )
      {
        throw new IllegalArgumentException( "Value cannot be null!" );
      }
      double value = aValue.doubleValue();
      return new Value( value, aUnit );
    }

    /**
     * Calculate the "optimal" separator to use for separating a value and a
     * unit.
     */
    public static Character getSeparator()
    {
      Logger logger = LoggerFactory.getLogger( Unit.class );

      char sep;
      if ( GraphicsEnvironment.isHeadless() )
      {
        // Cannot determine the optimal result, use a sensible default...
        logger.debug( "Using SPACE (headless fallback)..." );

        sep = SPACE;
      }
      else
      {
        Font font = UIManager.getFont( "Label.font" );

        if ( font.canDisplay( HAIR ) )
        {
          logger.debug( "Using HAIR..." );

          sep = HAIR;
        }
        else if ( font.canDisplay( THIN ) )
        {
          logger.debug( "Using THIN..." );

          sep = THIN;
        }
        else if ( font.canDisplay( NB ) )
        {
          logger.debug( "Using NB..." );

          sep = NB;
        }
        else
        {
          logger.debug( "Using SPACE (fallback)..." );

          sep = SPACE;
        }
      }

      return Character.valueOf( sep );
    }

    /**
     * Converts this value to another value with the given unit.
     * 
     * @param aUnit
     * @return a new {@link Value} instance, never <code>null</code>.
     */
    public Value convert( Unit aUnit )
    {
      if ( aUnit == null )
      {
        throw new IllegalArgumentException( "Unit cannot be null!" );
      }
      if ( aUnit.getClass() != this.unit.getClass() )
      {
        throw new IllegalArgumentException( "Unit must be equal to " + this.unit );
      }
      if ( aUnit == this.unit )
      {
        return this;
      }

      double result = this.value;

      int start = this.unit.ordinal();
      int end = aUnit.ordinal();

      if ( start < end )
      {
        // e.g. MB -> B
        result /= Math.pow( 10, 3 * ( end - start ) );
      }
      else if ( start > end )
      {
        // e.g. MB -> TB
        result *= Math.pow( 10, 3 * ( start - end ) );
      }

      return new Value( result, aUnit );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( Object obj )
    {
      if ( this == obj )
      {
        return true;
      }
      if ( obj == null )
      {
        return false;
      }
      if ( getClass() != obj.getClass() )
      {
        return false;
      }
      Value other = ( Value )obj;
      if ( this.unit == null )
      {
        if ( other.unit != null )
        {
          return false;
        }
      }
      else if ( !this.unit.equals( other.unit ) )
      {
        return false;
      }
      if ( Double.doubleToLongBits( this.value ) != Double.doubleToLongBits( other.value ) )
      {
        return false;
      }
      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void formatTo( Formatter aFormatter, int aFlags, int aWidth, int aPrecision )
    {
      StringBuilder sb = new StringBuilder();
      sb.append( "%1$" );

      if ( aWidth < 0 && aPrecision < 0 )
      {
        sb.append( 's' );
      }
      else
      {
        if ( ( aFlags & LEFT_JUSTIFY ) != 0 )
        {
          sb.append( '-' );
        }
        if ( aWidth >= 0 )
        {
          sb.append( aWidth );
        }
        if ( aPrecision >= 0 )
        {
          sb.append( '.' ).append( aPrecision );
        }
        sb.append( 'f' );
      }

      // Separator...
      if ( ( aFlags & ALTERNATE ) != 0 )
      {
        sb.append( getSeparator() );
      }
      else
      {
        sb.append( ' ' );
      }
      sb.append( "%2$s" );

      aFormatter.format( sb.toString(), this.value, this.unit.getDisplayName() );
    }

    /**
     * Returns the current value of unit.
     * 
     * @return the unit
     */
    public Unit getUnit()
    {
      return this.unit;
    }

    /**
     * Returns the current value of value.
     * 
     * @return the value
     */
    public double getValue()
    {
      return this.value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ( ( this.unit == null ) ? 0 : this.unit.hashCode() );
      long temp;
      temp = Double.doubleToLongBits( this.value );
      result = prime * result + ( int )( temp ^ ( temp >>> 32 ) );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return String.format( "%s", this );
    }
  }

  // METHODS

  /**
   * @return a display name for this unit, like "kHz" or "ms".
   */
  String getDisplayName();

  /**
   * @return a scale factor one needs to multiple a value with to get to the
   *         base unit.
   */
  double getScale();

  /**
   * @return the ordinal index of this unit, >= 0.
   */
  int ordinal();
}
