/**
 * 
 */
package nl.lxtreme.ols.util;


import java.util.regex.*;


/**
 * Provides some utility methods for parsing numbers, etc.
 */
public final class NumberUtils
{
  // ENUMERATIONS

  /**
   * Denotes how "k", "M" units should be interpreted.
   */
  public enum UnitDefinition
  {
    SI, BINARY;
  }

  // CONSTANTS

  private static final Pattern SMART_INT_PATTERN = Pattern.compile( "^([-+]?\\d+)(?:\\s*([kKM]))?.*$" );

  // CONSTRUCTORS

  /**
   * Creates a new NumberUtils instance, never used.
   */
  private NumberUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer.
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>.
   * @return the integer value part of the given text, or 0 if the text couldn't
   *         be parsed.
   */
  public static int smartParseInt( final String aText )
  {
    return smartParseInt( aText, UnitDefinition.BINARY );
  }

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer.
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>.
   * @return the integer value part of the given text, or 0 if the text couldn't
   *         be parsed.
   */
  public static int smartParseInt( final String aText, final UnitDefinition aUnitDefinition )
  {
    final Matcher matcher = SMART_INT_PATTERN.matcher( aText );
    if ( matcher.matches() )
    {
      final String number = matcher.group( 1 );
      final String unit = matcher.group( 2 );

      int result = Integer.valueOf( number );
      if ( unit != null )
      {
        result *= parseUnit( unit, aUnitDefinition );
      }
      return result;
    }
    return 0;
  }

  /**
   * @param aUnit
   * @param aUnitDefinition
   * @return
   */
  private static long parseUnit( final String aUnit, final UnitDefinition aUnitDefinition )
  {
    if ( "k".equalsIgnoreCase( aUnit ) )
    {
      return UnitDefinition.SI == aUnitDefinition ? 1000L : 1024L;
    }
    else if ( "m".equalsIgnoreCase( aUnit ) )
    {
      return UnitDefinition.SI == aUnitDefinition ? 1000000L : 1048576L;
    }
    else if ( "g".equalsIgnoreCase( aUnit ) )
    {
      return UnitDefinition.SI == aUnitDefinition ? 1000000000L : 1073741824L;
    }
    return 1L;
  }
}
