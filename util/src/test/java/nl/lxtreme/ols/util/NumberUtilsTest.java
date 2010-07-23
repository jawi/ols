/**
 * 
 */
package nl.lxtreme.ols.util;


import static org.junit.Assert.*;
import nl.lxtreme.ols.util.NumberUtils.*;

import org.junit.*;


/**
 * @author jawi
 *
 */
public class NumberUtilsTest
{
  // METHODS

  /**
   * 
   */
  @Test
  public void testSmartParseIntBinaryUnitOk()
  {
    assertEquals( 4096, NumberUtils.smartParseInt( "4k" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4K" ) );
    assertEquals( 4096 * 1024, NumberUtils.smartParseInt( "4M" ) );
    assertEquals( 4096 * 1024, NumberUtils.smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntFail()
  {
    assertEquals( 0, NumberUtils.smartParseInt( "test" ) );
    assertEquals( 0, NumberUtils.smartParseInt( "" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntOk()
  {
    assertEquals( -1, NumberUtils.smartParseInt( "-1" ) );
    assertEquals( 1, NumberUtils.smartParseInt( "1" ) );
    assertEquals( 2, NumberUtils.smartParseInt( "2 " ) );
    assertEquals( 3, NumberUtils.smartParseInt( "3 4" ) );
    assertEquals( 4, NumberUtils.smartParseInt( "4,5" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4k" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4K" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4M" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntSIUnitOk()
  {
    assertEquals( 4000, NumberUtils.smartParseInt( "4k", UnitDefinition.SI ) );
    assertEquals( 4000, NumberUtils.smartParseInt( "4K", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  MB", UnitDefinition.SI ) );
  }
}
