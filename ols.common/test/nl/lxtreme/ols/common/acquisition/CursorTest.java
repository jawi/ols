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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.awt.*;

import nl.lxtreme.ols.common.acquisition.Cursor.LabelStyle;
import nl.lxtreme.ols.common.acquisition.CursorBuilder.CursorImpl;

import org.junit.*;


/**
 * Test cases for {@link Cursor}s.
 */
public class CursorTest
{
  // METHODS

  /**
   * Tests how {@link Cursor#getLabel(LabelStyle)} behaves with state data.
   */
  @Test
  public void testLabelStyleForStateDataOk() throws Exception
  {
    AcquisitionData data = createStateData();

    CursorImpl cursor = createCursor( 0, 2L, "label", Color.WHITE );

    assertEquals( "1", cursor.getLabel( LabelStyle.INDEX_ONLY, data ) );
    assertEquals( "label", cursor.getLabel( LabelStyle.LABEL_ONLY, data ) );
    assertEquals( "#2", cursor.getLabel( LabelStyle.TIME_ONLY, data ) );

    assertEquals( "1: label", cursor.getLabel( LabelStyle.INDEX_LABEL, data ) );
    assertEquals( "label: #2", cursor.getLabel( LabelStyle.LABEL_TIME, data ) );
  }

  /**
   * Tests how {@link Cursor#getLabel(LabelStyle)} behaves with timing data.
   */
  @Test
  public void testLabelStyleForTimingDataOk() throws Exception
  {
    AcquisitionData data = createTimingData( 1, 1L );

    CursorImpl cursor = createCursor( 0, 2L, "label", Color.WHITE );

    assertEquals( "1", cursor.getLabel( LabelStyle.INDEX_ONLY, data ) );
    assertEquals( "label", cursor.getLabel( LabelStyle.LABEL_ONLY, data ) );
    assertEqualsWithTimeUnit( "1.0 s", cursor.getLabel( LabelStyle.TIME_ONLY, data ) );

    assertEquals( "1: label", cursor.getLabel( LabelStyle.INDEX_LABEL, data ) );
    assertEqualsWithTimeUnit( "label: 1.0 s", cursor.getLabel( LabelStyle.LABEL_TIME, data ) );
  }

  private void assertEqualsWithTimeUnit( String aExpected, String aActual )
  {
    assertNotNull( aActual );

    String expected = aExpected.replaceAll( "\\W+", " " );
    String actual = aActual.replaceAll( "\\W+", " " );

    assertEquals( expected, actual );
  }

  private CursorImpl createCursor( int aIndex, long aTimestamp, String aLabel, Color aColor )
  {
    CursorBuilder cb = new CursorBuilder();
    cb.setIndex( aIndex );
    cb.setTimestamp( aTimestamp );
    cb.setLabel( aLabel );
    cb.setColor( aColor );
    return cb.build( null );
  }

  private AcquisitionData createStateData()
  {
    AcquisitionData result = mock( AcquisitionData.class );
    when( result.hasTimingData() ).thenReturn( Boolean.FALSE );
    return result;
  }

  private AcquisitionData createTimingData( int aSampleRate, long aTriggerPosition )
  {
    AcquisitionData result = mock( AcquisitionData.class );
    when( result.hasTimingData() ).thenReturn( Boolean.TRUE );
    when( result.getSampleRate() ).thenReturn( aSampleRate );
    when( result.hasTriggerData() ).thenReturn( aTriggerPosition >= 0L );
    when( result.getTriggerPosition() ).thenReturn( aTriggerPosition );
    return result;
  }
}
