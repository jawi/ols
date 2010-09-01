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
package nl.lxtreme.ols.tool.spi;


import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.NumberUtils.BitOrder;


/**
 * @author jajans
 */
public class SPIAnalyserWorker extends BaseAsyncToolWorker<SPIDataSet>
{
  // INNER TYPES

  /**
   * Denotes either a rising, or falling edge.
   */
  public enum Edge
  {
    NONE, RISING, FALLING;

    /**
     * Returns the inverse of this edge, so for rising it returns falling, and
     * so on.
     * 
     * @return the inverse of this edge.
     */
    public Edge invert()
    {
      if ( this == RISING )
      {
        return FALLING;
      }
      if ( this == FALLING )
      {
        return RISING;
      }
      return NONE;
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SPIAnalyserWorker.class.getName() );

  // VARIABLES

  private int csMask;
  private int sckMask;
  private SPIMode mode;
  private int bitCount;
  private BitOrder bitOrder;
  private boolean reportCS;
  private boolean honourCS;
  private int mosiIdx;
  private int misoIdx;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public SPIAnalyserWorker( final DataContainer aData )
  {
    super( aData );
  }

  // METHODS

  /**
   * @param aBitCount
   */
  public void setBitCount( final int aBitCount )
  {
    this.bitCount = aBitCount;
  }

  /**
   * @param aCsMask
   */
  public void setCSMask( final int aCsMask )
  {
    this.csMask = aCsMask;
  }

  public void setHonourCS( final boolean aHonourCS )
  {
    this.honourCS = aHonourCS;
  }

  /**
   * @param aMisoMask
   */
  public void setMisoIndex( final int aMisoIndex )
  {
    this.misoIdx = aMisoIndex;
  }

  /**
   * @param aMode
   */
  public void setMode( final SPIMode aMode )
  {
    this.mode = aMode;
  }

  /**
   * @param aMosiMask
   */
  public void setMosiIndex( final int aMosiIndex )
  {
    this.mosiIdx = aMosiIndex;
  }

  /**
   * @param aOrder
   */
  public void setOrder( final BitOrder aOrder )
  {
    this.bitOrder = aOrder;
  }

  /**
   * @param aReportCS
   */
  public void setReportCS( final boolean aReportCS )
  {
    this.reportCS = aReportCS;

  }

  /**
   * @param aSckMask
   */
  public void setSCKMask( final int aSckMask )
  {
    this.sckMask = aSckMask;
  }

  /**
   * This is the SPI protocol decoder core The decoder scans for a decode start
   * event like CS high to low edge or the trigger of the captured data. After
   * this the decoder starts to decode the data by the selected mode, number of
   * bits and bit order. The decoded data are put to a JTable object directly.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected SPIDataSet doInBackground() throws Exception
  {
    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "csmask   = 0x" + Integer.toHexString( this.csMask ) );
      LOG.fine( "sckmask  = 0x" + Integer.toHexString( this.sckMask ) );
      LOG.fine( "misomask = 0x" + Integer.toHexString( 1 << this.misoIdx ) );
      LOG.fine( "mosimask = 0x" + Integer.toHexString( 1 << this.mosiIdx ) );
    }

    final int[] values = getValues();

    int startOfDecode;
    int endOfDecode;
    boolean slaveSelected = false;

    if ( isCursorsEnabled() )
    {
      endOfDecode = getSampleIndex( getCursorPosition( 1 ) + 1 );
      startOfDecode = getSampleIndex( getCursorPosition( 0 ) );

      // Search for a CS-low backwards from the first cursor...
      slaveSelected = searchSlaveSelected( startOfDecode, 0 ) >= 0;
    }
    else if ( hasTriggerData() )
    {
      endOfDecode = values.length;
      startOfDecode = getSampleIndex( getTriggerPosition() );

      // Search for a CS-low backwards from the trigger position...
      slaveSelected = searchSlaveSelected( startOfDecode, 0 ) >= 0;
    }
    else
    {
      endOfDecode = values.length;
      // Search for a CS-low forwards until the end...
      startOfDecode = searchSlaveSelected( 0, endOfDecode );

      slaveSelected = ( startOfDecode > 0 );
    }

    if ( !slaveSelected )
    {
      // no CS edge found, look for trigger
      LOG.log( Level.WARNING, "No CS start-condition found! Analysis aborted..." );
      return null;
    }

    final SPIDataSet decodedData = new SPIDataSet( startOfDecode, endOfDecode, this );
    if ( slaveSelected )
    {
      // now the trigger is in b, add trigger event to table
      reportCsLow( decodedData, startOfDecode );
    }

    /*
     * Use the mode parameter to determine which edges are to detect. Mode 0 and
     * mode 3 are sampling on the rising clk edge, mode 1 and 2 are sampling on
     * the falling edge. a is used for start of value, c is register for detect
     * line changes.
     */
    clockDataOnEdge( decodedData, this.mode );

    return decodedData;
  }

  /**
   * Decodes the SPI-data on a given clock edge.
   * 
   * @param aDecodedData
   *          the decoded data to fill;
   * @param aEdge
   *          the edge on which to sample.
   */
  private void clockDataOnEdge( final SPIDataSet aDecodedData, final SPIMode aMode )
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    final int startOfDecode = ( int )aDecodedData.getStartOfDecode();
    final int endOfDecode = ( int )aDecodedData.getEndOfDecode();

    final Edge sampleEdge = getSampleEdge( aMode );
    final Edge dataChangeEdge = getDataChangeEdge( aMode );

    final int misoMask = 1 << this.misoIdx;
    final int mosiMask = 1 << this.mosiIdx;

    // clear any existing annotations
    clearChannelAnnotations( this.misoIdx );
    clearChannelAnnotations( this.mosiIdx );

    // scanning for falling/rising clk edges
    int oldSckValue = ( values[startOfDecode] & this.sckMask );
    int oldCsValue = ( values[startOfDecode] & this.csMask );

    // We've already found the
    boolean slaveSelected = true;
    boolean dataEdgeSeen = false;
    int lastIdx = startOfDecode;
    int bitIdx = this.bitCount;

    int misovalue = 0;
    int mosivalue = 0;

    final double length = endOfDecode - startOfDecode;
    for ( int idx = startOfDecode + 1; idx < endOfDecode; idx++ )
    {
      final long time = calculateTime( timestamps[idx] );

      /* CLK edge detection */
      final int sckValue = values[idx] & this.sckMask;
      /* CS edge detection */
      final int csValue = values[idx] & this.csMask;

      final Edge slaveSelectEdge = determineEdge( oldCsValue, csValue );
      oldCsValue = csValue;

      if ( slaveSelectEdge == Edge.FALLING )
      {
        reportCsLow( aDecodedData, time );

        slaveSelected = true;
        dataEdgeSeen = false;
      }
      else if ( slaveSelectEdge == Edge.RISING )
      {
        reportCsHigh( aDecodedData, time );

        slaveSelected = false;
      }

      if ( this.honourCS && !slaveSelected )
      {
        // We should honour the slave-select, but the slave isn't
        // currently selected...
        continue;
      }

      final Edge clockEdge = determineEdge( oldSckValue, sckValue );
      oldSckValue = sckValue;

      // In case the clock is phase-shifted with respect to the data line,
      // we should wait until the first inverse edge is seen. To put it
      // otherwise, we should simply ignore the first seen clock edge and
      // wait for the second one...
      if ( dataChangeEdge == clockEdge )
      {
        dataEdgeSeen = true;
      }
      // Keep track where we saw the first clocked bit of a byte-value...
      if ( dataEdgeSeen )
      {
        if ( bitIdx == this.bitCount )
        {
          lastIdx = idx - 1;
        }
      }

      final boolean sampleEdgeSeen = dataEdgeSeen && ( sampleEdge == clockEdge );
      if ( sampleEdgeSeen )
      {
        // sample MiSo here; always MSB first, perform conversion later on...
        if ( ( values[idx] & misoMask ) == misoMask )
        {
          misovalue |= ( 1 << bitIdx );
        }
        // sample MoSi here; always MSB first, perform conversion later on...
        if ( ( values[idx] & mosiMask ) == mosiMask )
        {
          mosivalue |= ( 1 << bitIdx );
        }

        if ( bitIdx > 0 )
        {
          bitIdx--;
        }
        else if ( bitIdx == 0 )
        {
          // Perform bit-order conversion on the full byte...
          mosivalue = NumberUtils.convertByteOrder( mosivalue, this.bitOrder );
          misovalue = NumberUtils.convertByteOrder( misovalue, this.bitOrder );

          aDecodedData.reportData( time, misovalue, mosivalue );

          addChannelAnnotation( this.mosiIdx, timestamps[lastIdx], timestamps[idx],
              String.format( "MOSI: 0x%X (%c)", mosivalue, mosivalue ) );
          addChannelAnnotation( this.misoIdx, timestamps[lastIdx], timestamps[idx],
              String.format( "MISO: 0x%X (%c)", misovalue, misovalue ) );

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }

      setProgress( ( int )( ( idx - startOfDecode ) * 100.0 / length ) );
    }
  }

  /**
   * Determines the clock value edge.
   * 
   * @param aOldClockValue
   *          the previous clock value;
   * @param aNewClockValue
   *          the next clock value.
   * @return the edge between the given clock values, cannot be
   *         <code>null</code>.
   */
  private Edge determineEdge( final int aOldClockValue, final int aNewClockValue )
  {
    if ( aOldClockValue < aNewClockValue )
    {
      return Edge.RISING;
    }
    else if ( aOldClockValue > aNewClockValue )
    {
      return Edge.FALLING;
    }
    return Edge.NONE;
  }

  /**
   * Returns the data change edge, on which the MISO/MOSI lines are allowed to
   * change.
   * 
   * @param aMode
   *          the SPI mode to return the data change edge for, cannot be
   *          <code>null</code>.
   * @return the data change edge.
   */
  private Edge getDataChangeEdge( final SPIMode aMode )
  {
    return getSampleEdge( aMode ).invert();
  }

  /**
   * Returns the data sample edge, on which the MISO/MOSI lines are to be
   * sampled.
   * 
   * @param aMode
   *          the SPI mode to return the sample edge for, cannot be
   *          <code>null</code>.
   * @return the sample clock edge.
   */
  private Edge getSampleEdge( final SPIMode aMode )
  {
    return ( ( aMode == SPIMode.MODE_0 ) || ( aMode == SPIMode.MODE_3 ) ) ? Edge.RISING : Edge.FALLING;
  }

  /**
   * @param aDecodedData
   * @param aTimestamp
   */
  private void reportCsHigh( final SPIDataSet aDecodedData, final long aTimestamp )
  {
    if ( this.reportCS )
    {
      aDecodedData.reportCSHigh( aTimestamp );
    }
  }

  /**
   * @param aDecodedData
   * @param aTimestamp
   */
  private void reportCsLow( final SPIDataSet aDecodedData, final long aTimestamp )
  {
    if ( this.reportCS )
    {
      aDecodedData.reportCSLow( aTimestamp );
    }
  }

  /**
   * @param aSampleIndex
   * @param aI
   * @return
   */
  private int searchSlaveSelected( final int aStartIndex, final int aEndIndex )
  {
    final int[] values = getValues();

    /*
     * For analyze scan the CS line for a falling edge. If no edge could be
     * found, the position of the trigger is used for start of analysis. If no
     * trigger and no edge is found the analysis fails.
     */
    if ( aStartIndex > aEndIndex )
    {
      // Walk backwards...
      int oldCsValue = values[aStartIndex] & this.csMask;
      for ( int i = aStartIndex; i >= aEndIndex; i-- )
      {
        final int csValue = values[i] & this.csMask;
        if ( oldCsValue < csValue )
        {
          // found first falling edge; start decoding from here...
          if ( LOG.isLoggable( Level.FINE ) )
          {
            LOG.fine( "CS found at " + i );
          }

          return i;
        }
        oldCsValue = csValue;
      }
    }
    else
    {
      // Walk forwards...
      int oldCsValue = values[aStartIndex] & this.csMask;
      for ( int i = aStartIndex; i < aEndIndex; i++ )
      {
        final int csValue = values[i] & this.csMask;
        if ( oldCsValue > csValue )
        {
          // found first falling edge; start decoding from here...
          if ( LOG.isLoggable( Level.FINE ) )
          {
            LOG.fine( "CS found at " + i );
          }

          return i;
        }
        oldCsValue = csValue;
      }
    }

    return -1;
  }
}
