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
    RISING, FALLING;
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SPIAnalyserWorker.class.getName() );

  // VARIABLES

  private int csMask;
  private int sckMask;
  private int misoMask;
  private int mosiMask;
  private SPIMode mode;
  private int bitCount;
  private BitOrder bitOrder;
  private boolean reportCS;
  private boolean honourCS;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public SPIAnalyserWorker( final AnnotatedData aData )
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
  public void setMisoMask( final int aMisoMask )
  {
    this.misoMask = aMisoMask;
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
  public void setMosiMask( final int aMosiMask )
  {
    this.mosiMask = aMosiMask;
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
      LOG.fine( "misomask = 0x" + Integer.toHexString( this.misoMask ) );
      LOG.fine( "mosimask = 0x" + Integer.toHexString( this.mosiMask ) );
    }

    final int[] values = getValues();

    int startOfDecode = -1;
    int endOfDecode = values.length;
    boolean csFound = false;

    if ( hasTriggerData() )
    {
      startOfDecode = getSampleIndex( getTriggerPosition() );
    }

    /**
     * <pre>
     * At CPOL=0 the base value of the clock is zero:
     * * For CPHA=0, data are captured on the clock's rising edge (low->high
     *   transition) and data are propagated on a falling edge (high->low clock
     *   transition);
     * * For CPHA=1, data are captured on the clock's falling edge and data are
     *   propagated on a rising edge.
     * At CPOL=1 the base value of the clock is one (inversion of CPOL=0):
     * * For CPHA=0, data are captured on clock's falling edge and data are 
     *   propagated on a rising edge;
     * * For CPHA=1, data are captured on clock's rising edge and data are 
     *   propagated on a falling edge.
     * </pre>
     */

    if ( isCursorsEnabled() )
    {
      startOfDecode = getSampleIndex( getCursorPosition( 1 ) );
      endOfDecode = getSampleIndex( getCursorPosition( 2 ) + 1 );
    }
    else
    {
      /*
       * For analyze scan the CS line for a falling edge. If no edge could be
       * found, the position of the trigger is used for start of analysis. If no
       * trigger and no edge is found the analysis fails.
       */
      int oldCsValue = values[0] & this.csMask;

      for ( int i = 0; i < values.length; i++ )
      {
        final int csValue = values[i] & this.csMask;
        if ( oldCsValue > csValue )
        {
          // found first falling edge; start decoding from here...
          startOfDecode = i;
          csFound = true;

          if ( LOG.isLoggable( Level.FINE ) )
          {
            LOG.fine( "CS found at " + i );
          }

          break;
        }
        oldCsValue = csValue;
      }
    }

    if ( !csFound || ( startOfDecode < 0 ) )
    {
      // no CS edge found, look for trigger
      LOG.log( Level.WARNING, "No CS start-condition found! Analysis aborted..." );
      return null;
    }

    final SPIDataSet decodedData = new SPIDataSet( startOfDecode, endOfDecode, this );
    if ( csFound )
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
    clockDataOnEdge( decodedData, getSampleEdge() );

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
  private void clockDataOnEdge( final SPIDataSet aDecodedData, final Edge aEdge )
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    final int startOfDecode = ( int )aDecodedData.getStartOfDecode();
    final int endOfDecode = ( int )aDecodedData.getEndOfDecode();

    final int misoChannelIdx = ( int )( Math.log( this.misoMask ) / Math.log( 2 ) );
    final int mosiChannelIdx = ( int )( Math.log( this.mosiMask ) / Math.log( 2 ) );

    // scanning for falling/rising clk edges
    int oldSckValue = values[startOfDecode] & this.sckMask;
    int oldCsValue = values[startOfDecode] & this.csMask;
    int bitIdx = this.bitCount;

    int misovalue = 0;
    int mosivalue = 0;

    // We've already found the
    boolean slaveSelected = true;
    int lastIdx = startOfDecode;

    final double length = endOfDecode - startOfDecode;
    for ( int idx = startOfDecode; idx < endOfDecode; idx++ )
    {
      final long time = calculateTime( timestamps[idx] );

      /* CLK edge detection */
      final int sckValue = values[idx] & this.sckMask;
      /* CS edge detection */
      final int csValue = values[idx] & this.csMask;

      if ( oldCsValue > csValue )
      {
        // falling edge
        reportCsLow( aDecodedData, time );
        slaveSelected = true;
      }
      else if ( oldCsValue < csValue )
      {
        // rising edge
        reportCsHigh( aDecodedData, time );
        slaveSelected = false;
      }
      oldCsValue = csValue;

      if ( this.honourCS && !slaveSelected )
      {
        // We should honour the slave-select, but the slave isn't
        // currently selected...
        continue;
      }

      final boolean edgeSeen;
      if ( aEdge == Edge.RISING )
      {
        edgeSeen = ( oldSckValue < sckValue );
      }
      else
      {
        edgeSeen = ( oldSckValue > sckValue );
      }
      oldSckValue = sckValue;

      if ( edgeSeen )
      {
        if ( bitIdx == this.bitCount )
        {
          lastIdx = idx - 1;
        }

        // sample MiSo here; always MSB first, perform conversion later on...
        if ( ( values[idx] & this.misoMask ) == this.misoMask )
        {
          misovalue |= ( 1 << bitIdx );
        }
        // sample MoSi here; always MSB first, perform conversion later on...
        if ( ( values[idx] & this.mosiMask ) == this.mosiMask )
        {
          mosivalue |= ( 1 << bitIdx );
        }

        if ( bitIdx > 0 )
        {
          bitIdx--;
        }
        else
        {
          // Perform bit-order conversion on the full byte...
          mosivalue = NumberUtils.convertByteOrder( mosivalue, this.bitOrder );
          misovalue = NumberUtils.convertByteOrder( misovalue, this.bitOrder );

          aDecodedData.reportData( time, mosivalue, misovalue );

          addChannelAnnotation( mosiChannelIdx, timestamps[lastIdx], timestamps[idx], String.format( "MOSI: 0x%X (%c)",
              mosivalue, mosivalue ) );
          addChannelAnnotation( misoChannelIdx, timestamps[lastIdx], timestamps[idx], String.format( "MISO: 0x%X (%c)",
              misovalue, misovalue ) );

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }

      setProgress( ( int )( ( idx - startOfDecode ) * 100.0 / length ) );
    }
  }

  /**
   * Use the mode parameter to determine which edges are to detect. Mode 0 and
   * mode 3 are sampling on the rising clk edge, mode 1 and 2 are sampling on
   * the falling edge. a is used for start of value, c is register for detect
   * line changes.
   * 
   * @return the sample clock edge.
   */
  private Edge getSampleEdge()
  {
    return ( ( this.mode == SPIMode.MODE_0 ) || ( this.mode == SPIMode.MODE_3 ) ) ? Edge.RISING : Edge.FALLING;
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
}
