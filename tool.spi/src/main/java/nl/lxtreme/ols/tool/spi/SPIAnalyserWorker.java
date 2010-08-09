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


/**
 * @author jajans
 */
public class SPIAnalyserWorker extends BaseAsyncToolWorker<SPIDataSet>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SPIAnalyserWorker.class.getName() );

  // VARIABLES

  private int csMask;
  private int sckMask;
  private int misoMask;
  private int mosiMask;
  private SPIMode mode;
  private int bitCount;
  private Endianness order;

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
  public void setOrder( final Endianness aOrder )
  {
    this.order = aOrder;
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
    // process the captured data and write to output
    int a, c;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "csmask   = 0x" + Integer.toHexString( this.csMask ) );
      LOG.fine( "sckmask  = 0x" + Integer.toHexString( this.sckMask ) );
      LOG.fine( "misomask = 0x" + Integer.toHexString( this.misoMask ) );
      LOG.fine( "mosimask = 0x" + Integer.toHexString( this.mosiMask ) );
    }

    final int[] values = getValues();

    int startOfDecode = 0;
    int endOfDecode = values.length;
    // XXX tool context???
    // if ( isCursorsEnabled() )
    // {
    // startOfDecode = getSampleIndex( getCursorPosition( 1 ) );
    // endOfDecode = getSampleIndex( getCursorPosition( 2 ) + 1 );
    // }
    // else
    {
      /*
       * For analyze scan the CS line for a falling edge. If no edge could be
       * found, the position of the trigger is used for start of analysis. If no
       * trigger and no edge is found the analysis fails.
       */
      a = values[0] & this.csMask;
      c = 0;

      final double length = endOfDecode - startOfDecode;
      for ( int i = startOfDecode; i < endOfDecode; i++ )
      {
        if ( a > ( values[i] & this.csMask ) )
        {
          // cs to low found here
          startOfDecode = i;
          c = 1;

          if ( LOG.isLoggable( Level.FINE ) )
          {
            LOG.fine( "CS found at " + i );
          }

          break;
        }
        a = values[i] & this.csMask;

        setProgress( ( int )( i * 100.0 / length ) );
      }

      if ( c == 0 )
      {
        // no CS edge found, look for trigger
        if ( hasTriggerData() )
        {
          startOfDecode = getSampleIndex( getTriggerPosition() );
        }
      }
    }

    final SPIDataSet decodedData = new SPIDataSet( startOfDecode, endOfDecode );
    // now the trigger is in b, add trigger event to table
    decodedData.reportCSLow( startOfDecode );

    /*
     * Use the mode parameter to determine which edges are to detect. Mode 0 and
     * mode 3 are sampling on the rising clk edge, mode 1 and 2 are sampling on
     * the falling edge. a is used for start of value, c is register for detect
     * line changes.
     */
    if ( ( this.mode == SPIMode.MODE_0 ) || ( this.mode == SPIMode.MODE_3 ) )
    {
      clockDataOnRisingEdge( decodedData, startOfDecode, endOfDecode );
    }
    else
    {
      clockDataOnFallingEdge( decodedData, startOfDecode, endOfDecode );
    }

    return decodedData;
  }

  /**
   * @param aDecodedData
   * @param aStartOfDecode
   * @param aEndOfDecode
   */
  private void clockDataOnFallingEdge( final SPIDataSet aDecodedData, final int aStartOfDecode, final int aEndOfDecode )
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    int a;
    int c;
    int bitIdx;
    int mosivalue;
    int misovalue;
    int maxbits;

    // scanning for falling clk edges
    c = values[aStartOfDecode] & this.sckMask;
    a = values[aStartOfDecode] & this.csMask;
    bitIdx = this.bitCount;

    maxbits = bitIdx;
    misovalue = 0;
    mosivalue = 0;

    final double length = aEndOfDecode - aStartOfDecode;
    for ( int i = aStartOfDecode; i < aEndOfDecode; i++ )
    {
      if ( c > ( values[i] & this.sckMask ) )
      {
        // sample here
        if ( this.order == Endianness.MSB_FIRST )
        {
          if ( ( values[i] & this.misoMask ) == this.misoMask )
          {
            misovalue |= ( 1 << bitIdx );
          }
          if ( ( values[i] & this.mosiMask ) == this.mosiMask )
          {
            mosivalue |= ( 1 << bitIdx );
          }
        }
        else
        {
          if ( ( values[i] & this.misoMask ) == this.misoMask )
          {
            misovalue |= ( 1 << ( maxbits - bitIdx ) );
          }
          if ( ( values[i] & this.mosiMask ) == this.mosiMask )
          {
            mosivalue |= ( 1 << ( maxbits - bitIdx ) );
          }
        }

        if ( bitIdx > 0 )
        {
          bitIdx--;
        }
        else
        {
          aDecodedData.reportData( calculateTime( timestamps[i] ), mosivalue, misovalue );

          // System.out.println("MISO = 0x" + Integer.toHexString(misovalue));
          // System.out.println("MOSI = 0x" + Integer.toHexString(mosivalue));

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }
      c = values[i] & this.sckMask;

      /* CS edge detection */
      if ( a > ( values[i] & this.csMask ) )
      {
        // falling edge
        aDecodedData.reportCSLow( calculateTime( timestamps[i] ) );
      }
      else if ( a < ( values[i] & this.csMask ) )
      {
        // rising edge
        aDecodedData.reportCSHigh( calculateTime( timestamps[i] ) );
      }
      a = values[i] & this.csMask;

      setProgress( ( int )( i * 100.0 / length ) );
    }
  }

  /**
   * @param aDecodedData
   * @param aStartOfDecode
   * @param aEndOfDecode
   */
  private void clockDataOnRisingEdge( final SPIDataSet aDecodedData, final int aStartOfDecode, final int aEndOfDecode )
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    int a;
    int c;
    int bitIdx;
    int mosivalue;
    int misovalue;
    int maxbits;

    // scanning for rising clk edges
    c = values[aStartOfDecode] & this.sckMask;
    a = values[aStartOfDecode] & this.csMask;
    bitIdx = this.bitCount;
    maxbits = bitIdx;
    misovalue = 0;
    mosivalue = 0;

    final double length = aEndOfDecode - aStartOfDecode;
    for ( int i = aStartOfDecode; i < aEndOfDecode; i++ )
    {
      if ( c < ( values[i] & this.sckMask ) )
      {
        // sample here
        if ( this.order == Endianness.MSB_FIRST )
        {
          if ( ( values[i] & this.misoMask ) == this.misoMask )
          {
            misovalue |= ( 1 << bitIdx );
          }
          if ( ( values[i] & this.mosiMask ) == this.mosiMask )
          {
            mosivalue |= ( 1 << bitIdx );
          }
        }
        else
        {
          if ( ( values[i] & this.misoMask ) == this.misoMask )
          {
            misovalue |= ( 1 << ( maxbits - bitIdx ) );
          }
          if ( ( values[i] & this.mosiMask ) == this.mosiMask )
          {
            mosivalue |= ( 1 << ( maxbits - bitIdx ) );
          }
        }

        if ( bitIdx > 0 )
        {
          bitIdx--;
        }
        else
        {
          aDecodedData.reportData( calculateTime( timestamps[i] ), mosivalue, misovalue );

          // System.out.println("MISO = 0x" + Integer.toHexString(misovalue));
          // System.out.println("MOSI = 0x" + Integer.toHexString(mosivalue));

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }
      c = values[i] & this.sckMask;

      /* CS edge detection */
      if ( a > ( values[i] & this.csMask ) )
      {
        // falling edge
        aDecodedData.reportCSLow( calculateTime( timestamps[i] ) );
      }
      else if ( a < ( values[i] & this.csMask ) )
      {
        // rising edge
        aDecodedData.reportCSHigh( calculateTime( timestamps[i] ) );
      }
      a = values[i] & this.csMask;

      setProgress( ( int )( i * 100.0 / length ) );
    }
  }

}
