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
package nl.lxtreme.ols.tool.uart;


import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * @author jajans
 */
public class UARTAnalyserWorker extends BaseAsyncToolWorker<UARTDataSet>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( UARTAnalyserWorker.class.getName() );

  // VARIABLES

  private int rxdMask;
  private int txdMask;
  private int ctsMask;
  private int rtsMask;
  private int dcdMask;
  private int riMask;
  private int dsrMask;
  private int dtrMask;
  private boolean inverted;
  private UARTStopBits stopBits;
  private UARTParity parity;
  private int bitCount;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public UARTAnalyserWorker( final DataContainer aData )
  {
    super( aData );

    this.rxdMask = 0;
    this.txdMask = 0;
    this.ctsMask = 0;
    this.rtsMask = 0;
    this.dcdMask = 0;
    this.riMask = 0;
    this.dsrMask = 0;
    this.dtrMask = 0;
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
   * @param aCtsMask
   *          the ctsMask to set
   */
  public void setCtsMask( final int aCtsMask )
  {
    this.ctsMask = aCtsMask;
  }

  /**
   * @param aDcdMask
   *          the dcdMask to set
   */
  public void setDcdMask( final int aDcdMask )
  {
    this.dcdMask = aDcdMask;
  }

  /**
   * @param aDsrMask
   *          the dsrMask to set
   */
  public void setDsrMask( final int aDsrMask )
  {
    this.dsrMask = aDsrMask;
  }

  /**
   * @param aDtrMask
   *          the dtrMask to set
   */
  public void setDtrMask( final int aDtrMask )
  {
    this.dtrMask = aDtrMask;
  }

  /**
   * @param aInverted
   */
  public void setInverted( final boolean aInverted )
  {
    this.inverted = aInverted;
  }

  /**
   * @param aParity
   */
  public void setParity( final UARTParity aParity )
  {
    this.parity = aParity;
  }

  /**
   * @param aRiMask
   *          the riMask to set
   */
  public void setRiMask( final int aRiMask )
  {
    this.riMask = aRiMask;
  }

  /**
   * @param aRtsMask
   *          the rtsMask to set
   */
  public void setRtsMask( final int aRtsMask )
  {
    this.rtsMask = aRtsMask;
  }

  /**
   * @param aRxdMask
   *          the rxdMask to set
   */
  public void setRxdMask( final int aRxdMask )
  {
    this.rxdMask = aRxdMask;
  }

  /**
   * @param aStopBits
   */
  public void setStopBits( final UARTStopBits aStopBits )
  {
    this.stopBits = aStopBits;
  }

  /**
   * @param aTxdMask
   *          the txdMask to set
   */
  public void setTxdMask( final int aTxdMask )
  {
    this.txdMask = aTxdMask;
  }

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected UARTDataSet doInBackground() throws Exception
  {
    // process the captured data and write to output
    int i, a;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "rxdmask = 0x" + Integer.toHexString( this.rxdMask ) );
      LOG.fine( "txdmask = 0x" + Integer.toHexString( this.txdMask ) );
      LOG.fine( "ctsmask = 0x" + Integer.toHexString( this.ctsMask ) );
      LOG.fine( "rtsmask = 0x" + Integer.toHexString( this.rtsMask ) );
      LOG.fine( "dcdmask = 0x" + Integer.toHexString( this.dcdMask ) );
      LOG.fine( "rimask  = 0x" + Integer.toHexString( this.riMask ) );
      LOG.fine( "dsrmask = 0x" + Integer.toHexString( this.dsrMask ) );
      LOG.fine( "dtrmask = 0x" + Integer.toHexString( this.dtrMask ) );
    }

    /*
     * Start decode from trigger or if no trigger is available from the first
     * falling edge. The decoder works with two independant decoder runs. First
     * for RxD and then for TxD, after this CTS, RTS, etc. is detected if
     * enabled. After decoding all the decoded data are unsortet before the data
     * is displayed it must be sortet by time.
     */

    final long[] timestamps = getTimestamps();
    final int[] values = getValues();

    long startOfDecode = 0; // XXX
    long endOfDecode;

    /*
     * set the start of decode to the trigger if avail or find first state
     * change on the selected lines
     */
    if ( isCursorsEnabled() )
    {
      startOfDecode = getCursorPosition( 1 );
      endOfDecode = getCursorPosition( 2 ) + 1;
    }
    else
    {
      if ( hasTriggerData() )
      {
        startOfDecode = getTriggerPosition();
        // the trigger may be too late, a workaround is to go back some samples
        // here
        startOfDecode -= 10;
        if ( startOfDecode < 0 )
        {
          startOfDecode = 0;
        }
      }
      else
      {
        int mask = this.rxdMask | this.riMask | this.ctsMask | this.txdMask | this.dcdMask | this.riMask | this.dsrMask
            | this.dtrMask;
        a = values[0] & mask;
        for ( i = 0; i < values.length; i++ )
        {
          if ( a != ( values[i] & mask ) )
          {
            startOfDecode = timestamps[i];
            break;
          }
        }
      }
      endOfDecode = getAbsoluteLength();
    }

    final UARTDataSet decodedData = new UARTDataSet( startOfDecode, endOfDecode, this );

    // decode RxD
    final int sampleRate = getSampleRate();
    if ( this.rxdMask != 0 )
    {
      final BaudRateAnalyzer baudrate = new BaudRateAnalyzer( values, timestamps, this.rxdMask );

      if ( LOG.isLoggable( Level.FINE ) )
      {
        LOG.fine( baudrate.toString() );
      }

      final int bitLength = baudrate.getBest();
      if ( bitLength == 0 )
      {
        LOG.info( "No (usable) RxD-data found for determining bitlength/baudrate ..." );
      }
      else
      {
        // We know the avg. bitlength, so we can use it for calculating the
        // baudrate...
        decodedData.setSampledBitLength( bitLength );

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.fine( "Samplerate: " + sampleRate + ", bitlength: " + bitLength + ", baudrate = "
              + decodedData.getBaudRate() );
        }

        decodeData( decodedData, bitLength, this.rxdMask, UARTData.UART_TYPE_RXDATA );
      }
    }

    // decode TxD
    if ( this.txdMask != 0 )
    {
      final BaudRateAnalyzer baudrate = new BaudRateAnalyzer( values, timestamps, this.txdMask );

      if ( LOG.isLoggable( Level.FINE ) )
      {
        LOG.fine( baudrate.toString() );
      }

      final int bitLength = baudrate.getBest();
      if ( bitLength == 0 )
      {
        LOG.info( "No (usable) TxD-data found for determining bitlength/baudrate ..." );
      }
      else
      {
        // We know the avg. bitlength, so we can use it for calculating the
        // baudrate...
        decodedData.setSampledBitLength( bitLength );

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.fine( "Samplerate: " + sampleRate + ", bitlength: " + bitLength + ", baudrate = "
              + decodedData.getBaudRate() );
        }

        decodeData( decodedData, bitLength, this.txdMask, UARTData.UART_TYPE_TXDATA );
      }
    }

    // decode control lines
    decodeControl( decodedData, this.ctsMask, "CTS" );
    decodeControl( decodedData, this.rtsMask, "RTS" );
    decodeControl( decodedData, this.dcdMask, "DCD" );
    decodeControl( decodedData, this.riMask, "RI" );
    decodeControl( decodedData, this.dsrMask, "DSR" );
    decodeControl( decodedData, this.dtrMask, "DTR" );

    // sort the results by time
    decodedData.sort();

    return decodedData;
  }

  /**
   * decode a control line
   * 
   * @param aDataSet
   *          TODO
   * @param aMask
   *          bitmask for the control line
   * @param aName
   *          name string of the control line
   */
  private void decodeControl( final UARTDataSet aDataSet, final int aMask, final String aName )
  {
    if ( aMask == 0 )
    {
      return;
    }

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "Decode " + aName );
    }

    long i;
    int a = getDataAt( 0 ) & aMask;

    setProgress( 0 );

    final double length = aDataSet.getEndOfDecode() - aDataSet.getStartOfDecode();

    for ( i = aDataSet.getStartOfDecode(); i < aDataSet.getEndOfDecode(); i++ )
    {
      if ( a < ( getDataAt( i ) & aMask ) )
      {
        // rising edge
        aDataSet.reportControlHigh( i, aName );
      }
      if ( a > ( getDataAt( i ) & aMask ) )
      {
        // falling edge
        aDataSet.reportControlLow( i, aName );
      }
      a = getDataAt( i ) & aMask;

      // update progress
      setProgress( ( int )( i * 100.0 / length ) );
    }

    setProgress( 100 );
  }

  /**
   * decode a UART data line
   * 
   * @param aDataSet
   *          TODO
   * @param aBaud
   *          baudrate (counted samples per bit)
   * @param aMask
   *          bitmask for the dataline
   * @param aType
   *          type of the data (rx or tx)
   */
  private int decodeData( final UARTDataSet aDataSet, final int aBaud, final int aMask, final int aType )
  {
    if ( aMask == 0 )
    {
      return ( 0 );
    }

    long a = 0;
    int b = 0;
    long c = 0;
    long i = 0;
    int value = 0;
    int stopCount = ( int )Math.ceil( this.stopBits.getValue() );
    int parityCount = this.parity.isNone() ? 0 : 1;
    int count = 0;

    final long startOfDecode = aDataSet.getStartOfDecode();
    final long endOfDecode = aDataSet.getEndOfDecode();
    final double length = endOfDecode - startOfDecode;

    a = Math.max( 0, startOfDecode );

    while ( ( endOfDecode - a ) > ( ( this.bitCount + stopCount + parityCount ) * aBaud ) )
    {
      /*
       * find first falling edge this is the start of the startbit. If the
       * inverted checkbox is set find the first rising edge.
       */
      b = getDataAt( a ) & aMask;
      for ( i = a; i < endOfDecode; i++ )
      {
        if ( isInverted() )
        {
          if ( b < ( getDataAt( i ) & aMask ) )
          {
            c = i;
            break;
          }
        }
        else
        {
          if ( b > ( getDataAt( i ) & aMask ) )
          {
            c = i;
            break;
          }
        }
        b = getDataAt( i ) & aMask;

        // update progress
        setProgress( ( int )( ( i - a ) * 100.0 / length ) );
      }

      if ( i >= endOfDecode )
      {
        LOG.fine( "End decode" );
        break;
      }

      /*
       * Sampling is done in the middle of each bit the start bit must be low If
       * the inverted checkbox is set the startbit must be high
       */
      a = c + aBaud / 2;
      if ( isInverted() )
      {
        if ( ( getDataAt( a ) & aMask ) == 0 )
        {
          // this is not a start bit !
          if ( aType == UARTData.UART_TYPE_RXDATA )
          {
            aDataSet.reportStartError( calculateTime( a ), UARTData.UART_TYPE_RXEVENT );
          }
          else
          {
            aDataSet.reportStartError( calculateTime( a ), UARTData.UART_TYPE_TXEVENT );
          }
        }
      }
      else
      {
        if ( ( getDataAt( a ) & aMask ) != 0 )
        {
          // this is not a start bit !
          if ( aType == UARTData.UART_TYPE_RXDATA )
          {
            aDataSet.reportStartError( calculateTime( a ), UARTData.UART_TYPE_RXEVENT );
          }
          else
          {
            aDataSet.reportStartError( calculateTime( a ), UARTData.UART_TYPE_TXEVENT );
          }
        }
      }

      /*
       * sample the databits in the middle of the bit position
       */

      value = 0;
      for ( i = 0; i < this.bitCount; i++ )
      {
        a += aBaud;
        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) == 0 )
          {
            value |= ( 1 << i );
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            value |= ( 1 << i );
          }
        }
      }
      aDataSet.reportData( a, value, aType );
      count++;

      /*
       * sample parity bit if available
       */
      if ( isOddParity() )
      {
        a += aBaud;
        if ( ( Integer.bitCount( value ) & 1 ) == 0 )
        {
          if ( isInverted() )
          {
            // odd parity, bitcount is even --> parity bit must be 0 (inverted)
            if ( ( getDataAt( a ) & aMask ) != 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
          else
          {
            // odd parity, bitcount is even --> parity bit must be 1
            if ( ( getDataAt( a ) & aMask ) == 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
        }
        else
        {
          if ( isInverted() )
          {
            // odd parity, bitcount is odd --> parity bit must be 1 (Inverted)
            if ( ( getDataAt( a ) & aMask ) == 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
          else
          {
            // odd parity, bitcount is odd --> parity bit must be 0
            if ( ( getDataAt( a ) & aMask ) != 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
        }
      }

      if ( isEvenParity() )
      {
        a += aBaud;
        if ( ( Integer.bitCount( value ) & 1 ) == 0 )
        {
          if ( isInverted() )
          {
            // even parity, bitcount is even --> parity bit must be 1 (inverted)
            if ( ( getDataAt( a ) & aMask ) == 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
          else
          {
            // even parity, bitcount is even --> parity bit must be 0
            if ( ( getDataAt( a ) & aMask ) != 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
        }
        else
        {
          if ( isInverted() )
          {
            // even parity, bitcount is odd --> parity bit must be 0 (inverted)
            if ( ( getDataAt( a ) & aMask ) != 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
          else
          {
            // even parity, bitcount is odd --> parity bit must be 1
            if ( ( getDataAt( a ) & aMask ) == 0 )
            {
              // parity error
              if ( aType == UARTData.UART_TYPE_RXDATA )
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_RXEVENT );
              }
              else
              {
                aDataSet.reportParityError( a, UARTData.UART_TYPE_TXEVENT );
              }
            }
          }
        }
      }

      /*
       * sample stopbit(s)
       */
      a += aBaud;
      if ( this.stopBits == UARTStopBits.STOP_1 )
      {
        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) == 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
      }
      else if ( this.stopBits == UARTStopBits.STOP_15 )
      {
        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) == 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }

        a += ( aBaud / 4 );

        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
      }
      else
      /* if ( this.stopBits == UARTStopBits.STOP_2 ) */
      {
        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }

        a += aBaud;

        if ( isInverted() )
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
        else
        {
          if ( ( getDataAt( a ) & aMask ) != 0 )
          {
            // framing error
            if ( aType == UARTData.UART_TYPE_RXDATA )
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_RXEVENT );
            }
            else
            {
              aDataSet.reportFrameError( a, UARTData.UART_TYPE_TXEVENT );
            }
          }
        }
      }
    }

    setProgress( 100 );

    return ( count );
  }

  /**
   * @return
   */
  private boolean isEvenParity()
  {
    return this.parity == UARTParity.EVEN;
  }

  /**
   * @return
   */
  private boolean isInverted()
  {
    return this.inverted;
  }

  /**
   * @return
   */
  private boolean isOddParity()
  {
    return this.parity == UARTParity.ODD;
  }
}
