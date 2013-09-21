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
package nl.lxtreme.ols.tool.i2c;


import static nl.lxtreme.ols.util.NumberUtils.*;

import java.beans.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;


/**
 * Performs the actual I2C analysis.
 */
public class I2CAnalyserTask implements ToolTask<I2CDataSet>
{
  // CONSTANTS

  public static final String LINE_A = "LineA";
  public static final String LINE_B = "LineB";

  public static final String PROPERTY_AUTO_DETECT_SCL = "AutoDetectSCL";
  public static final String PROPERTY_AUTO_DETECT_SDA = "AutoDetectSDA";

  private static final String CHANNEL_SCL_NAME = "SCL";
  private static final String CHANNEL_SDA_NAME = "SDA";

  private static final int I2C_BITCOUNT = 8;

  private static final Logger LOG = Logger.getLogger( I2CAnalyserTask.class.getName() );

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;
  private final PropertyChangeSupport pcs;

  private boolean detectSDA_SCL;
  private boolean reportACK;
  private boolean reportNACK;
  private boolean reportStart;
  private boolean reportStop;
  private int lineAmask;
  private int lineAidx;
  private int lineBmask;
  private int lineBidx;
  private int sdaIdx;
  private int sclIdx;

  // CONSTRUCTORS

  /**
   * Creates a new I2CAnalyserTask instance.
   * 
   * @param aContext
   * @param aProgressListener
   */
  public I2CAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annotationListener = aAnnotationListener;

    this.pcs = new PropertyChangeSupport( this );

    // the default behaviour is remained as-is...
    this.detectSDA_SCL = true;
  }

  // METHODS

  /**
   * Adds the given property change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.pcs.addPropertyChangeListener( aListener );
  }

  /**
   * This is the I2C protocol decoder core The decoder scans for a decode start
   * event when one of the two lines is going low (start condition). After this
   * the decoder starts to decode the data.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public I2CDataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    // process the captured data and write to output
    int oldSCL, oldSDA, bitCount;
    int byteValue;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "Line A mask = 0x{0}", Integer.toHexString( this.lineAmask ) );
      LOG.log( Level.FINE, "Line B mask = 0x{0}", Integer.toHexString( this.lineBmask ) );
    }

    int startOfDecode = this.context.getStartSampleIndex();
    int endOfDecode = this.context.getEndSampleIndex();

    if ( this.detectSDA_SCL )
    {
      startOfDecode = autodetectDataAndClock( data, startOfDecode, endOfDecode );
    }
    else
    {
      //
      this.sclIdx = this.lineAidx;
      this.sdaIdx = this.lineBidx;
    }

    final int sdaMask = ( 1 << this.sdaIdx );
    final int sclMask = ( 1 << this.sclIdx );

    final I2CDataSet i2cDataSet = new I2CDataSet( startOfDecode, endOfDecode, data );

    // Prepare everything for the decoding results...
    prepareResults();

    /*
     * Now decode the bytes, SDA may only change when SCL is low. Otherwise it
     * may be a repeated start condition or stop condition. If the start/stop
     * condition is not at a byte boundary a bus error is detected. So we have
     * to scan for SCL rises and for SDA changes during SCL is high. Each byte
     * is followed by a 9th bit (ACK/NACK).
     */
    int idx = i2cDataSet.getStartOfDecode();
    int prevIdx = -1;

    oldSCL = values[idx] & sclMask;
    oldSDA = values[idx] & sdaMask;

    bitCount = I2C_BITCOUNT;
    byteValue = 0;

    boolean startCondFound = false;
    boolean tenBitAddress = false;
    int slaveAddress = 0x00;
    int direction = -1;

    if ( this.detectSDA_SCL )
    {
      // We've just found our start condition, start the report with that...
      reportStartCondition( i2cDataSet, startOfDecode );

      this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[startOfDecode],
          I2CDataSet.I2C_START ) );

      startCondFound = true;
    }

    for ( ; idx < i2cDataSet.getEndOfDecode(); idx++ )
    {
      final int dataValue = values[idx];

      final int sda = ( dataValue & sdaMask );
      final int scl = ( dataValue & sclMask );

      // detect SCL fall/rise
      if ( oldSCL > scl )
      {
        // SCL falls
        if ( ( prevIdx < 0 ) || ( bitCount == I2C_BITCOUNT ) )
        {
          prevIdx = idx;
        }

        if ( bitCount == 0 )
        {
          // store decoded byte
          reportData( i2cDataSet, prevIdx, idx, byteValue );

          final String annotation;
          if ( startCondFound )
          {
            // This is the (7- or 10-bit) address part...
            direction = ( byteValue & 0x01 );

            if ( ( byteValue & 0xf8 ) == 0xf0 )
            {
              // 10-bit address part...
              slaveAddress = ( byteValue & 0x06 ) << 6;
              tenBitAddress = true;

              annotation = String.format( "Setup %s 10-bit slave", ( direction == 1 ) ? "read from" : "write to" );
            }
            else
            {
              if ( tenBitAddress )
              {
                // 10-bit address needs the first few bits of the previous slave
                // address, so OR-ing is needed here...
                slaveAddress |= ( byteValue & 0xFF );
              }
              else
              {
                // 7-bit address, directly available. See issue #51
                slaveAddress = ( ( byteValue >> 1 ) & 0xFF );
              }
              startCondFound = false;

              annotation = String.format( tenBitAddress ? "Setup %s slave: 0x%X " : "Setup %s slave: 0x%X [0x%X]",
                  ( direction == 1 ) ? "read from" : "write to", Integer.valueOf( slaveAddress ),
                  Integer.valueOf( byteValue ) );

              tenBitAddress = false;
            }
          }
          else
          {
            annotation = String.format( "%s data: 0x%X (%c)", ( direction == 1 ) ? "Read" : "Write",
                Integer.valueOf( byteValue ), Integer.valueOf( byteValue ) );
          }

          this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[prevIdx],
              timestamps[idx], annotation ) );

          byteValue = 0;
        }
      }
      else if ( scl > oldSCL )
      {
        // SCL rises
        if ( sda != oldSDA )
        {
          reportBusError( i2cDataSet, idx );
        }
        else
        {
          // read SDA
          if ( bitCount != 0 )
          {
            bitCount--;
            if ( sda != 0 )
            {
              byteValue |= ( 1 << bitCount );
            }
          }
          else
          {
            // read the confirmation of the slave...
            if ( sda != 0 )
            {
              // NACK
              reportNACK( i2cDataSet, idx );

              this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[idx],
                  I2CDataSet.I2C_NACK ) );
            }
            else
            {
              // ACK
              reportACK( i2cDataSet, idx );

              this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[idx],
                  I2CDataSet.I2C_ACK ) );
            }

            // next byte
            bitCount = I2C_BITCOUNT;
            byteValue = 0;
          }
        }
      }

      // detect SDA change when SCL high
      if ( ( scl == sclMask ) && ( sda != oldSDA ) )
      {
        // SDA changes here
        if ( ( bitCount > 0 ) && ( bitCount < ( I2C_BITCOUNT - 1 ) ) )
        {
          // bus error, no complete byte detected
          reportBusError( i2cDataSet, idx );
        }
        else
        {
          if ( sda > oldSDA )
          {
            // SDA rises, this is a stop condition
            reportStopCondition( i2cDataSet, idx );

            this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[idx],
                I2CDataSet.I2C_STOP ) );

            slaveAddress = 0x00;
            direction = -1;
          }
          else
          {
            // SDA falls, this is a start condition
            reportStartCondition( i2cDataSet, idx );

            this.annotationListener.onAnnotation( new SampleDataAnnotation( this.sdaIdx, timestamps[idx],
                I2CDataSet.I2C_START ) );

            startCondFound = true;
          }

          // new byte
          bitCount = I2C_BITCOUNT;
          byteValue = 0;
        }
      }

      oldSCL = scl;
      oldSDA = sda;

      this.progressListener
          .setProgress( getPercentage( idx, i2cDataSet.getStartOfDecode(), i2cDataSet.getEndOfDecode() ) );
    }

    return i2cDataSet;
  }

  /**
   * Removes the given property change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.pcs.removePropertyChangeListener( aListener );
  }

  /**
   * @param aDetectSDA_SCL
   *          the detectSDA_SCL to set
   */
  public void setDetectSDA_SCL( final boolean aDetectSDA_SCL )
  {
    this.detectSDA_SCL = aDetectSDA_SCL;
  }

  /**
   * @param aLineAmask
   */
  public void setLineAIndex( final int aLineAidx )
  {
    this.lineAidx = aLineAidx;
    this.lineAmask = 1 << aLineAidx;
  }

  /**
   * @param aLineBmask
   */
  public void setLineBIndex( final int aLineBidx )
  {
    this.lineBidx = aLineBidx;
    this.lineBmask = 1 << aLineBidx;
  }

  /**
   * @param aReportACK
   */
  public void setReportACK( final boolean aReportACK )
  {
    this.reportACK = aReportACK;
  }

  /**
   * @param aReportNACK
   */
  public void setReportNACK( final boolean aReportNACK )
  {
    this.reportNACK = aReportNACK;
  }

  /**
   * @param aReportStart
   */
  public void setReportStart( final boolean aReportStart )
  {
    this.reportStart = aReportStart;
  }

  /**
   * @param aReportStop
   */
  public void setReportStop( final boolean aReportStop )
  {
    this.reportStop = aReportStop;
  }

  /**
   * @return the sclIdx
   */
  final int getSclIdx()
  {
    return this.sclIdx;
  }

  /**
   * @return the sdaIdx
   */
  final int getSdaIdx()
  {
    return this.sdaIdx;
  }

  /**
   * Tries to auto detect the SDA & SCL lines between the given boundries in the
   * data.
   * 
   * @param aStartOfDecode
   *          the starting sample index;
   * @param aEndOfDecode
   *          the ending sample index.
   * @return the (new) starting sample index at which the START condition
   *         occurred.
   */
  private int autodetectDataAndClock( final AcquisitionResult aData, final int aStartOfDecode, final int aEndOfDecode )
  {
    final int dataMask = this.lineAmask | this.lineBmask;

    final int[] values = aData.getValues();

    int sampleIdx;
    /*
     * first of all scan both lines until they are high (IDLE), then the first
     * line that goes low is the SDA line (START condition).
     */
    for ( sampleIdx = aStartOfDecode; sampleIdx < aEndOfDecode; sampleIdx++ )
    {
      final int dataValue = values[sampleIdx];

      if ( ( dataValue & dataMask ) == dataMask )
      {
        // IDLE found here
        break;
      }

      this.progressListener.setProgress( getPercentage( sampleIdx, aStartOfDecode, aEndOfDecode ) );
    }

    if ( sampleIdx == aEndOfDecode )
    {
      // no idle state could be found
      LOG.log( Level.WARNING, "No IDLE state found in data; aborting analysis..." );
      throw new IllegalStateException( "No IDLE state found!" );
    }

    // a is now the start of idle, now find the first start condition
    for ( ; sampleIdx < aEndOfDecode; sampleIdx++ )
    {
      final int sample = values[sampleIdx];
      final int dataValue = sample & dataMask;

      if ( ( dataValue != dataMask ) && ( dataValue != 0 ) )
      {
        final int lineAvalue = sample & this.lineAmask;
        final int lineBvalue = sample & this.lineBmask;

        // is one line low?
        if ( ( lineAvalue == 0 ) && ( lineBvalue != 0 ) )
        {
          // lineA is low and lineB is high here: lineA = SDA, lineB = SCL
          this.sdaIdx = this.lineAidx;
          this.sclIdx = this.lineBidx;

          break;
        }
        else if ( ( lineAvalue != 0 ) && ( lineBvalue == 0 ) )
        {
          // lineB is low and lineA is high here: lineA = SCL, lineB = SDA
          this.sdaIdx = this.lineBidx;
          this.sclIdx = this.lineAidx;

          break;
        }
      }

      this.progressListener.setProgress( getPercentage( sampleIdx, aStartOfDecode, aEndOfDecode ) );
    }

    if ( sampleIdx == aEndOfDecode )
    {
      // no start condition could be found
      LOG.log( Level.WARNING, "No START condition found! Analysis aborted..." );
      throw new IllegalStateException( "No START condition found!" );
    }

    return sampleIdx;
  }

  /**
   * Prepares everything for the upcoming results.
   */
  private void prepareResults()
  {
    // Tell our listeners what line A & B mean...
    this.pcs.firePropertyChange( PROPERTY_AUTO_DETECT_SCL, null, this.sclIdx == this.lineAidx ? LINE_A : LINE_B );
    this.pcs.firePropertyChange( PROPERTY_AUTO_DETECT_SDA, null, this.sdaIdx == this.lineBidx ? LINE_B : LINE_A );

    // Update the channel labels...
    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.sclIdx, CHANNEL_SCL_NAME ) );
    this.annotationListener.clearAnnotations( this.sclIdx );

    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.sdaIdx, CHANNEL_SDA_NAME ) );
    this.annotationListener.clearAnnotations( this.sdaIdx );
  }

  /**
   * @param aTime
   */
  private void reportACK( final I2CDataSet aDataSet, final int aSampleIdx )
  {
    if ( this.reportACK )
    {
      aDataSet.reportACK( this.sdaIdx, aSampleIdx );
    }
  }

  /**
   * @param aTime
   */
  private void reportBusError( final I2CDataSet aDataSet, final int aSampleIdx )
  {
    aDataSet.reportBusError( this.sdaIdx, aSampleIdx );
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  private void reportData( final I2CDataSet aDataSet, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aByteValue )
  {
    aDataSet.reportData( this.sdaIdx, aStartSampleIdx, aEndSampleIdx, aByteValue );
  }

  /**
   * @param aDataSet
   * @param aTime
   */
  private void reportNACK( final I2CDataSet aDataSet, final int aSampleIdx )
  {
    if ( this.reportNACK )
    {
      aDataSet.reportNACK( this.sdaIdx, aSampleIdx );
    }
  }

  /**
   * @param aTime
   */
  private void reportStartCondition( final I2CDataSet aDataSet, final int aSampleIdx )
  {
    if ( this.reportStart )
    {
      aDataSet.reportStartCondition( this.sdaIdx, aSampleIdx );
    }
  }

  /**
   * @param aTime
   */
  private void reportStopCondition( final I2CDataSet aDataSet, final int aSampleIdx )
  {
    if ( this.reportStop )
    {
      aDataSet.reportStopCondition( this.sdaIdx, aSampleIdx );
    }
  }
}

/* EOF */
