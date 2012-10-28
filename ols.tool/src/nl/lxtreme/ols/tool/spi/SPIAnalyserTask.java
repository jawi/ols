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
package nl.lxtreme.ols.tool.spi;


import static nl.lxtreme.ols.tool.api.AnnotationHelper.*;
import static nl.lxtreme.ols.tool.base.NumberUtils.*;

import java.beans.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.base.*;
import aQute.bnd.annotation.metatype.*;


/**
 * Provides a task for analyzing SPI traces.
 */
public class SPIAnalyserTask implements Callable<Void>
{
  // CONSTANTS

  static final String EVENT_CS_LOW = "CS_LOW";
  static final String EVENT_CS_HIGH = "CS_HIGH";
  static final String KEY_DATA_TYPE = "type";

  private static final String SPI_MOSI = "MOSI";
  private static final String SPI_MISO = "MISO";
  private static final String SPI_SCK = "SCK";
  private static final String SPI_CS = "/CS";

  private static final Logger LOG = Logger.getLogger( SPIAnalyserTask.class.getName() );

  public static final String PROPERTY_AUTO_DETECT_MODE = "AutoDetectSPIMode";

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final PropertyChangeSupport pcs;

  private final int csIdx;
  private final int sckIdx;
  private final SPIFIMode protocol;
  private SPIMode spiMode;
  private final int bitCount;
  private final BitOrder bitOrder;
  private final boolean reportCS;
  private final boolean honourCS;
  private final int mosiIdx;
  private final int misoIdx;
  private final int io2Idx;
  private final int io3Idx;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SPIAnalyserTask} instance.
   * 
   * @param aContext
   * @param aProgressListener
   */
  public SPIAnalyserTask( final ToolContext aContext, final Configuration aConfiguration )
  {
    this.context = aContext;
    this.progressListener = aContext.getProgressListener();

    this.pcs = new PropertyChangeSupport( this );

    SPIConfig config = Configurable.createConfigurable( SPIConfig.class, aConfiguration.asMap() );

    this.csIdx = config.csIdx();
    this.sckIdx = config.sckIdx();
    this.protocol = config.protocol();
    this.spiMode = config.mode();
    this.bitCount = config.bitCount() - 1;
    this.bitOrder = config.bitOrder();
    this.misoIdx = config.misoIdx();
    this.mosiIdx = config.mosiIdx();
    this.io2Idx = config.io2Idx();
    this.io3Idx = config.io3Idx();
    this.reportCS = config.reportCS();
    this.honourCS = config.honourCS();
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
   * This is the SPI protocol decoder core The decoder scans for a decode start
   * event like CS high to low edge or the trigger of the captured data. After
   * this the decoder starts to decode the data by the selected mode, number of
   * bits and bit order. The decoded data are put to a JTable object directly.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public Void call() throws ToolException
  {
    AnnotationHelper annotationHelper = new AnnotationHelper( this.context );

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "csmask   = 0x" + Integer.toHexString( 1 << this.csIdx ) );
      LOG.fine( "sckmask  = 0x" + Integer.toHexString( 1 << this.sckIdx ) );
      LOG.fine( "misomask = 0x" + Integer.toHexString( 1 << this.misoIdx ) );
      LOG.fine( "mosimask = 0x" + Integer.toHexString( 1 << this.mosiIdx ) );
    }

    final int startOfDecode = this.context.getStartSampleIndex();
    final int endOfDecode = this.context.getEndSampleIndex();

    final int slaveSelected = slaveSelected( startOfDecode, endOfDecode );

    if ( ( this.honourCS && ( slaveSelected < 0 ) ) || ( startOfDecode >= endOfDecode ) )
    {
      // no CS edge found, look for trigger
      LOG.log( Level.WARNING, "No CS start-condition found! Analysis aborted..." );
      throw new ToolException( "No CS start-condition found!" );
    }

    // Initialize the channel labels + clear any existing annotations...
    prepareResults( annotationHelper );

    if ( ( this.spiMode == null ) || ( this.spiMode == SPIMode.AUTODETECT ) )
    {
      LOG.log( Level.INFO, "Detecting which SPI mode is most probably used..." );
      this.spiMode = detectSPIMode( startOfDecode, endOfDecode );
    }

    // Notify any listeners of the detected mode...
    this.pcs.firePropertyChange( PROPERTY_AUTO_DETECT_MODE, null, this.spiMode );

    // Perform the actual decoding of the data line(s)...
    clockDataOnEdge( annotationHelper, this.spiMode, slaveSelected );

    return null;
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
   * Decodes the SPI-data on a given clock edge.
   * 
   * @param aDataSet
   *          the decoded data to fill;
   * @param aMode
   *          the SPI mode defining the edges on which data can be sampled and
   *          on which edges data can change.
   */
  private void clockDataOnEdge( final AnnotationHelper aAnnotationHelper, final SPIMode aMode,
      final int aSlaveSelectedIdx )
  {
    final AcquisitionData data = this.context.getAcquisitionData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    final int startOfDecode = Math.max( aSlaveSelectedIdx, this.context.getStartSampleIndex() );
    final int endOfDecode = this.context.getEndSampleIndex();

    final int mosiMask = ( 1 << this.mosiIdx ); // IO0
    final int misoMask = ( 1 << this.misoIdx ); // IO1
    final int io2Mask = ( 1 << this.io2Idx );
    final int io3Mask = ( 1 << this.io3Idx );
    final int sckMask = ( 1 << this.sckIdx );
    final int csMask = ( 1 << this.csIdx );

    // scanning for falling/rising clk edges
    int oldSckValue = ( values[startOfDecode] & sckMask );
    int oldCsValue = ( values[startOfDecode] & csMask );

    boolean slaveSelected = true;
    int dataStartIdx = startOfDecode;

    int bitIdx = this.bitCount;

    final int clockEdgeCount = ( this.bitCount + 1 ) * 2;
    int clockEdgeIdx = 0;

    int misovalue = 0;
    int mosivalue = 0;

    if ( aSlaveSelectedIdx >= 0 )
    {
      // now the trigger is in b, add trigger event to table
      if ( this.reportCS )
      {
        aAnnotationHelper.addAnnotation( this.csIdx, timestamps[aSlaveSelectedIdx], timestamps[aSlaveSelectedIdx] + 1,
            EVENT_CS_LOW, KEY_COLOR, "#c0ffc0" );
      }
    }

    for ( int idx = startOfDecode + 1; idx < endOfDecode; idx++ )
    {
      final int dataSample = values[idx];
      /* CLK edge detection */
      final int sckValue = ( dataSample & sckMask );
      /* CS edge detection */
      final int csValue = ( dataSample & csMask );

      final Edge slaveSelectEdge = Edge.toEdge( oldCsValue, csValue );
      oldCsValue = csValue;

      if ( slaveSelectEdge.isFalling() )
      {
        if ( this.reportCS )
        {
          aAnnotationHelper.addAnnotation( this.csIdx, timestamps[idx], timestamps[idx] + 1, EVENT_CS_LOW, KEY_COLOR,
              "#c0ffc0" );
        }

        slaveSelected = true;
      }
      else if ( slaveSelectEdge.isRising() )
      {
        if ( this.reportCS )
        {
          aAnnotationHelper.addAnnotation( this.csIdx, timestamps[idx], timestamps[idx] + 1, EVENT_CS_HIGH, KEY_COLOR,
              "#e0e0e0" );
        }

        slaveSelected = false;
        // it could be that we're waiting until a next clock cycle comes along;
        // however, the /CS signal might be going up before that cycle actually
        // comes...
        if ( bitIdx <= 0 )
        {
          // Full datagram decoded; first convert it to right bit order...
          misovalue = BitOrder.convertBitOrder( misovalue, ( this.bitCount + 1 ), this.bitOrder );
          mosivalue = BitOrder.convertBitOrder( mosivalue, ( this.bitCount + 1 ), this.bitOrder );

          if ( SPIFIMode.STANDARD.equals( this.protocol ) )
          {
            aAnnotationHelper.addSymbolAnnotation( this.mosiIdx, timestamps[dataStartIdx], timestamps[idx], mosivalue,
                KEY_DATA_TYPE, SPI_MOSI );
            aAnnotationHelper.addSymbolAnnotation( this.misoIdx, timestamps[dataStartIdx], timestamps[idx], misovalue,
                KEY_DATA_TYPE, SPI_MISO );
          }
          else
          {
            aAnnotationHelper.addSymbolAnnotation( this.mosiIdx, timestamps[dataStartIdx], timestamps[idx], mosivalue );
          }

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }

      if ( this.honourCS && !slaveSelected )
      {
        // We should honour the slave-select, but the slave isn't
        // currently selected...
        continue;
      }

      final Edge clockEdge = Edge.toEdge( oldSckValue, sckValue );
      oldSckValue = sckValue;

      final boolean sampleEdgeSeen;
      if ( clockEdge.isRising() || clockEdge.isFalling() )
      {
        clockEdgeIdx = ( clockEdgeIdx + 1 ) % clockEdgeCount;
        // When CPHA is '1', we should sample at the even numbered clock edges,
        // when CPHA is '0' we should sample at the odd numbered clock edges...
        sampleEdgeSeen = ( ( clockEdgeIdx + aMode.getCPHA() ) % 2 ) != 0;

        // First clock edge we've seen? If so, we should keep this index as our
        // start of data index...
        if ( sampleEdgeSeen && ( bitIdx == this.bitCount ) )
        {
          dataStartIdx = idx;
        }

        LOG.log( Level.FINE, "Clock edge: {0}, idx: {1}, sample? {2}", //
            new Object[] { clockEdge, Integer.valueOf( clockEdgeIdx ), Boolean.valueOf( sampleEdgeSeen ) } );
      }
      else
      {
        // Only actual clock edges should be taken into account...
        sampleEdgeSeen = false;
      }

      if ( sampleEdgeSeen )
      {
        if ( SPIFIMode.STANDARD.equals( this.protocol ) )
        {
          // sample MiSo here; always MSB first, perform conversion later on...
          if ( ( this.misoIdx >= 0 ) && ( ( dataSample & misoMask ) != 0 ) )
          {
            misovalue |= ( 1 << bitIdx );
          }
          // sample MoSi here; always MSB first, perform conversion later on...
          if ( ( this.mosiIdx >= 0 ) && ( ( dataSample & mosiMask ) != 0 ) )
          {
            mosivalue |= ( 1 << bitIdx );
          }

          if ( bitIdx >= 0 )
          {
            bitIdx--;
          }
        }
        else if ( SPIFIMode.DUAL.equals( this.protocol ) )
        {
          // Sample both MOSI/IO0 & MISO/IO1 here; they form two bits of our
          // symbol; we do MSB first, as the decoded symbol will be corrected
          // later on...
          if ( ( dataSample & misoMask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
          if ( ( dataSample & mosiMask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
        }
        else if ( SPIFIMode.QUAD.equals( this.protocol ) )
        {
          // Sample both MOSI/IO0, MISO/IO1, IO2 & IO3 here; they form four bits
          // of our symbol; we do MSB first, as the decoded symbol will be
          // corrected later on...
          if ( ( dataSample & io3Mask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
          if ( ( dataSample & io2Mask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
          if ( ( dataSample & misoMask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
          if ( ( dataSample & mosiMask ) != 0 )
          {
            mosivalue |= ( 1 << bitIdx );
          }
          bitIdx--;
        }

        if ( bitIdx < 0 )
        {
          // Full datagram decoded; first convert it to right bit order...
          misovalue = BitOrder.convertBitOrder( misovalue, ( this.bitCount + 1 ), this.bitOrder );
          mosivalue = BitOrder.convertBitOrder( mosivalue, ( this.bitCount + 1 ), this.bitOrder );

          if ( SPIFIMode.STANDARD.equals( this.protocol ) )
          {
            aAnnotationHelper.addSymbolAnnotation( this.mosiIdx, timestamps[dataStartIdx], timestamps[idx], mosivalue,
                KEY_DATA_TYPE, SPI_MOSI );
            aAnnotationHelper.addSymbolAnnotation( this.misoIdx, timestamps[dataStartIdx], timestamps[idx], misovalue,
                KEY_DATA_TYPE, SPI_MISO );
          }
          else
          {
            aAnnotationHelper.addSymbolAnnotation( this.mosiIdx, timestamps[dataStartIdx], timestamps[idx], mosivalue );
          }

          bitIdx = this.bitCount;
          misovalue = 0;
          mosivalue = 0;
        }
      }

      this.progressListener.setProgress( getPercentage( idx, startOfDecode, endOfDecode ) );
    }
  }

  /**
   * Tries the detect what the clock polarity of the contained data values is.
   * Based on this we can make a "educated" guess what SPI mode should be used
   * for the decoding of the remainder of data.
   * <p>
   * Currently, there is no way I can think of how the CPHA value can be
   * determined from the data. Hence, we can only determine the clock polarity
   * (CPOL), which also provides a good idea on what mode the SPI-data is.
   * </p>
   * 
   * @param aStartIndex
   *          the starting sample index to use;
   * @param aEndIndex
   *          the ending sample index to use.
   * @return the presumed SPI mode, either mode 0 or 2.
   */
  private SPIMode detectSPIMode( final int aStartIndex, final int aEndIndex )
  {
    final AcquisitionData data = this.context.getAcquisitionData();
    final Frequency<Integer> valueStats = new Frequency<Integer>();

    final int[] values = data.getValues();
    final int sckMask = 1 << this.sckIdx;

    // Determine the value of the clock line of each sample; the value that
    // occurs the most is probably the default polarity...
    for ( int i = aStartIndex; i < aEndIndex; i++ )
    {
      final int newValue = ( values[i] & sckMask ) >> this.sckIdx;
      valueStats.addValue( Integer.valueOf( newValue ) );
    }

    SPIMode result;

    // If the clock line's most occurring value is one, then
    // we're fairly sure that CPOL == 1...
    if ( valueStats.getHighestRanked().intValue() == 1 )
    {
      LOG.log( Level.INFO, "SPI mode is probably mode 2 or 3 (CPOL == 1). Assuming mode 2 ..." );
      result = SPIMode.MODE_2;
    }
    else
    {
      LOG.log( Level.INFO, "SPI mode is probably mode 0 or 1 (CPOL == 0). Assuming mode 0 ..." );
      result = SPIMode.MODE_0;
    }

    return result;
  }

  /**
   * Determines the channel labels that are used in the annotations and reports
   * and clears any existing annotations on the decoded channels.
   * 
   * @param aAnnotationHelper
   */
  private void prepareResults( final AnnotationHelper aAnnotationHelper )
  {
    this.context.clearAnnotations( this.mosiIdx, this.misoIdx, this.io2Idx, this.io3Idx, this.sckIdx, this.csIdx );

    if ( this.mosiIdx >= 0 )
    {
      String label = ( SPIFIMode.STANDARD.equals( this.protocol ) ? SPI_MOSI : "IO0" );
      aAnnotationHelper.addAnnotation( this.mosiIdx, label );
    }
    if ( this.misoIdx >= 0 )
    {
      String label = ( SPIFIMode.STANDARD.equals( this.protocol ) ? SPI_MISO : "IO1" );
      aAnnotationHelper.addAnnotation( this.misoIdx, label );
    }
    if ( this.io2Idx >= 0 )
    {
      aAnnotationHelper.addAnnotation( this.io2Idx, "IO2" );
    }
    if ( this.io3Idx >= 0 )
    {
      aAnnotationHelper.addAnnotation( this.io3Idx, "IO3" );
    }
    if ( this.sckIdx >= 0 )
    {
      aAnnotationHelper.addAnnotation( this.sckIdx, SPI_SCK );
    }
    if ( this.csIdx >= 0 )
    {
      aAnnotationHelper.addAnnotation( this.csIdx, SPI_CS );
    }
  }

  /**
   * @param aSampleIndex
   * @param aI
   * @return
   */
  private int searchSlaveSelected( final int aStartIndex, final int aEndIndex )
  {
    final AcquisitionData data = this.context.getAcquisitionData();
    final int[] values = data.getValues();

    final int csMask = 1 << this.csIdx;

    /*
     * For analyze scan the CS line for a falling edge. If no edge could be
     * found, the position of the trigger is used for start of analysis. If no
     * trigger and no edge is found the analysis fails.
     */
    int oldCsValue = values[aStartIndex] & csMask;
    for ( int i = aStartIndex + 1; i < aEndIndex; i++ )
    {
      final int csValue = values[i] & csMask;
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

    return -1;
  }

  /**
   * Tries to find whether the SPI-slave is actually selected due to CS going
   * low (defaults to high).
   * 
   * @param aStartIndex
   *          the starting sample index to use;
   * @param aEndIndex
   *          the ending sample index to use.
   * @return the sample index on which the slave is selected for the first time,
   *         or -1 if no such moment is found.
   */
  private int slaveSelected( final int aStartOfDecode, final int aEndOfDecode )
  {
    int slaveSelected = -1;

    if ( aStartOfDecode > 0 )
    {
      // Search for a CS-low backwards from the first cursor...
      slaveSelected = searchSlaveSelected( 0, aStartOfDecode );
    }

    if ( slaveSelected < 0 )
    {
      // Search for a CS-low forwards from the first cursor...
      slaveSelected = searchSlaveSelected( aStartOfDecode, aEndOfDecode );
    }

    return slaveSelected;
  }
}
