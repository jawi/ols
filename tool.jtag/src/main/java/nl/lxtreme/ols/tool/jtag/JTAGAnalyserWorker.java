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
package nl.lxtreme.ols.tool.jtag;


import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * @author jajans
 * @author Mario Schrenk
 */
public class JTAGAnalyserWorker extends BaseAsyncToolWorker<JTAGDataSet>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( JTAGAnalyserWorker.class.getName() );

  public static final String PROPERTY_AUTO_DETECT_MODE = "AutoDetectJTAGMode";

  // VARIABLES

  private int tmsIdx;
  private int tckIdx;
  private int tdiIdx;
  private int tdoIdx;

  private int JTAGState;
  private int oldJTAGState;
  private int startIdx;

  // CONSTRUCTORS

  /**
   * @param aData
   */
  public JTAGAnalyserWorker( final DataContainer aData, final ToolContext aContext )
  {
    super( aData, aContext );

    this.tdoIdx = -1;
    this.tdiIdx = -1;
  }

  // METHODS

  /**
   * Sets the TCK channel index.
   * 
   * @param aTckIndex
   *          the index of the "serial-clock" channel.
   */
  public void setTckIndex( final int aTckIndex )
  {
    this.tckIdx = aTckIndex;
  }

  /**
   * Sets the TDI channel index.
   * 
   * @param aTdiMask
   *          the index of the "master-out slave-in" channel.
   */
  public void setTdiIndex( final int aTdiIndex )
  {
    this.tdiIdx = aTdiIndex;
  }

  /**
   * Sets the TDO channel index.
   * 
   * @param aTdoMask
   *          the index of the "master-in slave-out" channel.
   */
  public void setTdoIndex( final int aTdoIndex )
  {
    this.tdoIdx = aTdoIndex;
  }

  /**
   * Sets the TMS channel index.
   * 
   * @param aTmsMask
   *          the index of the chip-select channel.
   */
  public void setTmsIndex( final int aTmsIndex )
  {
    this.tmsIdx = aTmsIndex;
  }

  /**
   * This is the JTAG protocol decoder core. The decoded data are put to a
   * JTable object directly.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected JTAGDataSet doInBackground() throws Exception
  {
    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "tmsmask = 0x" + Integer.toHexString( 1 << this.tmsIdx ) );
      LOG.fine( "tckmask = 0x" + Integer.toHexString( 1 << this.tckIdx ) );
      LOG.fine( "tdomask = 0x" + Integer.toHexString( 1 << this.tdoIdx ) );
      LOG.fine( "tdimask = 0x" + Integer.toHexString( 1 << this.tdiIdx ) );
    }

    final int startOfDecode = getContext().getStartSampleIndex();
    final int endOfDecode = getContext().getEndSampleIndex();

    // Initialize the channel labels + clear any existing annotations...
    prepareResults();

    final JTAGDataSet decodedData = new JTAGDataSet( startOfDecode, endOfDecode, this );

    // Perform the actual decoding of the data line(s)...
    clockDataOnEdge( decodedData, startOfDecode );

    return decodedData;
  }

  /**
   * Decodes the JTAG-data on a given clock edge.
   * 
   * @param aDataSet
   *          the decoded data to fill;
   * @param aMode
   *          the JTAG mode defining the edges on which data can be sampled and
   *          on which edges data can change.
   */
  private void clockDataOnEdge( final JTAGDataSet aDataSet, final int aSlaveSelectedIdx )
  {
    final int[] values = getValues();

    final int startOfDecode = Math.max( aSlaveSelectedIdx, aDataSet.getStartOfDecode() );
    final int endOfDecode = aDataSet.getEndOfDecode();

    final int tdoMask = ( 1 << this.tdoIdx );
    final int tdiMask = ( 1 << this.tdiIdx );
    final int tckMask = ( 1 << this.tckIdx );
    final int tmsMask = ( 1 << this.tmsIdx );

    // scanning for falling/rising clk edges
    int oldTckValue = ( values[startOfDecode] & tckMask );

    String State;
    int startTdiDataIdx = 0;
    int endTdiDataIdx = 0;
    String TdiData = "";
    String TdoData = "";

    this.JTAGState = 0;
    this.oldJTAGState = 0;
    this.startIdx = startOfDecode;

    LOG.log( Level.INFO, "clockDataOnEdge: " + startOfDecode + " to " + endOfDecode );

    final double length = endOfDecode - startOfDecode;
    for ( int idx = startOfDecode + 1; idx < endOfDecode; idx++ )
    {
      final int dataSample = values[idx];
      final int tckValue = ( dataSample & tckMask );
      final int tmsValue = ( dataSample & tmsMask );
      final int tdiValue = ( dataSample & tdiMask );
      final int tdoValue = ( dataSample & tdoMask );

      if ( oldTckValue != tckValue )
      {
        oldTckValue = tckValue;

        if ( tckValue != 0 )
        {
          // LOG.log( Level.INFO, "TCK rising edge  (" + idx + ", TMS: " +
          // tmsValue + ",  State: " + JTAGState + ")");

          if ( this.JTAGState == 0 )
          { // state 0: Test Logic Reset
            State = "Test Logic Reset";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 1;
            }
          }
          else if ( this.JTAGState == 1 )
          { // state 1: Run Test Idle
            State = "Run Test Idle";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 1;
            }
            else
            {
              this.JTAGState = 2;
            }
          }
          else if ( this.JTAGState == 2 )
          { // state 2: Select DR
            State = "Select DR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 3;
            }
            else
            {
              this.JTAGState = 9;
            }
          }
          else if ( this.JTAGState == 3 )
          { // state 3: Capture DR
            State = "Capture DR";
            TdiData = "";
            TdoData = "";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 4;
            }
            else
            {
              this.JTAGState = 5;
            }
          }
          else if ( this.JTAGState == 4 )
          { // state 4: Shift DR
            State = "Shift DR";
            if ( TdiData == "" )
            {
              startTdiDataIdx = idx;
            }
            endTdiDataIdx = idx;

            if ( tdiValue == 0 )
            {
              TdiData = "0" + TdiData;
            }
            else
            {
              TdiData = "1" + TdiData;
            }

            if ( tdoValue == 0 )
            {
              TdoData = "0" + TdoData;
            }
            else
            {
              TdoData = "1" + TdoData;
            }

            if ( tmsValue == 0 )
            {
              ;
            }
            else
            {
              this.JTAGState = 5;
            }
          }
          else if ( this.JTAGState == 5 )
          { // state 5: Exit1 DR
            State = "Exit1 DR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 6;
            }
            else
            {
              this.JTAGState = 8;
            }
          }
          else if ( this.JTAGState == 6 )
          { // state 6: Pause DR
            State = "Pause DR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 6;
            }
            else
            {
              this.JTAGState = 7;
            }
          }
          else if ( this.JTAGState == 7 )
          { // state 7: Exit2 DR
            State = "Exit2 DR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 4;
            }
            else
            {
              this.JTAGState = 8;
            }
          }
          else if ( this.JTAGState == 8 )
          { // state 8: Update DR
            State = "Update DR";
            addChannelAnnotation( this.tdiIdx, startTdiDataIdx, endTdiDataIdx, TdiData );
            addChannelAnnotation( this.tdoIdx, startTdiDataIdx, endTdiDataIdx, TdoData );

            if ( tmsValue == 0 )
            {
              this.JTAGState = 1;
            }
            else
            {
              this.JTAGState = 2;
            }
          }
          else if ( this.JTAGState == 9 )
          { // state 9: Select IR
            State = "Select IR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 10;
            }
            else
            {
              this.JTAGState = 0;
            }
          }
          else if ( this.JTAGState == 10 )
          { // state 10: Capture IR
            State = "Capture IR";
            TdiData = "";
            TdoData = "";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 11;
            }
            else
            {
              this.JTAGState = 12;
            }
          }
          else if ( this.JTAGState == 11 )
          { // state 11: Shift IR
            State = "Shift IR";

            if ( TdiData == "" )
            {
              startTdiDataIdx = idx;
            }
            endTdiDataIdx = idx;

            if ( tdiValue == 0 )
            {
              TdiData = "0" + TdiData;
            }
            else
            {
              TdiData = "1" + TdiData;
            }

            if ( tdoValue == 0 )
            {
              TdoData = "0" + TdoData;
            }
            else
            {
              TdoData = "1" + TdoData;
            }

            if ( tmsValue == 0 )
            {
              ;
            }
            else
            {
              this.JTAGState = 12;
            }
          }
          else if ( this.JTAGState == 12 )
          { // state 12: Exit1 IR
            State = "Exit1 IR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 13;
            }
            else
            {
              this.JTAGState = 15;
            }
          }
          else if ( this.JTAGState == 13 )
          { // state 13: Pause IR
            State = "Pause IR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 13;
            }
            else
            {
              this.JTAGState = 14;
            }
          }
          else if ( this.JTAGState == 14 )
          { // state 14: Exit2 IR
            State = "Exit2 IR";
            if ( tmsValue == 0 )
            {
              this.JTAGState = 11;
            }
            else
            {
              this.JTAGState = 15;
            }
          }
          else if ( this.JTAGState == 15 )
          { // state 15: Update IR
            State = "Update IR";
            addChannelAnnotation( this.tdiIdx, startTdiDataIdx, endTdiDataIdx, TdiData );
            // aDataSet.reportJTAGState( this.tdiIdx, startTdiDataIdx,
            // endTdiDataIdx, TdiData );
            addChannelAnnotation( this.tdoIdx, startTdiDataIdx, endTdiDataIdx, TdoData );
            // aDataSet.reportJTAGState( this.tdoIdx, startTdiDataIdx,
            // endTdiDataIdx, TdoData );

            if ( tmsValue == 0 )
            {
              this.JTAGState = 1;
            }
            else
            {
              this.JTAGState = 2;
            }
          }
          else
          {
            State = "ERROR";
          }

          if ( this.oldJTAGState != this.JTAGState )
          {
            // LOG.log( Level.INFO, "state transition: " + oldJTAGState + " to "
            // + JTAGState + " (" + StartIdx + "," + idx + ")");

            addChannelAnnotation( this.tmsIdx, this.startIdx, idx, State );
            aDataSet.reportJTAGState( this.tmsIdx, this.startIdx, idx, this.oldJTAGState );

            this.startIdx = idx + 1;
            this.oldJTAGState = this.JTAGState;
          }

          setProgress( ( int )( ( idx - startOfDecode ) * 100.0 / length ) );
        }
      }
    }
  }

  /**
   * Determines the channel labels that are used in the annotations and reports
   * and clears any existing annotations on the decoded channels.
   */
  private void prepareResults()
  {
    if ( this.tckIdx >= 0 )
    {
      updateChannelLabel( this.tckIdx, JTAGDataSet.JTAG_TCK );
      // clear any existing annotations
      clearChannelAnnotations( this.tckIdx );
    }
    if ( this.tmsIdx >= 0 )
    {
      updateChannelLabel( this.tmsIdx, JTAGDataSet.JTAG_TMS );
      // clear any existing annotations
      clearChannelAnnotations( this.tmsIdx );
    }
    if ( this.tdiIdx >= 0 )
    {
      updateChannelLabel( this.tdiIdx, JTAGDataSet.JTAG_TDI );
      // clear any existing annotations
      clearChannelAnnotations( this.tdiIdx );
    }
    if ( this.tdoIdx >= 0 )
    {
      updateChannelLabel( this.tdoIdx, JTAGDataSet.JTAG_TDO );
      // clear any existing annotations
      clearChannelAnnotations( this.tdoIdx );
    }
  }
}
