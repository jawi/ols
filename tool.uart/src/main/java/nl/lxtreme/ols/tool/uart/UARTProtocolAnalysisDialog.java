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


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;


/**
 * The Dialog Class
 * 
 * @author Frank Kunz
 *         The dialog class draws the basic dialog with a grid layout. The dialog
 *         consists of three main parts. A settings panel, a table panel
 *         and three buttons.
 */
final class UARTProtocolAnalysisDialog extends JDialog implements ActionListener, Runnable
{
  // CONSTANTS

  private static final long                         serialVersionUID = 1L;

  // VARIABLES

  private final String[]                            parityarray;
  private final String[]                            bitarray;
  private final String[]                            stoparray;
  private final JComboBox                           rxd;
  private final JComboBox                           txd;
  private final JComboBox                           cts;
  private final JComboBox                           rts;
  private final JComboBox                           dtr;
  private final JComboBox                           dsr;
  private final JComboBox                           dcd;
  private final JComboBox                           ri;
  private final JComboBox                           parity;
  private final JComboBox                           bits;
  private final JComboBox                           stop;
  private final JCheckBox                           inv;
  private final JButton                             btnConvert;
  private final JButton                             btnExport;
  private final JButton                             btnCancel;
  private final JProgressBar                        progress;
  private boolean                                   runFlag;
  private Thread                                    thrWorker;
  private CapturedData                              analysisData;
  private final JEditorPane                         outText;
  private final Vector<UARTProtocolAnalysisDataSet> decodedData;
  private final JFileChooser                        fileChooser;
  private long                                      startOfDecode;
  private long                                      endOfDecode;
  private int                                       decodedSymbols;
  private int                                       bitLength;
  private int                                       detectedErrors;

  // CONSTRUCTORS

  public UARTProtocolAnalysisDialog( final Frame frame, final String name )
  {
    super( frame, name, true );
    Container pane = getContentPane();
    pane.setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    this.decodedData = new Vector<UARTProtocolAnalysisDataSet>();
    this.startOfDecode = -1;
    this.decodedSymbols = 0;
    this.bitLength = 0;
    this.detectedErrors = 0;

    /*
     * add protocol settings elements
     */
    JPanel panSettings = new JPanel();
    panSettings.setLayout( new GridLayout( 12, 2, 5, 5 ) );
    panSettings.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    String channels[] = new String[33];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = new String( "Channel " + i );
    }
    channels[channels.length - 1] = new String( "unused" );

    panSettings.add( new JLabel( "RxD" ) );
    this.rxd = new JComboBox( channels );
    panSettings.add( this.rxd );

    panSettings.add( new JLabel( "TxD" ) );
    this.txd = new JComboBox( channels );
    panSettings.add( this.txd );

    panSettings.add( new JLabel( "CTS" ) );
    this.cts = new JComboBox( channels );
    this.cts.setSelectedItem( "unused" );
    panSettings.add( this.cts );

    panSettings.add( new JLabel( "RTS" ) );
    this.rts = new JComboBox( channels );
    this.rts.setSelectedItem( "unused" );
    panSettings.add( this.rts );

    panSettings.add( new JLabel( "DTR" ) );
    this.dtr = new JComboBox( channels );
    this.dtr.setSelectedItem( "unused" );
    panSettings.add( this.dtr );

    panSettings.add( new JLabel( "DSR" ) );
    this.dsr = new JComboBox( channels );
    this.dsr.setSelectedItem( "unused" );
    panSettings.add( this.dsr );

    panSettings.add( new JLabel( "DCD" ) );
    this.dcd = new JComboBox( channels );
    this.dcd.setSelectedItem( "unused" );
    panSettings.add( this.dcd );

    panSettings.add( new JLabel( "RI" ) );
    this.ri = new JComboBox( channels );
    this.ri.setSelectedItem( "unused" );
    panSettings.add( this.ri );

    panSettings.add( new JLabel( "Parity" ) );
    this.parityarray = new String[3];
    this.parityarray[0] = new String( "none" );
    this.parityarray[1] = new String( "odd" );
    this.parityarray[2] = new String( "even" );
    this.parity = new JComboBox( this.parityarray );
    panSettings.add( this.parity );

    panSettings.add( new JLabel( "Bits" ) );
    this.bitarray = new String[4];
    for ( int i = 0; i < this.bitarray.length; i++ )
    {
      this.bitarray[i] = new String( "" + ( i + 5 ) );
    }
    this.bits = new JComboBox( this.bitarray );
    this.bits.setSelectedItem( "8" );
    panSettings.add( this.bits );

    panSettings.add( new JLabel( "Stopbit" ) );
    this.stoparray = new String[3];
    this.stoparray[0] = new String( "1" );
    this.stoparray[1] = new String( "1.5" );
    this.stoparray[2] = new String( "2" );
    this.stop = new JComboBox( this.stoparray );
    panSettings.add( this.stop );

    this.inv = new JCheckBox();
    panSettings.add( new JLabel( "Invert" ) );
    panSettings.add( this.inv );

    pane.add( panSettings, createConstraints( 0, 0, 1, 1, 0, 0 ) );

    /*
     * add an empty output view
     */
    JPanel panTable = new JPanel();
    panTable.setLayout( new GridLayout( 1, 1, 5, 5 ) );
    panTable.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Results" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    this.outText = new JEditorPane( "text/html", toHtmlPage( true ) );
    this.outText.setMargin( new Insets( 5, 5, 5, 5 ) );
    panTable.add( new JScrollPane( this.outText ) );
    add( panTable, createConstraints( 1, 0, 3, 3, 1.0, 1.0 ) );

    /*
     * add progress bar
     */
    JPanel panProgress = new JPanel();
    panProgress.setLayout( new BorderLayout() );
    panProgress.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Progress" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    this.progress = new JProgressBar( 0, 100 );
    this.progress.setMinimum( 0 );
    this.progress.setValue( 0 );
    this.progress.setMaximum( 100 );
    panProgress.add( this.progress, BorderLayout.CENTER );
    add( panProgress, createConstraints( 0, 3, 3, 1, 1.0, 0 ) );

    /*
     * add buttons
     */
    this.btnConvert = new JButton( "Analyze" );
    this.btnConvert.addActionListener( this );
    add( this.btnConvert, createConstraints( 0, 4, 1, 1, 1.0, 0 ) );
    this.btnExport = new JButton( "Export" );
    this.btnExport.addActionListener( this );
    add( this.btnExport, createConstraints( 1, 4, 1, 1, 1.0, 0 ) );
    this.btnCancel = new JButton( "Close" );
    this.btnCancel.addActionListener( this );
    add( this.btnCancel, createConstraints( 2, 4, 1, 1, 1.0, 0 ) );

    this.fileChooser = new JFileChooser();
    // this.fileChooser.addChoosableFileFilter( ( FileFilter )new CSVFilter() );
    // this.fileChooser.addChoosableFileFilter( ( FileFilter )new HTMLFilter() );

    setSize( 1000, 550 );
    setResizable( false );
    this.runFlag = false;
    this.thrWorker = null;
  }

  private static GridBagConstraints createConstraints( final int x, final int y, final int w, final int h,
      final double wx, final double wy )
  {
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.fill = GridBagConstraints.BOTH;
    gbc.insets = new Insets( 4, 4, 4, 4 );
    gbc.gridx = x;
    gbc.gridy = y;
    gbc.gridwidth = w;
    gbc.gridheight = h;
    gbc.weightx = wx;
    gbc.weighty = wy;
    return ( gbc );
  }

  /**
   * Dialog Action handler
   */
  public void actionPerformed( final ActionEvent e )
  {
    if ( e.getActionCommand().equals( "Analyze" ) )
    {
      this.runFlag = true;
      this.thrWorker = new Thread( this );
      this.thrWorker.start();
    }
    else if ( e.getActionCommand().equals( "Close" ) )
    {
      setVisible( false );
    }
    else if ( e.getActionCommand().equals( "Export" ) )
    {
      if ( this.fileChooser.showSaveDialog( this ) == JFileChooser.APPROVE_OPTION )
      {
        File file = this.fileChooser.getSelectedFile();
        if ( this.fileChooser.getFileFilter().getDescription().equals( "Website (*.html)" ) )
        {
          storeToHtmlFile( file );
        }
        else
        {
          storeToCsvFile( file );
        }
      }
    }
    else if ( e.getActionCommand().equals( "Abort" ) )
    {
      this.runFlag = false;
    }
  }

  public void readProperties( final Properties properties )
  {
    // selectByIndex( this.rxd, properties.getProperty( "tools.UARTProtocolAnalysis.rxd" ) );
    // selectByIndex( this.txd, properties.getProperty( "tools.UARTProtocolAnalysis.txd" ) );
    // selectByIndex( this.cts, properties.getProperty( "tools.UARTProtocolAnalysis.cts" ) );
    // selectByIndex( this.rts, properties.getProperty( "tools.UARTProtocolAnalysis.rts" ) );
    // selectByIndex( this.dtr, properties.getProperty( "tools.UARTProtocolAnalysis.dtr" ) );
    // selectByIndex( this.dsr, properties.getProperty( "tools.UARTProtocolAnalysis.dsr" ) );
    // selectByIndex( this.dcd, properties.getProperty( "tools.UARTProtocolAnalysis.dcd" ) );
    // selectByIndex( this.ri, properties.getProperty( "tools.UARTProtocolAnalysis.ri" ) );
    // selectByValue( this.parity, this.parityarray, properties.getProperty( "tools.UARTProtocolAnalysis.parity" ) );
    // selectByValue( this.bits, this.bitarray, properties.getProperty( "tools.UARTProtocolAnalysis.bits" ) );
    // selectByValue( this.stop, this.stoparray, properties.getProperty( "tools.UARTProtocolAnalysis.stop" ) );
    this.inv.setSelected( Boolean.parseBoolean( properties.getProperty( "tools.UARTProtocolAnalysis.inverted" ) ) );
  }

  /**
   * runs the conversion when started
   */
  public void run()
  {
    setControlsEnabled( false );
    this.btnConvert.setText( "Abort" );
    decode();
    setControlsEnabled( true );
    this.btnConvert.setText( "Analyze" );
  }

  /**
   * shows the dialog and sets the data to use
   * 
   * @param data
   *          data to use for analysis
   */
  public void showDialog( final CapturedData data )
  {
    this.analysisData = data;
    setVisible( true );
  }

  public void writeProperties( final Properties properties )
  {
    properties.setProperty( "tools.UARTProtocolAnalysis.rxd", Integer.toString( this.rxd.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.txd", Integer.toString( this.txd.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.cts", Integer.toString( this.cts.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.rts", Integer.toString( this.rts.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.dtr", Integer.toString( this.dtr.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.dsr", Integer.toString( this.dsr.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.dcd", Integer.toString( this.dcd.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.ri", Integer.toString( this.ri.getSelectedIndex() ) );
    properties.setProperty( "tools.UARTProtocolAnalysis.parity", ( String )this.parity.getSelectedItem() );
    properties.setProperty( "tools.UARTProtocolAnalysis.bits", ( String )this.bits.getSelectedItem() );
    properties.setProperty( "tools.UARTProtocolAnalysis.stop", ( String )this.stop.getSelectedItem() );
    properties.setProperty( "tools.UARTProtocolAnalysis.inverted", "" + this.inv.isSelected() );
  }

  /**
   * calculate the time offset
   * 
   * @param time
   *          absolute sample number
   * @return time relative to data
   */
  private long calculateTime( final long time )
  {
    if ( this.analysisData.hasTriggerData() )
    {
      return time - this.analysisData.triggerPosition;
    }
    else
    {
      return time;
    }
  }

  /**
   * This is the UART protocol decoder core
   * The decoder scans for a decode start event like CS high to
   * low edge or the trigger of the captured data. After this the
   * decoder starts to decode the data by the selected mode, number
   * of bits and bit order. The decoded data are put to a JTable
   * object directly.
   */
  private void decode()
  {
    // process the captured data and write to output
    int i, a;

    // clear old data
    this.decodedData.clear();

    /*
     * Build bitmasks based on the RxD, TxD, CTS and RTS
     * pins.
     */

    int rxdmask = 0;
    if ( !( ( String )this.rxd.getSelectedItem() ).equals( "unused" ) )
    {
      rxdmask = ( 1 << this.rxd.getSelectedIndex() );
    }

    int txdmask = 0;
    if ( !( ( String )this.txd.getSelectedItem() ).equals( "unused" ) )
    {
      txdmask = ( 1 << this.txd.getSelectedIndex() );
    }

    int ctsmask = 0;
    if ( !( ( String )this.cts.getSelectedItem() ).equals( "unused" ) )
    {
      ctsmask = ( 1 << this.cts.getSelectedIndex() );
    }

    int rtsmask = 0;
    if ( !( ( String )this.rts.getSelectedItem() ).equals( "unused" ) )
    {
      rtsmask = ( 1 << this.rts.getSelectedIndex() );
    }

    int dcdmask = 0;
    if ( !( ( String )this.dcd.getSelectedItem() ).equals( "unused" ) )
    {
      dcdmask = ( 1 << this.dcd.getSelectedIndex() );
    }

    int rimask = 0;
    if ( !( ( String )this.ri.getSelectedItem() ).equals( "unused" ) )
    {
      rimask = ( 1 << this.ri.getSelectedIndex() );
    }

    int dsrmask = 0;
    if ( !( ( String )this.dsr.getSelectedItem() ).equals( "unused" ) )
    {
      dsrmask = ( 1 << this.dsr.getSelectedIndex() );
    }

    int dtrmask = 0;
    if ( !( ( String )this.dtr.getSelectedItem() ).equals( "unused" ) )
    {
      dtrmask = ( 1 << this.dtr.getSelectedIndex() );
    }

    System.out.println( "rxdmask = 0x" + Integer.toHexString( rxdmask ) );
    System.out.println( "txdmask = 0x" + Integer.toHexString( txdmask ) );
    System.out.println( "ctsmask = 0x" + Integer.toHexString( ctsmask ) );
    System.out.println( "rtsmask = 0x" + Integer.toHexString( rtsmask ) );
    System.out.println( "dcdmask = 0x" + Integer.toHexString( dcdmask ) );
    System.out.println( "rimask  = 0x" + Integer.toHexString( rimask ) );
    System.out.println( "dsrmask = 0x" + Integer.toHexString( dsrmask ) );
    System.out.println( "dtrmask = 0x" + Integer.toHexString( dtrmask ) );

    /*
     * Start decode from trigger or if no trigger is available from the
     * first falling edge.
     * The decoder works with two independant decoder runs. First for
     * RxD and then for TxD, after this CTS, RTS, etc. is detected if enabled.
     * After decoding all the decoded data are unsortet before the data is
     * displayed it must be sortet by time.
     */

    /*
     * set the start of decode to the trigger if avail or
     * find first state change on the selected lines
     */
    if ( this.analysisData.cursorEnabled )
    {
      this.startOfDecode = this.analysisData.getCursorPosition( 1 );
      this.endOfDecode = this.analysisData.getCursorPosition( 2 );
    }
    else
    {
      if ( this.analysisData.hasTriggerData() )
      {
        this.startOfDecode = this.analysisData.triggerPosition;
        // the trigger may be too late, a workaround is to go back some samples here
        this.startOfDecode -= 10;
        if ( this.startOfDecode < 0 )
        {
          this.startOfDecode = 0;
        }
      }
      else
      {
        int mask = rxdmask | rimask | ctsmask | txdmask | dcdmask | rimask | dsrmask | dtrmask;
        a = this.analysisData.values[0] & mask;
        for ( i = 0; i < this.analysisData.values.length; i++ )
        {
          if ( a != ( this.analysisData.values[i] & mask ) )
          {
            this.startOfDecode = this.analysisData.timestamps[i];
            break;
          }
        }
      }
      this.endOfDecode = this.analysisData.absoluteLength;
    }
    this.decodedSymbols = 0;
    this.detectedErrors = 0;

    // decode RxD
    if ( rxdmask != 0 )
    {
      BaudRateAnalyzer baudrate = new BaudRateAnalyzer( this.analysisData.values, this.analysisData.timestamps, rxdmask );
      System.out.println( baudrate.toString() );
      this.bitLength = baudrate.getBest();
      if ( this.bitLength == 0 )
      {
        System.out.println( "No data for decode" );
      }
      else
      {
        System.out.println( "Samplerate=" + this.analysisData.rate + " Bitlength=" + this.bitLength + " Baudrate="
            + this.analysisData.rate / this.bitLength );
        this.decodedSymbols += decodeData( this.bitLength, rxdmask, UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA );
      }
    }
    // decode TxD
    if ( txdmask != 0 )
    {
      BaudRateAnalyzer baudrate = new BaudRateAnalyzer( this.analysisData.values, this.analysisData.timestamps, txdmask );
      System.out.println( baudrate.toString() );
      this.bitLength = baudrate.getBest();
      if ( this.bitLength == 0 )
      {
        System.out.println( "No data for decode" );
      }
      else
      {
        System.out.println( "Samplerate=" + this.analysisData.rate + " Bitlength=" + this.bitLength + " Baudrate="
            + this.analysisData.rate / this.bitLength );
        this.decodedSymbols += decodeData( this.bitLength, txdmask, UARTProtocolAnalysisDataSet.UART_TYPE_TXDATA );
      }
    }
    // decode control lines
    decodeControl( ctsmask, "CTS" );
    decodeControl( rtsmask, "RTS" );
    decodeControl( dcdmask, "DCD" );
    decodeControl( rimask, "RI" );
    decodeControl( dsrmask, "DSR" );
    decodeControl( dtrmask, "DTR" );

    // sort the results by time
    Collections.sort( this.decodedData );

    this.outText.setText( toHtmlPage( false ) );
    this.outText.setEditable( false );
  }

  /**
   * decode a control line
   * 
   * @param mask
   *          bitmask for the control line
   * @param name
   *          name string of the control line
   */
  private void decodeControl( final int mask, final String name )
  {
    if ( mask == 0 )
    {
      return;
    }
    System.out.println( "Decode " + name );
    long i;
    int a;
    a = this.analysisData.getDataAt( 0 ) & mask;
    this.progress.setValue( 0 );
    for ( i = this.startOfDecode; i < this.endOfDecode; i++ )
    {
      if ( a < ( this.analysisData.getDataAt( i ) & mask ) )
      {
        // rising edge
        this.decodedData.add( new UARTProtocolAnalysisDataSet( i, name + "_HIGH" ) );
      }
      if ( a > ( this.analysisData.getDataAt( i ) & mask ) )
      {
        // falling edge
        this.decodedData.add( new UARTProtocolAnalysisDataSet( i, name + "_LOW" ) );
      }
      a = this.analysisData.getDataAt( i ) & mask;

      // update progress
      this.progress.setValue( ( int )( i * 100 / ( this.endOfDecode - this.startOfDecode ) ) );

      // abort here
      if ( !this.runFlag )
      {
        break;
      }
    }
    this.progress.setValue( 100 );
  }

  /**
   * decode a UART data line
   * 
   * @param baud
   *          baudrate (counted samples per bit)
   * @param mask
   *          bitmask for the dataline
   * @param type
   *          type of the data (rx or tx)
   */
  private int decodeData( final int baud, final int mask, final int type )
  {
    if ( mask == 0 )
    {
      return ( 0 );
    }
    long a = 0;
    int b = 0;
    long c = 0;
    long i = 0;
    int value = 0;
    int bitCount;
    int stopCount;
    int parityCount;
    int count = 0;

    bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() );
    if ( ( ( String )this.parity.getSelectedItem() ).equals( "none" ) )
    {
      parityCount = 0;
    }
    else
    {
      parityCount = 1;
    }
    if ( ( ( String )this.stop.getSelectedItem() ).equals( "1" ) )
    {
      stopCount = 1;
    }
    else
    {
      stopCount = 2;
    }

    if ( this.startOfDecode > 0 )
    {
      a = this.startOfDecode;
    }

    while ( ( this.endOfDecode - a ) > ( ( bitCount + stopCount + parityCount ) * baud ) )
    {

      /*
       * find first falling edge this
       * is the start of the startbit.
       * If the inverted checkbox is set find the first rising edge.
       */
      b = this.analysisData.getDataAt( a ) & mask;
      for ( i = a; i < this.endOfDecode; i++ )
      {
        if ( this.inv.isSelected() )
        {
          if ( b < ( this.analysisData.getDataAt( i ) & mask ) )
          {
            c = i;
            break;
          }
        }
        else
        {
          if ( b > ( this.analysisData.getDataAt( i ) & mask ) )
          {
            c = i;
            break;
          }
        }
        b = this.analysisData.getDataAt( i ) & mask;

        // update progress
        this.progress.setValue( ( int )( i * 100 / ( this.endOfDecode - this.startOfDecode ) ) );

        // abort here
        if ( !this.runFlag )
        {
          System.out.println( "Abort: count=" + count + " pos=" + i );
          i = this.endOfDecode;
          break;
        }
      }
      if ( i >= this.endOfDecode )
      {
        System.out.println( "End decode" );
        break;
      }

      /*
       * Sampling is done in the middle of each bit
       * the start bit must be low
       * If the inverted checkbox is set the startbit must be high
       */
      a = c + baud / 2;
      if ( this.inv.isSelected() )
      {
        if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
        {
          // this is not a start bit !
          if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
          {
            this.decodedData.add( new UARTProtocolAnalysisDataSet( calculateTime( a ), "START_ERR",
                UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
          }
          else
          {
            this.decodedData.add( new UARTProtocolAnalysisDataSet( calculateTime( a ), "START_ERR",
                UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
          }
          this.detectedErrors++;
        }
      }
      else
      {
        if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
        {
          // this is not a start bit !
          if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
          {
            this.decodedData.add( new UARTProtocolAnalysisDataSet( calculateTime( a ), "START_ERR",
                UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
          }
          else
          {
            this.decodedData.add( new UARTProtocolAnalysisDataSet( calculateTime( a ), "START_ERR",
                UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
          }
          this.detectedErrors++;
        }
      }

      /*
       * sample the databits in the middle of the bit position
       */

      value = 0;
      for ( i = 0; i < bitCount; i++ )
      {
        a += baud;
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
          {
            value |= ( 1 << i );
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            value |= ( 1 << i );
          }
        }
      }
      this.decodedData.add( new UARTProtocolAnalysisDataSet( a, value, type ) );
      count++;

      /*
       * sample parity bit if available
       */
      String parityText = ( String )this.parity.getSelectedItem();
      if ( parityText.equals( "odd" ) )
      {
        a += baud;
        if ( ( Integer.bitCount( value ) & 1 ) == 0 )
        {
          if ( this.inv.isSelected() )
          {
            // odd parity, bitcount is even --> parity bit must be 0 (inverted)
            if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
          else
          {
            // odd parity, bitcount is even --> parity bit must be 1
            if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
        }
        else
        {
          if ( this.inv.isSelected() )
          {
            // odd parity, bitcount is odd --> parity bit must be 1 (Inverted)
            if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
          else
          {
            // odd parity, bitcount is odd --> parity bit must be 0
            if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
        }
      }
      if ( parityText.equals( "even" ) )
      {
        a += baud;
        if ( ( Integer.bitCount( value ) & 1 ) == 0 )
        {
          if ( this.inv.isSelected() )
          {
            // even parity, bitcount is even --> parity bit must be 1 (inverted)
            if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
          else
          {
            // even parity, bitcount is even --> parity bit must be 0
            if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
        }
        else
        {
          if ( this.inv.isSelected() )
          {
            // even parity, bitcount is odd --> parity bit must be 0 (inverted)
            if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
          else
          {
            // even parity, bitcount is odd --> parity bit must be 1
            if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
            {
              // parity error
              if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
              }
              else
              {
                this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "PARITY_ERR",
                    UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
              }
              this.detectedErrors++;
            }
          }
        }
      }

      /*
       * sample stopbit(s)
       */
      String stopText = ( String )this.stop.getSelectedItem();
      a += baud;
      if ( stopText.equals( "1" ) )
      {
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
      }
      else if ( stopText.equals( "1.5" ) )
      {
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) == 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        a += ( baud / 4 );
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
      }
      else
      {
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        a += baud;
        if ( this.inv.isSelected() )
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
        else
        {
          if ( ( this.analysisData.getDataAt( a ) & mask ) != 0 )
          {
            // framing error
            if ( type == UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA )
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT ) );
            }
            else
            {
              this.decodedData.add( new UARTProtocolAnalysisDataSet( a, "FRAME_ERR",
                  UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT ) );
            }
            this.detectedErrors++;
          }
        }
      }
    }
    this.progress.setValue( 100 );
    return ( count );
  }

  /**
   * Convert sample count to time string.
   * 
   * @param count
   *          sample count (or index)
   * @return string containing time information
   */
  private String indexToTime( long count )
  {
    count -= this.startOfDecode;
    if ( count < 0 )
    {
      count = 0;
    }
    if ( this.analysisData.hasTimingData() )
    {
      return DisplayUtils.displayScaledTime( count, this.analysisData.rate );
    }
    else
    {
      return ( "" + count );
    }
  }

  /**
   * set the controls of the dialog enabled/disabled
   * 
   * @param enable
   *          status of the controls
   */
  private void setControlsEnabled( final boolean enable )
  {
    this.rxd.setEnabled( enable );
    this.txd.setEnabled( enable );
    this.cts.setEnabled( enable );
    this.rts.setEnabled( enable );
    this.dtr.setEnabled( enable );
    this.dsr.setEnabled( enable );
    this.dcd.setEnabled( enable );
    this.ri.setEnabled( enable );
    this.parity.setEnabled( enable );
    this.bits.setEnabled( enable );
    this.stop.setEnabled( enable );
    this.inv.setEnabled( enable );
    this.btnExport.setEnabled( enable );
    this.btnCancel.setEnabled( enable );
  }

  /**
   * exports the data to a CSV file
   * 
   * @param file
   *          File object
   */
  private void storeToCsvFile( final File file )
  {
    if ( this.decodedData.size() > 0 )
    {
      UARTProtocolAnalysisDataSet dSet;
      System.out.println( "writing decoded data to " + file.getPath() );
      try
      {
        BufferedWriter bw = new BufferedWriter( new FileWriter( file ) );

        bw.write( "\"" + "index" + "\",\"" + "time" + "\",\"" + "RxD data or event" + "\",\"" + "TxD data or event"
            + "\"" );
        bw.newLine();

        for ( int i = 0; i < this.decodedData.size(); i++ )
        {
          dSet = this.decodedData.get( i );
          switch ( dSet.type )
          {
            case UARTProtocolAnalysisDataSet.UART_TYPE_EVENT:
              bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + dSet.event + "\",\"" + dSet.event
                  + "\"" );
              break;
            case UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT:
              bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + dSet.event + "\",\"" + "\"" );
              break;
            case UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT:
              bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + "\",\"" + dSet.event + "\"" );
              break;
            case UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA:
              bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + dSet.data + "\",\"" + "\"" );
              break;
            case UARTProtocolAnalysisDataSet.UART_TYPE_TXDATA:
              bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + "\",\"" + dSet.data + "\"" );
              break;
            default:
              break;
          }
          bw.newLine();
        }
        bw.close();
      }
      catch ( Exception E )
      {
        E.printStackTrace( System.out );
      }
    }
  }

  /**
   * stores the data to a HTML file
   * 
   * @param file
   *          file object
   */
  private void storeToHtmlFile( final File file )
  {
    if ( this.decodedData.size() > 0 )
    {
      System.out.println( "writing decoded data to " + file.getPath() );
      try
      {
        BufferedWriter bw = new BufferedWriter( new FileWriter( file ) );

        // write the complete displayed html page to file
        bw.write( this.outText.getText() );

        bw.close();
      }
      catch ( Exception E )
      {
        E.printStackTrace( System.out );
      }
    }
  }

  /**
   * generate a HTML page
   * 
   * @param empty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final boolean empty )
  {
    Date now = new Date();
    DateFormat df = DateFormat.getDateInstance( DateFormat.LONG, Locale.US );
    int bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() );
    int bitAdder = 0;

    if ( bitCount % 4 != 0 )
    {
      bitAdder = 1;
    }

    // generate html page header
    String header = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
        + "<html>"
        + "  <head>"
        + "    <title></title>"
        + "    <meta content=\"\">"
        + "    <style>"
        + "           th { text-align:left;font-style:italic;font-weight:bold;font-size:medium;font-family:sans-serif;background-color:#C0C0FF; }"
        + "       </style>" + "  </head>" + "   <body>" + "       <H2>UART Analysis Results</H2>" + "       <hr>"
        + "           <div style=\"text-align:right;font-size:x-small;\">" + df.format( now ) + "           </div>"
        + "       <br>";

    // generate the statistics table
    String stats = new String();
    if ( !empty )
    {
      if ( this.bitLength == 0 )
      {
        stats = stats.concat( "<p style=\"color:red;\">Baudrate calculation failed !</p><br><br>" );
      }
      else
      {
        stats = stats.concat( "<table style=\"width:100%;\">" + "<TR><TD style=\"width:30%;\">Decoded Symbols</TD><TD>"
            + this.decodedSymbols + "</TD></TR>" + "<TR><TD style=\"width:30%;\">Detected Bus Errors</TD><TD>"
            + this.detectedErrors + "</TD></TR>" + "<TR><TD style=\"width:30%;\">Baudrate</TD><TD>"
            + this.analysisData.rate / this.bitLength + "</TD></TR>" + "</table>" + "<br>" + "<br>" );
        if ( this.bitLength < 15 )
        {
          stats = stats
              .concat( "<p style=\"color:red;\">The baudrate may be wrong, use a higher samplerate to avoid this !</p><br><br>" );
        }
      }
    }

    // generate the data table
    String data = "<table style=\"font-family:monospace;width:100%;\">"
        + "<tr><th style=\"width:15%;\">Index</th><th style=\"width:15%;\">Time</th><th style=\"width:10%;\">RxD Hex</th><th style=\"width:10%;\">RxD Bin</th><th style=\"width:8%;\">RxD Dec</th><th style=\"width:7%;\">RxD ASCII</th><th style=\"width:10%;\">TxD Hex</th><th style=\"width:10%;\">TxD Bin</th><th style=\"width:8%;\">TxD Dec</th><th style=\"width:7%;\">TxD ASCII</th></tr>";
    if ( empty )
    {
    }
    else
    {
      UARTProtocolAnalysisDataSet ds;
      for ( int i = 0; i < this.decodedData.size(); i++ )
      {
        ds = this.decodedData.get( i );
        switch ( ds.type )
        {
          case UARTProtocolAnalysisDataSet.UART_TYPE_EVENT:
            data = data.concat( "<tr style=\"background-color:#E0E0E0;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time ) + "</td><td>" + ds.event + "</td><td></td><td></td><td></td><td>" + ds.event
                + "</td><td></td><td></td><td></td></tr>" );
            break;
          case UARTProtocolAnalysisDataSet.UART_TYPE_RXEVENT:
            data = data.concat( "<tr style=\"background-color:#E0E0E0;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time ) + "</td><td>" + ds.event + "</td><td></td><td></td><td></td><td>"
                + "</td><td></td><td></td><td></td></tr>" );
            break;
          case UARTProtocolAnalysisDataSet.UART_TYPE_TXEVENT:
            data = data.concat( "<tr style=\"background-color:#E0E0E0;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time ) + "</td><td>" + "</td><td></td><td></td><td></td><td>" + ds.event
                + "</td><td></td><td></td><td></td></tr>" );
            break;
          case UARTProtocolAnalysisDataSet.UART_TYPE_RXDATA:
            data = data.concat( "<tr style=\"background-color:#FFFFFF;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time ) + "</td><td>" + "0x"
                + DisplayUtils.integerToHexString( ds.data, bitCount / 4 + bitAdder ) + "</td><td>" + "0b"
                + DisplayUtils.integerToBinString( ds.data, bitCount ) + "</td><td>" + ds.data + "</td><td>" );

            if ( ( ds.data >= 32 ) && ( bitCount == 8 ) )
            {
              data += ( char )ds.data;
            }
            data = data.concat( "</td><td>" + "</td><td>" + "</td><td>" + "</td><td>" );
            data = data.concat( "</td></tr>" );

            break;
          case UARTProtocolAnalysisDataSet.UART_TYPE_TXDATA:
            data = data.concat( "<tr style=\"background-color:#FFFFFF;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time ) + "</td><td>" + "</td><td>" + "</td><td>" + "</td><td>" );

            data = data.concat( "</td><td>" + "0x" + DisplayUtils.integerToHexString( ds.data, bitCount / 4 + bitAdder )
                + "</td><td>" + "0b" + DisplayUtils.integerToBinString( ds.data, bitCount ) + "</td><td>" + ds.data
                + "</td><td>" );

            if ( ( ds.data >= 32 ) && ( bitCount == 8 ) )
            {
              data += ( char )ds.data;
            }
            data = data.concat( "</td></tr>" );

            break;
          default:
            break;
        }

      }
    }
    data = data.concat( "</table" );

    // generate the footer table
    String footer = "   </body>" + "</html>";

    return ( header + stats + data + footer );
  }
}
