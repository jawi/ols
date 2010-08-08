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


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * The Dialog Class
 * 
 * @author Frank Kunz The dialog class draws the basic dialog with a grid
 *         layout. The dialog consists of three main parts. A settings panel, a
 *         table panel and three buttons.
 */
final class SPIProtocolAnalysisDialog extends JDialog implements ActionListener, Runnable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String[] modearray;
  private final String[] bitarray;
  private final String[] orderarray;
  private final JComboBox sck;
  private final JComboBox miso;
  private final JComboBox mosi;
  private final JComboBox cs;
  private final JComboBox mode;
  private final JComboBox bits;
  private AnnotatedData analysisData;
  private final JEditorPane outText;
  private final JComboBox order;
  private final Vector<SPIProtocolAnalysisDataSet> decodedData;
  private final JFileChooser fileChooser;
  private int startOfDecode;
  private int endOfDecode;
  private final JButton btnConvert;
  private final JButton btnExport;
  private final JButton btnCancel;
  private final JProgressBar progress;
  private boolean runFlag;
  private Thread thrWorker;

  // CONSTRUCTORS

  public SPIProtocolAnalysisDialog( final Frame frame, final String name )
  {
    super( frame, name, true );
    Container pane = getContentPane();
    pane.setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    this.decodedData = new Vector<SPIProtocolAnalysisDataSet>();
    this.startOfDecode = 0;

    /*
     * add protocol settings elements
     */
    JPanel panSettings = new JPanel();
    panSettings.setLayout( new GridLayout( 7, 2, 5, 5 ) );
    panSettings.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    String channels[] = new String[32];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = new String( "Channel " + i );
    }

    panSettings.add( new JLabel( "SCK" ) );
    this.sck = new JComboBox( channels );
    panSettings.add( this.sck );

    panSettings.add( new JLabel( "MISO" ) );
    this.miso = new JComboBox( channels );
    panSettings.add( this.miso );

    panSettings.add( new JLabel( "MOSI" ) );
    this.mosi = new JComboBox( channels );
    panSettings.add( this.mosi );

    panSettings.add( new JLabel( "/CS" ) );
    this.cs = new JComboBox( channels );
    panSettings.add( this.cs );

    panSettings.add( new JLabel( "Mode" ) );
    this.modearray = new String[4];
    for ( int i = 0; i < this.modearray.length; i++ )
    {
      this.modearray[i] = new String( "" + i );
    }
    this.mode = new JComboBox( this.modearray );
    panSettings.add( this.mode );

    panSettings.add( new JLabel( "Bits" ) );
    this.bitarray = new String[13];
    for ( int i = 0; i < this.bitarray.length; i++ )
    {
      this.bitarray[i] = new String( "" + ( i + 4 ) );
    }
    this.bits = new JComboBox( this.bitarray );
    this.bits.setSelectedItem( "8" );
    panSettings.add( this.bits );

    panSettings.add( new JLabel( "Order" ) );
    this.orderarray = new String[2];
    this.orderarray[0] = new String( "MSB first" );
    this.orderarray[1] = new String( "LSB first" );
    this.order = new JComboBox( this.orderarray );
    panSettings.add( this.order );
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
    add( panProgress, createConstraints( 0, 3, 4, 1, 1.0, 0 ) );

    /*
     * add buttons
     */
    this.btnConvert = new JButton( "Analyze" );
    this.btnConvert.addActionListener( this );
    add( this.btnConvert, createConstraints( 0, 4, 1, 1, 0.5, 0 ) );
    this.btnExport = new JButton( "Export" );
    this.btnExport.addActionListener( this );
    add( this.btnExport, createConstraints( 1, 4, 1, 1, 0.5, 0 ) );
    this.btnCancel = new JButton( "Close" );
    this.btnCancel.addActionListener( this );
    add( this.btnCancel, createConstraints( 2, 4, 1, 1, 0.5, 0 ) );

    this.fileChooser = new JFileChooser();
    // this.fileChooser.addChoosableFileFilter( ( FileFilter )new CSVFilter() );
    // this.fileChooser.addChoosableFileFilter( ( FileFilter )new HTMLFilter()
    // );

    setSize( 1000, 500 );
    setResizable( false );
    this.thrWorker = null;
    this.runFlag = false;
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

  public void readProperties( final String aNamespace, final Properties properties )
  {
    SwingComponentUtils.setSelectedItem( this.sck, properties.getProperty( aNamespace + ".sck" ) );
    SwingComponentUtils.setSelectedItem( this.miso, properties.getProperty( aNamespace + ".miso" ) );
    SwingComponentUtils.setSelectedItem( this.mosi, properties.getProperty( aNamespace + ".mosi" ) );
    SwingComponentUtils.setSelectedItem( this.cs, properties.getProperty( aNamespace + ".cs" ) );
    SwingComponentUtils.setSelectedItem( this.mode, this.modearray, properties.getProperty( aNamespace + ".mode" ) );
    SwingComponentUtils.setSelectedItem( this.bits, this.bitarray, properties.getProperty( aNamespace + ".bits" ) );
    SwingComponentUtils.setSelectedItem( this.order, this.orderarray, properties.getProperty( aNamespace + ".order" ) );
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
  public void showDialog( final AnnotatedData data )
  {
    this.analysisData = data;
    setVisible( true );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties properties )
  {
    properties.setProperty( aNamespace + ".sck", Integer.toString( this.sck.getSelectedIndex() ) );
    properties.setProperty( aNamespace + ".miso", Integer.toString( this.miso.getSelectedIndex() ) );
    properties.setProperty( aNamespace + ".mosi", Integer.toString( this.mosi.getSelectedIndex() ) );
    properties.setProperty( aNamespace + ".cs", Integer.toString( this.cs.getSelectedIndex() ) );
    properties.setProperty( aNamespace + ".mode", ( String )this.mode.getSelectedItem() );
    properties.setProperty( aNamespace + ".bits", ( String )this.bits.getSelectedItem() );
    properties.setProperty( aNamespace + ".order", ( String )this.order.getSelectedItem() );
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
    return this.analysisData.calculateTime( time );
  }

  /**
   * This is the SPI protocol decoder core The decoder scans for a decode start
   * event like CS high to low edge or the trigger of the captured data. After
   * this the decoder starts to decode the data by the selected mode, number of
   * bits and bit order. The decoded data are put to a JTable object directly.
   */
  private void decode()
  {
    // process the captured data and write to output
    int a, c;
    int bitCount, mosivalue, misovalue, maxbits;

    // clear old data
    this.decodedData.clear();

    /*
     * Buid bitmasks based on the SCK, MISO, MOSI and CS pins.
     */
    int csmask = ( 1 << this.cs.getSelectedIndex() );
    int sckmask = ( 1 << this.sck.getSelectedIndex() );
    int misomask = ( 1 << this.miso.getSelectedIndex() );
    int mosimask = ( 1 << this.mosi.getSelectedIndex() );

    System.out.println( "csmask   = 0x" + Integer.toHexString( csmask ) );
    System.out.println( "sckmask  = 0x" + Integer.toHexString( sckmask ) );
    System.out.println( "misomask = 0x" + Integer.toHexString( misomask ) );
    System.out.println( "mosimask = 0x" + Integer.toHexString( mosimask ) );

    final int[] values = this.analysisData.getValues();
    final long[] timestamps = this.analysisData.getTimestamps();

    this.startOfDecode = 0;
    this.endOfDecode = values.length;
    if ( this.analysisData.isCursorsEnabled() )
    {
      this.startOfDecode = this.analysisData.getSampleIndex( this.analysisData.getCursorPosition( 1 ) );
      this.endOfDecode = this.analysisData.getSampleIndex( this.analysisData.getCursorPosition( 2 ) + 1 );
    }
    else
    {
      /*
       * For analyze scan the CS line for a falling edge. If no edge could be
       * found, the position of the trigger is used for start of analysis. If no
       * trigger and no edge is found the analysis fails.
       */
      a = values[0] & csmask;
      c = 0;
      for ( int i = this.startOfDecode; i < this.endOfDecode; i++ )
      {
        if ( a > ( values[i] & csmask ) )
        {
          // cs to low found here
          this.startOfDecode = i;
          c = 1;
          System.out.println( "CS found at " + i );
          break;
        }
        a = values[i] & csmask;

        if ( this.runFlag == false )
        {
          return;
        }
        this.progress.setValue( ( int )( timestamps[i] * 100 / ( this.endOfDecode - this.startOfDecode ) ) );
      }
      if ( c == 0 )
      {
        // no CS edge found, look for trigger
        if ( this.analysisData.hasTriggerData() )
        {
          this.startOfDecode = this.analysisData.getSampleIndex( this.analysisData.getTriggerPosition() );
        }
      }
      // now the trigger is in b, add trigger event to table
      this.decodedData.addElement( new SPIProtocolAnalysisDataSet( this.startOfDecode, "CSLOW" ) );
    }

    /*
     * Use the mode parameter to determine which edges are to detect. Mode 0 and
     * mode 3 are sampling on the rising clk edge, mode 2 and 4 are sampling on
     * the falling edge. a is used for start of value, c is register for detect
     * line changes.
     */
    if ( ( this.mode.getSelectedItem().equals( "0" ) ) || ( this.mode.getSelectedItem().equals( "2" ) ) )
    {
      // scanning for rising clk edges
      c = values[this.startOfDecode] & sckmask;
      a = values[this.startOfDecode] & csmask;
      bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1;
      maxbits = bitCount;
      misovalue = 0;
      mosivalue = 0;
      for ( int i = this.startOfDecode; i < this.endOfDecode; i++ )
      {
        if ( c < ( values[i] & sckmask ) )
        {
          // sample here
          if ( this.order.getSelectedItem().equals( "MSB first" ) )
          {
            if ( ( values[i] & misomask ) == misomask )
            {
              misovalue |= ( 1 << bitCount );
            }
            if ( ( values[i] & mosimask ) == mosimask )
            {
              mosivalue |= ( 1 << bitCount );
            }
          }
          else
          {
            if ( ( values[i] & misomask ) == misomask )
            {
              misovalue |= ( 1 << ( maxbits - bitCount ) );
            }
            if ( ( values[i] & mosimask ) == mosimask )
            {
              mosivalue |= ( 1 << ( maxbits - bitCount ) );
            }
          }

          if ( bitCount > 0 )
          {
            bitCount--;
          }
          else
          {
            this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), mosivalue,
                misovalue ) );

            // System.out.println("MISO = 0x" + Integer.toHexString(misovalue));
            // System.out.println("MOSI = 0x" + Integer.toHexString(mosivalue));
            bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1;
            misovalue = 0;
            mosivalue = 0;

          }
        }
        c = values[i] & sckmask;

        /* CS edge detection */
        if ( a > ( values[i] & csmask ) )
        {
          // falling edge
          this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), "CSLOW" ) );
        }
        else if ( a < ( values[i] & csmask ) )
        {
          // rising edge
          this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), "CSHIGH" ) );
        }
        a = values[i] & csmask;

        if ( this.runFlag == false )
        {
          return;
        }
        this.progress.setValue( ( int )( timestamps[i] * 100 / ( this.endOfDecode - this.startOfDecode ) ) );
      }
    }
    else
    {
      // scanning for falling clk edges
      c = values[this.startOfDecode] & sckmask;
      a = values[this.startOfDecode] & csmask;
      bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1;
      maxbits = bitCount;
      misovalue = 0;
      mosivalue = 0;
      for ( int i = this.startOfDecode; i < this.endOfDecode; i++ )
      {
        if ( c > ( values[i] & sckmask ) )
        {
          // sample here
          if ( this.order.getSelectedItem().equals( "MSB first" ) )
          {
            if ( ( values[i] & misomask ) == misomask )
            {
              misovalue |= ( 1 << bitCount );
            }
            if ( ( values[i] & mosimask ) == mosimask )
            {
              mosivalue |= ( 1 << bitCount );
            }
          }
          else
          {
            if ( ( values[i] & misomask ) == misomask )
            {
              misovalue |= ( 1 << ( maxbits - bitCount ) );
            }
            if ( ( values[i] & mosimask ) == mosimask )
            {
              mosivalue |= ( 1 << ( maxbits - bitCount ) );
            }
          }

          if ( bitCount > 0 )
          {
            bitCount--;
          }
          else
          {
            this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), mosivalue,
                misovalue ) );

            // System.out.println("MISO = 0x" + Integer.toHexString(misovalue));
            // System.out.println("MOSI = 0x" + Integer.toHexString(mosivalue));
            bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1;
            misovalue = 0;
            mosivalue = 0;
          }
        }
        c = values[i] & sckmask;

        /* CS edge detection */
        if ( a > ( values[i] & csmask ) )
        {
          // falling edge
          this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), "CSLOW" ) );
        }
        else if ( a < ( values[i] & csmask ) )
        {
          // rising edge
          this.decodedData.addElement( new SPIProtocolAnalysisDataSet( calculateTime( timestamps[i] ), "CSHIGH" ) );
        }
        a = values[i] & csmask;

        if ( this.runFlag == false )
        {
          return;
        }
        this.progress.setValue( ( int )( timestamps[i] * 100 / ( this.endOfDecode - this.startOfDecode ) ) );
      }
    }

    this.outText.setText( toHtmlPage( false ) );
    this.outText.setEditable( false );
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
      return DisplayUtils.displayScaledTime( count, this.analysisData.getSampleRate() );
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
    this.sck.setEnabled( enable );
    this.miso.setEnabled( enable );
    this.mosi.setEnabled( enable );
    this.cs.setEnabled( enable );
    this.mode.setEnabled( enable );
    this.bits.setEnabled( enable );
    this.order.setEnabled( enable );
    this.btnExport.setEnabled( enable );
    this.btnCancel.setEnabled( enable );
  }

  /**
   * exports the table data to a CSV file
   * 
   * @param file
   *          File object
   */
  private void storeToCsvFile( final File file )
  {
    if ( this.decodedData.size() > 0 )
    {
      SPIProtocolAnalysisDataSet dSet;
      System.out.println( "writing decoded data to " + file.getPath() );
      try
      {
        BufferedWriter bw = new BufferedWriter( new FileWriter( file ) );

        bw.write( "\"" + "index" + "\",\"" + "time" + "\",\"" + "mosi data or event" + "\",\"" + "miso data or event"
            + "\"" );
        bw.newLine();

        for ( int i = 0; i < this.decodedData.size(); i++ )
        {
          dSet = this.decodedData.get( i );
          if ( dSet.isEvent() )
          {
            bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + dSet.event + "\",\"" + dSet.event
                + "\"" );
          }
          else
          {
            bw.write( "\"" + i + "\",\"" + indexToTime( dSet.time ) + "\",\"" + dSet.mosi + "\",\"" + dSet.miso + "\"" );
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
      + "       </style>" + "  </head>" + "   <body>" + "       <H2>SPI Analysis Results</H2>" + "       <hr>"
      + "           <div style=\"text-align:right;font-size:x-small;\">" + df.format( now ) + "           </div>"
      + "       <br>";

    // generate the data table
    String data = "<table style=\"font-family:monospace;width:100%;\">"
      + "<tr><th style=\"width:15%;\">Index</th><th style=\"width:15%;\">Time</th><th style=\"width:10%;\">MOSI Hex</th><th style=\"width:10%;\">MOSI Bin</th><th style=\"width:8%;\">MOSI Dec</th><th style=\"width:7%;\">MOSI ASCII</th><th style=\"width:10%;\">MISO Hex</th><th style=\"width:10%;\">MISO Bin</th><th style=\"width:8%;\">MISO Dec</th><th style=\"width:7%;\">MISO ASCII</th></tr>";
    if ( empty )
    {
    }
    else
    {
      SPIProtocolAnalysisDataSet ds;
      for ( int i = 0; i < this.decodedData.size(); i++ )
      {
        ds = this.decodedData.get( i );
        if ( ds.isEvent() )
        {
          // this is an event
          if ( ds.event.equals( "CSLOW" ) )
          {
            // start condition
            data = data.concat( "<tr style=\"background-color:#E0E0E0;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time )
                + "</td><td>CSLOW</td><td></td><td></td><td></td><td>CSLOW</td><td></td><td></td><td></td></tr>" );
          }
          else if ( ds.event.equals( "CSHIGH" ) )
          {
            // stop condition
            data = data.concat( "<tr style=\"background-color:#E0E0E0;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time )
                + "</td><td>CSHIGH</td><td></td><td></td><td></td><td>CSHIGH</td><td></td><td></td><td></td></tr>" );
          }
          else
          {
            // unknown event
            data = data.concat( "<tr style=\"background-color:#FF8000;\"><td>" + i + "</td><td>"
                + indexToTime( ds.time )
                + "</td><td>UNKNOWN</td><td></td><td></td><td></td><td>UNKNOWN</td><td></td><td></td><td></td></tr>" );
          }
        }
        else
        {
          data = data.concat( "<tr style=\"background-color:#FFFFFF;\"><td>" + i + "</td><td>" + indexToTime( ds.time )
              + "</td><td>" + "0x" + DisplayUtils.integerToHexString( ds.mosi, bitCount / 4 + bitAdder ) + "</td><td>"
              + "0b" + DisplayUtils.integerToBinString( ds.mosi, bitCount ) + "</td><td>" + ds.mosi + "</td><td>" );
          if ( ( ds.mosi >= 32 ) && ( bitCount == 8 ) )
          {
            data += ( char )ds.mosi;
          }
          data = data.concat( "</td><td>" + "0x" + DisplayUtils.integerToHexString( ds.miso, bitCount / 4 + bitAdder )
              + "</td><td>" + "0b" + DisplayUtils.integerToBinString( ds.miso, bitCount ) + "</td><td>" + ds.miso
              + "</td><td>" );
          if ( ( ds.miso >= 32 ) && ( bitCount == 8 ) )
          {
            data += ( char )ds.miso;
          }
          data = data.concat( "</td></tr>" );
        }
      }
    }
    data = data.concat( "</table" );

    // generate the footer table
    String footer = "   </body>" + "</html>";

    return ( header + data + footer );
  }
}
