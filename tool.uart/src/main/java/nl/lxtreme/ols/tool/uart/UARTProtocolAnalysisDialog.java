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


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;

import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * The Dialog Class
 * 
 * @author Frank Kunz The dialog class draws the basic dialog with a grid
 *         layout. The dialog consists of three main parts. A settings panel, a
 *         table panel and three buttons.
 */
public final class UARTProtocolAnalysisDialog extends BaseAsyncToolDialog<UARTDataSet, UARTAnalyserWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( UARTProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private final String[] parityarray;
  private final String[] bitarray;
  private final String[] stoparray;
  private final JComboBox rxd;
  private final JComboBox txd;
  private final JComboBox cts;
  private final JComboBox rts;
  private final JComboBox dtr;
  private final JComboBox dsr;
  private final JComboBox dcd;
  private final JComboBox ri;
  private final JComboBox parity;
  private final JComboBox bits;
  private final JComboBox stop;
  private final JCheckBox inv;
  private final JEditorPane outText;

  private final RunAnalysisAction runAnalysisAction;
  private final ExportAction exportAction;
  private final CloseAction closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public UARTProtocolAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    setMinimumSize( new Dimension( 640, 480 ) );

    setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

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

    add( panSettings, createConstraints( 0, 0, 1, 1, 0, 0 ) );

    /*
     * add an empty output view
     */
    JPanel panTable = new JPanel( new GridLayout( 1, 1, 5, 5 ) );
    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setMargin( new Insets( 5, 5, 5, 5 ) );
    panTable.add( new JScrollPane( this.outText ) );
    add( panTable, createConstraints( 1, 0, 1, 1, 1.0, 1.0 ) );

    /*
     * add buttons
     */
    this.runAnalysisAction = new RunAnalysisAction();
    JButton btnConvert = new JButton( this.runAnalysisAction );

    this.exportAction = new ExportAction();
    this.exportAction.setEnabled( false );
    JButton btnExport = new JButton( this.exportAction );

    this.closeAction = new CloseAction();
    JButton btnCancel = new JButton( this.closeAction );

    final JPanel buttons = new JPanel();
    final BoxLayout layoutMgr = new BoxLayout( buttons, BoxLayout.LINE_AXIS );
    buttons.setLayout( layoutMgr );
    buttons.add( Box.createHorizontalGlue() );
    buttons.add( btnConvert );
    buttons.add( btnExport );
    buttons.add( Box.createHorizontalStrut( 16 ) );
    buttons.add( btnCancel );

    add( buttons, //
        new GridBagConstraints( 0, 1, 2, 1, 1.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    pack();
  }

  // METHODS

  /**
   * @param x
   * @param y
   * @param w
   * @param h
   * @param wx
   * @param wy
   * @return
   */
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
   * This is the UART protocol decoder core The decoder scans for a decode start
   * event like CS high to low edge or the trigger of the captured data. After
   * this the decoder starts to decode the data by the selected mode, number of
   * bits and bit order. The decoded data are put to a JTable object directly.
   */
  @Override
  public void onToolWorkerReady( final UARTDataSet aAnalysisResult )
  {
    super.onToolWorkerReady( aAnalysisResult );

    try
    {
      this.outText.setText( toHtmlPage( null /* aFile */, aAnalysisResult ) );
      this.outText.setEditable( false );

      this.exportAction.setEnabled( !aAnalysisResult.isEmpty() );

      this.runAnalysisAction.restore();
      this.runAnalysisAction.setEnabled( false );
    }
    catch ( IOException exception )
    {
      // Should not happen in this situation!
      throw new RuntimeException( exception );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    SwingComponentUtils.setSelectedItem( this.rxd, aProperties.getProperty( "tools.UARTProtocolAnalysis.rxd" ) );
    SwingComponentUtils.setSelectedItem( this.txd, aProperties.getProperty( "tools.UARTProtocolAnalysis.txd" ) );
    SwingComponentUtils.setSelectedItem( this.cts, aProperties.getProperty( "tools.UARTProtocolAnalysis.cts" ) );
    SwingComponentUtils.setSelectedItem( this.rts, aProperties.getProperty( "tools.UARTProtocolAnalysis.rts" ) );
    SwingComponentUtils.setSelectedItem( this.dtr, aProperties.getProperty( "tools.UARTProtocolAnalysis.dtr" ) );
    SwingComponentUtils.setSelectedItem( this.dsr, aProperties.getProperty( "tools.UARTProtocolAnalysis.dsr" ) );
    SwingComponentUtils.setSelectedItem( this.dcd, aProperties.getProperty( "tools.UARTProtocolAnalysis.dcd" ) );
    SwingComponentUtils.setSelectedItem( this.ri, aProperties.getProperty( "tools.UARTProtocolAnalysis.ri" ) );
    SwingComponentUtils.setSelectedItem( this.parity, aProperties.getProperty( "tools.UARTProtocolAnalysis.parity" ) );
    SwingComponentUtils.setSelectedItem( this.bits, aProperties.getProperty( "tools.UARTProtocolAnalysis.bits" ) );
    SwingComponentUtils.setSelectedItem( this.stop, aProperties.getProperty( "tools.UARTProtocolAnalysis.stop" ) );
    this.inv.setSelected( Boolean.parseBoolean( aProperties.getProperty( "tools.UARTProtocolAnalysis.inverted" ) ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#reset()
   */
  @Override
  public void reset()
  {
    this.outText.setText( getEmptyHtmlPage() );
    this.outText.setEditable( false );

    this.exportAction.setEnabled( false );

    this.runAnalysisAction.restore();
    this.runAnalysisAction.setEnabled( true );

    setControlsEnabled( true );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    aProperties.setProperty( aNamespace + ".rxd", Integer.toString( this.rxd.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".txd", Integer.toString( this.txd.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".cts", Integer.toString( this.cts.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".rts", Integer.toString( this.rts.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".dtr", Integer.toString( this.dtr.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".dsr", Integer.toString( this.dsr.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".dcd", Integer.toString( this.dcd.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".ri", Integer.toString( this.ri.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".parity", ( String )this.parity.getSelectedItem() );
    aProperties.setProperty( aNamespace + ".bits", ( String )this.bits.getSelectedItem() );
    aProperties.setProperty( aNamespace + ".stop", ( String )this.stop.getSelectedItem() );
    aProperties.setProperty( aNamespace + ".inverted", "" + this.inv.isSelected() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#onToolWorkerSet(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void onToolWorkerSet( final UARTAnalyserWorker aToolWorker )
  {
    this.runAnalysisAction.setEnabled( aToolWorker.hasCapturedData() );
  }

  /**
   * set the controls of the dialog enabled/disabled
   * 
   * @param aEnable
   *          status of the controls
   */
  @Override
  protected void setControlsEnabled( final boolean aEnable )
  {
    this.rxd.setEnabled( aEnable );
    this.txd.setEnabled( aEnable );
    this.cts.setEnabled( aEnable );
    this.rts.setEnabled( aEnable );
    this.dtr.setEnabled( aEnable );
    this.dsr.setEnabled( aEnable );
    this.dcd.setEnabled( aEnable );
    this.ri.setEnabled( aEnable );
    this.parity.setEnabled( aEnable );
    this.bits.setEnabled( aEnable );
    this.stop.setEnabled( aEnable );
    this.inv.setEnabled( aEnable );

    this.closeAction.setEnabled( aEnable );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final UARTAnalyserWorker aToolWorker )
  {
    if ( !"unused".equals( this.rxd.getSelectedItem() ) )
    {
      aToolWorker.setRxdMask( 1 << this.rxd.getSelectedIndex() );
    }

    if ( !"unused".equals( this.txd.getSelectedItem() ) )
    {
      aToolWorker.setTxdMask( 1 << this.txd.getSelectedIndex() );
    }

    if ( !"unused".equals( this.cts.getSelectedItem() ) )
    {
      aToolWorker.setCtsMask( 1 << this.cts.getSelectedIndex() );
    }

    if ( !"unused".equals( this.rts.getSelectedItem() ) )
    {
      aToolWorker.setRtsMask( 1 << this.rts.getSelectedIndex() );
    }

    if ( !"unused".equals( this.dcd.getSelectedItem() ) )
    {
      aToolWorker.setDcdMask( 1 << this.dcd.getSelectedIndex() );
    }

    if ( !"unused".equals( this.ri.getSelectedItem() ) )
    {
      aToolWorker.setRiMask( 1 << this.ri.getSelectedIndex() );
    }

    if ( !"unused".equals( this.dsr.getSelectedItem() ) )
    {
      aToolWorker.setDsrMask( 1 << this.dsr.getSelectedIndex() );
    }

    if ( !"unused".equals( this.dtr.getSelectedItem() ) )
    {
      aToolWorker.setDtrMask( 1 << this.dtr.getSelectedIndex() );
    }

    // Other properties...
    aToolWorker.setInverted( this.inv.isSelected() );
    aToolWorker.setParity( UARTParity.parse( this.parity.getSelectedItem() ) );
    aToolWorker.setStopBits( UARTStopBits.parse( this.stop.getSelectedItem() ) );
    aToolWorker.setBitCount( NumberUtils.smartParseInt( ( String )this.bits.getSelectedItem(), 8 ) );
  }

  /**
   * exports the data to a CSV file
   * 
   * @param aFile
   *          File object
   */
  @Override
  protected void storeToCsvFile( final File aFile, final UARTDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "time", "event?", "event-type", "RxD event", "TxD event", "RxD data", "TxD data" );

      final List<UARTData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final UARTData ds = decodedData.get( i );

        String eventType = null;
        String rxdEvent = null;
        String txdEvent = null;
        String rxdData = null;
        String txdData = null;

        switch ( ds.getType() )
        {
          case UARTData.UART_TYPE_EVENT:
            eventType = ds.getEvent();
            break;

          case UARTData.UART_TYPE_RXEVENT:
            rxdEvent = ds.getEvent();
            break;

          case UARTData.UART_TYPE_TXEVENT:
            txdEvent = ds.getEvent();
            break;

          case UARTData.UART_TYPE_RXDATA:
            rxdData = ds.getEvent();
            break;

          case UARTData.UART_TYPE_TXDATA:
            txdData = ds.getEvent();
            break;

          default:
            break;
        }

        exporter.addRow( i, ds.getTimeDisplayValue(), ds.isEvent(), eventType, rxdEvent, txdEvent, rxdData, txdData );
      }

      exporter.close();
    }
    catch ( IOException exception )
    {
      if ( LOG.isLoggable( Level.WARNING ) )
      {
        LOG.log( Level.WARNING, "CSV export failed!", exception );
      }
    }
  }

  /**
   * stores the data to a HTML file
   * 
   * @param aFile
   *          file object
   */
  @Override
  protected void storeToHtmlFile( final File aFile, final UARTDataSet aDataSet )
  {
    try
    {
      toHtmlPage( aFile, aDataSet );
    }
    catch ( IOException exception )
    {
      if ( LOG.isLoggable( Level.WARNING ) )
      {
        LOG.log( Level.WARNING, "HTML export failed!", exception );
      }
    }
  }

  /**
   * Creates the HTML template for exports to HTML.
   * 
   * @param aExporter
   *          the HTML exporter instance to use, cannot be <code>null</code>.
   * @return a HTML exporter filled with the template, never <code>null</code>.
   */
  private HtmlExporter createHtmlTemplate( final HtmlExporter aExporter )
  {
    aExporter.addCssStyle( "body { font-family: sans-serif; } " );
    aExporter.addCssStyle( "table { border-width: 1px; border-spacing: 0px; border-color: gray;"
        + " border-collapse: collapse; border-style: solid; margin-bottom: 15px; } " );
    aExporter.addCssStyle( "table th { border-width: 1px; padding: 2px; border-style: solid; border-color: gray;"
        + " background-color: #C0C0FF; text-align: left; font-weight: bold; font-family: sans-serif; } " );
    aExporter.addCssStyle( "table td { border-width: 1px; padding: 2px; border-style: solid; border-color: gray;"
        + " font-family: monospace; } " );
    aExporter.addCssStyle( ".error { color: red; } " );
    aExporter.addCssStyle( ".warning { color: orange; } " );
    aExporter.addCssStyle( ".date { text-align: right; font-size: x-small; margin-bottom: 15px; } " );
    aExporter.addCssStyle( ".w100 { width: 100%; } " );
    aExporter.addCssStyle( ".w35 { width: 35%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".w8 { width: 8%; } " );
    aExporter.addCssStyle( ".w7 { width: 7%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "UART Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "Generated: ", "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );

    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Statistics" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Decoded bytes" );
    tr.addChild( TD ).addContent( "{decoded-bytes}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Detected bus errors" );
    tr.addChild( TD ).addContent( "{detected-bus-errors}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Baudrate" );
    tr.addChild( TD ).addContent( "{baudrate}" );

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addAttribute( "colspan", "2" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "RxD" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "TxD" );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w15" ).addContent( "Index" );
    tr.addChild( TH ).addAttribute( "class", "w15" ).addContent( "Time" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Hex" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Bin" );
    tr.addChild( TH ).addAttribute( "class", "w8" ).addContent( "Dec" );
    tr.addChild( TH ).addAttribute( "class", "w7" ).addContent( "ASCII" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Hex" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Bin" );
    tr.addChild( TH ).addAttribute( "class", "w8" ).addContent( "Dec" );
    tr.addChild( TH ).addAttribute( "class", "w7" ).addContent( "ASCII" );
    tbody = table.addChild( TBODY );
    tbody.addContent( "{decoded-data}" );

    return aExporter;
  }

  /**
   * generate a HTML page
   * 
   * @param empty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String getEmptyHtmlPage()
  {
    final HtmlExporter exporter = createHtmlTemplate( ExportUtils.createHtmlExporter() );
    return exporter.toString( new MacroResolver()
    {
      @Override
      public Object resolve( final String aMacro, final Element aParent )
      {
        if ( "date-now".equals( aMacro ) )
        {
          final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );
          return df.format( new Date() );
        }
        else if ( "decoded-bytes".equals( aMacro ) || "detected-bus-errors".equals( aMacro )
            || "baudrate".equals( aMacro ) )
        {
          return "-";
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          return null;
        }
        return null;
      }
    } );
  }

  /**
   * generate a HTML page
   * 
   * @param empty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final UARTDataSet aDataSet ) throws IOException
  {
    final int bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() );
    final int bitAdder = ( bitCount % 4 != 0 ) ? 1 : 0;

    final MacroResolver macroResolver = new MacroResolver()
    {
      @Override
      public Object resolve( final String aMacro, final Element aParent )
      {
        if ( "date-now".equals( aMacro ) )
        {
          final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );
          return df.format( new Date() );
        }
        else if ( "decoded-bytes".equals( aMacro ) )
        {
          return aDataSet.getDecodedSymbols();
        }
        else if ( "detected-bus-errors".equals( aMacro ) )
        {
          return aDataSet.getDetectedErrors();
        }
        else if ( "baudrate".equals( aMacro ) )
        {
          final String baudrate;
          if ( aDataSet.getBitLength() <= 0 )
          {
            baudrate = "<span class='error'>Baudrate calculation failed!</span>";
          }
          else
          {
            baudrate = String.format( "%d", aDataSet.getBaudRate() );
            if ( aDataSet.getBitLength() < 15 )
            {
              return baudrate
                  .concat( " <span class='warning'>The baudrate may be wrong, use a higher samplerate to avoid this!</span>" );
            }

            return baudrate;
          }
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<UARTData> decodedData = aDataSet.getData();
          Element tr;

          for ( int i = 0; i < decodedData.size(); i++ )
          {
            final UARTData ds = decodedData.get( i );

            if ( ds.isEvent() )
            {
              String rxEventData = "";
              String txEventData = "";

              String bgColor;
              if ( UARTData.UART_TYPE_EVENT == ds.getType() )
              {
                rxEventData = txEventData = ds.getEvent();
                bgColor = "#e0e0e0";
              }
              else if ( UARTData.UART_TYPE_RXEVENT == ds.getType() )
              {
                rxEventData = ds.getEvent();
                bgColor = "#c0ffc0";
              }
              else if ( UARTData.UART_TYPE_TXEVENT == ds.getType() )
              {
                txEventData = ds.getEvent();
                bgColor = "#c0ffc0";
              }
              else
              {
                // unknown event
                bgColor = "#ff8000";
              }

              if ( txEventData.endsWith( "_ERR" ) || rxEventData.endsWith( "_ERR" ) )
              {
                bgColor = "#ff8000";
              }

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( ds.getTimeDisplayValue() );
              tr.addChild( TD ).addContent( rxEventData );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD ).addContent( txEventData );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else
            {
              String rxDataHex = "", rxDataBin = "", rxDataDec = "", rxDataASCII = "";
              String txDataHex = "", txDataBin = "", txDataDec = "", txDataASCII = "";

              // Normal data...
              if ( UARTData.UART_TYPE_RXDATA == ds.getType() )
              {
                int rxData = ds.getData();

                rxDataHex = "0x" + DisplayUtils.integerToHexString( rxData, bitCount / 4 + bitAdder );
                rxDataBin = "0b" + DisplayUtils.integerToBinString( rxData, bitCount );
                rxDataDec = String.valueOf( rxData );
                if ( ( rxData >= 32 ) && ( bitCount == 8 ) )
                {
                  rxDataASCII = String.valueOf( ( char )rxData );
                }
              }
              else
              /* if ( UARTData.UART_TYPE_TXDATA == ds.getType() ) */
              {
                int txData = ds.getData();

                txDataHex = "0x" + DisplayUtils.integerToHexString( txData, bitCount / 4 + bitAdder );
                txDataBin = "0b" + DisplayUtils.integerToBinString( txData, bitCount );
                txDataDec = String.valueOf( txData );
                if ( ( txData >= 32 ) && ( bitCount == 8 ) )
                {
                  txDataASCII = String.valueOf( ( char )txData );
                }
              }

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( ds.getTimeDisplayValue() );
              tr.addChild( TD ).addContent( rxDataHex );
              tr.addChild( TD ).addContent( rxDataBin );
              tr.addChild( TD ).addContent( rxDataDec );
              tr.addChild( TD ).addContent( rxDataASCII );
              tr.addChild( TD ).addContent( txDataHex );
              tr.addChild( TD ).addContent( txDataBin );
              tr.addChild( TD ).addContent( txDataDec );
              tr.addChild( TD ).addContent( txDataASCII );
            }
          }
        }
        return null;
      }
    };

    if ( aFile == null )
    {
      final HtmlExporter exporter = createHtmlTemplate( ExportUtils.createHtmlExporter() );
      return exporter.toString( macroResolver );
    }
    else
    {
      final HtmlFileExporter exporter = ( HtmlFileExporter )createHtmlTemplate( ExportUtils.createHtmlExporter( aFile ) );
      exporter.write( macroResolver );
      exporter.close();
    }

    return null;
  }
}
