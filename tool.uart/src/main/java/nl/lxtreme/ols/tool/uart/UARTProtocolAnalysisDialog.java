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
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
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

  private JComboBox rxd;
  private JComboBox txd;
  private JComboBox cts;
  private JComboBox rts;
  private JComboBox dtr;
  private JComboBox dsr;
  private JComboBox dcd;
  private JComboBox ri;
  private JComboBox parity;
  private JComboBox bits;
  private JComboBox stop;
  private JCheckBox inv;
  private JEditorPane outText;

  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public UARTProtocolAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

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
      final String htmlPage;
      if ( aAnalysisResult != null )
      {
        htmlPage = toHtmlPage( null /* aFile */, aAnalysisResult );
        this.exportAction.setEnabled( !aAnalysisResult.isEmpty() );
      }
      else
      {
        htmlPage = getEmptyHtmlPage();
        this.exportAction.setEnabled( false );
      }

      this.outText.setText( htmlPage );
      this.outText.setEditable( false );

      this.runAnalysisAction.restore();
    }
    catch ( final IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        // Should not happen in this situation!
        throw new RuntimeException( exception );
      }
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    SwingComponentUtils.setSelectedIndex( this.rxd, aSettings.getInt( "rxd", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.txd, aSettings.getInt( "txd", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.cts, aSettings.getInt( "cts", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.rts, aSettings.getInt( "rts", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.dtr, aSettings.getInt( "dtr", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.dsr, aSettings.getInt( "dsr", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.dcd, aSettings.getInt( "dcd", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.ri, aSettings.getInt( "ri", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.parity, aSettings.getInt( "parity", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.bits, aSettings.getInt( "bits", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.stop, aSettings.getInt( "stop", -1 ) );
    SwingComponentUtils.setSelected( this.inv, Boolean.valueOf( aSettings.getBoolean( "inverted", false ) ) );
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

    setControlsEnabled( true );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "rxd", this.rxd.getSelectedIndex() );
    aSettings.putInt( "txd", this.txd.getSelectedIndex() );
    aSettings.putInt( "cts", this.cts.getSelectedIndex() );
    aSettings.putInt( "rts", this.rts.getSelectedIndex() );
    aSettings.putInt( "dtr", this.dtr.getSelectedIndex() );
    aSettings.putInt( "dsr", this.dsr.getSelectedIndex() );
    aSettings.putInt( "dcd", this.dcd.getSelectedIndex() );
    aSettings.putInt( "ri", this.ri.getSelectedIndex() );
    aSettings.putInt( "parity", this.parity.getSelectedIndex() );
    aSettings.putInt( "bits", this.bits.getSelectedIndex() );
    aSettings.putInt( "stop", this.stop.getSelectedIndex() );
    aSettings.putBoolean( "inverted", this.inv.isSelected() );
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
    // The value at index zero is "Unused", so extracting one of all items
    // causes all "unused" values to be equivalent to -1, which is interpreted
    // as not used...
    aToolWorker.setRxdIndex( this.rxd.getSelectedIndex() - 1 );
    aToolWorker.setTxdIndex( this.txd.getSelectedIndex() - 1 );
    aToolWorker.setCtsIndex( this.cts.getSelectedIndex() - 1 );
    aToolWorker.setRtsIndex( this.rts.getSelectedIndex() - 1 );
    aToolWorker.setDcdIndex( this.dcd.getSelectedIndex() - 1 );
    aToolWorker.setRiIndex( this.ri.getSelectedIndex() - 1 );
    aToolWorker.setDsrIndex( this.dsr.getSelectedIndex() - 1 );
    aToolWorker.setDtrIndex( this.dtr.getSelectedIndex() - 1 );

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

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "RxD event", "TxD event",
          "RxD data", "TxD data" );

      final List<UARTData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final UARTData ds = decodedData.get( i );

        final String startTime = aDataSet.getDisplayTime( ds.getStartSampleIndex() );
        final String endTime = aDataSet.getDisplayTime( ds.getEndSampleIndex() );

        String eventType = null;
        String rxdEvent = null;
        String txdEvent = null;
        String rxdData = null;
        String txdData = null;

        switch ( ds.getType() )
        {
          case UARTData.UART_TYPE_EVENT:
            eventType = ds.getEventName();
            break;

          case UARTData.UART_TYPE_RXEVENT:
            rxdEvent = ds.getEventName();
            break;

          case UARTData.UART_TYPE_TXEVENT:
            txdEvent = ds.getEventName();
            break;

          case UARTData.UART_TYPE_RXDATA:
            rxdData = Integer.toString( ds.getData() );
            break;

          case UARTData.UART_TYPE_TXDATA:
            txdData = Integer.toString( ds.getData() );
            break;

          default:
            break;
        }

        exporter.addRow( Integer.valueOf( i ), startTime, endTime, Boolean.valueOf( ds.isEvent() ), eventType,
            rxdEvent, txdEvent, rxdData, txdData );
      }

      exporter.close();
    }
    catch ( final IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
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
    catch ( final IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
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
   * @return
   */
  private JPanel createPreviewPane()
  {
    final JPanel panTable = new JPanel( new GridLayout( 1, 1, 0, 0 ) );

    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setEditable( false );

    panTable.add( new JScrollPane( this.outText ) );

    return panTable;
  }

  /**
   * @return
   */
  private JPanel createSettingsPane()
  {
    final String channels[] = new String[33];
    channels[0] = "Unused";
    for ( int i = 0; i < 32; i++ )
    {
      channels[i + 1] = new String( "Channel " + i );
    }

    final JPanel settings = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( settings, "Settings" );

    settings.add( createRightAlignedLabel( "RxD" ) );
    this.rxd = new JComboBox( channels );
    settings.add( this.rxd );

    settings.add( createRightAlignedLabel( "TxD" ) );
    this.txd = new JComboBox( channels );
    settings.add( this.txd );

    settings.add( createRightAlignedLabel( "CTS" ) );
    this.cts = new JComboBox( channels );
    this.cts.setSelectedItem( "unused" );
    settings.add( this.cts );

    settings.add( createRightAlignedLabel( "RTS" ) );
    this.rts = new JComboBox( channels );
    this.rts.setSelectedItem( "unused" );
    settings.add( this.rts );

    settings.add( createRightAlignedLabel( "DTR" ) );
    this.dtr = new JComboBox( channels );
    this.dtr.setSelectedItem( "unused" );
    settings.add( this.dtr );

    settings.add( createRightAlignedLabel( "DSR" ) );
    this.dsr = new JComboBox( channels );
    this.dsr.setSelectedItem( "unused" );
    settings.add( this.dsr );

    settings.add( createRightAlignedLabel( "DCD" ) );
    this.dcd = new JComboBox( channels );
    this.dcd.setSelectedItem( "unused" );
    settings.add( this.dcd );

    settings.add( createRightAlignedLabel( "RI" ) );
    this.ri = new JComboBox( channels );
    this.ri.setSelectedItem( "unused" );
    settings.add( this.ri );

    settings.add( createRightAlignedLabel( "Parity" ) );
    final String[] parityarray = new String[] { "None", "Odd", "Even" };
    this.parity = new JComboBox( parityarray );
    settings.add( this.parity );

    settings.add( createRightAlignedLabel( "Bits" ) );
    final String[] bitarray = new String[4];
    for ( int i = 0; i < bitarray.length; i++ )
    {
      bitarray[i] = new String( "" + ( i + 5 ) );
    }
    this.bits = new JComboBox( bitarray );
    this.bits.setSelectedItem( "8" );
    settings.add( this.bits );

    settings.add( createRightAlignedLabel( "Stopbit" ) );
    final String[] stoparray = new String[] { "1", "1.5", "2" };
    this.stop = new JComboBox( stoparray );
    settings.add( this.stop );

    this.inv = new JCheckBox();
    settings.add( createRightAlignedLabel( "Invert?" ) );
    settings.add( this.inv );

    SpringLayoutUtils.makeEditorGrid( settings, 10, 4 );

    return settings;
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
   * Initializes this dialog.
   */
  private void initDialog()
  {
    setMinimumSize( new Dimension( 640, 480 ) );

    final JComponent settingsPane = createSettingsPane();
    final JComponent previewPane = createPreviewPane();

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.add( settingsPane, new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTH,
        GridBagConstraints.NONE, new Insets( 2, 0, 2, 0 ), 0, 0 ) );
    contentPane.add( previewPane, new GridBagConstraints( 1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH,
        GridBagConstraints.BOTH, new Insets( 2, 0, 2, 0 ), 0, 0 ) );

    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = createExportButton();
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = createCloseButton();
    this.closeAction = closeButton.getAction();

    final JComponent buttons = SwingComponentUtils.createButtonPane( runAnalysisButton, exportButton, closeButton );

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttons, runAnalysisButton );
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
          return Integer.valueOf( aDataSet.getDecodedSymbols() );
        }
        else if ( "detected-bus-errors".equals( aMacro ) )
        {
          return Integer.valueOf( aDataSet.getDetectedErrors() );
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
            baudrate = String.format( "%d (exact: %d)", Integer.valueOf( aDataSet.getBaudRate() ),
                Integer.valueOf( aDataSet.getBaudRateExact() ) );
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
                rxEventData = txEventData = ds.getEventName();
                bgColor = "#e0e0e0";
              }
              else if ( UARTData.UART_TYPE_RXEVENT == ds.getType() )
              {
                rxEventData = ds.getEventName();
                bgColor = "#c0ffc0";
              }
              else if ( UARTData.UART_TYPE_TXEVENT == ds.getType() )
              {
                txEventData = ds.getEventName();
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
              tr.addChild( TD ).addContent( aDataSet.getDisplayTime( ds.getStartSampleIndex() ) );
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
                final int rxData = ds.getData();

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
                final int txData = ds.getData();

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
              tr.addChild( TD ).addContent( aDataSet.getDisplayTime( ds.getStartSampleIndex() ) );
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
