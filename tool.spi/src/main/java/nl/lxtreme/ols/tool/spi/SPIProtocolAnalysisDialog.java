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


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;

import java.awt.*;
import java.awt.event.*;
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
public final class SPIProtocolAnalysisDialog extends BaseAsyncToolDialog<SPIDataSet, SPIAnalyserWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SPIProtocolAnalysisDialog.class.getName() );

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
  private final JEditorPane outText;
  private final JComboBox order;

  private final RunAnalysisAction runAnalysisAction;
  private final ExportAction exportAction;
  private final CloseAction closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public SPIProtocolAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    setMinimumSize( new Dimension( 640, 480 ) );

    setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

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

  public void actionPerformed( final ActionEvent e )
  {
  }

  /**
   * This is the SPI protocol decoder core The decoder scans for a decode start
   * event like CS high to low edge or the trigger of the captured data. After
   * this the decoder starts to decode the data by the selected mode, number of
   * bits and bit order. The decoded data are put to a JTable object directly.
   */
  @Override
  public void onToolWorkerReady( final SPIDataSet aAnalysisResult )
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
    SwingComponentUtils.setSelectedIndex( this.sck, aProperties.getProperty( aNamespace + ".sck" ) );
    SwingComponentUtils.setSelectedIndex( this.miso, aProperties.getProperty( aNamespace + ".miso" ) );
    SwingComponentUtils.setSelectedIndex( this.mosi, aProperties.getProperty( aNamespace + ".mosi" ) );
    SwingComponentUtils.setSelectedIndex( this.cs, aProperties.getProperty( aNamespace + ".cs" ) );
    SwingComponentUtils.setSelectedIndex( this.mode, aProperties.getProperty( aNamespace + ".mode" ) );
    SwingComponentUtils.setSelectedIndex( this.bits, aProperties.getProperty( aNamespace + ".bits" ) );
    SwingComponentUtils.setSelectedIndex( this.order, aProperties.getProperty( aNamespace + ".order" ) );
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
    aProperties.setProperty( aNamespace + ".sck", Integer.toString( this.sck.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".miso", Integer.toString( this.miso.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".mosi", Integer.toString( this.mosi.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".cs", Integer.toString( this.cs.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".mode", Integer.toString( this.mode.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".bits", Integer.toString( this.bits.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".order", Integer.toString( this.order.getSelectedIndex() ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#onToolWorkerSet(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void onToolWorkerSet( final SPIAnalyserWorker aToolWorker )
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
    this.sck.setEnabled( aEnable );
    this.miso.setEnabled( aEnable );
    this.mosi.setEnabled( aEnable );
    this.cs.setEnabled( aEnable );
    this.mode.setEnabled( aEnable );
    this.bits.setEnabled( aEnable );
    this.order.setEnabled( aEnable );

    this.closeAction.setEnabled( aEnable );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final SPIAnalyserWorker aToolWorker )
  {
    aToolWorker.setBitCount( Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1 );
    aToolWorker.setCSMask( 1 << this.cs.getSelectedIndex() );
    aToolWorker.setSCKMask( 1 << this.sck.getSelectedIndex() );
    aToolWorker.setMisoMask( 1 << this.miso.getSelectedIndex() );
    aToolWorker.setMosiMask( 1 << this.mosi.getSelectedIndex() );
    aToolWorker.setOrder( "MSB first".equals( this.order.getSelectedItem() ) ? Endianness.MSB_FIRST
        : Endianness.LSB_FIRST );
    aToolWorker.setMode( SPIMode.parse( ( String )this.mode.getSelectedItem() ) );
  }

  /**
   * exports the table data to a CSV file
   * 
   * @param aFile
   *          File object
   */
  @Override
  protected void storeToCsvFile( final File aFile, final SPIDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "time", "event?", "event-type", "MOSI data", "MISO data" );

      final List<SPIData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final SPIData ds = decodedData.get( i );

        exporter
            .addRow( i, ds.getTimeDisplayValue(), ds.isEvent(), ds.getEvent(), ds.getMoSiValue(), ds.getMiSoValue() );
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
  protected void storeToHtmlFile( final File aFile, final SPIDataSet aDataSet )
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
    aExporter.addCssStyle( ".date { text-align: right; font-size: x-small; margin-bottom: 15px; } " );
    aExporter.addCssStyle( ".w100 { width: 100%; } " );
    aExporter.addCssStyle( ".w35 { width: 35%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".w8 { width: 8%; } " );
    aExporter.addCssStyle( ".w7 { width: 7%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "SPI Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "Generated: ", "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addAttribute( "colspan", "2" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "MOSI" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "MISO" );
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
   * Generates an empty HTML page.
   * 
   * @return String with HTML data.
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
        return null;
      }
    } );
  }

  /**
   * generate a HTML page
   * 
   * @param aDataSet
   *          the data set to create the HTML page for, cannot be
   *          <code>null</code>.
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final SPIDataSet aDataSet ) throws IOException
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
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<SPIData> decodedData = aDataSet.getData();
          Element tr;

          for ( int i = 0; i < decodedData.size(); i++ )
          {
            final SPIData ds = decodedData.get( i );

            if ( ds.isEvent() )
            {
              String event;
              String bgColor;

              // this is an event
              if ( SPIDataSet.SPI_CS_LOW.equals( ds.getEvent() ) )
              {
                // start condition
                event = ds.getEvent();
                bgColor = "#c0ffc0";
              }
              else if ( SPIDataSet.SPI_CS_HIGH.equals( ds.getEvent() ) )
              {
                // stop condition
                event = ds.getEvent();
                bgColor = "#e0e0e0";
              }
              else
              {
                // unknown event
                event = "UNKNOWN";
                bgColor = "#ff8000";
              }

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( ds.getTimeDisplayValue() );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else
            {
              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( ds.getTimeDisplayValue() );

              final int mosiValue = ds.getMoSiValue();
              final String mosiDataHex = DisplayUtils.integerToHexString( mosiValue, bitCount / 4 + bitAdder );
              final String mosiDataBin = DisplayUtils.integerToBinString( mosiValue, bitCount );
              final String mosiDataDec = String.valueOf( mosiValue );
              final String mosiDataASCII;
              if ( ( bitCount == 8 ) && Character.isLetterOrDigit( ( char )mosiValue ) )
              {
                mosiDataASCII = String.valueOf( ( char )mosiValue );
              }
              else
              {
                mosiDataASCII = "";
              }

              tr.addChild( TD ).addContent( "0x", mosiDataHex );
              tr.addChild( TD ).addContent( "0b", mosiDataBin );
              tr.addChild( TD ).addContent( mosiDataDec );
              tr.addChild( TD ).addContent( mosiDataASCII );

              final int misoValue = ds.getMiSoValue();
              String misoDataHex = DisplayUtils.integerToHexString( misoValue, bitCount / 4 + bitAdder );
              String misoDataBin = DisplayUtils.integerToBinString( misoValue, bitCount );
              String misoDataDec = String.valueOf( misoValue );
              String misoDataASCII;
              if ( ( bitCount == 8 ) && Character.isLetterOrDigit( ( char )misoValue ) )
              {
                misoDataASCII = String.valueOf( ( char )misoValue );
              }
              else
              {
                misoDataASCII = "";
              }

              tr.addChild( TD ).addContent( "0x", misoDataHex );
              tr.addChild( TD ).addContent( "0b", misoDataBin );
              tr.addChild( TD ).addContent( misoDataDec );
              tr.addChild( TD ).addContent( misoDataASCII );
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
