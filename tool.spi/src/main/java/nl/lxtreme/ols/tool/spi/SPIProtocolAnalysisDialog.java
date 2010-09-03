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
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.util.NumberUtils.BitOrder;
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
  private final JComboBox order;
  private final JEditorPane outText;
  private final JCheckBox reportCS;
  private final JCheckBox honourCS;

  private final RestorableAction runAnalysisAction;
  private final Action exportAction;
  private final Action closeAction;

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
    final JPanel panSettings = new JPanel();
    panSettings.setLayout( new GridLayout( 10, 2, 5, 5 ) );
    panSettings.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    final String channels[] = new String[32];
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

    panSettings.add( new JLabel( "Show /CS?" ) );
    this.reportCS = new JCheckBox();
    this.reportCS.setToolTipText( "Whether or not to show /CS transitions in analysis results?" );
    this.reportCS.setSelected( true );
    panSettings.add( this.reportCS );

    panSettings.add( new JLabel( "Honour /CS?" ) );
    this.honourCS = new JCheckBox();
    this.honourCS.setToolTipText( "Whether or not to use /CS in analysis results?" );
    this.honourCS.setSelected( false );
    panSettings.add( this.honourCS );

    panSettings.add( new JLabel() );
    panSettings.add( Box.createVerticalGlue() );

    add( panSettings, createConstraints( 0, 0, 1, 1, 0, 0 ) );

    /*
     * add an empty output view
     */
    final JPanel panTable = new JPanel( new GridLayout( 1, 1, 5, 5 ) );
    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setMargin( new Insets( 5, 5, 5, 5 ) );
    panTable.add( new JScrollPane( this.outText ) );
    add( panTable, createConstraints( 1, 0, 1, 1, 1.0, 1.0 ) );

    /*
     * add buttons
     */
    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = createExportButton();
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = createCloseButton();
    this.closeAction = closeButton.getAction();

    final JPanel buttons = new JPanel();
    final BoxLayout layoutMgr = new BoxLayout( buttons, BoxLayout.LINE_AXIS );
    buttons.setLayout( layoutMgr );
    buttons.add( Box.createHorizontalGlue() );
    buttons.add( runAnalysisButton );
    buttons.add( Box.createHorizontalStrut( 8 ) );
    buttons.add( exportButton );
    buttons.add( Box.createHorizontalStrut( 16 ) );
    buttons.add( closeButton );

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
    final GridBagConstraints gbc = new GridBagConstraints();
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
    SwingComponentUtils.setSelected( this.reportCS, aProperties.getProperty( aNamespace + ".reportCS" ) );
    SwingComponentUtils.setSelected( this.honourCS, aProperties.getProperty( aNamespace + ".honourCS" ) );
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

    setControlsEnabled( true );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    aProperties.setProperty( aNamespace + ".reportCS", Boolean.toString( this.reportCS.isSelected() ) );
    aProperties.setProperty( aNamespace + ".honourCS", Boolean.toString( this.honourCS.isSelected() ) );
    aProperties.setProperty( aNamespace + ".sck", Integer.toString( this.sck.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".miso", Integer.toString( this.miso.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".mosi", Integer.toString( this.mosi.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".cs", Integer.toString( this.cs.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".mode", Integer.toString( this.mode.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".bits", Integer.toString( this.bits.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".order", Integer.toString( this.order.getSelectedIndex() ) );
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
    this.reportCS.setEnabled( aEnable );
    this.honourCS.setEnabled( aEnable );
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
    aToolWorker.setCSIndex( this.cs.getSelectedIndex() );
    aToolWorker.setSCKIndex( this.sck.getSelectedIndex() );
    aToolWorker.setMisoIndex( this.miso.getSelectedIndex() );
    aToolWorker.setMosiIndex( this.mosi.getSelectedIndex() );
    aToolWorker.setOrder( "MSB first".equals( this.order.getSelectedItem() ) ? BitOrder.MSB_FIRST : BitOrder.LSB_FIRST );
    aToolWorker.setMode( SPIMode.parse( ( String )this.mode.getSelectedItem() ) );
    aToolWorker.setReportCS( this.reportCS.isSelected() );
    aToolWorker.setHonourCS( this.honourCS.isSelected() );
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

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "MOSI data", "MISO data" );

      final List<SPIData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final SPIData ds = decodedData.get( i );

        final String startTime = aDataSet.getDisplayTime( ds.getStartSampleIndex() );
        final String endTime = aDataSet.getDisplayTime( ds.getStartSampleIndex() );
        final String mosiDataValue = ds.isMosiData() ? Integer.toString( ds.getDataValue() ) : null;
        final String misoDataValue = ds.isMisoData() ? Integer.toString( ds.getDataValue() ) : null;

        exporter.addRow( i, startTime, endTime, ds.isEvent(), ds.getEventName(), mosiDataValue, misoDataValue );
      }

      exporter.close();
    }
    catch ( final IOException exception )
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
    catch ( final IOException exception )
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
              if ( SPIDataSet.SPI_CS_LOW.equals( ds.getEventName() ) )
              {
                // start condition
                event = ds.getEventName();
                bgColor = "#c0ffc0";
              }
              else if ( SPIDataSet.SPI_CS_HIGH.equals( ds.getEventName() ) )
              {
                // stop condition
                event = ds.getEventName();
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
              tr.addChild( TD ).addContent( aDataSet.getDisplayTime( ds.getStartSampleIndex() ) );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else if ( ds.isData() )
            {
              final int sampleIdx = ds.getStartSampleIndex();

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( aDataSet.getDisplayTime( sampleIdx ) );

              int mosiValue = ds.isMosiData() ? ds.getDataValue() : 0;
              int misoValue = ds.isMisoData() ? ds.getDataValue() : 0;

              // Try to coalesce equal timestamps...
              if ( ( i + 1 ) < decodedData.size() )
              {
                final SPIData nextDS = decodedData.get( i + 1 );
                if ( nextDS.getStartSampleIndex() == sampleIdx )
                {
                  mosiValue = nextDS.isMosiData() ? nextDS.getDataValue() : 0;
                  misoValue = nextDS.isMisoData() ? nextDS.getDataValue() : 0;
                  // Make sure to skip this DS in the next iteration...
                  i++;
                }
              }

              // MOSI value first, MISO value next...
              addDataValues( tr, i, sampleIdx, mosiValue );
              addDataValues( tr, i, sampleIdx, misoValue );
            }
          }
        }

        return null;
      }

      /**
       * @param aTableRow
       * @param aIdx
       * @param aSampleIdx
       * @param aValue
       */
      private void addDataValues( final Element aTableRow, final int aIdx, final int aSampleIdx, final int aValue )
      {
        final String mosiDataHex = DisplayUtils.integerToHexString( aValue, bitCount / 4 + bitAdder );
        final String mosiDataBin = DisplayUtils.integerToBinString( aValue, bitCount );
        final String mosiDataDec = String.valueOf( aValue );
        final String mosiDataASCII;
        if ( ( bitCount == 8 ) && Character.isLetterOrDigit( ( char )aValue ) )
        {
          mosiDataASCII = String.valueOf( ( char )aValue );
        }
        else
        {
          mosiDataASCII = "";
        }

        aTableRow.addChild( TD ).addContent( "0x", mosiDataHex );
        aTableRow.addChild( TD ).addContent( "0b", mosiDataBin );
        aTableRow.addChild( TD ).addContent( mosiDataDec );
        aTableRow.addChild( TD ).addContent( mosiDataASCII );
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
