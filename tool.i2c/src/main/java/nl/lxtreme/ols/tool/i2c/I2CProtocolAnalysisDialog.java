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
package nl.lxtreme.ols.tool.i2c;


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
public final class I2CProtocolAnalysisDialog extends BaseAsyncToolDialog<I2CDataSet, I2CAnalyserWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( I2CProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private final JComboBox lineA;
  private final JComboBox lineB;
  private final JEditorPane outText;
  private final JLabel busSetSCL;
  private final JLabel busSetSDA;
  private final JCheckBox detectSTART;
  private final JCheckBox detectSTOP;
  private final JCheckBox detectACK;
  private final JCheckBox detectNACK;
  private final RunAnalysisAction runAnalysisAction;
  private final ExportAction exportAction;
  private final CloseAction closeAction;

  // CONSTRUCTORS

  /**
   * @param aFrame
   * @param aName
   */
  public I2CProtocolAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    /*
     * add protocol settings elements
     */
    JPanel panSettings = new JPanel();
    panSettings.setLayout( new GridLayout( 6, 2, 5, 5 ) );
    panSettings.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    String channels[] = new String[32];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = new String( "Channel " + i );
    }

    panSettings.add( new JLabel( "Line A" ) );
    this.lineA = new JComboBox( channels );
    this.lineA.setSelectedIndex( 0 );
    panSettings.add( this.lineA );
    panSettings.add( new JLabel( "Line B" ) );
    this.lineB = new JComboBox( channels );
    this.lineB.setSelectedIndex( 1 );
    panSettings.add( this.lineB );
    this.detectSTART = new JCheckBox( "Show START", true );
    panSettings.add( this.detectSTART );
    panSettings.add( new JLabel( "" ) );
    this.detectSTOP = new JCheckBox( "Show STOP", true );
    panSettings.add( this.detectSTOP );
    panSettings.add( new JLabel( "" ) );
    this.detectACK = new JCheckBox( "Show ACK", true );
    panSettings.add( this.detectACK );
    panSettings.add( new JLabel( "" ) );
    this.detectNACK = new JCheckBox( "Show NACK", true );
    panSettings.add( this.detectNACK );
    panSettings.add( new JLabel( "" ) );
    add( panSettings, createConstraints( 0, 0, 1, 1, 0, 0 ) );

    /*
     * add bus configuration panel
     */
    JPanel panBusConfig = new JPanel();
    panBusConfig.setLayout( new GridLayout( 2, 2, 5, 5 ) );
    panBusConfig.setBorder( BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder( "Bus Configuration" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    panBusConfig.add( new JLabel( "SCL :" ) );
    this.busSetSCL = new JLabel( "<autodetect>" );
    panBusConfig.add( this.busSetSCL );
    panBusConfig.add( new JLabel( "SDA :" ) );
    this.busSetSDA = new JLabel( "<autodetect>" );
    panBusConfig.add( this.busSetSDA );
    add( panBusConfig, createConstraints( 0, 1, 1, 1, 0, 0 ) );

    /*
     * add an empty output view
     */
    JPanel panTable = new JPanel();
    panTable.setLayout( new GridLayout( 1, 1, 5, 5 ) );
    panTable.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Results" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setMargin( new Insets( 5, 5, 5, 5 ) );
    panTable.add( new JScrollPane( this.outText ) );
    add( panTable, createConstraints( 1, 0, 3, 3, 1.0, 1.0 ) );

    /*
     * add buttons
     */
    JPanel panButton = new JPanel();
    this.runAnalysisAction = new RunAnalysisAction();
    JButton btnConvert = new JButton( this.runAnalysisAction );
    panButton.add( btnConvert );

    this.exportAction = new ExportAction();
    this.exportAction.setEnabled( false );
    JButton btnExport = new JButton( this.exportAction );
    panButton.add( btnExport );

    this.closeAction = new CloseAction();
    JButton btnClose = new JButton( this.closeAction );
    panButton.add( btnClose );

    add( panButton, createConstraints( 3, 4, 1, 1, 0, 0 ) );

    // pack();
    setSize( 900, 500 );
    setResizable( false );
  }

  // CONSTRUCTORS

  /**
   * create constraints for GridBagLayout
   * 
   * @param x
   *          x grid position
   * @param y
   *          y grid position
   * @param w
   *          grid width
   * @param h
   *          grid height
   * @param wx
   *          weighting for extra horizontal space
   * @param wy
   *          weighting for extra vertical space
   * @return constraints object
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

  // METHODS

  /**
   * @return
   */
  public int getLineAmask()
  {
    return ( 1 << this.lineA.getSelectedIndex() );
  }

  /**
   * @return
   */
  public int getLineBmask()
  {
    return ( 1 << this.lineB.getSelectedIndex() );
  }

  /**
   * @return
   */
  public boolean isReportACK()
  {
    return this.detectACK.isSelected();
  }

  /**
   * @return
   */
  public boolean isReportNACK()
  {
    return this.detectNACK.isSelected();
  }

  /**
   * @return
   */
  public boolean isReportStart()
  {
    return this.detectSTART.isSelected();
  }

  /**
   * @return
   */
  public boolean isReportStop()
  {
    return this.detectSTOP.isSelected();
  }

  /**
   * This is the I2C protocol decoder core The decoder scans for a decode start
   * event when one of the two lines is going low (start condition). After this
   * the decoder starts to decode the data.
   */
  @Override
  public void onToolWorkerReady( final I2CDataSet aAnalysisResult )
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
      // This should not happen for the no-file exports!
      throw new RuntimeException( exception );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    SwingComponentUtils.setSelectedItem( this.lineA, aProperties.getProperty( aNamespace + ".lineA" ) );
    SwingComponentUtils.setSelectedItem( this.lineB, aProperties.getProperty( aNamespace + ".lineB" ) );
  }

  /**
   * Resets this dialog.
   */
  @Override
  public void reset()
  {
    final String emptyHtmlPage = getEmptyHtmlPage();
    this.outText.setText( emptyHtmlPage );
    this.outText.setEditable( false );

    this.exportAction.setEnabled( false );

    this.runAnalysisAction.restore();
    this.runAnalysisAction.setEnabled( true );

    setControlsEnabled( true );
  }

  /**
   * @param aSCLValue
   */
  public void setAutoDetectSCL( final String aSCLValue )
  {
    if ( I2CAnalyserWorker.LINE_A.equals( aSCLValue ) )
    {
      this.busSetSCL.setText( ( String )this.lineA.getSelectedItem() );
    }
    else if ( I2CAnalyserWorker.LINE_B.equals( aSCLValue ) )
    {
      this.busSetSCL.setText( ( String )this.lineB.getSelectedItem() );
    }
  }

  /**
   * @param aSCLValue
   */
  public void setAutoDetectSDA( final String aSDAValue )
  {
    if ( I2CAnalyserWorker.LINE_A.equals( aSDAValue ) )
    {
      this.busSetSDA.setText( ( String )this.lineA.getSelectedItem() );
    }
    else if ( I2CAnalyserWorker.LINE_B.equals( aSDAValue ) )
    {
      this.busSetSDA.setText( ( String )this.lineB.getSelectedItem() );
    }
  }

  /**
   * set the controls of the dialog enabled/disabled
   * 
   * @param aEnabled
   *          status of the controls
   */
  @Override
  public void setControlsEnabled( final boolean aEnabled )
  {
    this.lineA.setEnabled( aEnabled );
    this.lineB.setEnabled( aEnabled );
    this.detectSTART.setEnabled( aEnabled );
    this.detectSTOP.setEnabled( aEnabled );
    this.detectACK.setEnabled( aEnabled );
    this.detectNACK.setEnabled( aEnabled );

    this.exportAction.setEnabled( aEnabled );
    this.closeAction.setEnabled( aEnabled );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    aProperties.setProperty( aNamespace + ".lineA", Integer.toString( this.lineA.getSelectedIndex() ) );
    aProperties.setProperty( aNamespace + ".lineB", Integer.toString( this.lineB.getSelectedIndex() ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final I2CAnalyserWorker aToolWorker )
  {
    aToolWorker.setLineAmask( getLineAmask() );
    aToolWorker.setLineBmask( getLineBmask() );
    aToolWorker.setReportACK( isReportACK() );
    aToolWorker.setReportNACK( isReportNACK() );
    aToolWorker.setReportStart( isReportStart() );
    aToolWorker.setReportStop( isReportStop() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#storeToCsvFile(java.io.File,
   *      java.lang.Object)
   */
  @Override
  protected void storeToCsvFile( final File aSelectedFile, final I2CDataSet aAnalysisResult )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aSelectedFile );

      exporter.setHeaders( "index", "time", "event?", "event-type", "data" );

      final List<I2CData> dataSet = aAnalysisResult.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final I2CData ds = dataSet.get( i );

        exporter.addRow( i, ds.getTimeDisplayValue(), ds.isEvent(), ds.getEvent(), ( char )ds.getValue() );
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
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#storeToHtmlFile(java.io.File,
   *      java.lang.Object)
   */
  @Override
  protected void storeToHtmlFile( final File aSelectedFile, final I2CDataSet aAnalysisResult )
  {
    try
    {
      toHtmlPage( aSelectedFile, aAnalysisResult );
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
    aExporter.addCssStyle( "th { text-align: left; font-style: italic; font-weight: bold; "
        + "font-size: medium; font-family: sans-serif; background-color: #C0C0FF; } " );
    aExporter.addCssStyle( "tbody { margin-top: 1.5em; } " );
    aExporter.addCssStyle( ".date { text-align: right; font-size: x-small; } " );
    aExporter.addCssStyle( ".w100 { width: 100%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w20 { width: 20%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".mono { width: 100%; font-family: monospace; }" );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "I<sup>2</sup>C Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Bus configuration" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "SDA" );
    tr.addChild( TD ).addContent( "{sda-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "SCL" );
    tr.addChild( TD ).addContent( "{scl-bus-config}" );

    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Statistics" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Decoded bytes" );
    tr.addChild( TD ).addContent( "{decoded-bytes}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Detected bus errors" );
    tr.addChild( TD ).addContent( "{detected-bus-errors}" );

    table = body.addChild( TABLE ).addAttribute( "class", "mono" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addContent( "Index" );
    tr.addChild( TH ).addAttribute( "class", "w15" ).addContent( "Time" );
    tr.addChild( TH ).addAttribute( "class", "w20" ).addContent( "Hex" );
    tr.addChild( TH ).addAttribute( "class", "w20" ).addContent( "Bin" );
    tr.addChild( TH ).addAttribute( "class", "w20" ).addContent( "Dec" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "ASCII" );
    tbody = table.addChild( TBODY );
    tbody.addContent( "{decoded-data}" );

    return aExporter;
  }

  /**
   * Returns an "empty" HTML page.
   * 
   * @return an empty HTML page string, never <code>null</code>.
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
        else if ( "sda-bus-config".equals( aMacro ) || "scl-bus-config".equals( aMacro ) )
        {
          return "&lt;auto detect&gt;";
        }
        else if ( "decoded-bytes".equals( aMacro ) || "detected-bus-errors".equals( aMacro ) )
        {
          return "&mdash;";
        }
        return null;
      }
    } );
  }

  /**
   * generate a HTML page
   * 
   * @param aEmpty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final I2CDataSet aAnalysisResult ) throws IOException
  {
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
        else if ( "sda-bus-config".equals( aMacro ) )
        {
          return I2CProtocolAnalysisDialog.this.busSetSDA.getText();
        }
        else if ( "scl-bus-config".equals( aMacro ) )
        {
          return I2CProtocolAnalysisDialog.this.busSetSCL.getText();
        }
        else if ( "decoded-bytes".equals( aMacro ) )
        {
          return aAnalysisResult.getDecodedByteCount();
        }
        else if ( "detected-bus-errors".equals( aMacro ) )
        {
          return aAnalysisResult.getBusErrorCount();
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<I2CData> dataSet = aAnalysisResult.getData();
          Element tr;

          for ( int i = 0; i < dataSet.size(); i++ )
          {
            final I2CData data = dataSet.get( i );

            if ( data.isEvent() )
            {
              // this is an event
              final String event = data.getEvent();

              String bgColor;
              if ( I2CDataSet.I2C_START.equals( event ) || I2CDataSet.I2C_STOP.equals( event ) )
              {
                bgColor = "#e0e0e0";
              }
              else if ( I2CDataSet.I2C_ACK.equals( event ) )
              {
                bgColor = "#c0ffc0";
              }
              else if ( I2CDataSet.I2C_NACK.equals( event ) )
              {
                bgColor = "#ffc0c0";
              }
              else
              {
                // unknown event
                bgColor = "#ff8000";
              }

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( data.getTimeDisplayValue() );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else
            {
              final int value = data.getValue();

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( data.getTimeDisplayValue() );
              tr.addChild( TD ).addContent( "0x" + DisplayUtils.integerToHexString( value, 2 ) );
              tr.addChild( TD ).addContent( "0b" + DisplayUtils.integerToBinString( value, 8 ) );
              tr.addChild( TD ).addContent( String.valueOf( value ) );
              tr.addChild( TD ).addContent( String.valueOf( ( char )value ) );
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
