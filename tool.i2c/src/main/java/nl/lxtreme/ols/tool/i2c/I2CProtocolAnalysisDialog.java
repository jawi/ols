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
 * Provides a main dialog for the I2C analyser.
 * 
 * @author Frank Kunz
 * @author J.W. Janssen
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

    setMinimumSize( new Dimension( 640, 480 ) );

    setLayout( new GridBagLayout() );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    /*
     * add protocol settings elements
     */
    final JPanel panSettings = new JPanel();
    panSettings.setLayout( new GridLayout( 6, 2, 5, 5 ) );
    panSettings.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    final String channels[] = new String[32];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = "Channel " + i;
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
    panSettings.add( new JLabel( " " ) );
    this.detectSTOP = new JCheckBox( "Show STOP", true );
    panSettings.add( this.detectSTOP );
    panSettings.add( new JLabel( " " ) );
    this.detectACK = new JCheckBox( "Show ACK", true );
    panSettings.add( this.detectACK );
    panSettings.add( new JLabel( " " ) );
    this.detectNACK = new JCheckBox( "Show NACK", true );
    panSettings.add( this.detectNACK );
    panSettings.add( new JLabel( " " ) );

    add( panSettings, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    /*
     * add bus configuration panel
     */
    final JPanel panBusConfig = new JPanel();
    panBusConfig.setLayout( new GridLayout( 2, 2, 5, 5 ) );
    panBusConfig.setBorder( BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder( "Bus Configuration" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    panBusConfig.add( new JLabel( "SCL :" ) );
    this.busSetSCL = new JLabel( "<autodetect>" );
    panBusConfig.add( this.busSetSCL );
    panBusConfig.add( new JLabel( "SDA :" ) );
    this.busSetSDA = new JLabel( "<autodetect>" );
    panBusConfig.add( this.busSetSDA );

    add( panBusConfig, //
        new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    /*
     * add an empty output view
     */
    final JPanel output = new JPanel( new GridLayout( 1, 1, 5, 5 ) );
    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setMargin( new Insets( 5, 5, 5, 5 ) );
    output.add( new JScrollPane( this.outText ) );

    add( output, //
        new GridBagConstraints( 1, 0, 1, 2, 1.0, 1.0, GridBagConstraints.EAST, GridBagConstraints.BOTH, COMP_INSETS, 0,
            0 ) );

    /*
     * add buttons
     */
    this.runAnalysisAction = new RunAnalysisAction();
    final JButton btnConvert = new JButton( this.runAnalysisAction );

    this.exportAction = new ExportAction();
    this.exportAction.setEnabled( false );
    final JButton btnExport = new JButton( this.exportAction );

    this.closeAction = new CloseAction();
    final JButton btnCancel = new JButton( this.closeAction );

    final JPanel buttons = new JPanel();
    final BoxLayout layoutMgr = new BoxLayout( buttons, BoxLayout.LINE_AXIS );
    buttons.setLayout( layoutMgr );
    buttons.add( Box.createHorizontalGlue() );
    buttons.add( btnConvert );
    buttons.add( btnExport );
    buttons.add( Box.createHorizontalStrut( 16 ) );
    buttons.add( btnCancel );

    add( buttons, //
        new GridBagConstraints( 0, 2, 2, 1, 1.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    pack();
  }

  // CONSTRUCTORS

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
      final String htmlPage;
      if ( aAnalysisResult != null )
      {
        htmlPage = toHtmlPage( null /* aFile */, aAnalysisResult );
        this.exportAction.setEnabled( true );
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
    SwingComponentUtils.setSelectedIndex( this.lineA, aProperties.getProperty( aNamespace + ".lineA" ) );
    SwingComponentUtils.setSelectedIndex( this.lineB, aProperties.getProperty( aNamespace + ".lineB" ) );

    SwingComponentUtils.setSelected( this.detectSTART, aProperties.getProperty( aNamespace + ".detectStart" ) );
    SwingComponentUtils.setSelected( this.detectSTOP, aProperties.getProperty( aNamespace + ".detectStop" ) );
    SwingComponentUtils.setSelected( this.detectNACK, aProperties.getProperty( aNamespace + ".detectNack" ) );
    SwingComponentUtils.setSelected( this.detectACK, aProperties.getProperty( aNamespace + ".detectAck" ) );
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

    aProperties.setProperty( aNamespace + ".detectStart", Boolean.toString( this.detectSTART.isSelected() ) );
    aProperties.setProperty( aNamespace + ".detectStop", Boolean.toString( this.detectSTOP.isSelected() ) );
    aProperties.setProperty( aNamespace + ".detectNack", Boolean.toString( this.detectNACK.isSelected() ) );
    aProperties.setProperty( aNamespace + ".detectAck", Boolean.toString( this.detectACK.isSelected() ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final I2CAnalyserWorker aToolWorker )
  {
    aToolWorker.setLineAIndex( this.lineA.getSelectedIndex() );
    aToolWorker.setLineBIndex( this.lineB.getSelectedIndex() );

    aToolWorker.setReportACK( this.detectACK.isSelected() );
    aToolWorker.setReportNACK( this.detectNACK.isSelected() );
    aToolWorker.setReportStart( this.detectSTART.isSelected() );
    aToolWorker.setReportStop( this.detectSTOP.isSelected() );
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
    catch ( final IOException exception )
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
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w20 { width: 20%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );

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

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
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
          return "-";
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
