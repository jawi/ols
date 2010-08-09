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


import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
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

  private static final Logger LOG = Logger.getLogger( I2CAnalyserWorker.class.getName() );

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
   * This is the I2C protocol decoder core The decoder scans for a decode start
   * event when one of the two lines is going low (start condition). After this
   * the decoder starts to decode the data.
   */
  @Override
  public void createReport( final I2CDataSet aAnalysisResult )
  {
    super.createReport( aAnalysisResult );

    this.outText.setText( toHtmlPage( aAnalysisResult ) );
    this.outText.setEditable( false );

    this.exportAction.setEnabled( !aAnalysisResult.isEmpty() );
    this.runAnalysisAction.restore();
    this.runAnalysisAction.setEnabled( false );
  }

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
      BufferedWriter bw = new BufferedWriter( new FileWriter( aSelectedFile ) );

      bw.write( "\"index\",\"time\",\"data or event\"" );
      bw.newLine();

      final List<I2CData> dataSet = aAnalysisResult.getDecodedData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        I2CData data = dataSet.get( i );
        if ( data.isEvent() )
        {
          bw.write( i + ",\"" + indexToTime( aAnalysisResult, data.getTime() ) + "\",\"" + data.getEvent() + "\"" );
        }
        else
        {
          bw.write( i + ",\"" + indexToTime( aAnalysisResult, data.getTime() ) + "\",\"" + ( char )data.getValue()
              + "\"" );
        }
        bw.newLine();
      }
      bw.flush();
      bw.close();
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
      BufferedWriter bw = new BufferedWriter( new FileWriter( aSelectedFile ) );

      // write the complete displayed html page to file
      bw.write( this.outText.getText() );
      bw.flush();
      bw.close();
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
   * @return
   */
  private String getEmptyHtmlPage()
  {
    Date now = new Date();
    DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );

    // generate html page header
    String header = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
      + "<html>"
      + "  <head>"
      + "    <title></title>"
      + "    <meta content=\"\">"
      + "    <style>"
      + "           th { text-align:left;font-style:italic;font-weight:bold;font-size:medium;font-family:sans-serif;background-color:#C0C0FF; }"
      + "       </style>" + "  </head>" + "   <body>" + "       <H2>I2C Analysis Results</H2>" + "       <hr>"
      + "           <div style=\"text-align:right;font-size:x-small;\">" + df.format( now ) + "           </div>"
      + "       <br>";

    // generate the bus configuration table
    final StringBuilder busConfig = new StringBuilder();
    busConfig.append( "<table style=\"width:100%;\">" );
    busConfig.append( "<tr><td colspan=\"2\">Bus configuration</td></tr>" );
    busConfig.append( "<tr><td style=\"width:30%;\">" ).append( "SDA" ).append( "</td><td>" ).append(
    "&lt;autodetect&gt;" ).append( "</td></tr>" );
    busConfig.append( "<tr><td style=\"width:30%;\">" ).append( "SCL" ).append( "</td><td>" ).append(
    "&lt;autodetect&gt;" ).append( "</td></tr>" );
    busConfig.append( "</table><br><br>" );

    // generate the statistics table
    String stats = "<table style=\"width:100%;\">";
    stats = stats.concat( "<tr><td style=\"width:30%;\">Decoded Bytes</td><TD>-</td></tr>"
        + "<tr><td style=\"width:30%;\">Detected Bus Errors</td><TD>-</td></tr>" );
    stats = stats.concat( "</table>" + "<br>" + "<br>" );

    // generate the data table
    String data = "<table style=\"font-family:monospace;width:100%;\">"
      + "<tr><th style=\"width:15%;\">Index</th><th style=\"width:15%;\">Time</th><th style=\"width:20%;\">Hex</th><th style=\"width:20%;\">Bin</th><th style=\"width:20%;\">Dec</th><th style=\"width:10%;\">ASCII</th></tr>";
    data = data.concat( "</table>" );

    // generate the footer table
    String footer = "   </body>" + "</html>";

    return ( header + busConfig + stats + data + footer );
  }

  /**
   * Convert sample count to time string.
   * 
   * @param aCount
   *          sample count (or index)
   * @return string containing time information
   */
  private String indexToTime( final I2CDataSet aDataSet, final long aCount )
  {
    final long count = Math.max( 0, aCount - aDataSet.getStartSampleIndex() );
    if ( getAnalysisData().hasTimingData() )
    {
      return DisplayUtils.displayScaledTime( count, getAnalysisData().getSampleRate() );
    }
    else
    {
      return ( "" + count );
    }
  }

  /**
   * generate a HTML page
   * 
   * @param aEmpty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final I2CDataSet aAnalysisResult )
  {
    final StringBuilder data = new StringBuilder();

    final Date now = new Date();
    final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );

    // generate html page header
    data.append( "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" );
    data.append( "<html><head><title>I2C Analysis Results</title>" );
    data.append( "<style>" );
    data.append( " th { text-align:left; font-style:italic; font-weight:bold; background-color:#C0C0FF; }" );
    data.append( " .data { font-family: monospace; }" );
    data.append( " .date { text-align:right; font-size: x-small; }" );
    data.append( "</style>" );
    data.append( "</head><body><h1>I2C Analysis Results</h1><hr>" );
    data.append( "<div class=\"date\">" ).append( df.format( now ) ).append( "</div>" );

    // generate the bus configuration table
    data.append( "<table style=\"width:100%;\">" );
    data.append( "<tr><td colspan=\"2\">Bus configuration</td></tr>" );
    data.append( "<tr><td style=\"width:30%;\">" ).append( "SDA" ).append( "</td><td>" ).append(
        this.busSetSDA.getText() ).append( "</td></tr>" );
    data.append( "<tr><td style=\"width:30%;\">" ).append( "SCL" ).append( "</td><td>" ).append(
        this.busSetSCL.getText() ).append( "</td></tr>" );
    data.append( "</table><br><br>" );

    // generate the statistics table
    data.append( "<table style=\"width:100%;\">" );
    data.append( "<tr><td style=\"width:30%;\">" ).append( "Decoded Bytes" ).append( "</td><td>" ).append(
        aAnalysisResult.getDecodedByteCount() ).append( "</td></tr>" );
    data.append( "<tr><td style=\"width:30%;\">" ).append( "Detected Bus Errors" ).append( "</td><td>" ).append(
        aAnalysisResult.getBusErrorCount() ).append( "</td></tr>" );
    data.append( "</table><br><br>" );

    // generate the data table
    data.append( "<table class=\"data\" style=\"width:100%;\">" );
    data.append( "<tr>" ).append( "<th style=\"width:10%;\">" ).append( "Index" ).append( "</th>" ).append(
    "<th style=\"width:20%;\">" ).append( "Time" ).append( "</th>" ).append( "<th style=\"width:20%;\">" ).append(
    "Hex" ).append( "</th>" ).append( "<th style=\"width:20%;\">" ).append( "Bin" ).append( "</th>" ).append(
    "<th style=\"width:20%;\">" ).append( "Dec" ).append( "</th>" ).append( "<th style=\"width:10%;\">" ).append(
    "ASCII" ).append( "</th>" ).append( "</tr>" );

    final List<I2CData> dataSet = aAnalysisResult.getDecodedData();

    I2CData ds;
    for ( int i = 0; i < dataSet.size(); i++ )
    {
      ds = dataSet.get( i );

      final long time = ds.getTime();
      if ( ds.isEvent() )
      {
        // this is an event
        final String event = ds.getEvent();

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

        data.append( "<tr style=\"background-color:" ).append( bgColor ).append( ";\"><td>" ).append( i ).append(
        "</td><td>" ).append( indexToTime( aAnalysisResult, time ) ).append( "</td><td>" ).append( event ).append(
        "</td><td></td><td></td><td></td></tr>" );
      }
      else
      {
        final int value = ds.getValue();
        data.append( "<tr><td>" ).append( i ).append( "</td><td>" ).append( indexToTime( aAnalysisResult, time ) )
        .append( "</td><td>0x" ).append( DisplayUtils.integerToHexString( value, 2 ) ).append( "</td><td>0b" )
        .append( DisplayUtils.integerToBinString( value, 8 ) ).append( "</td><td>" ).append( value ).append(
        "</td><td>" );
        data.append( ( char )value );
        data.append( "</td></tr>" );
      }
    }
    data.append( "</table></body></html>" );

    return data.toString();
  }
}
