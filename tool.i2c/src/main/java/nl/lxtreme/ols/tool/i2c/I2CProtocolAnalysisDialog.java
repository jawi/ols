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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.i2c;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
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

  private JLabel lineALabel;
  private JComboBox lineA;
  private JLabel lineBLabel;
  private JComboBox lineB;
  private JEditorPane outText;
  private JLabel busSetSCL;
  private JLabel busSetSDA;
  private JCheckBox detectSDA_SCL;
  private JCheckBox detectSTART;
  private JCheckBox detectSTOP;
  private JCheckBox detectACK;
  private JCheckBox detectNACK;
  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * Creates a new I2CProtocolAnalysisDialog instance.
   * 
   * @param aOwner
   *          the owner of this dialog;
   * @param aName
   *          the name of this dialog;
   * @param aContext
   *          the tool context.
   */
  public I2CProtocolAnalysisDialog( final Window aOwner, final String aName, final ToolContext aContext )
  {
    super( aOwner, aName, aContext );

    initDialog();

    setLocationRelativeTo( getOwner() );
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
        // This should not happen for the no-file exports!
        throw new RuntimeException( exception );
      }
    }
  }

  // CONSTRUCTORS

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    this.lineA.setSelectedIndex( aSettings.getInt( "lineA", this.lineA.getSelectedIndex() ) );
    this.lineB.setSelectedIndex( aSettings.getInt( "lineB", this.lineB.getSelectedIndex() ) );

    this.detectSTART.setSelected( aSettings.getBoolean( "detectStart", this.detectSTART.isSelected() ) );
    this.detectSTOP.setSelected( aSettings.getBoolean( "detectStop", this.detectSTOP.isSelected() ) );
    this.detectNACK.setSelected( aSettings.getBoolean( "detectNack", this.detectNACK.isSelected() ) );
    this.detectACK.setSelected( aSettings.getBoolean( "detectAck", this.detectACK.isSelected() ) );
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
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "lineA", this.lineA.getSelectedIndex() );
    aSettings.putInt( "lineB", this.lineB.getSelectedIndex() );

    aSettings.putBoolean( "detectStart", this.detectSTART.isSelected() );
    aSettings.putBoolean( "detectStop", this.detectSTOP.isSelected() );
    aSettings.putBoolean( "detectNack", this.detectNACK.isSelected() );
    aSettings.putBoolean( "detectAck", this.detectACK.isSelected() );
  }

  /**
   * @param aCheckBox
   */
  final void syncLineLabels( final JCheckBox aCheckBox )
  {
    if ( aCheckBox.isSelected() )
    {
      I2CProtocolAnalysisDialog.this.lineALabel.setText( "Line A" );
      I2CProtocolAnalysisDialog.this.lineBLabel.setText( "Line B" );
    }
    else
    {
      I2CProtocolAnalysisDialog.this.lineALabel.setText( "SCL" );
      I2CProtocolAnalysisDialog.this.lineBLabel.setText( "SDA" );
    }
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final I2CAnalyserWorker aToolWorker )
  {
    aToolWorker.setLineAIndex( this.lineA.getSelectedIndex() );
    aToolWorker.setLineBIndex( this.lineB.getSelectedIndex() );

    aToolWorker.setDetectSDA_SCL( this.detectSDA_SCL.isSelected() );

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

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "data" );

      final List<I2CData> dataSet = aAnalysisResult.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final I2CData ds = dataSet.get( i );

        final String startTime = aAnalysisResult.getDisplayTime( ds.getStartSampleIndex() );
        final String endTime = aAnalysisResult.getDisplayTime( ds.getEndSampleIndex() );
        final String data = ds.isEvent() ? "" : Character.toString( ( char )ds.getValue() );

        exporter.addRow( Integer.valueOf( i ), startTime, endTime, Boolean.valueOf( ds.isEvent() ), ds.getEventName(),
            data );
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
   * @return
   */
  private JPanel createPreviewPane()
  {
    final JPanel output = new JPanel( new GridLayout( 1, 1, 0, 0 ) );

    this.outText = new JEditorPane( "text/html", getEmptyHtmlPage() );
    this.outText.setEditable( false );

    output.add( new JScrollPane( this.outText ) );

    return output;
  }

  /**
   * @return
   */
  private JPanel createSettingsPane()
  {
    final int channelCount = getChannels();

    this.busSetSCL = new JLabel( "<autodetect>" );
    this.busSetSDA = new JLabel( "<autodetect>" );

    final JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "Settings" );

    this.lineALabel = createRightAlignedLabel( "Line A" );
    this.lineBLabel = createRightAlignedLabel( "Line B" );

    panel.add( this.lineALabel );
    this.lineA = SwingComponentUtils.createChannelSelector( channelCount );
    this.lineA.setSelectedIndex( 0 );
    panel.add( this.lineA );

    panel.add( this.lineBLabel );
    this.lineB = SwingComponentUtils.createChannelSelector( channelCount );
    this.lineB.setSelectedIndex( 1 );
    panel.add( this.lineB );

    this.detectSDA_SCL = new JCheckBox();
    this.detectSDA_SCL.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JCheckBox cb = ( JCheckBox )aEvent.getSource();
        syncLineLabels( cb );
      }
    } );
    this.detectSDA_SCL.setSelected( true );
    panel.add( createRightAlignedLabel( "Detect SDA & SCL?" ) );
    panel.add( this.detectSDA_SCL );

    this.detectSTART = new JCheckBox();
    this.detectSTART.setSelected( true );
    panel.add( createRightAlignedLabel( "Show START?" ) );
    panel.add( this.detectSTART );

    this.detectSTOP = new JCheckBox();
    this.detectSTOP.setSelected( true );
    panel.add( createRightAlignedLabel( "Show STOP?" ) );
    panel.add( this.detectSTOP );

    this.detectACK = new JCheckBox();
    this.detectACK.setSelected( true );
    panel.add( createRightAlignedLabel( "Show ACK?" ) );
    panel.add( this.detectACK );

    this.detectNACK = new JCheckBox();
    this.detectNACK.setSelected( true );
    panel.add( createRightAlignedLabel( "Show NACK?" ) );
    panel.add( this.detectNACK );

    SpringLayoutUtils.addSeparator( panel, "Bus configuration" );

    panel.add( createRightAlignedLabel( "SCL" ) );
    panel.add( this.busSetSCL );

    panel.add( createRightAlignedLabel( "SDA" ) );
    panel.add( this.busSetSDA );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 4 );

    return panel;
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
   *
   */
  private void initDialog()
  {
    setMinimumSize( new Dimension( 640, 480 ) );

    final JComponent settingsPane = createSettingsPane();
    final JComponent previewPane = createPreviewPane();

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.add( settingsPane, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    contentPane.add( previewPane, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = createExportButton();
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = createCloseButton();
    this.closeAction = closeButton.getAction();

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( runAnalysisButton, exportButton, closeButton );

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane, runAnalysisButton );

    pack();
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
          return Integer.valueOf( aAnalysisResult.getDecodedByteCount() );
        }
        else if ( "detected-bus-errors".equals( aMacro ) )
        {
          return Integer.valueOf( aAnalysisResult.getBusErrorCount() );
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
              final String event = data.getEventName();

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
              tr.addChild( TD ).addContent( aAnalysisResult.getDisplayTime( data.getStartSampleIndex() ) );
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
              tr.addChild( TD ).addContent( aAnalysisResult.getDisplayTime( data.getStartSampleIndex() ) );
              tr.addChild( TD ).addContent( "0x" + StringUtils.integerToHexString( value, 2 ) );
              tr.addChild( TD ).addContent( "0b" + StringUtils.integerToBinString( value, 8 ) );
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
