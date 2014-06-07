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
package nl.lxtreme.ols.tool.can;


import static nl.lxtreme.ols.tool.base.ExportUtils.HtmlExporter.*;

import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ExportUtils.CsvExporter;
import nl.lxtreme.ols.tool.base.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.tool.base.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.tool.base.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.tool.base.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.tool.i2c.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a main dialog for the CAN-bus decoder.
 */
public final class CanBusConfigDialog extends BaseToolDialog<CanBusDataSet> implements ExportAware<CanBusDataSet>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JEditorPane outText;
  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * Creates a new I2CProtocolAnalysisDialog instance.
   * 
   * @param aOwner
   *          the owner of this dialog;
   * @param aTool
   *          the {@link I2CAnalyser} tool.
   * @param aToolContext
   *          the tool context;
   */
  public CanBusConfigDialog( Window aOwner, CanBusDecoder aTool, ToolContext aToolContext )
  {
    super( aOwner, aTool, aToolContext );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void exportToFile( final File aOutputFile, final ExportFormat aFormat ) throws IOException
  {
    if ( ExportFormat.CSV.equals( aFormat ) )
    {
      storeToCsvFile( aOutputFile, getLastResult() );
    }
    else if ( ExportFormat.HTML.equals( aFormat ) )
    {
      storeToHtmlFile( aOutputFile, getLastResult() );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    // TODO: read user settings and restore UI components...
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

    this.runAnalysisAction.restore();

    setControlsEnabled( true );

    this.exportAction.setEnabled( false );
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
    // TODO enable/disable any relevant UI components...

    this.closeAction.setEnabled( aEnabled );
    this.exportAction.setEnabled( aEnabled );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    // TODO write the state of the UI components to allow them to be restored
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final CanBusDataSet aAnalysisResult )
  {
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
      // This should not happen for the no-file exports!
      throw new RuntimeException( exception );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolStarted()
  {
    // NO-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<CanBusDataSet> aToolTask )
  {
    CanBusDecoderTask toolTask = ( CanBusDecoderTask )aToolTask;
    // Define the decoding area to use...
    toolTask.setDecodingArea( getMarkerAIndex(), getMarkerBIndex() );
    // TODO configure the task with the additional UI settings...
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
    body.addChild( H1 ).addContent( "CAN-bus decoder results" );
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
    final JPanel panel = new JPanel( new SpringLayout() );

    addDecoderAreaPane( panel );

    SpringLayoutUtils.addSeparator( panel, "Settings" );
    
    // TODO add UI components...

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

    final JButton runAnalysisButton = ToolUtils.createRunAnalysisButton( this );
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = ToolUtils.createExportButton( this );
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = ToolUtils.createCloseButton();
    this.closeAction = closeButton.getAction();

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( runAnalysisButton, exportButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, contentPane, buttonPane, runAnalysisButton );

    pack();
  }

  /**
   * Stores the given analysis results to the given file in CSV format.
   * 
   * @param aSelectedFile
   * @param aAnalysisResult
   */
  private void storeToCsvFile( final File aSelectedFile, final CanBusDataSet aAnalysisResult )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aSelectedFile );

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "data" );

      final List<CanBusData> dataSet = aAnalysisResult.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final CanBusData ds = dataSet.get( i );

        final String startTime = formatTime( aAnalysisResult.getTime( ds.getStartSampleIndex() ) );
        final String endTime = formatTime( aAnalysisResult.getTime( ds.getEndSampleIndex() ) );
        // TODO add additional data

        exporter.addRow( Integer.valueOf( i ), startTime, endTime, Boolean.valueOf( ds.isEvent() ), ds.getEventName() );
      }

      exporter.close();
    }
    catch ( final IOException exception )
    {
      logWarning( "CSV export failed!", exception );
    }
  }

  /**
   * Stores the given analysis results to the given file as HTML.
   * 
   * @param aSelectedFile
   * @param aAnalysisResult
   */
  private void storeToHtmlFile( final File aSelectedFile, final CanBusDataSet aAnalysisResult )
  {
    try
    {
      toHtmlPage( aSelectedFile, aAnalysisResult );
    }
    catch ( final IOException exception )
    {
      logWarning( "HTML export failed!", exception );
    }
  }

  /**
   * generate a HTML page
   * 
   * @param aEmpty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final CanBusDataSet aAnalysisResult ) throws IOException
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
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<CanBusData> dataSet = aAnalysisResult.getData();
          Element tr;

          for ( int i = 0; i < dataSet.size(); i++ )
          {
            final CanBusData data = dataSet.get( i );
            if ( data.isEvent() )
            {
              // this is an event
              final String event = data.getEventName();

              String bgColor = "#ff8000"; // XXX

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( formatTime( aAnalysisResult.getTime( data.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else
            {
              final int value = 0; // TODO get actual decoded data

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( formatTime( aAnalysisResult.getTime( data.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( NumberUtils.integerToHexString( value, 2 ) );
              tr.addChild( TD ).addContent( NumberUtils.integerToBinString( value, 8 ) );
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
