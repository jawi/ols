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
package nl.lxtreme.ols.tool.jtag;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.math.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;


/**
 * The Dialog Class for configuring the JTAG protocol analysis settings.
 *
 * @author Mario Schrenk
 */
public final class JTAGProtocolAnalysisDialog extends BaseToolDialog<JTAGDataSet> implements ExportAware<JTAGDataSet>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( JTAGProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private JComboBox tck;
  private JComboBox tdo;
  private JComboBox tdi;
  private JComboBox tms;
  private JEditorPane outText;

  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * Creates a new JTAGProtocolAnalysisDialog instance.
   *
   * @param aOwner
   *          the owner of this dialog;
   * @param aToolContext
   *          the tool context;
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aTool
   *          the {@link JTAGAnalyser} tool.
   */
  public JTAGProtocolAnalysisDialog( final Window aOwner, final ToolContext aToolContext, final BundleContext aContext,
      final JTAGAnalyser aTool )
  {
    super( aOwner, aToolContext, aContext, aTool );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  public void exportToFile( final File aOutputFile, final ExportFormat aFormat ) throws IOException
  {
    if ( ExportFormat.HTML.equals( aFormat ) )
    {
      storeToHtmlFile( aOutputFile, getLastResult() );
    }
    else if ( ExportFormat.CSV.equals( aFormat ) )
    {
      storeToCsvFile( aOutputFile, getLastResult() );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    // Issue #114: avoid setting illegal values...
    setComboBoxIndex( this.tck, aSettings, "tck" );
    setComboBoxIndex( this.tms, aSettings, "tms" );
    setComboBoxIndex( this.tdi, aSettings, "tdi" );
    setComboBoxIndex( this.tdo, aSettings, "tdo" );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#reset()
   */
  @Override
  public void reset()
  {
    this.outText.setText( getEmptyHtmlPage() );
    this.outText.setEditable( false );

    this.runAnalysisAction.restore();

    setControlsEnabled( true );

    this.exportAction.setEnabled( false );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "tck", this.tck.getSelectedIndex() );
    aSettings.putInt( "tms", this.tms.getSelectedIndex() );
    aSettings.putInt( "tdi", this.tdi.getSelectedIndex() );
    aSettings.putInt( "tdo", this.tdo.getSelectedIndex() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final JTAGDataSet aAnalysisResult )
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
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        // Should not happen in this situation!
        throw new RuntimeException( exception );
      }
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
  protected void prepareToolTask( final ToolTask<JTAGDataSet> aToolTask )
  {
    JTAGAnalyserTask toolTask = ( JTAGAnalyserTask )aToolTask;

    toolTask.setTmsIndex( this.tms.getSelectedIndex() );
    toolTask.setTckIndex( this.tck.getSelectedIndex() );
    toolTask.setTdoIndex( this.tdo.getSelectedIndex() - 1 );
    toolTask.setTdiIndex( this.tdi.getSelectedIndex() - 1 );
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
    this.tck.setEnabled( aEnable );
    this.tms.setEnabled( aEnable );
    this.tdi.setEnabled( aEnable );
    this.tdo.setEnabled( aEnable );

    this.closeAction.setEnabled( aEnable );
    this.exportAction.setEnabled( aEnable );
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
    aExporter.addCssStyle( ".w50 { width: 50%; } " );
    aExporter.addCssStyle( ".w45 { width: 45%; } " );
    aExporter.addCssStyle( ".w44 { width: 44%; } " );
    aExporter.addCssStyle( ".w40 { width: 40%; } " );
    aExporter.addCssStyle( ".w35 { width: 35%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w28 { width: 28%; } " );
    aExporter.addCssStyle( ".w20 { width: 20%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w12 { width: 12%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".w8 { width: 8%; } " );
    aExporter.addCssStyle( ".w7 { width: 7%; } " );
    aExporter.addCssStyle( ".w5 { width: 5%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "JTAG Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "Generated: ", "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w44" ).addAttribute( "colspan", "3" );
    tr.addChild( TH ).addAttribute( "class", "w28" ).addAttribute( "colspan", "2" ).addContent( "TDI" );
    tr.addChild( TH ).addAttribute( "class", "w28" ).addAttribute( "colspan", "2" ).addContent( "TDO" );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w5" ).addContent( "Index" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Time" );
    tr.addChild( TH ).addAttribute( "class", "w28" ).addContent( "State" );
    tr.addChild( TH ).addAttribute( "class", "w8" ).addContent( "Hex" );
    tr.addChild( TH ).addAttribute( "class", "w20" ).addContent( "Bin" );
    tr.addChild( TH ).addAttribute( "class", "w8" ).addContent( "Hex" );
    tr.addChild( TH ).addAttribute( "class", "w20" ).addContent( "Bin" );
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
    final int channelCount = getData().getChannels();

    final JPanel settings = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( settings, "Settings" );

    settings.add( createRightAlignedLabel( "TCK" ) );
    this.tck = SwingComponentUtils.createChannelSelector( channelCount, 0 );
    settings.add( this.tck );

    settings.add( createRightAlignedLabel( "TMS" ) );
    this.tms = SwingComponentUtils.createChannelSelector( channelCount, 1 );
    settings.add( this.tms );

    settings.add( createRightAlignedLabel( "TDI" ) );
    this.tdi = SwingComponentUtils.createOptionalChannelSelector( channelCount, 3 );
    settings.add( this.tdi );

    settings.add( createRightAlignedLabel( "TDO" ) );
    this.tdo = SwingComponentUtils.createOptionalChannelSelector( channelCount, 4 );
    settings.add( this.tdo );

    SpringLayoutUtils.makeEditorGrid( settings, 10, 4 );

    return settings;
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

    final JButton runAnalysisButton = ToolUtils.createRunAnalysisButton( this );
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = ToolUtils.createExportButton( this );
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = ToolUtils.createCloseButton();
    this.closeAction = closeButton.getAction();

    final JComponent buttons = SwingComponentUtils.createButtonPane( runAnalysisButton, exportButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, contentPane, buttons, runAnalysisButton );
  }

  /**
   * exports the table data to a CSV file
   *
   * @param aFile
   *          File object
   */
  private void storeToCsvFile( final File aFile, final JTAGDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "time", "state", "TDI data", "TDO data" );

      final List<JTAGData> dataSet = aDataSet.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final JTAGData data = dataSet.get( i );

        final String time = Unit.Time.format( aDataSet.getTime( data.getStartSampleIndex() ) );
        final String event = data.isEvent() ? data.getEventName() : null;

        BigInteger tdiData = null;
        BigInteger tdoData = null;

        // Try to coalesce equal timestamps...
        if ( ( i + 1 ) < dataSet.size() )
        {
          final JTAGData next = dataSet.get( i + 1 );
          if ( next.getStartSampleIndex() == data.getStartSampleIndex() )
          {
            tdiData = ( BigInteger )( next.isTdiData() ? next.getDataValue() : data.getDataValue() );
            tdoData = ( BigInteger )( next.isTdoData() ? next.getDataValue() : data.getDataValue() );
            // Make sure to skip this entry in the next iteration...
            i++;
          }
        }

        if ( ( tdiData == null ) && data.isTdiData() )
        {
          tdiData = ( BigInteger )data.getDataValue();
          tdoData = null;
        }
        else if ( ( tdoData == null ) && data.isTdoData() )
        {
          tdiData = null;
          tdoData = ( BigInteger )data.getDataValue();
        }

        final String tdiDataValue = tdiData != null ? "0x" + tdiData.toString( 16 ) : null;
        final String tdoDataValue = tdoData != null ? "0x" + tdoData.toString( 16 ) : null;

        exporter.addRow( Integer.valueOf( i ), time, event, tdiDataValue, tdoDataValue );
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
  private void storeToHtmlFile( final File aFile, final JTAGDataSet aDataSet )
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
   * generate a HTML page
   *
   * @param aDataSet
   *          the data set to create the HTML page for, cannot be
   *          <code>null</code>.
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final JTAGDataSet aAnalysisResult ) throws IOException
  {
    LOG.log( Level.INFO, "toHtmlPage" );

    final MacroResolver macroResolver = new MacroResolver()
    {
      @Override
      public Object resolve( final String aMacro, final Element aParent )
      {
        if ( "date-now".equals( aMacro ) )
        {
          LOG.log( Level.INFO, "toHtmlPage date-now" );

          final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );
          return df.format( new Date() );
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          LOG.log( Level.INFO, "toHtmlPage decoded-data" );

          final List<JTAGData> dataSet = aAnalysisResult.getData();
          Element tr;

          for ( int i = 0; i < dataSet.size(); i++ )
          {
            final JTAGData data = dataSet.get( i );

            if ( data.isEvent() )
            {
              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: #fefeff;" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aAnalysisResult.getTime( data.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( String.valueOf( data.getDataValue() ) );
              tr.addChild( TD ).addAttribute( "colspan", "4" );
            }
            else
            {
              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aAnalysisResult.getTime( data.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( data.getEventName() );

              BigInteger tdiData = null;
              BigInteger tdoData = null;

              // Try to coalesce equal timestamps...
              if ( ( i + 1 ) < dataSet.size() )
              {
                final JTAGData next = dataSet.get( i + 1 );
                if ( next.getStartSampleIndex() == data.getStartSampleIndex() )
                {
                  tdiData = ( BigInteger )( next.isTdiData() ? next.getDataValue() : data.getDataValue() );
                  tdoData = ( BigInteger )( next.isTdoData() ? next.getDataValue() : data.getDataValue() );
                  // Make sure to skip this entry in the next iteration...
                  i++;
                }
              }

              if ( ( tdiData == null ) && data.isTdiData() )
              {
                tdiData = ( BigInteger )data.getDataValue();
                tdoData = null;
              }
              else if ( ( tdoData == null ) && data.isTdoData() )
              {
                tdiData = null;
                tdoData = ( BigInteger )data.getDataValue();
              }

              if ( tdiData != null )
              {
                tr.addChild( TD ).addContent( "0x", tdiData.toString( 16 ) );
                tr.addChild( TD ).addContent( "0b", tdiData.toString( 2 ) );
              }
              else
              {
                tr.addChild( TD ).addAttribute( "colspan", "2" );
              }
              if ( tdoData != null )
              {
                tr.addChild( TD ).addContent( "0x", tdoData.toString( 16 ) );
                tr.addChild( TD ).addContent( "0b", tdoData.toString( 2 ) );
              }
              else
              {
                tr.addChild( TD ).addAttribute( "colspan", "2" );
              }
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
