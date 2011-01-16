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
package nl.lxtreme.ols.tool.jtag;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

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
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.prefs.*;


/**
 * The Dialog Class
 * 
 * @author Frank Kunz The dialog class draws the basic dialog with a grid
 *         layout. The dialog consists of three main parts. A settings panel, a
 *         table panel and three buttons.
 * @author Mario Schrenk
 */
public final class JTAGProtocolAnalysisDialog extends BaseAsyncToolDialog<JTAGDataSet, JTAGAnalyserWorker>
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
   * @param aOwner
   * @param aName
   */
  public JTAGProtocolAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * This is the JTAG protocol decoder core. The decoded data are put to a
   * JTable object directly.
   */
  @Override
  public void onToolWorkerReady( final JTAGDataSet aAnalysisResult )
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
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  public void readPreferences( final Preferences aPrefs )
  {
    SwingComponentUtils.setSelectedIndex( this.tck, aPrefs.getInt( "TCK", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.tms, aPrefs.getInt( "TMS", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.tdi, aPrefs.getInt( "TDI", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.tdo, aPrefs.getInt( "TDO", -1 ) );
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
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  public void writePreferences( final Preferences aProperties )
  {
    aProperties.putInt( "tck", this.tck.getSelectedIndex() );
    aProperties.putInt( "tms", this.tms.getSelectedIndex() );
    aProperties.putInt( "tdi", this.tdi.getSelectedIndex() );
    aProperties.putInt( "tdo", this.tdo.getSelectedIndex() );
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
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final JTAGAnalyserWorker aToolWorker )
  {
    aToolWorker.setTmsIndex( this.tms.getSelectedIndex() );
    aToolWorker.setTckIndex( this.tck.getSelectedIndex() );
    aToolWorker.setTdoIndex( this.tdo.getSelectedIndex() );
    aToolWorker.setTdiIndex( this.tdi.getSelectedIndex() );
  }

  /**
   * exports the table data to a CSV file
   * 
   * @param aFile
   *          File object
   */
  @Override
  protected void storeToCsvFile( final File aFile, final JTAGDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "time", "state", "TDI data", "TDO data" );

      final List<JTAGData> dataSet = aDataSet.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final JTAGData data = dataSet.get( i );

        final String Time = aDataSet.getDisplayTime( data.getStartSampleIndex() );
        final String Event = data.isEvent() ? data.getEventName() : State( data.getDataValue() );
        final String tdiDataValue = data.isTdiData() ? Integer.toString( data.getDataValue() ) : null;
        final String tdoDataValue = data.isTdoData() ? Integer.toString( data.getDataValue() ) : null;

        exporter.addRow( i, Time, Event, tdiDataValue, tdoDataValue );
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
  protected void storeToHtmlFile( final File aFile, final JTAGDataSet aDataSet )
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
   * @return
   */
  private JComponent createButtonPane()
  {
    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAnalysisAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton exportButton = createExportButton();
    this.exportAction = exportButton.getAction();
    this.exportAction.setEnabled( false );

    final JButton closeButton = createCloseButton();
    this.closeAction = closeButton.getAction();

    return SwingComponentUtils.createButtonPane( closeButton, runAnalysisButton, exportButton );
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
    tr.addChild( TH ).addAttribute( "class", "w7" ).addContent( "Index" );
    tr.addChild( TH ).addAttribute( "class", "w12" ).addContent( "Time" );
    tr.addChild( TH ).addAttribute( "class", "w25" ).addContent( "State" );
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
    final String channels[] = new String[32];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = new String( "Channel " + i );
    }

    final String dataChannels[] = new String[33];
    for ( int i = 0; i < 32; i++ )
    {
      dataChannels[i] = new String( "Channel " + i );
    }
    dataChannels[dataChannels.length - 1] = "Unused";

    final JPanel settings = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( settings, "Settings" );

    settings.add( createRightAlignedLabel( "TCK" ) );
    this.tck = new JComboBox( channels );
    this.tck.setSelectedIndex( 0 );
    settings.add( this.tck );

    settings.add( createRightAlignedLabel( "TMS" ) );
    this.tms = new JComboBox( channels );
    this.tms.setSelectedIndex( 1 );
    settings.add( this.tms );

    settings.add( createRightAlignedLabel( "TDI" ) );
    this.tdi = new JComboBox( dataChannels );
    this.tdi.setSelectedIndex( 2 );
    settings.add( this.tdi );

    settings.add( createRightAlignedLabel( "TDO" ) );
    this.tdo = new JComboBox( dataChannels );
    this.tdo.setSelectedIndex( 3 );
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

    final JComponent buttons = createButtonPane();

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttons );
  }

  private String State( final int value )
  {
    if ( value == 0 )
    {
      return "Test Logic Reset";
    }
    else if ( value == 1 )
    {
      return "Run Test Idle";
    }
    else if ( value == 2 )
    {
      return "Select DR";
    }
    else if ( value == 3 )
    {
      return "Capture DR";
    }
    else if ( value == 4 )
    {
      return "Shift DR";
    }
    else if ( value == 5 )
    {
      return "Exit1 DR";
    }
    else if ( value == 6 )
    {
      return "Pause DR";
    }
    else if ( value == 7 )
    {
      return "Exit2 DR";
    }
    else if ( value == 8 )
    {
      return "Update DR";
    }
    else if ( value == 9 )
    {
      return "Select IR";
    }
    else if ( value == 10 )
    {
      return "Capture IR";
    }
    else if ( value == 11 )
    {
      return "Shift IR";
    }
    else if ( value == 12 )
    {
      return "Exit1 IR";
    }
    else if ( value == 13 )
    {
      return "Pause IR";
    }
    else if ( value == 14 )
    {
      return "Exit2 IR";
    }
    else if ( value == 15 )
    {
      return "Update IR";
    }
    else
    {
      return "ERROR";
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
              // this is an event
              final String event = data.getEventName();

              String bgColor = "#e0e0e0";

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
              final int value = data.getDataValue();
              final String State = State( value );

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( aAnalysisResult.getDisplayTime( data.getStartSampleIndex() ) );
              tr.addChild( TD ).addContent( State );
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
