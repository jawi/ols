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
package nl.lxtreme.ols.tool.asm45;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.StringUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * Provides a main dialog for the Asm45 analyser.
 *
 * @author Ansgar Kueckes
 */
public final class Asm45ProtocolAnalysisDialog extends BaseToolDialog<Asm45DataSet> implements
ExportAware<Asm45DataSet>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( Asm45ProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private JLabel idaLinesLabel;
  private JLabel bscLinesLabel;
  private JLabel idaLinesConfigLabel;
  private JLabel bscLinesConfigLabel;
  private JLabel lineSMCLabel;
  private JLabel lineSTMLabel;
  private JLabel lineEBGLabel;
  private JLabel lineBYTELabel;
  private JLabel lineBLLabel;
  private JLabel lineWRTLabel;
  private JLabel lineSYNCLabel;
  private JComboBox lineSMC;
  private JComboBox lineSTM;
  private JComboBox lineEBG;
  private JComboBox lineBYTE;
  private JComboBox lineBL;
  private JComboBox lineWRT;
  private JComboBox lineSYNC;
  private JCheckBox showInst;
  private JCheckBox showData;
  private JCheckBox showBusGrants;
  private JEditorPane outText;

  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * Creates a new Asm45ProtocolAnalysisDialog instance.
   *
   * @param aOwner
   *          the owner of this dialog;
   * @param aName
   *          the name of this dialog;
   * @param aContext
   *          the tool context.
   */
  public Asm45ProtocolAnalysisDialog( final Window aOwner, final ToolContext aToolContext,
      final BundleContext aContext, final Asm45Analyser aTool )
  {
    super( aOwner, aToolContext, aContext, aTool );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

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
    // Issue #114: avoid setting illegal values...
    setComboBoxIndex( this.lineSMC, aSettings, "lineSMC" );
    setComboBoxIndex( this.lineSTM, aSettings, "lineSTM" );
    setComboBoxIndex( this.lineEBG, aSettings, "lineEBG" );
    setComboBoxIndex( this.lineBYTE, aSettings, "lineBYTE" );
    setComboBoxIndex( this.lineBL, aSettings, "lineBL" );
    setComboBoxIndex( this.lineWRT, aSettings, "lineWRT" );
    setComboBoxIndex( this.lineSYNC, aSettings, "lineSYNC" );

    this.showInst.setSelected( aSettings.getBoolean( "showInst", this.showInst.isSelected() ) );
    this.showData.setSelected( aSettings.getBoolean( "showData", this.showData.isSelected() ) );
    this.showBusGrants.setSelected( aSettings.getBoolean( "showBusGrants", this.showBusGrants.isSelected() ) );
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
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "lineSMC", this.lineSMC.getSelectedIndex() );
    aSettings.putInt( "lineSTM", this.lineSTM.getSelectedIndex() );
    aSettings.putInt( "lineEBG", this.lineEBG.getSelectedIndex() );
    aSettings.putInt( "lineBYTE", this.lineBYTE.getSelectedIndex() );
    aSettings.putInt( "lineBL", this.lineBL.getSelectedIndex() );
    aSettings.putInt( "lineWRT", this.lineWRT.getSelectedIndex() );
    aSettings.putInt( "lineSYNC", this.lineSYNC.getSelectedIndex() );
    aSettings.putBoolean( "showInst", this.showInst.isSelected() );
    aSettings.putBoolean( "showData", this.showData.isSelected() );
    aSettings.putBoolean( "showBusGrants", this.showBusGrants.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final Asm45DataSet aAnalysisResult )
  {
    try
    {
      final String htmlPage;
      if ( aAnalysisResult != null )
      {
        htmlPage = toHtmlPage( null /* aFile */, aAnalysisResult );
      }
      else
      {
        htmlPage = getEmptyHtmlPage();
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
  protected void prepareToolTask( final ToolTask<Asm45DataSet> aToolTask )
  {
    final Asm45AnalyserTask toolTask = ( Asm45AnalyserTask )aToolTask;

    toolTask.setLineSMCIndex( this.lineSMC.getSelectedIndex() );
    toolTask.setLineSTMIndex( this.lineSTM.getSelectedIndex() );
    toolTask.setLineEBGIndex( this.lineEBG.getSelectedIndex() );
    toolTask.setLineBYTEIndex( this.lineBYTE.getSelectedIndex() );
    toolTask.setLineBLIndex( this.lineBL.getSelectedIndex() );
    toolTask.setLineWRTIndex( this.lineWRT.getSelectedIndex() );
    toolTask.setLineSYNCIndex( this.lineSYNC.getSelectedIndex() );
    toolTask.setReportInst( this.showInst.isSelected() );
    toolTask.setReportData( this.showData.isSelected() );
    toolTask.setReportBusGrants( this.showBusGrants.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void setControlsEnabled( final boolean aEnabled )
  {
    this.lineSMC.setEnabled( aEnabled );
    this.lineSTM.setEnabled( aEnabled );
    this.lineEBG.setEnabled( aEnabled );
    this.lineBYTE.setEnabled( aEnabled );
    this.lineBL.setEnabled( aEnabled );
    this.lineWRT.setEnabled( aEnabled );
    this.lineSYNC.setEnabled( aEnabled );
    this.showInst.setEnabled( aEnabled );
    this.showData.setEnabled( aEnabled );
    this.showBusGrants.setEnabled( aEnabled );

    this.closeAction.setEnabled( aEnabled );
    this.exportAction.setEnabled( aEnabled );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected boolean validateToolSettings()
  {
    boolean result = super.validateToolSettings();

    if ( result && ( getContext().getChannels() != Ols.MAX_CHANNELS ) )
    {
      JErrorDialog.showDialog( getOwner(), "Cannot start analysis!", "Not enough channels!",
          "For the Asm45 decoder, you need to have 32 channels enabled." );
      result = false;
    }

    return result;
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
        + " background-color: #E0E0FF; text-align: center; font-weight: bold; font-family: sans-serif; } " );
    aExporter.addCssStyle( "table td { border-width: 1px; padding: 2px; border-style: solid; border-color: gray;"
        + " font-family: monospace; } " );
    aExporter.addCssStyle( ".date { text-align: right; font-size: x-small; margin-bottom: 15px; } " );
    aExporter.addCssStyle( ".w100 { width: 100%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w20 { width: 20%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "Asm45 Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Bus configuration" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "IDA0/..IDA15/" );
    tr.addChild( TD ).addContent( "0..15" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "BSC0/..BSC5/" );
    tr.addChild( TD ).addContent( "16..21" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "SMC/" );
    tr.addChild( TD ).addContent( "{smc-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "STM/" );
    tr.addChild( TD ).addContent( "{stm-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "EBG" );
    tr.addChild( TD ).addContent( "{ebg-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "BYTE" );
    tr.addChild( TD ).addContent( "{byte-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "BL" );
    tr.addChild( TD ).addContent( "{bl-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "WRT/" );
    tr.addChild( TD ).addContent( "{wrt-bus-config}" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "SYNC" );
    tr.addChild( TD ).addContent( "{sync-bus-config}" );

    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Statistics" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Decoded words" );
    tr.addChild( TD ).addContent( "{decoded-words}" );

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Index" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Clocks" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Block" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Address" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Value" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Bus Grant" );
    tr.addChild( TH ).addAttribute( "class", "w10" ).addContent( "Type" );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addContent( "Event" );
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
    final int channelCount = getData().getChannels();

    final JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "Settings" );

    this.idaLinesLabel = createRightAlignedLabel( "IDA0/..IDA15/" );
    this.bscLinesLabel = createRightAlignedLabel( "BSC0/..BSC5/" );
    this.idaLinesConfigLabel = createRightAlignedLabel( "0..15" );
    this.bscLinesConfigLabel = createRightAlignedLabel( "16..21" );
    this.lineSMCLabel = createRightAlignedLabel( "SMC/" );
    this.lineSTMLabel = createRightAlignedLabel( "STM/" );
    this.lineEBGLabel = createRightAlignedLabel( "EBG" );
    this.lineBYTELabel = createRightAlignedLabel( "BYTE" );
    this.lineBLLabel = createRightAlignedLabel( "BL" );
    this.lineWRTLabel = createRightAlignedLabel( "WRT/" );
    this.lineSYNCLabel = createRightAlignedLabel( "SYNC" );

    panel.add( this.idaLinesLabel );
    panel.add( this.idaLinesConfigLabel );
    panel.add( this.bscLinesLabel );
    panel.add( this.bscLinesConfigLabel );

    panel.add( this.lineSMCLabel );
    this.lineSMC = SwingComponentUtils.createChannelSelector( channelCount, 22 );
    panel.add( this.lineSMC );

    panel.add( this.lineSTMLabel );
    this.lineSTM = SwingComponentUtils.createChannelSelector( channelCount, 23 );
    panel.add( this.lineSTM );

    panel.add( this.lineEBGLabel );
    this.lineEBG = SwingComponentUtils.createChannelSelector( channelCount, 25 );
    panel.add( this.lineEBG );

    panel.add( this.lineBYTELabel );
    this.lineBYTE = SwingComponentUtils.createChannelSelector( channelCount, 26 );
    panel.add( this.lineBYTE );

    panel.add( this.lineBLLabel );
    this.lineBL = SwingComponentUtils.createChannelSelector( channelCount, 27 );
    panel.add( this.lineBL );

    panel.add( this.lineWRTLabel );
    this.lineWRT = SwingComponentUtils.createChannelSelector( channelCount, 29 );
    panel.add( this.lineWRT );

    panel.add( this.lineSYNCLabel );
    this.lineSYNC = SwingComponentUtils.createChannelSelector( channelCount, 30 );
    panel.add( this.lineSYNC );

    this.showInst = new JCheckBox();
    this.showInst.setSelected( true );
    panel.add( createRightAlignedLabel( "Show instructions" ) );
    panel.add( this.showInst );

    this.showData = new JCheckBox();
    this.showData.setSelected( true );
    panel.add( createRightAlignedLabel( "Show data transfers" ) );
    panel.add( this.showData );

    this.showBusGrants = new JCheckBox();
    this.showBusGrants.setSelected( false );
    panel.add( createRightAlignedLabel( "Show bus grants" ) );
    panel.add( this.showBusGrants );

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
      @SuppressWarnings( "boxing" )
      public Object resolve( final String aMacro, final Element aParent )
      {
        if ( "date-now".equals( aMacro ) )
        {
          final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );
          return df.format( new Date() );
        }
        else if ( "smc-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSMC.getSelectedIndex();
        }
        else if ( "stm-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSTM.getSelectedIndex();
        }
        else if ( "ebg-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineEBG.getSelectedIndex();
        }
        else if ( "byte-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineBYTE.getSelectedIndex();
        }
        else if ( "bl-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineBL.getSelectedIndex();
        }
        else if ( "wrt-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineWRT.getSelectedIndex();
        }
        else if ( "sync-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSYNC.getSelectedIndex();
        }
        else if ( "decoded-words".equals( aMacro ) )
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
  private void storeToCsvFile( final File aSelectedFile, final Asm45DataSet aAnalysisResult )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aSelectedFile );

      exporter.setHeaders( "index", "clocks", "block", "address", "value", "bus grant", "type", "event" );

      final List<Asm45Data> dataSet = aAnalysisResult.getData();
      for ( int i = 0; i < dataSet.size(); i++ )
      {
        final Asm45Data ds = dataSet.get( i );
        exporter.addRow( Integer.valueOf( i ), Integer.valueOf( ds.getClocks() ),
            integerToHexString( ds.getBlock(), 2 ), StringUtils.integerToHexString( ds.getAddress(), 4 ),
            integerToHexString( ds.getValue(), 4 ), ds.getBusGrant() ? "X" : "-", ds.getType(), ds.getEvent() );
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
   * Stores the given analysis results to the given file as HTML.
   *
   * @param aSelectedFile
   * @param aAnalysisResult
   */
  private void storeToHtmlFile( final File aSelectedFile, final Asm45DataSet aAnalysisResult )
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
   * generate a HTML page
   *
   * @param aEmpty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  // table data:
  //
  // Index, Clocks, Block, Address, Value, Type, Event
  //
  private String toHtmlPage( final File aFile, final Asm45DataSet aAnalysisResult ) throws IOException
  {
    final MacroResolver macroResolver = new MacroResolver()
    {
      @Override
      @SuppressWarnings( "boxing" )
      public Object resolve( final String aMacro, final Element aParent )
      {
        if ( "date-now".equals( aMacro ) )
        {
          final DateFormat df = DateFormat.getDateInstance( DateFormat.LONG );
          return df.format( new Date() );
        }
        else if ( "smc-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSMC.getSelectedIndex();
        }
        else if ( "stm-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSTM.getSelectedIndex();
        }
        else if ( "ebg-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineEBG.getSelectedIndex();
        }
        else if ( "byte-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineBYTE.getSelectedIndex();
        }
        else if ( "bl-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineBL.getSelectedIndex();
        }
        else if ( "wrt-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineWRT.getSelectedIndex();
        }
        else if ( "sync-bus-config".equals( aMacro ) )
        {
          return Asm45ProtocolAnalysisDialog.this.lineSYNC.getSelectedIndex();
        }
        else if ( "decoded-words".equals( aMacro ) )
        {
          return Integer.valueOf( aAnalysisResult.getDecodedWordCount() );
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<Asm45Data> dataSet = aAnalysisResult.getData();
          Element tr;

          for ( int i = 0; i < dataSet.size(); i++ )
          {
            final Asm45Data data = dataSet.get( i );
            int index = i - aAnalysisResult.getTriggerEvent();

            String bgColor;

            if ( index == 0 )
            {
              // trigger event
              bgColor = "#ffa0ff";
            }
            else if ( data.getType().equals( Asm45Data.TYPE_INSTRUCTION ) )
            {
              // machine instruction
              bgColor = "#ffffff";
            }
            else
            {
              // data transfer (w/ or w/o bus grant)
              if ( data.getBusGrant() )
              {
                bgColor = "#64ff64";
              }
              else
              {
                bgColor = "#e0e0ff";
              }
            }

            tr = aParent.addChild( TR )
                .addAttribute( "style", "background-color: " + bgColor + "; text-align: center;" );
            tr.addChild( TD ).addContent( String.valueOf( index ) );
            tr.addChild( TD ).addContent( String.valueOf( data.getClocks() ) );
            tr.addChild( TD ).addContent( integerToHexString( data.getBlock(), 2 ) );
            tr.addChild( TD ).addContent( integerToHexString( data.getAddress(), 4 ) );
            tr.addChild( TD ).addContent( integerToHexString( data.getValue(), 4 ) );
            tr.addChild( TD ).addContent( data.getBusGrant() ? "X" : "-" );
            tr.addChild( TD ).addContent( data.getType() );
            tr.addChild( TD ).addAttribute( "style", "text-align: left;" ).addContent( data.getEvent() );
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
