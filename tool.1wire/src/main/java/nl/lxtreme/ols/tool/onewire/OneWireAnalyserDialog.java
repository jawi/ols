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
package nl.lxtreme.ols.tool.onewire;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jawi
 */
public class OneWireAnalyserDialog extends BaseAsyncToolDialog<OneWireDataSet, OneWireAnalyserWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JComboBox owLine;
  private JComboBox owMode;
  private JEditorPane outText;
  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public OneWireAnalyserDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#onToolWorkerReady(java.lang.Object)
   */
  @Override
  public void onToolWorkerReady( final OneWireDataSet aAnalysisResult )
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

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.owMode.setSelectedIndex( aSettings.getInt( "owMode", this.owMode.getSelectedIndex() ) );
    this.owLine.setSelectedIndex( aSettings.getInt( "owLine", this.owLine.getSelectedIndex() ) );
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
   * set the controls of the dialog enabled/disabled
   * 
   * @param aEnabled
   *          status of the controls
   */
  @Override
  public void setControlsEnabled( final boolean aEnabled )
  {
    this.owLine.setEnabled( aEnabled );
    this.owMode.setEnabled( aEnabled );

    this.closeAction.setEnabled( aEnabled );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "owLine", this.owLine.getSelectedIndex() );
    aSettings.putInt( "owMode", this.owMode.getSelectedIndex() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final OneWireAnalyserWorker aToolWorker )
  {
    aToolWorker.setOneWireLineIndex( this.owLine.getSelectedIndex() );
    aToolWorker.setOneWireBusMode( ( this.owMode.getSelectedIndex() == 0 ) ? OneWireBusMode.STANDARD
        : OneWireBusMode.OVERDRIVE );
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
    body.addChild( H1 ).addContent( "1-Wire&reg; Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
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
   * Creates the settings pane.
   * 
   * @return a settings pane, never <code>null</code>.
   */
  private JComponent createSettingsPane()
  {
    final String channels[] = new String[32];
    for ( int i = 0; i < 32; i++ )
    {
      channels[i] = "Channel " + i;
    }

    final String modes[] = new String[] { "Standard", "Overdrive" };

    final JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "Settings" );

    panel.add( createRightAlignedLabel( "1-Wire Line" ) );
    this.owLine = new JComboBox( channels );
    this.owLine.setSelectedIndex( 0 );
    panel.add( this.owLine );

    panel.add( createRightAlignedLabel( "Bus Mode" ) );
    this.owMode = new JComboBox( modes );
    this.owMode.setSelectedIndex( 0 );
    panel.add( this.owMode );

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
   * Initializes this dialog.
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
  private String toHtmlPage( final File aFile, final OneWireDataSet aAnalysisResult ) throws IOException
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
          final List<OneWireData> dataSet = aAnalysisResult.getData();
          Element tr;

          for ( int i = 0; i < dataSet.size(); i++ )
          {
            final OneWireData data = dataSet.get( i );

            if ( data.isEvent() )
            {
              // this is an event
              final String event = data.getEventName();

              String bgColor;
              if ( OneWireDataSet.OW_RESET.equals( event ) )
              {
                bgColor = "#e0e0e0";
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
