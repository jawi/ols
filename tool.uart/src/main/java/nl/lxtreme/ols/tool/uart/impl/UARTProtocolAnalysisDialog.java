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
package nl.lxtreme.ols.tool.uart.impl;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.StringUtils.*;
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
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.tool.uart.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
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
 * The Dialog Class
 *
 * @author Frank Kunz The dialog class draws the basic dialog with a grid
 *         layout. The dialog consists of three main parts. A settings panel, a
 *         table panel and three buttons.
 */
public final class UARTProtocolAnalysisDialog extends BaseToolDialog<UARTDataSet> implements ExportAware<UARTDataSet>
{
  // INNER TYPES

  /**
   * Provides a combobox renderer for {@link UARTParity} values.
   */
  static final class UARTParityItemRenderer extends EnumItemRenderer<Parity>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final Parity aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( Parity.EVEN.equals( aValue ) )
      {
        text = "Even parity";
      }
      else if ( Parity.NONE.equals( aValue ) )
      {
        text = "No parity";
      }
      else if ( Parity.ODD.equals( aValue ) )
      {
        text = "Odd parity";
      }
      return text;
    }
  }

  /**
   * Provides a combobox renderer for {@link UARTStopBits} values.
   */
  static final class UARTStopBitsItemRenderer extends EnumItemRenderer<StopBits>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final StopBits aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( StopBits.ONE.equals( aValue ) )
      {
        text = "1";
      }
      else if ( StopBits.ONE_HALF.equals( aValue ) )
      {
        text = "1.5";
      }
      else if ( StopBits.TWO.equals( aValue ) )
      {
        text = "2";
      }
      return text;
    }
  }

  /**
   * Provides a combobox renderer for {@link BitOrder} values.
   */
  static final class UARTBitOrderItemRenderer extends EnumItemRenderer<BitOrder>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final BitOrder aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( BitOrder.LSB_FIRST.equals( aValue ) )
      {
        text = "LSB first";
      }
      else if ( BitOrder.MSB_FIRST.equals( aValue ) )
      {
        text = "MSB first";
      }
      return text;
    }
  }

  /**
   * Provides a combobox renderer for {@link BitEncoding} values.
   */
  static final class UARTBitEncodingItemRenderer extends EnumItemRenderer<BitEncoding>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final BitEncoding aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( BitEncoding.HIGH_IS_MARK.equals( aValue ) )
      {
        text = "High is mark (1)";
      }
      else if ( BitEncoding.HIGH_IS_SPACE.equals( aValue ) )
      {
        text = "High is space (0)";
      }
      return text;
    }
  }

  /**
   * Provides a combobox renderer for {@link BitLevel} values.
   */
  static final class UARTIdleLevelItemRenderer extends EnumItemRenderer<BitLevel>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final BitLevel aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( BitLevel.HIGH.equals( aValue ) )
      {
        text = "High (start = L, stop = H)";
      }
      else if ( BitLevel.LOW.equals( aValue ) )
      {
        text = "Low (start = H, stop = L)";
      }
      return text;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( UARTProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private JComboBox rxd;
  private JComboBox txd;
  private JComboBox cts;
  private JComboBox rts;
  private JComboBox dtr;
  private JComboBox dsr;
  private JComboBox dcd;
  private JComboBox ri;
  private JComboBox parity;
  private JComboBox bits;
  private JComboBox stop;
  private JComboBox bitEncoding;
  private JComboBox bitOrder;
  private JComboBox idleLevel;
  private JCheckBox autoDetectBaudRate;
  private JComboBox baudrate;
  private JEditorPane outText;

  private RestorableAction runAnalysisAction;
  private Action closeAction;
  private Action exportAction;

  // CONSTRUCTORS

  /**
   * Creates a new UARTProtocolAnalysisDialog instance.
   *
   * @param aOwner
   *          the owner of this dialog;
   * @param aToolContext
   *          the tool context;
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aTool
   *          the {@link UARTAnalyser} tool.
   */
  public UARTProtocolAnalysisDialog( final Window aOwner, final ToolContext aToolContext, final BundleContext aContext,
      final UARTAnalyser aTool )
  {
    super( aOwner, aToolContext, aContext, aTool );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void exportToFile( final File aOutputFile, final nl.lxtreme.ols.tool.base.ExportAware.ExportFormat aFormat )
      throws IOException
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
   * {@inheritDoc}
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    // Issue #114: avoid setting illegal values...
    setComboBoxIndex( this.rxd, aSettings, "rxd" );
    setComboBoxIndex( this.txd, aSettings, "txd" );
    setComboBoxIndex( this.cts, aSettings, "cts" );
    setComboBoxIndex( this.rts, aSettings, "rts" );
    setComboBoxIndex( this.dtr, aSettings, "dtr" );
    setComboBoxIndex( this.dsr, aSettings, "dsr" );
    setComboBoxIndex( this.dcd, aSettings, "dcd" );
    setComboBoxIndex( this.ri, aSettings, "ri" );

    this.parity.setSelectedIndex( aSettings.getInt( "parity", this.parity.getSelectedIndex() ) );
    this.bits.setSelectedIndex( aSettings.getInt( "bits", this.bits.getSelectedIndex() ) );
    this.stop.setSelectedIndex( aSettings.getInt( "stop", this.stop.getSelectedIndex() ) );
    this.idleLevel.setSelectedIndex( aSettings.getInt( "idle-state", this.idleLevel.getSelectedIndex() ) );
    this.bitEncoding.setSelectedIndex( aSettings.getInt( "bit-encoding", this.bitEncoding.getSelectedIndex() ) );
    this.bitOrder.setSelectedIndex( aSettings.getInt( "bit-order", this.bitOrder.getSelectedIndex() ) );
    this.baudrate.setSelectedItem( Integer.valueOf( aSettings.getInt( "baudrate", 9600 ) ) );
    this.autoDetectBaudRate.setSelected( aSettings.getBoolean( "auto-baudrate", this.autoDetectBaudRate.isSelected() ) );
  }

  /**
   * {@inheritDoc}
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
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "rxd", this.rxd.getSelectedIndex() );
    aSettings.putInt( "txd", this.txd.getSelectedIndex() );
    aSettings.putInt( "cts", this.cts.getSelectedIndex() );
    aSettings.putInt( "rts", this.rts.getSelectedIndex() );
    aSettings.putInt( "dtr", this.dtr.getSelectedIndex() );
    aSettings.putInt( "dsr", this.dsr.getSelectedIndex() );
    aSettings.putInt( "dcd", this.dcd.getSelectedIndex() );
    aSettings.putInt( "ri", this.ri.getSelectedIndex() );
    aSettings.putInt( "parity", this.parity.getSelectedIndex() );
    aSettings.putInt( "bits", this.bits.getSelectedIndex() );
    aSettings.putInt( "stop", this.stop.getSelectedIndex() );
    aSettings.putInt( "idle-state", this.idleLevel.getSelectedIndex() );
    aSettings.putInt( "bit-encoding", this.bitEncoding.getSelectedIndex() );
    aSettings.putInt( "bit-order", this.bitOrder.getSelectedIndex() );
    aSettings.putInt( "baudrate", ( ( Integer )this.baudrate.getSelectedItem() ).intValue() );
    aSettings.putBoolean( "auto-baudrate", this.autoDetectBaudRate.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final UARTDataSet aAnalysisResult )
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
  protected void prepareToolTask( final ToolTask<UARTDataSet> aToolTask )
  {
    final UARTAnalyserTask toolTask = ( UARTAnalyserTask )aToolTask;

    // The value at index zero is "Unused", so extracting one of all items
    // causes all "unused" values to be equivalent to -1, which is interpreted
    // as not used...
    toolTask.setRxdIndex( this.rxd.getSelectedIndex() - 1 );
    toolTask.setTxdIndex( this.txd.getSelectedIndex() - 1 );
    toolTask.setCtsIndex( this.cts.getSelectedIndex() - 1 );
    toolTask.setRtsIndex( this.rts.getSelectedIndex() - 1 );
    toolTask.setDcdIndex( this.dcd.getSelectedIndex() - 1 );
    toolTask.setRiIndex( this.ri.getSelectedIndex() - 1 );
    toolTask.setDsrIndex( this.dsr.getSelectedIndex() - 1 );
    toolTask.setDtrIndex( this.dtr.getSelectedIndex() - 1 );
    // Handle the auto detect option for baudrates...
    if ( this.autoDetectBaudRate.isSelected() )
    {
      toolTask.setBaudRate( UARTAnalyserTask.AUTO_DETECT_BAUDRATE );
    }
    else
    {
      toolTask.setBaudRate( ( ( Integer )this.baudrate.getSelectedItem() ).intValue() );
    }

    // Other properties...
    toolTask.setIdleLevel( ( BitLevel )this.idleLevel.getSelectedItem() );
    toolTask.setBitEncoding( ( BitEncoding )this.bitEncoding.getSelectedItem() );
    toolTask.setBitOrder( ( BitOrder )this.bitOrder.getSelectedItem() );
    toolTask.setParity( ( Parity )this.parity.getSelectedItem() );
    toolTask.setStopBits( ( StopBits )this.stop.getSelectedItem() );
    toolTask.setBitCount( NumberUtils.smartParseInt( ( String )this.bits.getSelectedItem(), 8 ) );
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
    this.rxd.setEnabled( aEnable );
    this.txd.setEnabled( aEnable );
    this.cts.setEnabled( aEnable );
    this.rts.setEnabled( aEnable );
    this.dtr.setEnabled( aEnable );
    this.dsr.setEnabled( aEnable );
    this.dcd.setEnabled( aEnable );
    this.ri.setEnabled( aEnable );
    this.parity.setEnabled( aEnable );
    this.bits.setEnabled( aEnable );
    this.stop.setEnabled( aEnable );
    this.idleLevel.setEnabled( aEnable );
    this.bitEncoding.setEnabled( aEnable );
    this.bitOrder.setEnabled( aEnable );

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
    aExporter.addCssStyle( ".error { color: red; } " );
    aExporter.addCssStyle( ".warning { color: orange; } " );
    aExporter.addCssStyle( ".date { text-align: right; font-size: x-small; margin-bottom: 15px; } " );
    aExporter.addCssStyle( ".w100 { width: 100%; } " );
    aExporter.addCssStyle( ".w35 { width: 35%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".w8 { width: 8%; } " );
    aExporter.addCssStyle( ".w7 { width: 7%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "UART Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "Generated: ", "{date-now}" );

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
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "Baudrate" );
    tr.addChild( TD ).addContent( "{baudrate}" );

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addAttribute( "colspan", "2" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "RxD" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "TxD" );
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

    final Integer[] baudrates = new Integer[AsyncSerialDataDecoder.COMMON_BAUDRATES.length];
    for ( int i = 0; i < baudrates.length; i++ )
    {
      baudrates[i] = Integer.valueOf( AsyncSerialDataDecoder.COMMON_BAUDRATES[i] );
    }
    final String[] bitarray = new String[10];
    // allow symbol lengths between 5 and 14 bits...
    for ( int i = 0; i < bitarray.length; i++ )
    {
      bitarray[i] = String.format( "%d", Integer.valueOf( i + 5 ) );
    }

    final JPanel settings = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( settings, "Settings" );

    settings.add( createRightAlignedLabel( "RxD" ) );
    this.rxd = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.rxd );

    settings.add( createRightAlignedLabel( "TxD" ) );
    this.txd = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.txd );

    settings.add( createRightAlignedLabel( "CTS" ) );
    this.cts = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.cts );

    settings.add( createRightAlignedLabel( "RTS" ) );
    this.rts = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.rts );

    settings.add( createRightAlignedLabel( "DTR" ) );
    this.dtr = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.dtr );

    settings.add( createRightAlignedLabel( "DSR" ) );
    this.dsr = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.dsr );

    settings.add( createRightAlignedLabel( "DCD" ) );
    this.dcd = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.dcd );

    settings.add( createRightAlignedLabel( "RI" ) );
    this.ri = SwingComponentUtils.createOptionalChannelSelector( channelCount );
    settings.add( this.ri );

    settings.add( createRightAlignedLabel( "Baudrate" ) );
    this.autoDetectBaudRate = new JCheckBox( "Auto detect" );
    settings.add( this.autoDetectBaudRate );

    settings.add( new JLabel( "" ) );
    this.baudrate = new JComboBox( baudrates );
    // Issue #90: allow custom baudrates to be specified...
    this.baudrate.setEditable( true );
    this.baudrate.setSelectedIndex( 0 );
    settings.add( this.baudrate );

    this.autoDetectBaudRate.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JCheckBox cb = ( JCheckBox )aEvent.getSource();
        UARTProtocolAnalysisDialog.this.baudrate.setEnabled( !cb.isSelected() );
      }
    } );

    settings.add( createRightAlignedLabel( "Parity" ) );
    this.parity = new JComboBox( Parity.values() );
    this.parity.setSelectedIndex( 0 );
    this.parity.setRenderer( new UARTParityItemRenderer() );
    settings.add( this.parity );

    settings.add( createRightAlignedLabel( "Bits" ) );
    this.bits = new JComboBox( bitarray );
    this.bits.setSelectedIndex( 3 );
    settings.add( this.bits );

    settings.add( createRightAlignedLabel( "Stopbits" ) );
    this.stop = new JComboBox( StopBits.values() );
    this.stop.setSelectedIndex( 0 );
    this.stop.setRenderer( new UARTStopBitsItemRenderer() );
    settings.add( this.stop );

    settings.add( createRightAlignedLabel( "Idle level" ) );
    this.idleLevel = new JComboBox( BitLevel.values() );
    this.idleLevel.setSelectedIndex( 0 );
    this.idleLevel.setRenderer( new UARTIdleLevelItemRenderer() );
    settings.add( this.idleLevel );

    settings.add( createRightAlignedLabel( "Bit encoding" ) );
    this.bitEncoding = new JComboBox( BitEncoding.values() );
    this.bitEncoding.setSelectedIndex( 0 );
    this.bitEncoding.setRenderer( new UARTBitEncodingItemRenderer() );
    settings.add( this.bitEncoding );

    settings.add( createRightAlignedLabel( "Bit order" ) );
    this.bitOrder = new JComboBox( BitOrder.values() );
    this.bitOrder.setSelectedIndex( 0 );
    this.bitOrder.setRenderer( new UARTBitOrderItemRenderer() );
    settings.add( this.bitOrder );

    SpringLayoutUtils.makeEditorGrid( settings, 10, 4 );

    return settings;
  }

  /**
   * generate a HTML page
   *
   * @param empty
   *          if this is true an empty output is generated
   * @return String with HTML data
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
        else if ( "decoded-bytes".equals( aMacro ) || "detected-bus-errors".equals( aMacro )
            || "baudrate".equals( aMacro ) )
        {
          return "-";
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          return null;
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
   * exports the data to a CSV file
   *
   * @param aFile
   *          File object
   */
  private void storeToCsvFile( final File aFile, final UARTDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "RxD event", "TxD event",
          "RxD data", "TxD data" );

      final List<UARTData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final UARTData ds = decodedData.get( i );

        final String startTime = Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) );
        final String endTime = Unit.Time.format( aDataSet.getTime( ds.getEndSampleIndex() ) );

        String eventType = null;
        String rxdEvent = null;
        String txdEvent = null;
        String rxdData = null;
        String txdData = null;

        switch ( ds.getType() )
        {
          case UARTData.UART_TYPE_EVENT:
            eventType = ds.getEventName();
            break;

          case UARTData.UART_TYPE_RXEVENT:
            rxdEvent = ds.getEventName();
            break;

          case UARTData.UART_TYPE_TXEVENT:
            txdEvent = ds.getEventName();
            break;

          case UARTData.UART_TYPE_RXDATA:
            rxdData = Integer.toString( ds.getData() );
            break;

          case UARTData.UART_TYPE_TXDATA:
            txdData = Integer.toString( ds.getData() );
            break;

          default:
            break;
        }

        exporter.addRow( Integer.valueOf( i ), startTime, endTime, Boolean.valueOf( ds.isEvent() ), eventType,
            rxdEvent, txdEvent, rxdData, txdData );
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
  private void storeToHtmlFile( final File aFile, final UARTDataSet aDataSet )
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
   * @param empty
   *          if this is true an empty output is generated
   * @return String with HTML data
   */
  private String toHtmlPage( final File aFile, final UARTDataSet aDataSet ) throws IOException
  {
    final int bitCount = Integer.parseInt( ( String )this.bits.getSelectedItem() );
    final int bitAdder = ( ( bitCount % 4 ) != 0 ) ? 1 : 0;

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
          return Integer.valueOf( aDataSet.getDecodedSymbols() );
        }
        else if ( "detected-bus-errors".equals( aMacro ) )
        {
          return Integer.valueOf( aDataSet.getDetectedErrors() );
        }
        else if ( "baudrate".equals( aMacro ) )
        {
          final String baudrate;
          if ( aDataSet.getBaudRate() <= 0 )
          {
            baudrate = "<span class='error'>Baudrate calculation failed!</span>";
          }
          else
          {
            baudrate = String.format( "%d (exact: %d)", Integer.valueOf( aDataSet.getBaudRate() ),
                Integer.valueOf( aDataSet.getBaudRateExact() ) );
            if ( !aDataSet.isBitLengthUsable() )
            {
              return baudrate
                  .concat( " <span class='warning'>The baudrate may be wrong, use a higher samplerate to avoid this!</span>" );
            }

            return baudrate;
          }
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<UARTData> decodedData = aDataSet.getData();
          Element tr;

          for ( int i = 0; i < decodedData.size(); i++ )
          {
            final UARTData ds = decodedData.get( i );

            if ( ds.isEvent() )
            {
              String rxEventData = "";
              String txEventData = "";

              String bgColor;
              if ( UARTData.UART_TYPE_EVENT == ds.getType() )
              {
                rxEventData = txEventData = ds.getEventName();
                bgColor = "#e0e0e0";
              }
              else if ( UARTData.UART_TYPE_RXEVENT == ds.getType() )
              {
                rxEventData = ds.getEventName();
                bgColor = "#c0ffc0";
              }
              else if ( UARTData.UART_TYPE_TXEVENT == ds.getType() )
              {
                txEventData = ds.getEventName();
                bgColor = "#c0ffc0";
              }
              else
              {
                // unknown event
                bgColor = "#ff8000";
              }

              if ( txEventData.endsWith( "_ERR" ) || rxEventData.endsWith( "_ERR" ) )
              {
                bgColor = "#ff8000";
              }

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( rxEventData );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD ).addContent( txEventData );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else
            {
              String rxDataHex = "", rxDataBin = "", rxDataDec = "", rxDataASCII = "";
              String txDataHex = "", txDataBin = "", txDataDec = "", txDataASCII = "";

              // Normal data...
              if ( UARTData.UART_TYPE_RXDATA == ds.getType() )
              {
                final int rxData = ds.getData();

                rxDataHex = integerToHexString( rxData, ( bitCount / 4 ) + bitAdder );
                rxDataBin = integerToBinString( rxData, bitCount );
                rxDataDec = String.valueOf( rxData );
                rxDataASCII = toASCII( ( char )rxData );
              }
              else
              /* if ( UARTData.UART_TYPE_TXDATA == ds.getType() ) */
              {
                final int txData = ds.getData();

                txDataHex = integerToHexString( txData, ( bitCount / 4 ) + bitAdder );
                txDataBin = integerToBinString( txData, bitCount );
                txDataDec = String.valueOf( txData );
                txDataASCII = toASCII( txData );
              }

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( "0x", rxDataHex );
              tr.addChild( TD ).addContent( "0b", rxDataBin );
              tr.addChild( TD ).addContent( rxDataDec );
              tr.addChild( TD ).addContent( rxDataASCII );
              tr.addChild( TD ).addContent( "0x", txDataHex );
              tr.addChild( TD ).addContent( "0b", txDataBin );
              tr.addChild( TD ).addContent( txDataDec );
              tr.addChild( TD ).addContent( txDataASCII );
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
