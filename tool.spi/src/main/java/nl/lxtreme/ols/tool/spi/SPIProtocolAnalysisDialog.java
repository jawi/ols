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
package nl.lxtreme.ols.tool.spi;


import static nl.lxtreme.ols.util.ExportUtils.HtmlExporter.*;
import static nl.lxtreme.ols.util.StringUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
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
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.ExportUtils.CsvExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;
import nl.lxtreme.ols.util.NumberUtils.BitOrder;
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
public final class SPIProtocolAnalysisDialog extends BaseToolDialog<SPIDataSet> implements ExportAware<SPIDataSet>,
    PropertyChangeListener
{
  // INNER TYPES

  /**
   * Provides a combobox renderer for {@link BitOrder} enums.
   */
  static class BitOrderItemRenderer extends EnumItemRenderer<BitOrder>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.util.swing.component.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final BitOrder aValue )
    {
      switch ( aValue )
      {
        case LSB_FIRST:
          return "LSB first";
        case MSB_FIRST:
          return "MSB first";
      }
      // Strange, we shouldn't be here...
      LOG.warning( "We should not be here actually! Value = " + aValue );
      return super.getDisplayValue( aValue );
    }
  }

  static class SPIFIModeRenderer extends EnumItemRenderer<SPIFIMode>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final SPIFIMode aValue )
    {
      switch ( aValue )
      {
        case STANDARD:
          return "Standard";
        case DUAL:
          return "Dual mode";
        case QUAD:
          return "Quad mode";
      }
      return super.getDisplayValue( aValue );
    }
  }

  /**
   * Provides a combobox renderer for SPIMode enums.
   */
  static class SPIModeRenderer extends EnumItemRenderer<SPIMode>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.util.swing.component.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final SPIMode aValue )
    {
      switch ( aValue )
      {
        case AUTODETECT:
          return "Auto-detect";
        case MODE_0:
          return "Mode 0";
        case MODE_1:
          return "Mode 1";
        case MODE_2:
          return "Mode 2";
        case MODE_3:
          return "Mode 3";
      }
      return super.getDisplayValue( aValue );
    }

    /**
     * @see nl.lxtreme.ols.util.swing.component.EnumItemRenderer#getToolTip(java.lang.Object)
     */
    @Override
    protected String getToolTip( final Object aValue )
    {
      switch ( ( SPIMode )aValue )
      {
        case AUTODETECT:
          return "Tries to determine the SPI mode based on the clock polarity (CPOL).";
        case MODE_0:
          return "CPOL = 0, CPHA = 0";
        case MODE_1:
          return "CPOL = 0, CPHA = 1";
        case MODE_2:
          return "CPOL = 1, CPHA = 0";
        case MODE_3:
          return "CPOL = 1, CPHA = 1";
      }
      // Strange, we shouldn't be here...
      LOG.warning( "We should not be here actually! Value = " + aValue );
      return super.getToolTip( aValue );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SPIProtocolAnalysisDialog.class.getName() );

  // VARIABLES

  private JLabel misoLabel;
  private JLabel mosiLabel;
  private JLabel io2Label;
  private JLabel io3Label;

  private JComboBox sck;
  private JComboBox miso; // IO0
  private JComboBox mosi; // IO1
  private JComboBox io2; // IO2
  private JComboBox io3; // IO3
  private JComboBox cs;
  private JComboBox mode;
  private JComboBox bits;
  private JComboBox order;
  private JComboBox spifiMode;
  private JEditorPane outText;
  private JCheckBox reportCS;
  private JCheckBox honourCS;
  private JCheckBox invertCS;

  private RestorableAction runAnalysisAction;
  private Action exportAction;
  private Action closeAction;

  private SPIMode detectedSPIMode;

  // CONSTRUCTORS

  /**
   * Creates a new SPIProtocolAnalysisDialog instance.
   *
   * @param aOwner
   *          the owner of this dialog;
   * @param aToolContext
   *          the tool context;
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aTool
   *          the {@link SPIAnalyser} tool.
   */
  public SPIProtocolAnalysisDialog( final Window aOwner, final ToolContext aToolContext, final BundleContext aContext,
      final SPIAnalyser aTool )
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
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    final String name = aEvent.getPropertyName();

    if ( SPIAnalyserTask.PROPERTY_AUTO_DETECT_MODE.equals( name ) )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          final Object value = aEvent.getNewValue();

          setAutoDetectSPIMode( ( SPIMode )value );
        }
      } );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.spifiMode.setSelectedIndex( aSettings.getInt( "protocol", this.spifiMode.getSelectedIndex() ) );
    this.reportCS.setSelected( aSettings.getBoolean( "reportCS", this.reportCS.isSelected() ) );
    this.honourCS.setSelected( aSettings.getBoolean( "honourCS", this.honourCS.isSelected() ) );
    this.invertCS.setSelected( aSettings.getBoolean( "invertCS", this.invertCS.isSelected() ) );

    // Issue #114: avoid setting illegal values...
    setComboBoxIndex( this.sck, aSettings, "sck" );
    setComboBoxIndex( this.miso, aSettings, "miso" );
    setComboBoxIndex( this.mosi, aSettings, "mosi" );
    setComboBoxIndex( this.io2, aSettings, "io2" );
    setComboBoxIndex( this.io3, aSettings, "io3" );
    setComboBoxIndex( this.cs, aSettings, "cs" );

    this.mode.setSelectedIndex( aSettings.getInt( "mode", this.mode.getSelectedIndex() ) );
    this.bits.setSelectedIndex( aSettings.getInt( "bits", this.bits.getSelectedIndex() ) );
    this.order.setSelectedIndex( aSettings.getInt( "order", this.order.getSelectedIndex() ) );

    // Make sure the settings are reflected in the UI...
    updateSPIFIModeSettings( ( SPIFIMode )this.spifiMode.getSelectedItem() );
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
   * Sets the auto detected SPI mode to the given value.
   *
   * @param aMode
   *          the detected SPI mode, cannot be <code>null</code>.
   */
  public void setAutoDetectSPIMode( final SPIMode aMode )
  {
    this.detectedSPIMode = aMode;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putBoolean( "reportCS", this.reportCS.isSelected() );
    aSettings.putBoolean( "honourCS", this.honourCS.isSelected() );
    aSettings.putBoolean( "invertCS", this.invertCS.isSelected() );
    aSettings.putInt( "protocol", this.spifiMode.getSelectedIndex() );
    aSettings.putInt( "sck", this.sck.getSelectedIndex() );
    aSettings.putInt( "miso", this.miso.getSelectedIndex() );
    aSettings.putInt( "mosi", this.mosi.getSelectedIndex() );
    aSettings.putInt( "io2", this.io2.getSelectedIndex() );
    aSettings.putInt( "io3", this.io3.getSelectedIndex() );
    aSettings.putInt( "cs", this.cs.getSelectedIndex() );
    aSettings.putInt( "mode", this.mode.getSelectedIndex() );
    aSettings.putInt( "bits", this.bits.getSelectedIndex() );
    aSettings.putInt( "order", this.order.getSelectedIndex() );
  }

  /**
   * @param aMode
   *          the SPIFI mode to update the settings to, may be <code>null</code>
   *          .
   */
  final void updateSPIFIModeSettings( final SPIFIMode aMode )
  {
    boolean enabled = false;
    if ( ( aMode == null ) || SPIFIMode.STANDARD.equals( aMode ) )
    {
      this.misoLabel.setText( "MISO" );
      this.mosiLabel.setText( "MOSI" );
    }
    else
    {
      this.mosiLabel.setText( "IO0" );
      this.misoLabel.setText( "IO1" );

      enabled = SPIFIMode.QUAD.equals( aMode );
    }

    this.io2Label.setEnabled( enabled );
    this.io2.setEnabled( enabled );
    this.io3Label.setEnabled( enabled );
    this.io3.setEnabled( enabled );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final SPIDataSet aAnalysisResult )
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
    // No-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<SPIDataSet> aToolTask )
  {
    SPIAnalyserTask toolTask = ( SPIAnalyserTask )aToolTask;

    toolTask.setBitCount( Integer.parseInt( ( String )this.bits.getSelectedItem() ) - 1 );
    toolTask.setCSIndex( this.cs.getSelectedIndex() );
    toolTask.setSCKIndex( this.sck.getSelectedIndex() );
    toolTask.setIO0Index( this.mosi.getSelectedIndex() - 1 );
    toolTask.setIO1Index( this.miso.getSelectedIndex() - 1 );
    toolTask.setIO2Index( this.io2.getSelectedIndex() - 1 );
    toolTask.setIO3Index( this.io3.getSelectedIndex() - 1 );
    toolTask.setProtocol( ( SPIFIMode )this.spifiMode.getSelectedItem() );
    toolTask.setReportCS( this.reportCS.isSelected() );
    toolTask.setHonourCS( this.honourCS.isSelected() );
    toolTask.setInvertCS( this.invertCS.isSelected() );
    toolTask.setOrder( ( BitOrder )this.order.getSelectedItem() );
    toolTask.setSPIMode( ( SPIMode )this.mode.getSelectedItem() );

    // Register ourselves as property change listener...
    toolTask.addPropertyChangeListener( this );
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
    this.spifiMode.setEnabled( aEnable );
    this.reportCS.setEnabled( aEnable );
    this.honourCS.setEnabled( aEnable );
    this.sck.setEnabled( aEnable );
    this.miso.setEnabled( aEnable );
    this.mosi.setEnabled( aEnable );
    this.io2.setEnabled( aEnable );
    this.io3.setEnabled( aEnable );
    this.cs.setEnabled( aEnable );
    this.mode.setEnabled( aEnable );
    this.bits.setEnabled( aEnable );
    this.order.setEnabled( aEnable );

    this.closeAction.setEnabled( aEnable );
    this.exportAction.setEnabled( aEnable );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected boolean validateToolSettings()
  {
    BitSet bitset = new BitSet();
    bitset.set( this.cs.getSelectedIndex() );
    bitset.set( this.sck.getSelectedIndex() );
    int expectedBitCount = 2;

    final SPIFIMode protocol = ( SPIFIMode )this.spifiMode.getSelectedItem();
    if ( SPIFIMode.DUAL.equals( protocol ) )
    {
      // Both MOSI/IO0 & MISO/IO1 should be defined...
      if ( ( this.mosi.getSelectedIndex() < 1 ) || ( this.miso.getSelectedIndex() < 1 ) )
      {
        JErrorDialog.showDialog( getOwner(), "Cannot start analysis!", "Invalid settings detected!",
            "For dual-mode SPI, you need to assign both IO0 and IO1." );
        return false;
      }

      bitset.set( this.mosi.getSelectedIndex() - 1 );
      bitset.set( this.miso.getSelectedIndex() - 1 );
      expectedBitCount += 2;
    }
    else if ( SPIFIMode.QUAD.equals( protocol ) )
    {
      // All IO0..3 should be defined...
      if ( ( this.mosi.getSelectedIndex() < 1 ) || ( this.miso.getSelectedIndex() < 1 ) || //
          ( this.io2.getSelectedIndex() < 1 ) || ( this.io3.getSelectedIndex() < 1 ) )
      {
        JErrorDialog.showDialog( getOwner(), "Cannot start analysis!", "Invalid settings detected!",
            "For quad-mode SPI, you need to assign IO0, IO1, IO2 and IO3." );
        return false;
      }

      bitset.set( this.mosi.getSelectedIndex() - 1 );
      bitset.set( this.miso.getSelectedIndex() - 1 );
      bitset.set( this.io2.getSelectedIndex() - 1 );
      bitset.set( this.io3.getSelectedIndex() - 1 );
      expectedBitCount += 4;
    }
    else
    {
      if ( this.miso.getSelectedIndex() > 0 )
      {
        bitset.set( this.miso.getSelectedIndex() - 1 );
        expectedBitCount++;
      }
      if ( this.mosi.getSelectedIndex() > 0 )
      {
        bitset.set( this.mosi.getSelectedIndex() - 1 );
        expectedBitCount++;
      }
    }

    if ( bitset.cardinality() != expectedBitCount )
    {
      JErrorDialog.showDialog( getOwner(), "Cannot start analysis!", "Invalid settings detected!",
          "Not all signals are assigned to unique channels." );
      return false;
    }

    return true;
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
    aExporter.addCssStyle( ".w35 { width: 35%; } " );
    aExporter.addCssStyle( ".w30 { width: 30%; } " );
    aExporter.addCssStyle( ".w15 { width: 15%; } " );
    aExporter.addCssStyle( ".w10 { width: 10%; } " );
    aExporter.addCssStyle( ".w8 { width: 8%; } " );
    aExporter.addCssStyle( ".w7 { width: 7%; } " );

    final Element body = aExporter.getBody();
    body.addChild( H1 ).addContent( "SPI Analysis results" );
    body.addChild( HR );
    body.addChild( DIV ).addAttribute( "class", "date" ).addContent( "Generated: ", "{date-now}" );

    Element table, tr, thead, tbody;

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    tbody = table.addChild( TBODY );
    tr = tbody.addChild( TR );
    tr.addChild( TH ).addAttribute( "colspan", "2" ).addContent( "Configuration" );
    tr = tbody.addChild( TR );
    tr.addChild( TD ).addAttribute( "class", "w30" ).addContent( "SPI mode" );
    tr.addChild( TD ).addContent( "{detected-spi-mode}" );

    table = body.addChild( TABLE ).addAttribute( "class", "w100" );
    thead = table.addChild( THEAD );
    tr = thead.addChild( TR );
    tr.addChild( TH ).addAttribute( "class", "w30" ).addAttribute( "colspan", "2" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "MOSI" );
    tr.addChild( TH ).addAttribute( "class", "w35" ).addAttribute( "colspan", "4" ).addContent( "MISO" );
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

    final JPanel settings = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( settings, "Settings" );

    settings.add( createRightAlignedLabel( "Protocol" ) );
    this.spifiMode = new JComboBox( SPIFIMode.values() );
    this.spifiMode.setSelectedIndex( 0 );
    this.spifiMode.setRenderer( new SPIFIModeRenderer() );
    this.spifiMode.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox cb = ( JComboBox )aEvent.getSource();

        updateSPIFIModeSettings( ( SPIFIMode )cb.getSelectedItem() );
      }
    } );
    settings.add( this.spifiMode );

    settings.add( createRightAlignedLabel( "/CS" ) );
    this.cs = SwingComponentUtils.createChannelSelector( channelCount, 3 );
    settings.add( this.cs );

    settings.add( createRightAlignedLabel( "SCK" ) );
    this.sck = SwingComponentUtils.createChannelSelector( channelCount, 0 );
    settings.add( this.sck );

    this.mosiLabel = createRightAlignedLabel( "MOSI" );

    settings.add( this.mosiLabel );
    this.mosi = SwingComponentUtils.createOptionalChannelSelector( channelCount, 3 );
    settings.add( this.mosi );

    this.misoLabel = createRightAlignedLabel( "MISO" );

    settings.add( this.misoLabel );
    this.miso = SwingComponentUtils.createOptionalChannelSelector( channelCount, 2 );
    settings.add( this.miso );

    this.io2Label = createRightAlignedLabel( "IO2" );

    settings.add( this.io2Label );
    this.io2 = SwingComponentUtils.createOptionalChannelSelector( channelCount, 0 );
    settings.add( this.io2 );

    this.io3Label = createRightAlignedLabel( "IO3" );

    settings.add( this.io3Label );
    this.io3 = SwingComponentUtils.createOptionalChannelSelector( channelCount, 0 );
    settings.add( this.io3 );

    settings.add( createRightAlignedLabel( "SPI Mode" ) );
    this.mode = new JComboBox( SPIMode.values() );
    this.mode.setSelectedIndex( 2 );
    this.mode.setRenderer( new SPIModeRenderer() );
    settings.add( this.mode );

    settings.add( createRightAlignedLabel( "Bits" ) );
    // #106: support up to 32 bits; not all intermediary values should be
    // necessary, I guess. Alternatively, we could use /CS as indicator for
    // the symbol-size, though that would imply that master and slave always
    // talk in the same symbol-size during a transaction...
    String[] bitarray = new String[] { "4", "5", "6", "7", "8", "9", "10", "11", //
        "12", "13", "14", "15", "16", "24", "32" };
    this.bits = new JComboBox( bitarray );
    this.bits.setSelectedIndex( 4 );
    settings.add( this.bits );

    settings.add( createRightAlignedLabel( "Order" ) );
    this.order = new JComboBox( BitOrder.values() );
    this.order.setSelectedIndex( 0 );
    this.order.setRenderer( new BitOrderItemRenderer() );
    settings.add( this.order );

    settings.add( createRightAlignedLabel( "Show /CS?" ) );
    this.reportCS = new JCheckBox();
    this.reportCS.setToolTipText( "Whether or not to show /CS transitions in analysis results?" );
    this.reportCS.setSelected( true );
    settings.add( this.reportCS );

    settings.add( createRightAlignedLabel( "Honour /CS?" ) );
    this.honourCS = new JCheckBox();
    this.honourCS.setToolTipText( "Whether or not to use /CS in analysis results?" );
    this.honourCS.setSelected( false );
    settings.add( this.honourCS );

    settings.add( createRightAlignedLabel( "Invert CS?" ) );
    this.invertCS = new JCheckBox();
    this.invertCS.setToolTipText( "Whether CS is default high (= unchecked) or default low (= checked)." );
    this.invertCS.setSelected( false );
    settings.add( this.invertCS );

    SpringLayoutUtils.makeEditorGrid( settings, 10, 4 );

    updateSPIFIModeSettings( null );

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
  private void storeToCsvFile( final File aFile, final SPIDataSet aDataSet )
  {
    try
    {
      final CsvExporter exporter = ExportUtils.createCsvExporter( aFile );

      exporter.setHeaders( "index", "start-time", "end-time", "event?", "event-type", "MOSI data", "MISO data" );

      final List<SPIData> decodedData = aDataSet.getData();
      for ( int i = 0; i < decodedData.size(); i++ )
      {
        final SPIData ds = decodedData.get( i );

        final String startTime = Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) );
        final String endTime = Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) );
        final String mosiDataValue = ds.isMosiData() ? Integer.toString( ds.getDataValue() ) : null;
        final String misoDataValue = ds.isMisoData() ? Integer.toString( ds.getDataValue() ) : null;

        exporter.addRow( Integer.valueOf( i ), startTime, endTime, Boolean.valueOf( ds.isEvent() ), ds.getEventName(),
            mosiDataValue, misoDataValue );
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
  private void storeToHtmlFile( final File aFile, final SPIDataSet aDataSet )
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
  private String toHtmlPage( final File aFile, final SPIDataSet aDataSet ) throws IOException
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
        else if ( "detected-spi-mode".equals( aMacro ) )
        {
          String result = "<unknown>";
          switch ( SPIProtocolAnalysisDialog.this.detectedSPIMode )
          {
            case MODE_0:
              result = "Mode 0 (CPOL = 0, CPHA = 0)";
              break;
            case MODE_1:
              result = "Mode 1 (CPOL = 0, CPHA = 1)";
              break;
            case MODE_2:
              result = "Mode 2 (CPOL = 1, CPHA = 0)";
              break;
            case MODE_3:
              result = "Mode 3 (CPOL = 1, CPHA = 1)";
              break;
            default:
              break;
          }
          return result;
        }
        else if ( "decoded-data".equals( aMacro ) )
        {
          final List<SPIData> decodedData = aDataSet.getData();
          Element tr;

          for ( int i = 0; i < decodedData.size(); i++ )
          {
            final SPIData ds = decodedData.get( i );

            if ( ds.isEvent() )
            {
              String event;
              String bgColor;

              // this is an event
              if ( SPIDataSet.SPI_CS_LOW.equals( ds.getEventName() ) )
              {
                // start condition
                event = ds.getEventName();
                bgColor = "#c0ffc0";
              }
              else if ( SPIDataSet.SPI_CS_HIGH.equals( ds.getEventName() ) )
              {
                // stop condition
                event = ds.getEventName();
                bgColor = "#e0e0e0";
              }
              else
              {
                // unknown event
                event = "UNKNOWN";
                bgColor = "#ff8000";
              }

              tr = aParent.addChild( TR ).addAttribute( "style", "background-color: " + bgColor + ";" );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aDataSet.getTime( ds.getStartSampleIndex() ) ) );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD ).addContent( event );
              tr.addChild( TD );
              tr.addChild( TD );
              tr.addChild( TD );
            }
            else if ( ds.isData() )
            {
              final int sampleIdx = ds.getStartSampleIndex();

              tr = aParent.addChild( TR );
              tr.addChild( TD ).addContent( String.valueOf( i ) );
              tr.addChild( TD ).addContent( Unit.Time.format( aDataSet.getTime( sampleIdx ) ) );

              int mosiValue = ds.isMosiData() ? ds.getDataValue() : 0;
              int misoValue = ds.isMisoData() ? ds.getDataValue() : 0;

              // Try to coalesce equal timestamps...
              if ( ( i + 1 ) < decodedData.size() )
              {
                final SPIData nextDS = decodedData.get( i + 1 );
                if ( nextDS.getStartSampleIndex() == sampleIdx )
                {
                  mosiValue = nextDS.isMosiData() ? nextDS.getDataValue() : mosiValue;
                  misoValue = nextDS.isMisoData() ? nextDS.getDataValue() : misoValue;
                  // Make sure to skip this DS in the next iteration...
                  i++;
                }
              }

              // MOSI value first, MISO value next...
              addDataValues( tr, i, sampleIdx, mosiValue );
              addDataValues( tr, i, sampleIdx, misoValue );
            }
          }
        }

        return null;
      }

      /**
       * @param aTableRow
       * @param aIdx
       * @param aSampleIdx
       * @param aValue
       */
      private void addDataValues( final Element aTableRow, final int aIdx, final int aSampleIdx, final int aValue )
      {
        aTableRow.addChild( TD ).addContent( "0x", integerToHexString( aValue, ( bitCount / 4 ) + bitAdder ) );
        aTableRow.addChild( TD ).addContent( "0b", integerToBinString( aValue, bitCount ) );
        aTableRow.addChild( TD ).addContent( String.valueOf( aValue ) );
        aTableRow.addChild( TD ).addContent( toASCII( aValue ) );
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
