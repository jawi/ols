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
package org.sump.device.logicsniffer;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.basic.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.NumberUtils.UnitDefinition;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.rxtx.*;

import org.sump.device.logicsniffer.LogicSnifferConfig.ClockSource;


/**
 * Provides the configuration dialog for the Open Bench Logic Sniffer device.
 */
@SuppressWarnings( "boxing" )
public class LogicSnifferConfigDialog extends JDialog implements ActionListener, Configurable, Closeable
{
  // INNER TYPES

  /**
   * Renders a binary size.
   */
  static final class BinarySizeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      final Integer size = ( Integer )aValue;
      final String value = DisplayUtils.displaySize( size.doubleValue() );
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a clock source.
   */
  static final class ClockSourceComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      String label;

      final ClockSource size = ( ClockSource )aValue;
      switch ( size )
      {
        case INTERNAL:
          label = "Internal";
          break;
        case EXTERNAL_FALLING:
          label = "External / Rising";
          break;
        case EXTERNAL_RISING:
          label = "External / Falling";
          break;
        default:
          label = "Unknown";
          break;
      }

      return super.getListCellRendererComponent( aList, label, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a device type.
   */
  static final class DeviceTypeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      String label;

      final DeviceType size = ( DeviceType )aValue;
      switch ( size )
      {
        case BP:
          label = "BusPirate";
          break;
        case OLS:
          label = "OLS";
          break;
        case SUMP:
          label = "Original SUMP";
          break;
        default:
          label = "Unknown";
          break;
      }

      return super.getListCellRendererComponent( aList, label, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Listens to the ratio slider and updates a label with the chosen ratio
   * accordingly.
   */
  static final class TriggerRatioChangeListener implements ChangeListener
  {
    static final int DEFAULT_RATIO = 50;

    private final JLabel label;

    /**
     * @param aListeningLabel
     */
    public TriggerRatioChangeListener( final JLabel aListeningLabel )
    {
      this.label = aListeningLabel;
      updateLabel( DEFAULT_RATIO, DEFAULT_RATIO );
    }

    /**
     * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
     */
    @Override
    public void stateChanged( final ChangeEvent aEvent )
    {
      final JSlider slider = ( JSlider )aEvent.getSource();

      final int before = slider.getValue();
      final int after = ( slider.getMaximum() - before );

      slider.setToolTipText( updateLabel( before, after ) );
    }

    /**
     * @param aBeforeRatio
     * @param aAfterRatio
     * @return
     */
    private String updateLabel( final int aBeforeRatio, final int aAfterRatio )
    {
      final String ratioText = String
          .format( "%d / %d", Integer.valueOf( aBeforeRatio ), Integer.valueOf( aAfterRatio ) );
      this.label.setText( ratioText );
      return ratioText;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** The serial port baudrates that can be chosen. */
  private static final String[] BAUDRATES = { "921600bps", "460800bps", "230400bps", "115200bps", "57600bps",
      "38400bps", "19200bps" };
  /** The capture speeds supported by the OLS. */
  private static final String[] CAPTURE_SPEEDS = { "200MHz", "100MHz", "50MHz", "20MHz", "10MHz", "5MHz", "2MHz",
      "1MHz", "500kHz", "200kHz", "100kHz", "50kHz", "20kHz", "10kHz", "1kHz", "500Hz", "200Hz", "100Hz", "50Hz",
      "20Hz", "10Hz" };
  /** The capture sizes supported by the OLS. */
  private static final Integer[] CAPTURE_SIZES = { 24576, 12288, 6144, 3072, 2048, 1024, 512, 256, 128, 64 };
  /** The numbering schemes supported by the OLS. */
  private static final String[] NUMBER_SCHEMES = { "Inside", "Outside", "Test Mode" };

  // VARIABLES

  private JComboBox deviceTypeSelect;
  private JComboBox numberSchemeSelect;
  private JComboBox portSelect;
  private JComboBox portRateSelect;
  private JComboBox sourceSelect;
  private JComboBox sizeSelect;
  private JComboBox speedSelect;
  private JComboBox triggerTypeSelect;
  private JCheckBox maxSampleSize;
  private JCheckBox filterEnable;
  private JCheckBox rleEnable;
  private JCheckBox triggerEnable;
  private JTabbedPane triggerStageTabs;
  private JComboBox[] triggerLevel;
  private JTextField[] triggerDelay;
  private JComboBox[] triggerMode;
  private JComboBox[] triggerChannel;
  private JCheckBox[] triggerStart;
  private JCheckBox[][] triggerMask;
  private JCheckBox[][] triggerValue;
  private JCheckBox[] channelGroup;
  private JButton captureButton;
  private JComponent groupsPanel;
  private int triggerStages;
  private final LogicSnifferConfig config;
  private boolean dialogResult;
  private JSlider ratioSlider;
  private JLabel ratioLabel;

  private transient volatile boolean listening = true;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferConfigDialog instance.
   * 
   * @param aParent
   *          the parent window of this dialog;
   * @param aConfig
   *          the logic sniffer device to configure.
   */
  public LogicSnifferConfigDialog( final Window aParent, final LogicSnifferConfig aConfig )
  {
    super( aParent, "OLS Capture settings", ModalityType.DOCUMENT_MODAL );

    this.config = aConfig;

    initDialog();
    buildDialog();

    // sync dialog status with device
    updateFields();
  }

  // METHODS

  /**
   * @param x
   * @param y
   * @param w
   * @param h
   * @param wx
   * @param wy
   * @return
   */
  private static GridBagConstraints createConstraints( final int x, final int y, final int w, final int h,
      final double wx, final double wy )
  {
    final GridBagConstraints gbc = new GridBagConstraints();
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.anchor = GridBagConstraints.BASELINE;
    gbc.insets = new Insets( 2, 2, 2, 2 );
    gbc.gridx = x;
    gbc.gridy = y;
    gbc.gridwidth = w;
    gbc.gridheight = h;
    gbc.weightx = wx;
    gbc.weighty = wy;
    return ( gbc );
  }

  /**
   * Handles all action events for this component.
   */
  public void actionPerformed( final ActionEvent aEvent )
  {
    if ( !this.listening )
    {
      return;
    }

    final Object o = aEvent.getSource();
    final String l = aEvent.getActionCommand();

    // ignore all events when dialog is not displayed
    if ( !isVisible() )
    {
      return;
    }

    if ( o == this.triggerEnable )
    {
      updateConfig();
      updateFields();
    }
    else if ( o == this.sourceSelect )
    {
      updateConfig();
      updateFields();
    }
    else if ( o == this.speedSelect )
    {
      updateConfig();
      updateFields();
    }
    else if ( l.equals( "Capture" ) )
    {
      this.dialogResult = updateConfig();

      if ( this.dialogResult )
      {
        close();
      }
    }
    else
    {
      updateFields();
    }
  }

  /**
   * Properly closes the dialog. This method makes sure timer and worker thread
   * are stopped before the dialog is closed.
   * 
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  public final void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    this.listening = false;
    try
    {
      final String preferredPortName = aSettings.get( "port", null );
      if ( ( preferredPortName != null ) && !"null".equals( preferredPortName ) )
      {
        this.portSelect.setSelectedItem( preferredPortName );
      }
      this.portRateSelect.setSelectedIndex( aSettings.getInt( "portRate", -1 ) );
      this.sourceSelect.setSelectedIndex( aSettings.getInt( "source", -1 ) );
      this.numberSchemeSelect.setSelectedIndex( aSettings.getInt( "numberScheme", -1 ) );
      this.speedSelect.setSelectedIndex( aSettings.getInt( "speed", -1 ) );
      this.sizeSelect.setSelectedIndex( aSettings.getInt( "size", -1 ) );
      this.maxSampleSize.setSelected( aSettings.getBoolean( "autosize", false ) );
      this.ratioSlider.setValue( aSettings.getInt( "ratio", TriggerRatioChangeListener.DEFAULT_RATIO ) );
      this.filterEnable.setSelected( aSettings.getBoolean( "filter", false ) );
      this.rleEnable.setSelected( aSettings.getBoolean( "rle", false ) );
      this.triggerEnable.setSelected( aSettings.getBoolean( "trigger", false ) );
      this.triggerTypeSelect.setSelectedIndex( aSettings.getInt( "triggerType", -1 ) );

      for ( int stage = 0; stage < this.triggerStages; stage++ )
      {
        final String prefix = "triggerStage." + stage;

        this.triggerDelay[stage].setText( aSettings.get( prefix + ".delay", "" ) );

        this.triggerLevel[stage].setSelectedIndex( aSettings.getInt( prefix + ".level", -1 ) );
        this.triggerMode[stage].setSelectedIndex( aSettings.getInt( prefix + ".mode", -1 ) );
        this.triggerChannel[stage].setSelectedIndex( aSettings.getInt( prefix + ".channel", -1 ) );

        final String mask = aSettings.get( prefix + ".mask", "" );
        for ( int i = 0; ( i < 32 ) && ( i < mask.length() ); i++ )
        {
          this.triggerMask[stage][i].setSelected( mask.charAt( i ) == '1' );
        }

        final String value = aSettings.get( prefix + ".value", "" );
        for ( int i = 0; ( i < 32 ) && ( i < value.length() ); i++ )
        {
          this.triggerValue[stage][i].setSelected( value.charAt( i ) == '1' );
        }

        this.triggerStart[stage].setSelected( aSettings.getBoolean( prefix + ".startCapture", false ) );
      }

      final String group = aSettings.get( "channelGroup", "" );
      for ( int i = 0; ( i < 4 ) && ( i < group.length() ); i++ )
      {
        this.channelGroup[i].setSelected( group.charAt( i ) == '1' );
      }

      updateConfig();
      updateFields();
    }
    finally
    {
      this.listening = true;
    }
  }

  /**
   * @return
   */
  public boolean showDialog()
  {
    // make sure we've got a predictable result; otherwise it causes
    // (re)captures successive uses; Thanks to erik for reporting this issue.
    this.dialogResult = false;

    setVisible( true );
    return this.dialogResult;
  }

  /** activates / deactivates dialog options according to device status */
  public void updateFields()
  {
    updateFields( true );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(java.util.Properties)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.put( "port", String.valueOf( this.portSelect.getSelectedItem() ) );
    aSettings.putInt( "portRate", this.portRateSelect.getSelectedIndex() );
    aSettings.putInt( "source", this.sourceSelect.getSelectedIndex() );
    aSettings.putInt( "numberScheme", this.numberSchemeSelect.getSelectedIndex() );
    aSettings.putInt( "speed", this.speedSelect.getSelectedIndex() );
    aSettings.putBoolean( "autosize", this.maxSampleSize.isSelected() );
    aSettings.putInt( "size", this.sizeSelect.getSelectedIndex() );
    aSettings.putInt( "ratio", this.ratioSlider.getValue() );
    aSettings.putBoolean( "filter", this.filterEnable.isSelected() );
    aSettings.putBoolean( "rle", this.rleEnable.isSelected() );
    aSettings.putBoolean( "trigger", this.triggerEnable.isSelected() );
    aSettings.putInt( "triggerType", this.triggerTypeSelect.getSelectedIndex() );

    for ( int stage = 0; stage < this.triggerStages; stage++ )
    {
      final String prefix = "triggerStage." + stage;

      aSettings.put( prefix + ".delay", this.triggerDelay[stage].getText() );
      aSettings.putInt( prefix + ".level", this.triggerLevel[stage].getSelectedIndex() );
      aSettings.putInt( prefix + ".mode", this.triggerMode[stage].getSelectedIndex() );
      aSettings.putInt( prefix + ".channel", this.triggerChannel[stage].getSelectedIndex() );

      final StringBuffer mask = new StringBuffer();
      for ( int i = 0; i < 32; i++ )
      {
        mask.append( this.triggerMask[stage][i].isSelected() ? "1" : "0" );
      }
      aSettings.put( prefix + ".mask", mask.toString() );

      final StringBuffer value = new StringBuffer();
      for ( int i = 0; i < 32; i++ )
      {
        value.append( this.triggerValue[stage][i].isSelected() ? "1" : "0" );
      }
      aSettings.put( prefix + ".value", value.toString() );

      aSettings.putBoolean( prefix + ".startCapture", this.triggerStart[stage].isSelected() );
    }

    final StringBuffer group = new StringBuffer();
    for ( int i = 0; i < 4; i++ )
    {
      group.append( this.channelGroup[i].isSelected() ? "1" : "0" );
    }
    aSettings.put( "channelGroup", group.toString() );
  }

  /**
   * Updates the controls to a given device type.
   * 
   * @param aType
   *          the device type to update the controls for, cannot be
   *          <code>null</code>.
   */
  void updateDeviceType( final DeviceType aType )
  {
    final boolean isBusPirate = aType == DeviceType.BP;

    // TODO
    this.triggerEnable.setEnabled( !isBusPirate );
    this.filterEnable.setEnabled( !isBusPirate );
    this.rleEnable.setEnabled( !isBusPirate );
  }

  /**
   * Builds this dialog by adding all components to it.
   */
  private void buildDialog()
  {
    final JTabbedPane tabs = new JTabbedPane( SwingConstants.TOP, JTabbedPane.SCROLL_TAB_LAYOUT );
    tabs.addTab( "Connection", createConnectionSettingsPane() );
    tabs.addTab( "Acquisition", createAcquisitionSettingsPane() );
    tabs.addTab( "Triggers", createTriggerPane() );

    final JButton cancel = StandardActionFactory.createCloseButton();

    this.captureButton = new JButton( "Capture" );
    this.captureButton.addActionListener( this );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( this.captureButton, cancel );

    SwingComponentUtils.setupDialogContentPane( this, tabs, buttonPane, this.captureButton );

    pack();
  }

  /**
   * Creates the "acquisition settings" pane.
   * 
   * @return a panel, never <code>null</code>.
   */
  private JPanel createAcquisitionSettingsPane()
  {
    final JPanel connectionPane = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( connectionPane, "Acquisition settings" );

    connectionPane.add( createRightAlignedLabel( "Number scheme" ) );
    connectionPane.add( this.numberSchemeSelect );

    connectionPane.add( createRightAlignedLabel( "Sampling Clock" ) );
    connectionPane.add( this.sourceSelect );

    connectionPane.add( createRightAlignedLabel( "Sampling Rate" ) );
    connectionPane.add( this.speedSelect );

    SpringLayoutUtils.addSeparator( connectionPane, "" );

    connectionPane.add( createRightAlignedLabel( "Channel Groups" ) );
    connectionPane.add( this.groupsPanel );

    connectionPane.add( createRightAlignedLabel( "Recording Size" ) );
    connectionPane.add( this.maxSampleSize );
    connectionPane.add( new JLabel() );
    connectionPane.add( this.sizeSelect );

    SpringLayoutUtils.addSeparator( connectionPane, "Options" );

    connectionPane.add( createRightAlignedLabel( "Noise Filter" ) );
    connectionPane.add( this.filterEnable );

    connectionPane.add( createRightAlignedLabel( "Run Length Encoding" ) );
    connectionPane.add( this.rleEnable );

    SpringLayoutUtils.makeEditorGrid( connectionPane, 10, 10 );

    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( connectionPane, new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( new JLabel(), new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.BOTH, new Insets( 0, 0, 0, 0 ), 0, 0 ) );

    return result;
  }

  /**
   * Creates the "connection settings" pane.
   * 
   * @return a panel, never <code>null</code>.
   */
  private JPanel createConnectionSettingsPane()
  {
    final JPanel connectionPane = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( connectionPane, "General" );

    connectionPane.add( createRightAlignedLabel( "Analyzer port" ) );
    connectionPane.add( this.portSelect );

    connectionPane.add( createRightAlignedLabel( "Port Speed" ) );
    connectionPane.add( this.portRateSelect );

    SpringLayoutUtils.addSeparator( connectionPane, "" );

    connectionPane.add( createRightAlignedLabel( "Device type" ) );
    connectionPane.add( this.deviceTypeSelect );

    SpringLayoutUtils.makeEditorGrid( connectionPane, 10, 10 );

    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( connectionPane, new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( new JLabel(), new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.BOTH, new Insets( 0, 0, 0, 0 ), 0, 0 ) );

    return result;
  }

  /**
   * @param aStage
   * @return
   */
  private JPanel createMaskValueEditor( final int aStage )
  {
    final JPanel maskValuePanel = new JPanel( new SpringLayout() );

    final JLabel[] channelLabels = new JLabel[32];

    maskValuePanel.add( new JLabel( " " ) );
    for ( int j = 32; j > 0; j-- )
    {
      final String channel = ( ( j % 8 ) == 0 ) || ( ( j % 8 ) == 1 ) ? String.format( "%2d", Integer.valueOf( j - 1 ) )
          : "";
      channelLabels[j - 1] = new JLabel( channel );
      maskValuePanel.add( channelLabels[j - 1] );
    }

    maskValuePanel.add( createRightAlignedLabel( "Mask" ) );
    this.triggerMask[aStage] = new JCheckBox[32];
    for ( int j = 32; j > 0; j-- )
    {
      final JCheckBox triggerEnabled = new JCheckBox();
      triggerEnabled.setBorder( BorderFactory.createEmptyBorder() );
      triggerEnabled.setEnabled( false );

      this.triggerMask[aStage][j - 1] = triggerEnabled;
      maskValuePanel.add( triggerEnabled );
    }

    maskValuePanel.add( createRightAlignedLabel( "Value" ) );

    this.triggerValue[aStage] = new JCheckBox[32];
    for ( int j = 32; j > 0; j-- )
    {
      final JCheckBox valueEnabled = new JCheckBox();
      valueEnabled.setBorder( BorderFactory.createEmptyBorder() );
      valueEnabled.setEnabled( false );

      this.triggerValue[aStage][j - 1] = valueEnabled;
      maskValuePanel.add( valueEnabled );
    }

    SpringLayoutUtils.makeCompactGrid( maskValuePanel, //
        3, 33, //
        1, 1, //
        1, 1 );

    return maskValuePanel;
  }

  /**
   * @return
   */
  private JPanel createTriggerPane()
  {
    final JPanel generalPane = new JPanel( new SpringLayout() );
    generalPane.add( createRightAlignedLabel( "Trigger" ) );
    generalPane.add( this.triggerEnable );
    generalPane.add( new JLabel() );

    generalPane.add( createRightAlignedLabel( "Before/After ratio" ) );
    generalPane.add( this.ratioSlider );
    generalPane.add( this.ratioLabel );

    generalPane.add( createRightAlignedLabel( "Type" ) );
    generalPane.add( this.triggerTypeSelect );
    generalPane.add( new JLabel() );

    SpringLayoutUtils.makeCompactGrid( generalPane, 3, 3, 6, 6, 6, 6 );

    final JPanel triggerPane = new JPanel( new GridBagLayout() );
    triggerPane.add( generalPane, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(
            0, 0, 0, 0 ), 0, 0 ) );
    triggerPane.add( this.triggerStageTabs, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(
            0, 0, 0, 0 ), 0, 0 ) );

    return triggerPane;
  }

  /**
   * Determines the maximum sample count that is supported by the OLS for a
   * given number of channel groups.
   * 
   * @return a maximum sample count, or -1 if no maximum could be determined.
   */
  private int determineMaxSampleCount()
  {
    int enabledChannelGroups = 0;
    for ( JCheckBox element : this.channelGroup )
    {
      if ( element.isSelected() )
      {
        enabledChannelGroups++;
      }
    }

    if ( enabledChannelGroups == 1 )
    {
      return 24576;
    }
    else if ( enabledChannelGroups == 2 )
    {
      return 12288;
    }
    else if ( ( enabledChannelGroups == 3 ) || ( enabledChannelGroups == 4 ) )
    {
      return 6144;
    }

    return -1;
  }

  /**
   * Initializes this dialog by creating all components for it.
   */
  private void initDialog()
  {
    this.portSelect = new JLazyComboBox( new JLazyComboBox.ItemProvider()
    {
      @Override
      public Object[] getItems()
      {
        return CommPortUtils.getAvailablePorts();
      }
    } );
    // allow people to put their own port name into it...
    this.portSelect.setEditable( true );

    this.portRateSelect = new JComboBox( BAUDRATES );
    this.portRateSelect.setSelectedIndex( 3 ); // 115k2

    this.deviceTypeSelect = new JComboBox( DeviceType.values() );
    this.deviceTypeSelect.setRenderer( new DeviceTypeComboBoxRenderer() );
    this.deviceTypeSelect.setSelectedIndex( 1 ); // OLS
    this.deviceTypeSelect.setEnabled( false ); // XXX
    this.deviceTypeSelect.addItemListener( new ItemListener()
    {
      /**
       * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
       */
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        final DeviceType selected = ( DeviceType )combobox.getSelectedItem();
        updateDeviceType( selected );
      }
    } );

    this.numberSchemeSelect = new JComboBox( NUMBER_SCHEMES );
    this.numberSchemeSelect.setSelectedIndex( 0 );

    this.sourceSelect = new JComboBox( ClockSource.values() );
    this.sourceSelect.setRenderer( new ClockSourceComboBoxRenderer() );
    this.sourceSelect.setSelectedIndex( 0 );
    this.sourceSelect.addActionListener( this );

    this.speedSelect = new JComboBox( CAPTURE_SPEEDS );
    this.speedSelect.setSelectedIndex( 1 );
    this.speedSelect.addActionListener( this );

    this.groupsPanel = new JPanel();
    this.groupsPanel.setLayout( new GridLayout( 1, 4 ) );

    this.channelGroup = new JCheckBox[4];
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      this.channelGroup[i] = new JCheckBox( Integer.toString( i ) );
      this.channelGroup[i].setSelected( true );
      this.channelGroup[i].addActionListener( this );
      this.channelGroup[i].setActionCommand( "channel" );
      this.groupsPanel.add( this.channelGroup[i] );
    }

    this.sizeSelect = new JComboBox( CAPTURE_SIZES );
    this.sizeSelect.setRenderer( new BinarySizeComboBoxRenderer() );
    this.sizeSelect.setSelectedIndex( 2 );

    this.maxSampleSize = new JCheckBox( "Automatic (maximum)" );
    this.maxSampleSize.setSelected( false );
    this.maxSampleSize.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JCheckBox checkbox = ( JCheckBox )aEvent.getSource();

        LogicSnifferConfigDialog.this.sizeSelect.setEnabled( !checkbox.isSelected() );
        LogicSnifferConfigDialog.this.sizeSelect.setSelectedItem( Integer.valueOf( determineMaxSampleCount() ) );
      }
    } );

    this.filterEnable = new JCheckBox( "Enabled" );
    this.filterEnable.setSelected( true );
    this.filterEnable.setEnabled( false );

    this.rleEnable = new JCheckBox( "Enabled" );
    this.rleEnable.setSelected( false );
    this.rleEnable.setEnabled( true );

    this.triggerEnable = new JCheckBox( "Enabled" );
    this.triggerEnable.addActionListener( this );

    this.ratioLabel = new JLabel( "" );
    SwingComponentUtils.fixLabelWidth( this.ratioLabel, "100 / 100" );

    this.ratioSlider = new JSlider( SwingConstants.HORIZONTAL, 0, 100, 50 );
    this.ratioSlider.setMajorTickSpacing( 10 );
    this.ratioSlider.setMinorTickSpacing( 5 );
    this.ratioSlider.setPaintLabels( true );
    this.ratioSlider.setPaintTicks( true );
    this.ratioSlider.setPreferredSize( new Dimension( 300, 50 ) );
    this.ratioSlider.addChangeListener( new TriggerRatioChangeListener( this.ratioLabel ) );

    final String[] types = { "Simple", "Complex" };
    this.triggerTypeSelect = new JComboBox( types );
    // Select first item by default...
    this.triggerTypeSelect.setSelectedIndex( 0 );
    this.triggerTypeSelect.addActionListener( this );

    this.triggerStageTabs = new JTabbedPane();
    this.triggerStages = this.config.getMaxTriggerStages();
    this.triggerMask = new JCheckBox[4][];
    this.triggerValue = new JCheckBox[4][];
    this.triggerLevel = new JComboBox[4];
    this.triggerDelay = new JTextField[4];
    this.triggerMode = new JComboBox[4];
    this.triggerChannel = new JComboBox[4];
    this.triggerStart = new JCheckBox[4];
    for ( int i = 0; i < this.triggerStages; i++ )
    {
      final JPanel stagePane = new JPanel( new GridBagLayout() );
      stagePane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

      final String[] levels = { "Immediately", "On Level 1", "On Level 2", "On Level 3" };
      this.triggerLevel[i] = new JComboBox( levels );
      this.triggerLevel[i].setSelectedIndex( i );

      stagePane.add( createRightAlignedLabel( "Arm" ), createConstraints( 0, 0, 1, 1, 1.0, 1.0 ) );
      stagePane.add( this.triggerLevel[i], createConstraints( 1, 0, 1, 1, 0.5, 1.0 ) );

      final String[] modes = { "Parallel", "Serial" };
      this.triggerMode[i] = new JComboBox( modes );
      this.triggerMode[i].setSelectedIndex( 0 );

      stagePane.add( createRightAlignedLabel( "Mode" ), createConstraints( 2, 0, 1, 1, 0.5, 1.0 ) );
      stagePane.add( this.triggerMode[i], createConstraints( 3, 0, 1, 1, 0.5, 1.0 ) );

      this.triggerMode[i].addActionListener( this );
      final String[] channels = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
          "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31" };
      this.triggerChannel[i] = new JComboBox( channels );
      this.triggerChannel[i].setSelectedIndex( 0 );

      stagePane.add( createRightAlignedLabel( "Channel" ), createConstraints( 4, 0, 1, 1, 0.5, 1.0 ) );
      stagePane.add( this.triggerChannel[i], createConstraints( 5, 0, 1, 1, 0.5, 1.0 ) );

      final JPanel maskValueEditor = createMaskValueEditor( i );
      stagePane.add( maskValueEditor, createConstraints( 0, 1, 6, 1, 1.0, 1.0 ) );

      stagePane.add( createRightAlignedLabel( "Action" ), createConstraints( 0, 4, 1, 1, 1.0, 1.0 ) );

      this.triggerStart[i] = new JCheckBox( "Start Capture    (otherwise trigger level will rise by one)" );
      stagePane.add( this.triggerStart[i], createConstraints( 1, 4, 3, 1, 1.0, 1.0 ) );

      stagePane.add( createRightAlignedLabel( "Delay" ), createConstraints( 4, 4, 1, 1, 0.5, 1.0 ) );
      this.triggerDelay[i] = new JTextField( "0" );
      this.triggerDelay[i].setToolTipText( "Delays trigger # samples after its condition is met." );
      stagePane.add( this.triggerDelay[i], createConstraints( 5, 4, 1, 1, 0.5, 1.0 ) );

      this.triggerStageTabs.add( String.format( "Stage %d", Integer.valueOf( i + 1 ) ), stagePane );
    }
  }

  /**
   * Sets the enabled state of all available trigger check boxes and the ratio
   * select.
   * 
   * @param aEnable
   *          <code>true</code> to enable trigger configuration fields,
   *          <code>false</code> to disable them
   */
  private void setTriggerEnabled( final boolean aEnable )
  {
    final int channels = this.config.getChannelCount();
    final boolean complex = "Complex".equals( ( String )this.triggerTypeSelect.getSelectedItem() );
    if ( !complex )
    {
      this.triggerStageTabs.setSelectedIndex( 0 );
    }
    this.triggerTypeSelect.setEnabled( aEnable );
    this.ratioSlider.setEnabled( aEnable );

    for ( int stage = 0; stage < this.triggerStages; stage++ )
    {
      for ( int i = 0; i < channels; i++ )
      {
        this.triggerMask[stage][i].setEnabled( aEnable );
        this.triggerValue[stage][i].setEnabled( aEnable );
      }
      for ( int i = channels; i < 32; i++ )
      {
        this.triggerMask[stage][i].setEnabled( false );
        this.triggerValue[stage][i].setEnabled( false );
      }
      this.triggerStageTabs.setEnabledAt( stage, aEnable && ( ( stage == 0 ) || complex ) );
      this.triggerLevel[stage].setEnabled( aEnable && complex );
      this.triggerDelay[stage].setEnabled( aEnable );
      this.triggerMode[stage].setEnabled( aEnable );
      if ( aEnable && ( this.triggerMode[stage].getSelectedIndex() == 1 ) )
      {
        this.triggerChannel[stage].setEnabled( true );
      }
      else
      {
        this.triggerChannel[stage].setEnabled( false );
      }
      this.triggerStart[stage].setEnabled( aEnable && complex );
    }
  }

  /**
   * writes the dialog settings to the device configuration.
   */
  private boolean updateConfig()
  {
    String value;
    boolean result = true;

    // set port + baudrate
    this.config.setPortName( ( String )this.portSelect.getSelectedItem() );
    this.config.setBaudrate( NumberUtils.smartParseInt( ( String )this.portRateSelect.getSelectedItem() ) );

    // set clock source
    this.config.setClockSource( ( ClockSource )this.sourceSelect.getSelectedItem() );

    // set enabled channel groups
    int enabledChannels = 0;
    int enabledChannelGroups = 0;
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      if ( this.channelGroup[i].isSelected() )
      {
        enabledChannels |= 0xff << ( 8 * i );
        enabledChannelGroups++;
      }
    }
    this.config.setEnabledChannels( enabledChannels );

    // The OLS is capable of "auto" selecting the maximum capture size itself,
    // and does this based on the number of enabled channel groups...
    final int maxSampleCount = determineMaxSampleCount();

    // set sample rate; use a default to ensure the internal state remains
    // correct...
    value = ( String )this.speedSelect.getSelectedItem();
    int f = NumberUtils.smartParseInt( value, UnitDefinition.SI, LogicSnifferDevice.CLOCK );
    this.config.setSampleRate( f );

    // set sample count
    int sampleCount = ( ( Integer )this.sizeSelect.getSelectedItem() ).intValue();
    if ( this.maxSampleSize.isSelected() )
    {
      sampleCount = maxSampleCount;
      this.sizeSelect.setSelectedItem( Integer.valueOf( maxSampleCount ) );
    }
    this.config.setSampleCount( sampleCount );

    // set before / after ratio
    double r = 1.0 - ( this.ratioSlider.getValue() / ( double )this.ratioSlider.getMaximum() );
    this.config.setRatio( r );

    // set filter
    this.config.setFilterEnabled( this.filterEnable.isSelected() );
    this.config.setRleEnabled( this.rleEnable.isSelected() );

    // set number scheme
    value = ( String )this.numberSchemeSelect.getSelectedItem();
    if ( "Inside".equals( value ) )
    {
      this.config.setTestModeEnabled( false );
      this.config.setAltNumberSchemeEnabled( false );
    }
    else if ( "Outside".equals( value ) )
    {
      this.config.setTestModeEnabled( false );
      this.config.setAltNumberSchemeEnabled( true );
    }
    else if ( "Test Mode".equals( value ) )
    {
      this.config.setTestModeEnabled( true );
      this.config.setAltNumberSchemeEnabled( false );
    }

    // set trigger
    final boolean triggerEnabled = this.triggerEnable.isSelected();
    this.config.setTriggerEnabled( triggerEnabled );
    if ( triggerEnabled )
    {
      final boolean complex = "Complex".equals( ( String )this.triggerTypeSelect.getSelectedItem() );
      for ( int stage = 0; stage < this.triggerStages; stage++ )
      {
        int m = 0;
        int v = 0;
        for ( int i = 0; i < 32; i++ )
        {
          if ( this.triggerMask[stage][i].isSelected() )
          {
            m |= 1 << i;
          }
          if ( this.triggerValue[stage][i].isSelected() )
          {
            v |= 1 << i;
          }
        }
        final int level = this.triggerLevel[stage].getSelectedIndex();
        final int delay = NumberUtils.smartParseInt( this.triggerDelay[stage].getText() );
        if ( result && ( delay > 65535 ) )
        {
          result = ( JOptionPane.showConfirmDialog( this, //
              "Trigger delay for stage " + stage + " is larger than 65535 cycles! Continue capture?", //
              "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE ) == JOptionPane.YES_OPTION );
        }

        final int channel = this.triggerChannel[stage].getSelectedIndex();
        final boolean startCapture = this.triggerStart[stage].isSelected();
        if ( complex )
        {
          if ( this.triggerMode[stage].getSelectedIndex() == 0 )
          {
            this.config.setParallelTrigger( stage, m, v, level, delay, startCapture );
          }
          else
          {
            this.config.setSerialTrigger( stage, channel, m, v, level, delay, startCapture );
          }
        }
        else
        {
          if ( stage == 0 )
          {
            if ( this.triggerMode[stage].getSelectedIndex() == 0 )
            {
              this.config.setParallelTrigger( stage, m, v, 0, delay, true );
            }
            else
            {
              this.config.setSerialTrigger( stage, channel, m, v, 0, delay, true );
            }
          }
          else
          {
            // make sure stages > 0 will not interfere
            this.config.setParallelTrigger( stage, 0, 0, 3, 0, false );
          }
        }
      }
    }

    // Determine whether the chosen sample count is larger than the OLS can
    // provide us in the chosen channel group-selection...
    if ( result && ( maxSampleCount >= 0 ) && ( sampleCount > maxSampleCount ) )
    {
      result = ( JOptionPane.showConfirmDialog( this, //
          "Sample count too large for chosen channel groups! Continue capture?", //
          "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE ) == JOptionPane.YES_OPTION );
    }
    // When no channel groups are enabled, the capture won't be very useful...
    if ( result && ( enabledChannelGroups == 0 ) )
    {
      result = ( JOptionPane.showConfirmDialog( this, //
          "No channel groups are enabled! Continue capture?", //
          "Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE ) == JOptionPane.YES_OPTION );
    }

    return result;
  }

  /**
   * activates / deactivates dialog options according to device status
   */
  private void updateFields( final boolean aEnable )
  {
    this.triggerEnable.setSelected( this.config.isTriggerEnabled() );
    setTriggerEnabled( this.config.isTriggerEnabled() );

    this.filterEnable.setEnabled( this.config.isFilterAvailable() && aEnable );

    final int availableChannelGroups = this.config.getChannelCount() / 8;
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      final boolean enabled = aEnable && ( i < availableChannelGroups );
      this.channelGroup[i].setEnabled( enabled );
    }

    this.speedSelect.setEnabled( this.config.isInternalClock() );
  }

}

/* EOF */
