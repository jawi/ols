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
package org.sump.device.logicsniffer;


import static nl.lxtreme.ols.api.Ols.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static org.sump.device.logicsniffer.ConfigDialogHelper.*;

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

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.profile.DeviceProfile.NumberingScheme;
import org.sump.device.logicsniffer.profile.DeviceProfile.TriggerType;


/**
 * Provides the configuration dialog for the Open Bench Logic Sniffer device.
 */
public final class LogicSnifferConfigDialog extends JDialog implements ActionListener, Configurable, Closeable
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
      Object value = aValue;
      if ( value instanceof Integer )
      {
        value = DisplayUtils.displaySize( ( ( Integer )aValue ).doubleValue() );
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a binary size.
   */
  static final class CaptureSpeedComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof Integer )
      {
        value = DisplayUtils.displayFrequency( ( ( Integer )value ).doubleValue() );
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a clock source.
   */
  static final class ClockSourceComboBoxRenderer extends EnumItemRenderer<CaptureClockSource>
  {
    private static final long serialVersionUID = 1L;

    /**
     * @see nl.lxtreme.ols.util.swing.component.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final CaptureClockSource aValue )
    {
      switch ( aValue )
      {
        case INTERNAL:
          return "Internal";
        case EXTERNAL_FALLING:
          return "External / Rising";
        case EXTERNAL_RISING:
          return "External / Falling";
      }
      return super.getDisplayValue( aValue );
    }
  }

  /**
   * Provides a combobox model for device profile types.
   */
  final class DeviceProfileTypeComboBoxModel extends AbstractListModel implements ComboBoxModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private volatile Object selected = null;

    // METHODS

    /**
     * Finds the element with a given type.
     * 
     * @param aType
     *          the identifying type of the element to find.
     * @return the element with the given identifying type, or <code>null</code>
     *         if no such type could be found.
     */
    public Object findElementByType( final String aType )
    {
      return getDeviceProfileManager().getProfile( aType );
    }

    /**
     * @see javax.swing.ListModel#getElementAt(int)
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      return getDeviceProfileManager().getProfiles().get( aIndex );
    }

    /**
     * @see javax.swing.ComboBoxModel#getSelectedItem()
     */
    @Override
    public Object getSelectedItem()
    {
      return this.selected;
    }

    /**
     * @see javax.swing.ListModel#getSize()
     */
    @Override
    public int getSize()
    {
      return getDeviceProfileManager().getSize();
    }

    /**
     * @see javax.swing.ComboBoxModel#setSelectedItem(java.lang.Object)
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      this.selected = aItem;
    }
  }

  /**
   * Provides a combobox renderer for device profiles.
   */
  static final class DeviceProfileTypeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof DeviceProfile )
      {
        final DeviceProfile profile = ( DeviceProfile )value;
        value = profile.getDescription();
        if ( ( value == null ) || ( String.valueOf( value ).isEmpty() ) )
        {
          value = profile.getType();
        }
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a numbering scheme.
   */
  static final class NumberSchemeComboBoxRenderer extends EnumItemRenderer<NumberingScheme>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final NumberingScheme aValue )
    {
      switch ( aValue )
      {
        case DEFAULT:
          return "Default";
        case INSIDE:
          return "Inside";
        case OUTSIDE:
          return "Outside";
      }
      return super.getDisplayValue( aValue );
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
  private static final String rleWarning = "The last channel will always be low when RLE is enabled!";

  /** The serial port baudrates that can be chosen. */
  private static final String[] BAUDRATES = { "921600bps", "460800bps", "230400bps", "115200bps", "57600bps",
      "38400bps", "19200bps" };

  // VARIABLES

  private final DeviceProfileManagerTracker deviceProfileManagerTracker;

  private JComboBox deviceTypeSelect;
  private JComboBox numberSchemeSelect;
  private JComboBox portSelect;
  private JComboBox portRateSelect;
  private JComboBox sourceSelect;
  private JComboBox sizeSelect;
  private JComboBox speedSelect;
  private JComboBox triggerTypeSelect;
  private JCheckBox maxSampleSize;
  private JCheckBox testModeEnable;
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
  private final LogicSnifferConfig config;
  private boolean dialogResult;
  private JSlider ratioSlider;
  private JLabel ratioLabel;
  private JLabel warningLabel;

  private JTextField[] triggerHexMask;
  private JTextField[] triggerHexValue;
  private JButton[] applyHexMaskButton;
  private JButton[] applyHexValueButton;
  private JCheckBox[] invertHexValue;

  private volatile boolean listening = true;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferConfigDialog instance.
   * 
   * @param aParent
   *          the parent window of this dialog;
   * @param aConfig
   *          the logic sniffer device to configure.
   */
  public LogicSnifferConfigDialog( final Window aParent, final LogicSnifferConfig aConfig,
      final DeviceProfileManagerTracker aDeviceProfileManagerTracker )
  {
    super( aParent, "OLS Capture settings", ModalityType.DOCUMENT_MODAL );

    this.deviceProfileManagerTracker = aDeviceProfileManagerTracker;
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
   * Converts a given bitmask to a hexadecimal representation.
   * 
   * @param aMask
   *          the bitmask to convert to a hexadecimal representation, cannot be
   *          <code>null</code>.
   * @return a hexadecimal representation, never <code>null</code>.
   */
  private static String maskToHexString( final String aMask )
  {
    try
    {
      final int value = ( int )Long.parseLong( aMask, 2 );
      return Integer.toHexString( Integer.reverse( value ) );
    }
    catch ( NumberFormatException exception )
    {
      return "";
    }
  }

  /**
   * Handles all action events for this component.
   */
  public void actionPerformed( final ActionEvent aEvent )
  {
    // ignore all events when dialog is not displayed or we're not supposed to
    // listen to events...
    if ( !this.listening || !isVisible() )
    {
      return;
    }

    final int stage = this.triggerStageTabs.getSelectedIndex();

    final Object source = aEvent.getSource();
    if ( source == this.captureButton )
    {
      this.dialogResult = updateConfig( true /* aWarnUserIfConfigIncorrect */);

      if ( this.dialogResult )
      {
        close();
      }
    }
    else if ( source == this.applyHexMaskButton[stage] )
    {
      try
      {
        final long hexMask = Long.parseLong( this.triggerHexMask[stage].getText(), 16 );
        for ( int i = 0; i < MAX_CHANNELS; i++ )
        {
          boolean value = ( ( hexMask >>> i ) & 0x01 ) != 0;
          this.triggerMask[stage][i].setSelected( value );
        }

        updateConfig( false /* aWarnUserIfConfigIncorrect */);
        updateFields();
      }
      catch ( NumberFormatException e )
      {
        JOptionPane.showMessageDialog( this, "Illegal number format!\nPlease enter a hexadecimal value." );
      }
    }
    else if ( source == this.applyHexValueButton[stage] )
    {
      try
      {
        long hexValue = Long.parseLong( this.triggerHexValue[stage].getText(), 16 );
        if ( this.invertHexValue[stage].isSelected() )
        {
          hexValue = ~hexValue;
        }

        for ( int i = 0; i < MAX_CHANNELS; i++ )
        {
          boolean value = ( ( hexValue >>> i ) & 0x01 ) != 0;
          this.triggerValue[stage][i].setSelected( value );
        }

        updateConfig( false /* aWarnUserIfConfigIncorrect */);
        updateFields();
      }
      catch ( NumberFormatException e )
      {
        JOptionPane.showMessageDialog( this, "Illegal number format!\nPlease enter a hexadecimal value." );
      }
    }
    else
    {
      updateConfig( false /* aWarnUserIfConfigIncorrect */);
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
      final String preferredDeviceType = aSettings.get( "deviceType", null );
      if ( ( preferredDeviceType != null ) && !"null".equals( preferredDeviceType ) )
      {
        final Object element = ( ( DeviceProfileTypeComboBoxModel )this.deviceTypeSelect.getModel() )
            .findElementByType( preferredDeviceType );
        if ( element != null )
        {
          this.deviceTypeSelect.setSelectedItem( element );
        }
      }
      this.portRateSelect.setSelectedIndex( aSettings.getInt( "portRate", this.portRateSelect.getSelectedIndex() ) );
      this.sourceSelect.setSelectedIndex( aSettings.getInt( "source", this.sourceSelect.getSelectedIndex() ) );
      this.numberSchemeSelect.setSelectedIndex( aSettings.getInt( "numberScheme",
          this.numberSchemeSelect.getSelectedIndex() ) );
      this.testModeEnable.setSelected( aSettings.getBoolean( "testMode", this.testModeEnable.isSelected() ) );
      this.speedSelect.setSelectedIndex( aSettings.getInt( "speed", this.speedSelect.getSelectedIndex() ) );
      this.sizeSelect.setSelectedIndex( aSettings.getInt( "size", this.sizeSelect.getSelectedIndex() ) );
      this.maxSampleSize.setSelected( aSettings.getBoolean( "autosize", this.maxSampleSize.isSelected() ) );
      this.ratioSlider.setValue( aSettings.getInt( "ratio", TriggerRatioChangeListener.DEFAULT_RATIO ) );
      this.filterEnable.setSelected( aSettings.getBoolean( "filter", this.filterEnable.isSelected() ) );
      this.rleEnable.setSelected( aSettings.getBoolean( "rle", this.rleEnable.isSelected() ) );
      this.triggerEnable.setSelected( aSettings.getBoolean( "trigger", this.triggerEnable.isSelected() ) );
      this.triggerTypeSelect.setSelectedIndex( aSettings.getInt( "triggerType",
          this.triggerTypeSelect.getSelectedIndex() ) );

      for ( int stage = 0; stage < LogicSnifferConfig.TRIGGER_STAGES; stage++ )
      {
        final String prefix = "triggerStage." + stage;

        this.triggerDelay[stage].setText( aSettings.get( prefix + ".delay", "" ) );

        this.triggerLevel[stage].setSelectedIndex( aSettings.getInt( prefix + ".level",
            this.triggerLevel[stage].getSelectedIndex() ) );
        this.triggerMode[stage].setSelectedIndex( aSettings.getInt( prefix + ".mode",
            this.triggerMode[stage].getSelectedIndex() ) );
        this.triggerChannel[stage].setSelectedIndex( aSettings.getInt( prefix + ".channel",
            this.triggerChannel[stage].getSelectedIndex() ) );

        final String mask = aSettings.get( prefix + ".mask", "" );
        for ( int i = 0; ( i < MAX_CHANNELS ) && ( i < mask.length() ); i++ )
        {
          this.triggerMask[stage][i].setSelected( mask.charAt( i ) == '1' );
        }

        final String value = aSettings.get( prefix + ".value", "" );
        for ( int i = 0; ( i < MAX_CHANNELS ) && ( i < value.length() ); i++ )
        {
          this.triggerValue[stage][i].setSelected( value.charAt( i ) == '1' );
        }

        this.triggerStart[stage].setSelected( aSettings.getBoolean( prefix + ".startCapture",
            this.triggerStart[stage].isSelected() ) );

        String hexMask = maskToHexString( mask );
        this.triggerHexMask[stage].setText( hexMask );

        String hexValue = maskToHexString( value );
        this.triggerHexValue[stage].setText( hexValue );

        // this.invertHexValue[stage].setSelected( false );
      }

      final String group = aSettings.get( "channelGroup", "" );
      for ( int i = 0; ( i < MAX_BLOCKS ) && ( i < group.length() ); i++ )
      {
        this.channelGroup[i].setSelected( group.charAt( i ) == '1' );
      }

      updateConfig( false /* aWarnUserIfConfigIncorrect */);
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

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(java.util.Properties)
   */
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.put( "port", String.valueOf( this.portSelect.getSelectedItem() ) );
    aSettings.put( "deviceType", String.valueOf( this.deviceTypeSelect.getSelectedItem() ) );
    aSettings.putInt( "portRate", this.portRateSelect.getSelectedIndex() );
    aSettings.putInt( "source", this.sourceSelect.getSelectedIndex() );
    aSettings.putInt( "numberScheme", this.numberSchemeSelect.getSelectedIndex() );
    aSettings.putBoolean( "testMode", this.testModeEnable.isSelected() );
    aSettings.putInt( "speed", this.speedSelect.getSelectedIndex() );
    aSettings.putBoolean( "autosize", this.maxSampleSize.isSelected() );
    aSettings.putInt( "size", this.sizeSelect.getSelectedIndex() );
    aSettings.putInt( "ratio", this.ratioSlider.getValue() );
    aSettings.putBoolean( "filter", this.filterEnable.isSelected() );
    aSettings.putBoolean( "rle", this.rleEnable.isSelected() );
    aSettings.putBoolean( "trigger", this.triggerEnable.isSelected() );
    aSettings.putInt( "triggerType", this.triggerTypeSelect.getSelectedIndex() );

    for ( int stage = 0; stage < LogicSnifferConfig.TRIGGER_STAGES; stage++ )
    {
      final String prefix = "triggerStage." + stage;

      aSettings.put( prefix + ".delay", this.triggerDelay[stage].getText() );
      aSettings.putInt( prefix + ".level", this.triggerLevel[stage].getSelectedIndex() );
      aSettings.putInt( prefix + ".mode", this.triggerMode[stage].getSelectedIndex() );
      aSettings.putInt( prefix + ".channel", this.triggerChannel[stage].getSelectedIndex() );

      final StringBuffer mask = new StringBuffer();
      for ( int i = 0; i < MAX_CHANNELS; i++ )
      {
        mask.append( this.triggerMask[stage][i].isSelected() ? "1" : "0" );
      }
      aSettings.put( prefix + ".mask", mask.toString() );

      final StringBuffer value = new StringBuffer();
      for ( int i = 0; i < MAX_CHANNELS; i++ )
      {
        value.append( this.triggerValue[stage][i].isSelected() ? "1" : "0" );
      }
      aSettings.put( prefix + ".value", value.toString() );

      aSettings.putBoolean( prefix + ".startCapture", this.triggerStart[stage].isSelected() );
    }

    final StringBuffer group = new StringBuffer();
    for ( int i = 0; i < MAX_BLOCKS; i++ )
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
  void updateDeviceType( final DeviceProfile aProfile )
  {
    // "Publish" the device type to the device configuration...
    this.config.setDeviceProfile( aProfile );

    // Noise filter supported?
    updateCheckBoxState( this.filterEnable, aProfile.isNoiseFilterSupported() );
    // RLE supported?
    updateCheckBoxState( this.rleEnable, aProfile.isRleSupported() );
    // Test mode supported at all?
    updateCheckBoxState( this.testModeEnable, aProfile.isTestModeSupported() );

    // Triggers supported at all?
    updateCheckBoxState( this.triggerEnable, aProfile.isTriggerSupported() );
    // Complex triggers supported?
    updateTriggerTypeComboBoxModel( this.triggerTypeSelect, aProfile );
    // Update trigger mask editors...
    updateTriggerChannels( this.triggerMask, this.triggerValue, aProfile );

    // Enable the supported number of channel groups...
    updateChannelGroups( this.channelGroup, aProfile );

    // Update the capture speeds...
    updateCaptureSpeedComboBoxModel( this.speedSelect, aProfile );
    // Update the capture sizes...
    updateComboBoxModel( this.sizeSelect, aProfile.getCaptureSizes() );
    // Update the capture clock sources...
    updateComboBoxModel( this.sourceSelect, aProfile.getCaptureClock() );
    // Update the numbering schemes...
    updateComboBoxModel( this.numberSchemeSelect, aProfile.getChannelNumberingSchemes() );
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

    connectionPane.add( createRightAlignedLabel( "Test mode" ) );
    connectionPane.add( this.testModeEnable );

    connectionPane.add( createRightAlignedLabel( "Noise Filter" ) );
    connectionPane.add( this.filterEnable );

    connectionPane.add( createRightAlignedLabel( "Run Length Encoding" ) );
    connectionPane.add( this.rleEnable );

    SpringLayoutUtils.makeEditorGrid( connectionPane, 10, 10 );

    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( connectionPane, new GridBagConstraints( 0, 0, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( this.warningLabel, new GridBagConstraints( 0, 1, 1, 0, 1.0, 0.1, GridBagConstraints.CENTER,
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

    final JLabel[] channelLabels = new JLabel[MAX_CHANNELS];

    maskValuePanel.add( new JLabel( " " ) );
    for ( int j = MAX_CHANNELS; j > 0; j-- )
    {
      final String channel = ( ( j % 8 ) == 0 ) || ( ( j % 8 ) == 1 ) ? String.format( "%2d", Integer.valueOf( j - 1 ) )
          : "";
      channelLabels[j - 1] = new JLabel( channel );
      maskValuePanel.add( channelLabels[j - 1] );
    }

    maskValuePanel.add( createRightAlignedLabel( "Mask" ) );
    this.triggerMask[aStage] = new JCheckBox[MAX_CHANNELS];
    for ( int j = MAX_CHANNELS; j > 0; j-- )
    {
      final JCheckBox triggerEnabled = new JCheckBox();
      triggerEnabled.setBorder( BorderFactory.createEmptyBorder() );
      triggerEnabled.setEnabled( false );

      this.triggerMask[aStage][j - 1] = triggerEnabled;
      maskValuePanel.add( triggerEnabled );
    }

    maskValuePanel.add( createRightAlignedLabel( "Value" ) );

    this.triggerValue[aStage] = new JCheckBox[MAX_CHANNELS];
    for ( int j = MAX_CHANNELS; j > 0; j-- )
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
  private int determineMaxSampleCount( final int aEnabledChannelGroups )
  {
    final DeviceProfile deviceProfile = this.config.getDeviceProfile();
    if ( deviceProfile != null )
    {
      return deviceProfile.getMaximumCaptureSizeFor( aEnabledChannelGroups );
    }

    return -1;
  }

  /**
   * "Forces" the capture size combobox to the given sample count.
   * <p>
   * This method will search for the "best" matching capture size for the given
   * sample count. It could well be that the given sample count is not available
   * directly, so take the closest match.
   * </p>
   * 
   * @param aSampleCount
   *          the sample count that should be set.
   */
  private void forceCaptureSizeTo( final Integer aSampleCount )
  {
    this.sizeSelect.setSelectedItem( aSampleCount );
  }

  /**
   * Returns the current device profile manager, never <code>null</code>.
   * 
   * @return the device profile manager to return, cannot be <code>null</code>.
   */
  private DeviceProfileManager getDeviceProfileManager()
  {
    return this.deviceProfileManagerTracker.getService();
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

    this.numberSchemeSelect = new JComboBox();
    this.numberSchemeSelect.setRenderer( new NumberSchemeComboBoxRenderer() );
    this.numberSchemeSelect.addActionListener( this );

    this.sourceSelect = new JComboBox();
    this.sourceSelect.setRenderer( new ClockSourceComboBoxRenderer() );
    this.sourceSelect.addActionListener( this );

    this.speedSelect = new JComboBox();
    this.speedSelect.setRenderer( new CaptureSpeedComboBoxRenderer() );
    this.speedSelect.addActionListener( this );

    this.groupsPanel = new JPanel();
    this.groupsPanel.setLayout( new GridLayout( 1, 4 ) );

    this.channelGroup = new JCheckBox[MAX_BLOCKS];
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      this.channelGroup[i] = new JCheckBox( Integer.toString( i ) );
      this.channelGroup[i].setSelected( ( i == 0 ) );
      this.channelGroup[i].addActionListener( this );
      this.groupsPanel.add( this.channelGroup[i] );
    }

    this.sizeSelect = new JComboBox();
    this.sizeSelect.setRenderer( new BinarySizeComboBoxRenderer() );
    this.sizeSelect.addActionListener( this );

    this.maxSampleSize = new JCheckBox( "Automatic (maximum)" );
    this.maxSampleSize.setSelected( false );
    this.maxSampleSize.addActionListener( this );

    this.testModeEnable = new JCheckBox( "Enabled" );
    this.testModeEnable.setSelected( true );
    this.testModeEnable.setEnabled( false );
    this.testModeEnable.addActionListener( this );

    this.filterEnable = new JCheckBox( "Enabled" );
    this.filterEnable.setSelected( true );
    this.filterEnable.setEnabled( false );
    this.filterEnable.addActionListener( this );

    this.rleEnable = new JCheckBox( "Enabled" );
    this.rleEnable.setSelected( false );
    this.rleEnable.setEnabled( true );
    this.rleEnable.addActionListener( this );

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

    this.triggerTypeSelect = new JComboBox();
    this.triggerTypeSelect.addActionListener( this );

    this.triggerStageTabs = new JTabbedPane();
    this.triggerMask = new JCheckBox[LogicSnifferConfig.TRIGGER_STAGES][];
    this.triggerValue = new JCheckBox[LogicSnifferConfig.TRIGGER_STAGES][];
    this.triggerLevel = new JComboBox[LogicSnifferConfig.TRIGGER_STAGES];
    this.triggerDelay = new JTextField[LogicSnifferConfig.TRIGGER_STAGES];
    this.triggerMode = new JComboBox[LogicSnifferConfig.TRIGGER_STAGES];
    this.triggerChannel = new JComboBox[LogicSnifferConfig.TRIGGER_STAGES];
    this.triggerStart = new JCheckBox[LogicSnifferConfig.TRIGGER_STAGES];

    // @@@
    this.triggerHexMask = new JTextField[LogicSnifferConfig.TRIGGER_STAGES];
    this.triggerHexValue = new JTextField[LogicSnifferConfig.TRIGGER_STAGES];
    this.applyHexMaskButton = new JButton[LogicSnifferConfig.TRIGGER_STAGES];
    this.applyHexValueButton = new JButton[LogicSnifferConfig.TRIGGER_STAGES];
    this.invertHexValue = new JCheckBox[LogicSnifferConfig.TRIGGER_STAGES];

    for ( int i = 0; i < LogicSnifferConfig.TRIGGER_STAGES; i++ )
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

      // @@@
      stagePane.add( createRightAlignedLabel( "Hex Mask" ), createConstraints( 0, 4, 1, 1, 0.5, 1.0 ) );
      this.triggerHexMask[i] = new JTextField( "0" );
      this.triggerHexMask[i].setToolTipText( "hexadecimal trigger mask." );
      stagePane.add( this.triggerHexMask[i], createConstraints( 1, 4, 1, 1, 0.5, 1.0 ) );

      stagePane.add( createRightAlignedLabel( "Hex Value" ), createConstraints( 2, 4, 1, 1, 0.5, 1.0 ) );
      this.triggerHexValue[i] = new JTextField( "0" );
      this.triggerHexValue[i].setToolTipText( "hexadecimal trigger value." );
      stagePane.add( this.triggerHexValue[i], createConstraints( 3, 4, 1, 1, 0.5, 1.0 ) );

      this.invertHexValue[i] = new JCheckBox( "Invert" );
      stagePane.add( this.invertHexValue[i], createConstraints( 4, 4, 1, 1, 1.0, 1.0 ) );

      this.applyHexMaskButton[i] = new JButton( "Apply Hex Mask" );
      stagePane.add( this.applyHexMaskButton[i], createConstraints( 1, 5, 1, 1, 1.0, 1.0 ) );
      this.applyHexMaskButton[i].addActionListener( this );

      this.applyHexValueButton[i] = new JButton( "Apply Hex Value" );
      stagePane.add( this.applyHexValueButton[i], createConstraints( 3, 5, 1, 1, 1.0, 1.0 ) );
      this.applyHexValueButton[i].addActionListener( this );

      stagePane.add( createRightAlignedLabel( "Action" ), createConstraints( 0, 6, 1, 1, 1.0, 1.0 ) );

      this.triggerStart[i] = new JCheckBox( "Start Capture    (otherwise trigger level will rise by one)" );
      stagePane.add( this.triggerStart[i], createConstraints( 1, 6, 3, 1, 1.0, 1.0 ) );

      stagePane.add( createRightAlignedLabel( "Delay" ), createConstraints( 4, 6, 1, 1, 0.5, 1.0 ) );
      this.triggerDelay[i] = new JTextField( "0" );
      this.triggerDelay[i].setToolTipText( "Delays trigger # samples after its condition is met." );
      stagePane.add( this.triggerDelay[i], createConstraints( 5, 6, 1, 1, 0.5, 1.0 ) );

      this.triggerStageTabs.add( String.format( "Stage %d", Integer.valueOf( i + 1 ) ), stagePane );
    }

    this.warningLabel = new JLabel( " " );
    this.warningLabel.setFont( this.warningLabel.getFont().deriveFont( Font.BOLD ) );

    // NOTE: create this component as last component, as it will fire an event
    // that uses all other components!!!
    this.deviceTypeSelect = new JComboBox( new DeviceProfileTypeComboBoxModel() );
    this.deviceTypeSelect.setRenderer( new DeviceProfileTypeComboBoxRenderer() );
    this.deviceTypeSelect.addItemListener( new ItemListener()
    {
      /**
       * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
       */
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        final DeviceProfile profile = ( DeviceProfile )combobox.getSelectedItem();
        if ( profile != null )
        {
          LogicSnifferConfigDialog.this.listening = false;
          try
          {
            updateDeviceType( profile );

            updateConfig( false /* aWarnUserIfConfigIncorrect */);
            updateFields();
          }
          finally
          {
            LogicSnifferConfigDialog.this.listening = true;
          }
        }
      }
    } );
    // By default, select the "OLS" device, if available...
    this.deviceTypeSelect.setSelectedItem( getDeviceProfileManager().getDefaultProfile() );
  }

  /**
   * Sets the enabled state of all available trigger check boxes and the ratio
   * select.
   * 
   * @param aEnable
   *          <code>true</code> to enable trigger configuration fields,
   *          <code>false</code> to disable them;
   * @param aAvailableTriggerStages
   *          the number of available trigger stages, according to our device
   *          configuration.
   */
  private void setTriggerEnabled( final boolean aEnable, final int aAvailableTriggerStages )
  {
    final int channelCount = this.config.getChannelCount();

    final boolean complex = TriggerType.COMPLEX.equals( this.triggerTypeSelect.getSelectedItem() );
    if ( !complex )
    {
      this.triggerStageTabs.setSelectedIndex( 0 );
    }
    this.triggerTypeSelect.setEnabled( aEnable );
    this.ratioSlider.setEnabled( aEnable );

    for ( int stage = 0; stage < LogicSnifferConfig.TRIGGER_STAGES; stage++ )
    {
      final boolean stageEnabled = aEnable && ( stage < aAvailableTriggerStages );
      for ( int i = 0; i < MAX_CHANNELS; i++ )
      {
        final boolean enabled = stageEnabled && ( i < channelCount );
        updateCheckBoxState( this.triggerMask[stage][i], enabled );
        updateCheckBoxState( this.triggerValue[stage][i], enabled );
      }

      this.triggerStageTabs.setEnabledAt( stage, stageEnabled && ( ( stage == 0 ) || complex ) );

      this.triggerLevel[stage].setEnabled( stageEnabled && complex );
      this.triggerDelay[stage].setEnabled( stageEnabled );
      this.triggerMode[stage].setEnabled( stageEnabled );
      if ( aEnable && ( this.triggerMode[stage].getSelectedIndex() == 1 ) )
      {
        this.triggerChannel[stage].setEnabled( true );
      }
      else
      {
        this.triggerChannel[stage].setEnabled( false );
      }
      this.triggerStart[stage].setEnabled( stageEnabled && complex );

      // @@@
      this.triggerHexMask[stage].setEnabled( stageEnabled );
      this.triggerHexValue[stage].setEnabled( stageEnabled );
      this.applyHexMaskButton[stage].setEnabled( stageEnabled );
      this.applyHexValueButton[stage].setEnabled( stageEnabled );
      this.invertHexValue[stage].setEnabled( stageEnabled );

    }
  }

  /**
   * Synchronizes the configuration settings in the UI with the device
   * configuration object.
   * 
   * @param aWarnUserIfConfigIncorrect
   *          <code>true</code> if the user should be warned when an "incorrect"
   *          configuration is present, <code>false</code> to not warn the user,
   *          but only return the state in the return value.
   * @return <code>true</code> if the configuration is correct,
   *         <code>false</code> if the configuration is incorrect.
   */
  private boolean updateConfig( final boolean aWarnUserIfConfigIncorrect )
  {
    String value;
    boolean result = true;

    // set port + baudrate
    this.config.setPortName( ( String )this.portSelect.getSelectedItem() );
    this.config.setBaudrate( NumberUtils.smartParseInt( String.valueOf( this.portRateSelect.getSelectedItem() ) ) );

    // set clock source
    this.config.setClockSource( ( CaptureClockSource )this.sourceSelect.getSelectedItem() );

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
    // and does this based on chosen device type and the number of enabled
    // channel groups...
    final int maxSampleCount = determineMaxSampleCount( enabledChannelGroups );

    // set sample rate; use a default to ensure the internal state remains
    // correct...
    value = String.valueOf( this.speedSelect.getSelectedItem() );
    int f = NumberUtils.smartParseInt( value, UnitDefinition.SI, LogicSnifferDevice.CLOCK );
    this.config.setSampleRate( f );

    // set sample count
    value = String.valueOf( this.sizeSelect.getSelectedItem() );
    int sampleCount = NumberUtils.smartParseInt( value );

    if ( this.maxSampleSize.isSelected() )
    {
      sampleCount = maxSampleCount;
      forceCaptureSizeTo( Integer.valueOf( sampleCount ) );
    }
    if ( sampleCount > 0 )
    {
      this.config.setSampleCount( sampleCount );
    }

    // set before / after ratio
    double r = 1.0 - ( this.ratioSlider.getValue() / ( double )this.ratioSlider.getMaximum() );
    this.config.setRatio( r );

    // set filter
    this.config.setFilterEnabled( this.filterEnable.isSelected() );
    this.config.setRleEnabled( this.rleEnable.isSelected() );

    // set number scheme
    NumberingScheme scheme = ( NumberingScheme )this.numberSchemeSelect.getSelectedItem();
    this.config.setAltNumberSchemeEnabled( NumberingScheme.OUTSIDE.equals( scheme ) );

    // set testing mode
    this.config.setTestModeEnabled( this.testModeEnable.isSelected() );

    // set trigger
    final boolean triggerEnabled = this.triggerEnable.isSelected();
    this.config.setTriggerEnabled( triggerEnabled );
    if ( triggerEnabled )
    {
      final boolean complex = TriggerType.COMPLEX.equals( this.triggerTypeSelect.getSelectedItem() );
      for ( int stage = 0; stage < LogicSnifferConfig.TRIGGER_STAGES; stage++ )
      {
        int m = 0;
        int v = 0;
        for ( int i = 0; i < MAX_CHANNELS; i++ )
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

        result &= ( delay <= 65535 );
        if ( !result && aWarnUserIfConfigIncorrect )
        {
          result = SwingComponentUtils.askConfirmation( this, "Trigger delay for stage " + stage
              + " is larger than 65535 cycles! Continue capture?" );
        }

        final boolean parallelTriggerStage = this.triggerMode[stage].getSelectedIndex() == 0;
        final int channel = this.triggerChannel[stage].getSelectedIndex();
        final boolean startCapture = this.triggerStart[stage].isSelected();

        if ( complex )
        {
          if ( parallelTriggerStage )
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
            if ( parallelTriggerStage )
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

    // Determine whether the chosen device type matches the one found...
    DeviceProfile deviceProfile = this.config.getDeviceProfile();
    if ( ( deviceProfile != null ) && result )
    {
      final DeviceProfile selectedProfile = ( DeviceProfile )this.deviceTypeSelect.getSelectedItem();
      if ( !deviceProfile.equals( selectedProfile ) )
      {
        result = SwingComponentUtils.askConfirmation( this,
            "Device type does not match selected type! Continue capture?" );
      }
    }

    // Determine whether the chosen sample count is larger than the OLS can
    // provide us in the chosen channel group-selection...
    if ( ( maxSampleCount >= 0 ) && result )
    {
      result = ( sampleCount <= maxSampleCount );
      if ( !result && aWarnUserIfConfigIncorrect )
      {
        result = SwingComponentUtils.askConfirmation( this,
            "Sample count too large for chosen channel groups! Continue capture?" );
      }
    }
    // When no channel groups are enabled, the capture won't be very useful...
    if ( result )
    {
      result = ( enabledChannelGroups > 0 );
      if ( !result && aWarnUserIfConfigIncorrect )
      {
        result = SwingComponentUtils.askConfirmation( this, "No channel groups are enabled! Continue capture?" );
      }
    }

    return result;
  }

  /**
   * activates / deactivates dialog options according to device status
   */
  private void updateFields()
  {
    final int availableChannelGroups = this.config.getGroupCount();
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      final boolean enabled = ( i < availableChannelGroups );
      this.channelGroup[i].setEnabled( enabled );
    }

    this.triggerEnable.setSelected( this.config.isTriggerEnabled() );
    setTriggerEnabled( this.config.isTriggerEnabled(), this.config.getMaxTriggerStages() );

    this.filterEnable.setEnabled( this.config.isFilterAvailable() );

    this.speedSelect.setEnabled( this.config.isInternalClock() );

    this.sizeSelect.setEnabled( !this.maxSampleSize.isSelected() );

    this.warningLabel.setText( " " );
    if ( this.rleEnable.isSelected() )
    {
      this.warningLabel.setText( rleWarning );
    }
  }

}

/* EOF */
