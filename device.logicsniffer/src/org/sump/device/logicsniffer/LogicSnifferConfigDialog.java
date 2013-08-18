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
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.basic.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.NumberUtils.UnitDefinition;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.profile.DeviceProfile.DeviceInterface;
import org.sump.device.logicsniffer.profile.DeviceProfile.NumberingScheme;
import org.sump.device.logicsniffer.profile.DeviceProfile.TriggerType;
import org.sump.device.logicsniffer.protocol.*;

import purejavacomm.*;


/**
 * Provides the configuration dialog for the Open Bench Logic Sniffer device.
 */
public final class LogicSnifferConfigDialog extends JDialog implements Configurable, Closeable
{
  // INNER TYPES

  /**
   * Renders a binary size.
   */
  final class BinarySizeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof Integer )
      {
        double size = ( ( Integer )value ).doubleValue();

        int enabledGroups = getEnabledChannelGroups();
        if ( enabledGroups > 0 )
        {
          int sampleRate = getSelectedSampleRate();
          double time = ( enabledGroups != 0 ) ? size / ( sampleRate * enabledGroups ) : 0.0;

          value = String.format( "<html>%s&nbsp;&nbsp;<span style='color:gray;font-size:0.85em;'>(%s)</span></html>",
              Unit.SizeSI.format( size ), Unit.Time.format( time ) );
        }
        else
        {
          value = String.format( "%s", Unit.SizeSI.format( size ) );
        }
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
        value = Unit.Frequency.format( ( ( Integer )value ).doubleValue() );
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
   * Renders a device interface.
   */
  static final class DeviceInterfaceComboBoxRenderer extends EnumItemRenderer<DeviceInterface>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final DeviceInterface aValue )
    {
      switch ( aValue )
      {
        case NETWORK:
          return "Network";
        case SERIAL:
          return "Serial port";
        case USB:
          return "USB";
      }
      return super.getDisplayValue( aValue );
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
      "38400bps", "19200bps", "14400bps", "9600bps", "4800bps" };

  // VARIABLES

  private final LogicSnifferDevice logicSnifferDevice;

  private LogicSnifferDeviceProfilePanel deviceProfilePanel;

  private JComboBox connTypeSelect;
  private JTextField remAddress;
  private JTextField remPort;
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
  private boolean dialogResult;
  private JSlider ratioSlider;
  private JLabel ratioLabel;
  private JLabel warningLabel;

  private JTextField[] triggerHexMask;
  private JTextField[] triggerHexValue;
  private JButton[] applyHexMaskButton;
  private JButton[] applyHexValueButton;
  private JCheckBox[] invertHexValue;

  private DeviceProfile deviceProfile;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferConfigDialog instance.
   * 
   * @param aParent
   *          the parent window of this dialog;
   * @param aDevice
   *          the logic sniffer device to configure.
   */
  public LogicSnifferConfigDialog( final Window aParent, final LogicSnifferDevice aDevice )
  {
    super( aParent, "OLS Capture settings", ModalityType.DOCUMENT_MODAL );

    this.logicSnifferDevice = aDevice;

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
   * @return a {@link LogicSnifferConfig} instance, never <code>null</code>.
   */
  public LogicSnifferConfig getConfiguration()
  {
    final LogicSnifferConfig config = new LogicSnifferConfig();

    // the current device profile...
    config.setDeviceProfile( this.deviceProfile );

    // how should we connect to our device?
    config.setConnectionURI( getConnectionURI() );

    // set clock source
    config.setClockSource( ( CaptureClockSource )this.sourceSelect.getSelectedItem() );

    // set enabled channel groups
    int enabledChannels = 0;
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      if ( this.channelGroup[i].isSelected() )
      {
        enabledChannels |= 0xff << ( 8 * i );
      }
    }
    config.setEnabledChannels( enabledChannels );

    // set sample rate; use a default to ensure the internal state remains
    // correct...
    config.setSampleRate( getSelectedSampleRate() );

    // set sample count
    config.setSampleCount( getSelectedSampleCount() );

    // set before / after ratio
    double r = 1.0 - ( this.ratioSlider.getValue() / ( double )this.ratioSlider.getMaximum() );
    config.setRatio( r );

    // set filter
    config.setFilterEnabled( this.filterEnable.isEnabled() && this.filterEnable.isSelected() );
    config.setRleEnabled( this.rleEnable.isEnabled() && this.rleEnable.isSelected() );

    // set number scheme
    NumberingScheme scheme = ( NumberingScheme )this.numberSchemeSelect.getSelectedItem();
    config.setAltNumberSchemeEnabled( NumberingScheme.OUTSIDE.equals( scheme ) );

    // set testing mode
    config.setTestModeEnabled( this.testModeEnable.isEnabled() && this.testModeEnable.isSelected() );

    // set trigger
    final boolean triggerEnabled = this.triggerEnable.isEnabled() && this.triggerEnable.isSelected();
    config.setTriggerEnabled( triggerEnabled );

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

        final boolean parallelTriggerStage = this.triggerMode[stage].getSelectedIndex() == 0;
        final int channel = this.triggerChannel[stage].getSelectedIndex();
        final boolean startCapture = this.triggerStart[stage].isSelected();

        if ( complex )
        {
          if ( parallelTriggerStage )
          {
            config.setParallelTrigger( stage, m, v, level, delay, startCapture );
          }
          else
          {
            config.setSerialTrigger( stage, channel, m, v, level, delay, startCapture );
          }
        }
        else
        {
          if ( stage == 0 )
          {
            if ( parallelTriggerStage )
            {
              config.setParallelTrigger( stage, m, v, 0, delay, true );
            }
            else
            {
              config.setSerialTrigger( stage, channel, m, v, 0, delay, true );
            }
          }
          else
          {
            // make sure stages > 0 will not interfere
            config.setParallelTrigger( stage, 0, 0, 3, 0, false );
          }
        }
      }
    }

    return config;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  public void readPreferences( final UserSettings aSettings )
  {
    this.connTypeSelect.setSelectedIndex( aSettings.getInt( "connType", this.connTypeSelect.getSelectedIndex() ) );
    this.remAddress.setText( aSettings.get( "remAddress", "" ) );
    this.remPort.setText( aSettings.get( "remPort", "" ) );

    // Delegate to the contained panels...
    this.deviceProfilePanel.readPreferences( aSettings );
    final String preferredPortName = aSettings.get( "port", null );
    if ( ( preferredPortName != null ) && !"null".equals( preferredPortName ) )
    {
      this.portSelect.setSelectedItem( preferredPortName );
    }
    final String preferredPortRate = aSettings.get( "portRate", null );
    if ( ( preferredPortRate != null ) && !"null".equals( preferredPortRate ) )
    {
      int value = NumberUtils.safeParseInt( preferredPortRate, -1 );
      if ( ( value >= 0 ) && ( value < BAUDRATES.length ) )
      {
        // Regard this as an index...
        this.portRateSelect.setSelectedIndex( value );
      }
      else
      {
        // Regard this as a baud rate...
        this.portRateSelect.setSelectedItem( preferredPortRate );
      }
    }
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
    this.triggerTypeSelect
        .setSelectedIndex( aSettings.getInt( "triggerType", this.triggerTypeSelect.getSelectedIndex() ) );

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

    updateFields();
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
    aSettings.putInt( "connType", this.connTypeSelect.getSelectedIndex() );
    aSettings.put( "remAddress", this.remAddress.getText() );
    aSettings.putInt( "remPort", NumberUtils.smartParseInt( this.remPort.getText() ) );
    aSettings.put( "port", String.valueOf( this.portSelect.getSelectedItem() ) );
    aSettings.put( "portRate", String.valueOf( this.portRateSelect.getSelectedItem() ) );
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

    // Delegate to the contained panels...
    this.deviceProfilePanel.writePreferences( aSettings );
  }

  /**
   * Returns the connection URI to connect to the logic sniffer device.
   * 
   * @return the connection string, used for the connector service, never
   *         <code>null</code>.
   */
  final String getConnectionURI()
  {
    String result;

    if ( this.connTypeSelect.getSelectedItem() == DeviceInterface.NETWORK )
    {
      final String address = this.remAddress.getText();
      final Integer port = getNumericValue( this.remPort );

      result = String.format( "socket://%s:%d", address, port );
    }
    else if ( this.connTypeSelect.getSelectedItem() == DeviceInterface.SERIAL )
    {
      final String portName = getComboBoxText( this.portSelect );
      final Integer baudrate = getNumericValue( this.portRateSelect );

      result = String.format( "comm:%s;baudrate=%d;bitsperchar=8;parity=none;stopbits=1;flowcontrol=xon_xoff",
          portName, baudrate );

      if ( this.deviceProfile != null )
      {
        Integer openDelay = Integer.valueOf( this.deviceProfile.getOpenPortDelay() );
        boolean dtrValue = this.deviceProfile.isOpenPortDtr();

        result = String.format( "%s;dtr=%s;delay=%d", result, ( dtrValue ? "on" : "off" ), openDelay );

        Integer recvTimeout = this.deviceProfile.getReceiveTimeout();
        if ( recvTimeout != null )
        {
          result = String.format( "%s;recv_timeout=%d", result, recvTimeout );
        }
      }
    }
    else
    {
      throw new IllegalStateException( "Unknown/unsupported device interface: " + this.connTypeSelect.getSelectedItem() );
    }

    return result;
  }

  /**
   * Updates the controls to a given device type.
   * 
   * @param aProfile
   *          the device type to update the controls for, can be
   *          <code>null</code> if no device profile is set.
   */
  final void updateDeviceProfile( final DeviceProfile aProfile )
  {
    // "Publish" the device type to the device configuration...
    this.deviceProfile = aProfile;

    // Notify the user something is wrong when we've got no profile...
    this.captureButton.setEnabled( aProfile != null );

    if ( aProfile == null )
    {
      return;
    }

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
   * activates / deactivates dialog options according to device status
   */
  final void updateFields()
  {
    boolean mandatoryFieldsFilled = false;
    if ( isSerialConnection() )
    {
      Object port = this.portSelect.getSelectedItem();
      Object speed = this.portRateSelect.getSelectedItem();

      mandatoryFieldsFilled = ( port != null ) && !"".equals( port ) && ( speed != null ) && !"".equals( speed );
    }
    else if ( isNetworkConnection() )
    {
      String host = this.remAddress.getText();
      String port = this.remPort.getText();

      mandatoryFieldsFilled = ( host != null ) && !"".equals( host ) && ( port != null ) && !"".equals( port );
    }

    final int availableChannelGroups = getChannelGroupCount();
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      final boolean enabled = ( i < availableChannelGroups );
      this.channelGroup[i].setEnabled( enabled );
      if ( !enabled )
      {
        // deselect if the channel group is not enabled...
        this.channelGroup[i].setSelected( false );
      }
    }

    final boolean filterEnabled = ( this.deviceProfile != null ) && this.deviceProfile.isNoiseFilterSupported()
        && !isDdrMode();
    updateCheckBoxState( this.filterEnable, filterEnabled );

    final boolean triggerSupported = ( this.deviceProfile != null ) && this.deviceProfile.isTriggerSupported();
    updateCheckBoxState( this.triggerEnable, triggerSupported );

    final int triggerStages = this.deviceProfile != null ? this.deviceProfile.getTriggerStages() : 0;
    setTriggerEnabled( this.triggerEnable.isSelected(), triggerStages );

    this.speedSelect.setEnabled( this.sourceSelect.getSelectedItem() == CaptureClockSource.INTERNAL );

    this.sizeSelect.setEnabled( !this.maxSampleSize.isSelected() );

    final boolean serialPortSelected = this.connTypeSelect.getSelectedItem() == DeviceInterface.SERIAL;
    this.portSelect.setEnabled( serialPortSelected );
    this.portRateSelect.setEnabled( serialPortSelected );

    final boolean networkSelected = this.connTypeSelect.getSelectedItem() == DeviceInterface.NETWORK;
    this.remAddress.setEnabled( networkSelected );
    this.remPort.setEnabled( networkSelected );

    this.warningLabel.setText( " " );
    if ( this.rleEnable.isSelected() )
    {
      this.warningLabel.setText( rleWarning );
    }

    if ( this.maxSampleSize.isSelected() )
    {
      forceCaptureSizeTo( getSelectedSampleCount() );
    }

    this.captureButton.setEnabled( mandatoryFieldsFilled );
    this.deviceProfilePanel.showMetadataButton.setEnabled( mandatoryFieldsFilled );
  }

  /**
   * Builds this dialog by adding all components to it.
   */
  private void buildDialog()
  {
    this.captureButton = new JButton( "Capture" );
    final JButton cancel = StandardActionFactory.createCloseButton();

    final JTabbedPane tabs = new JTabbedPane( SwingConstants.TOP, JTabbedPane.SCROLL_TAB_LAYOUT );
    tabs.addTab( "Connection", createConnectionSettingsPane() );
    tabs.addTab( "Acquisition", createAcquisitionSettingsPane() );
    tabs.addTab( "Triggers", createTriggerPane() );

    this.captureButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        boolean configCorrect = verifyConfiguration( true /* aWarnUserIfConfigIncorrect */);

        LogicSnifferConfigDialog.this.dialogResult = configCorrect;

        if ( configCorrect )
        {
          close();
        }
      }
    } );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( this.captureButton, cancel );

    SwingComponentUtils.setupWindowContentPane( this, tabs, buttonPane, this.captureButton );

    pack();

    // Make sure all components are in-sync with the current settings...
    updateFields();
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

    connectionPane.add( createRightAlignedLabel( "Connection type" ) );
    connectionPane.add( this.connTypeSelect );

    SpringLayoutUtils.addSeparator( connectionPane, "" );

    connectionPane.add( createRightAlignedLabel( "Remote host address" ) );
    connectionPane.add( this.remAddress );

    connectionPane.add( createRightAlignedLabel( "Remote port" ) );
    connectionPane.add( this.remPort );

    SpringLayoutUtils.addSeparator( connectionPane, "" );

    connectionPane.add( createRightAlignedLabel( "Analyzer port" ) );
    connectionPane.add( this.portSelect );

    connectionPane.add( createRightAlignedLabel( "Port Speed" ) );
    connectionPane.add( this.portRateSelect );

    SpringLayoutUtils.addSeparator( connectionPane, "" );

    this.deviceProfilePanel = new LogicSnifferDeviceProfilePanel( this.logicSnifferDevice )
    {
      @Override
      protected String getConnectionURI()
      {
        return LogicSnifferConfigDialog.this.getConnectionURI();
      }

      @Override
      protected void updateDeviceProfile( final DeviceProfile aProfile )
      {
        LogicSnifferConfigDialog.this.updateDeviceProfile( aProfile );
      }
    };

    this.deviceProfilePanel.buildPanel( connectionPane );

    SpringLayoutUtils.makeEditorGrid( connectionPane, 10, 10 );

    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( connectionPane, new GridBagConstraints( 0, 0, 1, 1, 0.4, 0.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( new JLabel(), new GridBagConstraints( 0, 1, 1, 1, 0.6, 1.0, GridBagConstraints.CENTER,
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
    if ( this.deviceProfile != null )
    {
      return this.deviceProfile.getMaximumCaptureSizeFor( aEnabledChannelGroups );
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
  private void forceCaptureSizeTo( final int aSampleCount )
  {
    this.sizeSelect.setSelectedItem( Integer.valueOf( aSampleCount ) );
  }

  /**
   * Determines, based on the current settings, what the maximum number of
   * channels would be.
   * 
   * @return a channel count, >= 0.
   */
  private int getChannelCount()
  {
    int channels = 32;
    if ( isDdrMode() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      channels = 16;
    }

    // When there's a device profile available, ask it for the maximum number of
    // channels...
    if ( this.deviceProfile != null )
    {
      channels = Math.min( channels, this.deviceProfile.getChannelCount() );
    }

    return channels;
  }

  /**
   * @return the maximum number of channel groups, handling DDR-mode if this is
   *         selected.
   */
  private int getChannelGroupCount()
  {
    int count = 4; // default

    if ( isDdrMode() )
    {
      count >>= 1;
    }

    if ( this.deviceProfile != null )
    {
      count = Math.min( count, this.deviceProfile.getChannelGroupCount() );
    }

    return count;
  }

  /**
   * @return
   */
  private int getEnabledChannelGroups()
  {
    int enabledChannelGroups = 0;
    for ( JCheckBox element : this.channelGroup )
    {
      if ( element.isSelected() )
      {
        enabledChannelGroups++;
      }
    }
    return enabledChannelGroups;
  }

  /**
   * @return
   */
  private int getSelectedSampleCount()
  {
    int result = determineMaxSampleCount( getEnabledChannelGroups() );

    Integer sampleCount = getNumericValue( this.sizeSelect );
    if ( ( sampleCount != null ) && !this.maxSampleSize.isSelected() )
    {
      result = sampleCount.intValue();
    }

    return result;
  }

  /**
   * @return the selected sample rate, in Hertz.
   */
  private int getSelectedSampleRate()
  {
    final String value = getComboBoxText( this.speedSelect );
    return NumberUtils.smartParseInt( value, UnitDefinition.SI, SumpProtocolConstants.CLOCK );
  }

  /**
   * Initializes this dialog by creating all components for it.
   */
  private void initDialog()
  {
    final ActionListener fieldUpdater = new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateFields();
      }
    };

    final DeviceInterface[] devInterfaces = new DeviceInterface[] { DeviceInterface.NETWORK, DeviceInterface.SERIAL };

    this.connTypeSelect = new JComboBox( devInterfaces );
    this.connTypeSelect.setRenderer( new DeviceInterfaceComboBoxRenderer() );
    this.connTypeSelect.setSelectedItem( DeviceInterface.SERIAL );
    this.connTypeSelect.addActionListener( fieldUpdater );

    this.remAddress = new JTextField( "localhost" );
    this.remAddress.addActionListener( fieldUpdater );

    this.remPort = new JTextField( "5678" );
    this.remPort.addActionListener( fieldUpdater );

    this.portSelect = new JLazyComboBox( new JLazyComboBox.ItemProvider()
    {
      @Override
      @SuppressWarnings( "unchecked" )
      public Object[] getItems()
      {
        final Enumeration<CommPortIdentifier> portIdentifiers = CommPortIdentifier.getPortIdentifiers();
        final List<String> portList = new ArrayList<String>();

        while ( portIdentifiers.hasMoreElements() )
        {
          CommPortIdentifier portId = portIdentifiers.nextElement();
          if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
          {
            portList.add( portId.getName() );
          }
        }

        return portList.toArray( new String[portList.size()] );
      }
    } );
    // allow people to put their own port name into it...
    this.portSelect.setEditable( true );
    this.portSelect.addActionListener( fieldUpdater );

    this.portRateSelect = new JComboBox( BAUDRATES );
    this.portRateSelect.setEditable( true );
    this.portRateSelect.setSelectedIndex( 3 ); // 115k2
    this.portRateSelect.addActionListener( fieldUpdater );

    this.numberSchemeSelect = new JComboBox();
    this.numberSchemeSelect.setRenderer( new NumberSchemeComboBoxRenderer() );
    this.numberSchemeSelect.addActionListener( fieldUpdater );

    this.sourceSelect = new JComboBox();
    this.sourceSelect.setRenderer( new ClockSourceComboBoxRenderer() );
    this.sourceSelect.addActionListener( fieldUpdater );

    this.speedSelect = new JComboBox();
    this.speedSelect.setRenderer( new CaptureSpeedComboBoxRenderer() );
    this.speedSelect.addActionListener( fieldUpdater );

    this.groupsPanel = new JPanel();
    this.groupsPanel.setLayout( new GridLayout( 1, 4 ) );

    this.channelGroup = new JCheckBox[MAX_BLOCKS];
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      this.channelGroup[i] = new JCheckBox( Integer.toString( i ) );
      this.channelGroup[i].setSelected( ( i == 0 ) );
      this.channelGroup[i].addActionListener( fieldUpdater );
      this.groupsPanel.add( this.channelGroup[i] );
    }

    this.sizeSelect = new JComboBox();
    this.sizeSelect.setRenderer( new BinarySizeComboBoxRenderer() );
    this.sizeSelect.addActionListener( fieldUpdater );

    this.maxSampleSize = new JCheckBox( "Automatic (maximum)" );
    this.maxSampleSize.setSelected( false );
    this.maxSampleSize.addActionListener( fieldUpdater );

    this.testModeEnable = new JCheckBox( "Enabled" );
    this.testModeEnable.setSelected( true );
    this.testModeEnable.setEnabled( false );
    this.testModeEnable.addActionListener( fieldUpdater );

    this.filterEnable = new JCheckBox( "Enabled" );
    this.filterEnable.setSelected( true );
    this.filterEnable.setEnabled( false );
    this.filterEnable.addActionListener( fieldUpdater );

    this.rleEnable = new JCheckBox( "Enabled" );
    this.rleEnable.setSelected( false );
    this.rleEnable.setEnabled( true );
    this.rleEnable.addActionListener( fieldUpdater );

    this.triggerEnable = new JCheckBox( "Enabled" );
    this.triggerEnable.addActionListener( fieldUpdater );

    this.ratioLabel = new JLabel( "" );
    SwingComponentUtils.fixLabelWidth( this.ratioLabel, "100 / 100" );

    this.ratioSlider = new JSlider( SwingConstants.HORIZONTAL, 0, 100, 50 );
    this.ratioSlider.setMajorTickSpacing( 10 );
    this.ratioSlider.setMinorTickSpacing( 5 );
    this.ratioSlider.setPaintLabels( true );
    this.ratioSlider.setPaintTicks( true );
    this.ratioSlider.addChangeListener( new TriggerRatioChangeListener( this.ratioLabel ) );
    // Issue #82: set the minimum and preferred size to avoid having a
    // "squeezed" slider in the UI...
    final Dimension size = new Dimension( 350, 50 );
    this.ratioSlider.setMinimumSize( size );
    this.ratioSlider.setPreferredSize( size );

    this.triggerTypeSelect = new JComboBox();
    this.triggerTypeSelect.addActionListener( fieldUpdater );

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

      this.triggerMode[i].addActionListener( fieldUpdater );
      final String[] channels = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
          "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31" };
      this.triggerChannel[i] = new JComboBox( channels );
      this.triggerChannel[i].setSelectedIndex( 0 );

      stagePane.add( createRightAlignedLabel( "Channel" ), createConstraints( 4, 0, 1, 1, 0.5, 1.0 ) );
      stagePane.add( this.triggerChannel[i], createConstraints( 5, 0, 1, 1, 0.5, 1.0 ) );

      final JPanel maskValueEditor = createMaskValueEditor( i );
      stagePane.add( maskValueEditor, createConstraints( 0, 1, 6, 1, 1.0, 1.0 ) );

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
      this.applyHexMaskButton[i].putClientProperty( "EDITOR", this.triggerHexMask[i] );
      this.applyHexMaskButton[i].putClientProperty( "MASKS", this.triggerMask[i] );
      stagePane.add( this.applyHexMaskButton[i], createConstraints( 1, 5, 1, 1, 1.0, 1.0 ) );
      this.applyHexMaskButton[i].addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          final JComponent source = ( JComponent )aEvent.getSource();
          final JTextField textField = ( JTextField )source.getClientProperty( "EDITOR" );
          final JCheckBox[] triggerMask = ( JCheckBox[] )source.getClientProperty( "MASKS" );

          try
          {
            final long hexMask = Long.parseLong( textField.getText(), 16 );
            for ( int i = 0; i < MAX_CHANNELS; i++ )
            {
              boolean value = ( ( hexMask >>> i ) & 0x01 ) != 0;
              triggerMask[i].setSelected( value );
            }
          }
          catch ( NumberFormatException e )
          {
            JOptionPane.showMessageDialog( LogicSnifferConfigDialog.this,
                "Illegal number format!\nPlease enter a hexadecimal value." );
          }
          finally
          {
            updateFields();
          }
        }
      } );

      this.applyHexValueButton[i] = new JButton( "Apply Hex Value" );
      this.applyHexValueButton[i].putClientProperty( "EDITOR", this.triggerHexValue[i] );
      this.applyHexValueButton[i].putClientProperty( "VALUES", this.triggerValue[i] );
      this.applyHexValueButton[i].putClientProperty( "INVERT", this.invertHexValue[i] );
      stagePane.add( this.applyHexValueButton[i], createConstraints( 3, 5, 1, 1, 1.0, 1.0 ) );
      this.applyHexValueButton[i].addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          final JComponent source = ( JComponent )aEvent.getSource();
          final JTextField textField = ( JTextField )source.getClientProperty( "EDITOR" );
          final JCheckBox[] triggerValue = ( JCheckBox[] )source.getClientProperty( "VALUES" );
          final JCheckBox invertHexValue = ( JCheckBox )source.getClientProperty( "INVERT" );

          try
          {
            long hexValue = Long.parseLong( textField.getText(), 16 );
            if ( invertHexValue.isSelected() )
            {
              hexValue = ~hexValue;
            }

            for ( int i = 0; i < MAX_CHANNELS; i++ )
            {
              boolean value = ( ( hexValue >>> i ) & 0x01 ) != 0;
              triggerValue[i].setSelected( value );
            }
          }
          catch ( NumberFormatException e )
          {
            JOptionPane.showMessageDialog( LogicSnifferConfigDialog.this,
                "Illegal number format!\nPlease enter a hexadecimal value." );
          }
          finally
          {
            updateFields();
          }
        }
      } );

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
  }

  /**
   * @return <code>true</code> if double-data rate mode is enabled,
   *         <code>false</code> if it disabled.
   */
  private boolean isDdrMode()
  {
    return getSelectedSampleRate() > SumpProtocolConstants.CLOCK;
  }

  /**
   * @return <code>true</code> if a serial connection is to be established,
   *         <code>false</code> otherwise.
   */
  private boolean isSerialConnection()
  {
    return DeviceInterface.SERIAL.equals( this.connTypeSelect.getSelectedItem() );
  }

  /**
   * @return <code>true</code> if a network connection is to be established,
   *         <code>false</code> otherwise.
   */
  private boolean isNetworkConnection()
  {
    return DeviceInterface.NETWORK.equals( this.connTypeSelect.getSelectedItem() );
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
    final int channelCount = getChannelCount();

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
   * Verifies whether the current configuration settings are correct.
   * 
   * @param aWarnUserIfConfigIncorrect
   *          <code>true</code> if the user should be warned when an "incorrect"
   *          configuration is present, <code>false</code> to not warn the user,
   *          but only return the state in the return value.
   * @return <code>true</code> if the configuration is correct,
   *         <code>false</code> if the configuration is incorrect.
   */
  private boolean verifyConfiguration( final boolean aWarnUserIfConfigIncorrect )
  {
    boolean result = true;

    if ( this.deviceProfile == null )
    {
      if ( aWarnUserIfConfigIncorrect )
      {
        JOptionPane.showMessageDialog( this, "No device profile is selected!\n"
            + "Is the device(port) properly configurated?", "Invalid settings detected!", JOptionPane.WARNING_MESSAGE );
      }

      // Consider this to be fatal enough to stop immediately...
      return false;
    }

    // set trigger
    final boolean triggerEnabled = this.triggerEnable.isSelected();
    if ( triggerEnabled )
    {
      for ( int stage = 0; result && ( stage < LogicSnifferConfig.TRIGGER_STAGES ); stage++ )
      {
        final Integer delay = getNumericValue( this.triggerDelay[stage] );

        result &= ( ( delay != null ) && ( delay.intValue() <= 65535 ) );
        if ( !result && aWarnUserIfConfigIncorrect )
        {
          result = SwingComponentUtils.askConfirmation( this, "Trigger delay for stage " + stage
              + " is larger than 65535 cycles! Continue capture?" );
        }
      }
    }

    final int enabledChannelGroups = getEnabledChannelGroups();
    final int maxSampleCount = determineMaxSampleCount( enabledChannelGroups );
    final int sampleCount = getSelectedSampleCount();

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
}

/* EOF */
