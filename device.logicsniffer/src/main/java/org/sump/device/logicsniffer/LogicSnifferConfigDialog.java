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


import java.awt.*;
import java.awt.Dialog.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.*;


/**
 * 
 */
public class LogicSnifferConfigDialog extends JComponent implements ActionListener, Configurable
{
  // CONSTANTS

  private static final long        serialVersionUID = 1L;

  private static final String      NAME             = "OLS";

  // VARIABLES

  private final JComboBox          portSelect;
  private final JComboBox          portRateSelect;
  private final JComboBox          numberSchemeSelect;
  private final JComboBox          sourceSelect;
  private final JComboBox          speedSelect;
  private final JComboBox          sizeSelect;
  private final JCheckBox          filterEnable;
  private final JCheckBox          rleEnable;
  private final JCheckBox          triggerEnable;
  private final JComboBox          triggerTypeSelect;
  private final JTabbedPane        triggerStageTabs;
  private final JComboBox[]        triggerLevel;
  private final JTextField[]       triggerDelay;
  private final JComboBox[]        triggerMode;
  private final JComboBox[]        triggerChannel;
  private final JCheckBox[]        triggerStart;
  private final JCheckBox[][]      triggerMask;
  private final JCheckBox[][]      triggerValue;
  private final JCheckBox[]        channelGroup;
  private final JButton            captureButton;
  private JDialog                  dialog;
  private final int                triggerStages;
  private final LogicSnifferDevice device;
  private boolean                  dialogResult;

  private final JSlider            ratioSlider;

  private final JLabel             ratioLabel;

  // CONSTRUCTORS

  /**
   * 
   */
  public LogicSnifferConfigDialog( final LogicSnifferDevice aDevice )
  {
    super();

    setLayout( new GridBagLayout() );
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    this.device = aDevice;

    // connection pane
    final JPanel connectionPane = new JPanel();
    connectionPane.setLayout( new GridLayout( 6, 2, 5, 5 ) );
    connectionPane.setBorder( BorderFactory.createCompoundBorder( BorderFactory
        .createTitledBorder( "Connection Settings" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    final String[] ports = LogicSnifferDevice.getPorts();
    this.portSelect = new JComboBox( ports );
    connectionPane.add( new JLabel( "Analyzer Port:" ) );
    connectionPane.add( this.portSelect );

    final String[] portRates =
    { "115200bps(LL)", "57600bps (LH)", "38400bps (HL)", "19200bps (HH)" };
    this.portRateSelect = new JComboBox( portRates );
    connectionPane.add( new JLabel( "Port Speed (SW1,SW0):" ) );
    connectionPane.add( this.portRateSelect );

    final String[] numberSchemes =
    { "Inside", "Outside", "Test Mode" };
    this.numberSchemeSelect = new JComboBox( numberSchemes );
    connectionPane.add( new JLabel( "Number Scheme:" ) );
    connectionPane.add( this.numberSchemeSelect );

    connectionPane.add( new JLabel() );
    connectionPane.add( new JLabel() );
    connectionPane.add( new JLabel() );
    connectionPane.add( new JLabel() );
    connectionPane.add( new JLabel() );
    connectionPane.add( new JLabel() );

    add( connectionPane, createConstraints( 0, 0, 1, 1, 1.0, 0.5 ) );

    // settings pane
    final JPanel settingsPane = new JPanel();
    settingsPane.setLayout( new GridLayout( 6, 2, 5, 5 ) );
    settingsPane.setBorder( BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder( "Analyzer Settings" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    final String[] sources =
    { "Internal", "External / Rising", "External / Falling" };
    this.sourceSelect = new JComboBox( sources );
    this.sourceSelect.addActionListener( this );
    settingsPane.add( new JLabel( "Sampling Clock:" ) );
    settingsPane.add( this.sourceSelect );

    final String[] speeds =
    { "200MHz", "100MHz", "50MHz", "20MHz", "10MHz", "5MHz", "2MHz", "1MHz", "500kHz", "200kHz", "100kHz", "50kHz",
        "20kHz", "10kHz", "1kHz", "500Hz", "200Hz", "100Hz", "50Hz", "20Hz", "10Hz" };
    this.speedSelect = new JComboBox( speeds );
    this.speedSelect.setSelectedIndex( 1 );
    this.speedSelect.addActionListener( this );
    settingsPane.add( new JLabel( "Sampling Rate:" ) );
    settingsPane.add( this.speedSelect );

    final Container groups = new Container();
    groups.setLayout( new GridLayout( 1, 4 ) );
    this.channelGroup = new JCheckBox[4];
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      this.channelGroup[i] = new JCheckBox( Integer.toString( i ) );
      this.channelGroup[i].setSelected( true );
      groups.add( this.channelGroup[i] );
    }
    settingsPane.add( new JLabel( "Channel Groups:" ) );
    settingsPane.add( groups );

    final String[] sizes =
    { "24K", "12K", "6K", "2K", "1K", "512", "256", "128", "64" };
    this.sizeSelect = new JComboBox( sizes );
    this.sizeSelect.setSelectedIndex( 2 );
    settingsPane.add( new JLabel( "Recording Size:" ) );
    settingsPane.add( this.sizeSelect );

    add( settingsPane, createConstraints( 1, 0, 1, 1, 1.0, 0.5 ) );

    this.filterEnable = new JCheckBox( "Enable" );
    this.filterEnable.setSelected( true );
    this.filterEnable.setEnabled( false );
    settingsPane.add( new JLabel( "Noise Filter: " ) );
    settingsPane.add( this.filterEnable );

    this.rleEnable = new JCheckBox( "Enable" );
    this.rleEnable.setSelected( false );
    this.rleEnable.setEnabled( true );
    settingsPane.add( new JLabel( "RLE: " ) );
    settingsPane.add( this.rleEnable );

    // trigger pane
    final JPanel triggerPane = new JPanel();
    triggerPane.setLayout( new GridBagLayout() );
    triggerPane.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Trigger Settings" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );
    this.triggerEnable = new JCheckBox( "Enable" );
    this.triggerEnable.addActionListener( this );
    triggerPane.add( new JLabel( "Trigger: " ), createConstraints( 0, 0, 1, 1, 0.0, 1.0 ) );
    triggerPane.add( this.triggerEnable, createConstraints( 1, 0, 1, 1, 0.0, 1.0 ) );
    triggerPane.add( new JLabel(), createConstraints( 2, 0, 1, 1, 1.0, 1.0 ) );

    this.ratioLabel = new JLabel( " 50/ 50" );
    final JLabel ratioSliderLabel = new JLabel( "Before/After Ratio: " );

    this.ratioSlider = new JSlider( SwingConstants.HORIZONTAL, 0, 100, 50 );
    this.ratioSlider.setMajorTickSpacing( 10 );
    this.ratioSlider.setMinorTickSpacing( 5 );
    this.ratioSlider.setPaintLabels( true );
    this.ratioSlider.setPaintTicks( true );
    this.ratioSlider.addChangeListener( new ChangeListener()
    {
      private final JLabel label = LogicSnifferConfigDialog.this.ratioLabel;

      @Override
      public void stateChanged( final ChangeEvent aEvent )
      {
        final JSlider slider = ( JSlider )aEvent.getSource();

        final int before = slider.getValue();
        final int after = ( slider.getMaximum() - before );

        final String ratioText = String.format( "% 3d/% 3d", before, after );
        slider.setToolTipText( ratioText );
        this.label.setText( ratioText );
      }
    } );

    triggerPane.add( ratioSliderLabel, createConstraints( 0, 1, 1, 1, 0.5, 1.0 ) );
    triggerPane.add( this.ratioSlider, createConstraints( 1, 1, 1, 1, 1.0, 1.0 ) );
    triggerPane.add( this.ratioLabel, createConstraints( 2, 1, 1, 1, 0.5, 1.0 ) );

    final String[] types =
    { "Simple", "Complex" };
    this.triggerTypeSelect = new JComboBox( types );
    this.triggerTypeSelect.addActionListener( this );
    triggerPane.add( new JLabel( "Type: " ), createConstraints( 0, 2, 1, 1, 0.5, 1.0 ) );
    triggerPane.add( this.triggerTypeSelect, createConstraints( 1, 2, 1, 1, 0.5, 1.0 ) );

    triggerPane.add( new JLabel( " " ), createConstraints( 0, 3, 1, 1, 1.0, 1.0 ) );

    this.triggerStageTabs = new JTabbedPane();
    this.triggerStages = this.device.getTriggerStageCount();
    this.triggerMask = new JCheckBox[4][];
    this.triggerValue = new JCheckBox[4][];
    this.triggerLevel = new JComboBox[4];
    this.triggerDelay = new JTextField[4];
    this.triggerMode = new JComboBox[4];
    this.triggerChannel = new JComboBox[4];
    this.triggerStart = new JCheckBox[4];
    for ( int i = 0; i < this.triggerStages; i++ )
    {
      final JPanel stagePane = new JPanel();
      stagePane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
      stagePane.setLayout( new GridBagLayout() );

      final String[] levels =
      { "Immediatly", "On Level 1", "On Level 2", "On Level 3" };
      this.triggerLevel[i] = new JComboBox( levels );
      if ( i > 0 )
      {
        this.triggerLevel[i].setSelectedIndex( 3 );
      }
      stagePane.add( new JLabel( "Arm:" ), createConstraints( 0, 0, 1, 1, 1.0, 1.0 ) );
      stagePane.add( this.triggerLevel[i], createConstraints( 1, 0, 1, 1, 0.5, 1.0 ) );
      final String[] modes =
      { "Parallel", "Serial" };
      this.triggerMode[i] = new JComboBox( modes );
      stagePane.add( new JLabel( "Mode:", SwingConstants.RIGHT ), createConstraints( 2, 0, 1, 1, 0.5, 1.0 ) );
      stagePane.add( this.triggerMode[i], createConstraints( 3, 0, 1, 1, 0.5, 1.0 ) );
      this.triggerMode[i].addActionListener( this );
      final String[] channels =
      { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
          "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31" };
      this.triggerChannel[i] = new JComboBox( channels );
      stagePane.add( new JLabel( "Channel:", JLabel.RIGHT ), createConstraints( 4, 0, 1, 1, 0.5, 1.0 ) );
      stagePane.add( this.triggerChannel[i], createConstraints( 5, 0, 1, 1, 0.5, 1.0 ) );

      stagePane.add( new JLabel( "31" ), createConstraints( 1, 1, 1, 1, 1.0, 1.0 ) );
      stagePane.add( new JLabel( "0", JLabel.RIGHT ), createConstraints( 5, 1, 1, 1, 1.0, 1.0 ) );
      stagePane.add( new JLabel( "Mask:" ), createConstraints( 0, 2, 1, 1, 1.0, 1.0 ) );
      this.triggerMask[i] = createChannelList( stagePane, createConstraints( 1, 2, 5, 1, 1.0, 1.0 ) );
      stagePane.add( new JLabel( "Value:" ), createConstraints( 0, 3, 1, 1, 1.0, 1.0 ) );
      this.triggerValue[i] = createChannelList( stagePane, createConstraints( 1, 3, 5, 1, 1.0, 1.0 ) );

      stagePane.add( new JLabel( "Action:" ), createConstraints( 0, 4, 1, 1, 1.0, 1.0 ) );
      this.triggerStart[i] = new JCheckBox( "Start Capture    (otherwise trigger level will rise by one)" );
      stagePane.add( this.triggerStart[i], createConstraints( 1, 4, 3, 1, 1.0, 1.0 ) );
      stagePane.add( new JLabel( "Delay:", JLabel.RIGHT ), createConstraints( 4, 4, 1, 1, 0.5, 1.0 ) );
      this.triggerDelay[i] = new JTextField( "0" );
      stagePane.add( this.triggerDelay[i], createConstraints( 5, 4, 1, 1, 0.5, 1.0 ) );
      this.triggerStageTabs.add( "Stage " + i, stagePane );
    }
    triggerPane.add( this.triggerStageTabs, createConstraints( 0, 4, 3, 1, 1.0, 1.0 ) );
    add( triggerPane, createConstraints( 0, 2, 3, 2, 1.0, 0.5 ) );

    add( new JLabel(), createConstraints( 0, 5, 1, 1, 0.5, 0 ) );

    this.captureButton = new JButton( "Capture" );
    this.captureButton.addActionListener( this );
    add( this.captureButton, createConstraints( 1, 5, 1, 1, 0.5, 0 ) );

    final JButton cancel = new JButton( "Close" );
    cancel.addActionListener( this );
    add( cancel, createConstraints( 2, 5, 1, 1, 0.5, 0 ) );
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
    gbc.fill = GridBagConstraints.BOTH;
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
  public void actionPerformed( final ActionEvent event )
  {
    final Object o = event.getSource();
    final String l = event.getActionCommand();

    // ignore all events when dialog is not displayed
    if ( ( this.dialog == null ) || !this.dialog.isVisible() )
    {
      return;
    }

    if ( o == this.triggerEnable )
    {
      updateDevice();
      updateFields();

    }
    else if ( o == this.sourceSelect )
    {
      updateDevice();
      updateFields();

    }
    else if ( o == this.speedSelect )
    {
      updateDevice();
      updateFields();

    }
    else if ( l.equals( "Capture" ) )
    {
      this.dialogResult = true;
      updateDevice();
      close();
    }
    else if ( l.equals( "Close" ) )
    {
      this.dialogResult = false;
      close();
    }
    else
    {
      updateFields();
    }
  }

  /**
   * @return
   */
  public int getPortBaudrate()
  {
    return smartParseInt( ( String )this.portRateSelect.getSelectedItem() );
  }

  /**
   * @return
   */
  public String getPortName()
  {
    return ( String )this.portSelect.getSelectedItem();
  }

  public void readProperties( final Properties properties )
  {
    selectByValue( this.portSelect, properties.getProperty( NAME + ".port" ) );
    selectByValue( this.portRateSelect, properties.getProperty( NAME + ".portRate" ) );
    selectByValue( this.sourceSelect, properties.getProperty( NAME + ".source" ) );
    selectByValue( this.speedSelect, properties.getProperty( NAME + ".speed" ) );
    selectByValue( this.sizeSelect, properties.getProperty( NAME + ".size" ) );
    selectByValue( this.ratioSlider, properties.getProperty( NAME + ".ratio" ) );
    this.filterEnable.setSelected( "true".equals( properties.getProperty( NAME + ".filter" ) ) );
    this.triggerEnable.setSelected( "true".equals( properties.getProperty( NAME + ".trigger" ) ) );
    selectByValue( this.triggerTypeSelect, properties.getProperty( NAME + ".triggerType" ) );

    for ( int stage = 0; stage < this.triggerStages; stage++ )
    {
      selectByValue( this.triggerLevel[stage], properties.getProperty( NAME + ".triggerStage" + stage + "Level" ) );
      this.triggerDelay[stage].setText( properties.getProperty( NAME + ".triggerStage" + stage + "Delay" ) );
      selectByValue( this.triggerMode[stage], properties.getProperty( NAME + ".triggerStage" + stage + "Mode" ) );
      selectByValue( this.triggerChannel[stage], properties.getProperty( NAME + ".triggerStage" + stage + "Channel" ) );

      final String mask = properties.getProperty( NAME + ".triggerStage" + stage + "Mask" );
      if ( mask != null )
      {
        for ( int i = 0; ( i < 32 ) && ( i < mask.length() ); i++ )
        {
          this.triggerMask[stage][i].setSelected( mask.charAt( i ) == '1' );
        }
      }

      final String value = properties.getProperty( NAME + ".triggerStage" + stage + "Value" );
      if ( value != null )
      {
        for ( int i = 0; ( i < 32 ) && ( i < value.length() ); i++ )
        {
          this.triggerValue[stage][i].setSelected( value.charAt( i ) == '1' );
        }
      }

      this.triggerStart[stage].setSelected( "true".equals( properties.getProperty( NAME + ".triggerStage" + stage
          + "StartCapture" ) ) );
    }

    final String group = properties.getProperty( NAME + ".channelGroup" );
    if ( group != null )
    {
      for ( int i = 0; ( i < 4 ) && ( i < group.length() ); i++ )
      {
        this.channelGroup[i].setSelected( group.charAt( i ) == '1' );
      }
    }

    updateDevice();
    updateFields();
  }

  /**
   * @return
   */
  public boolean showDialog()
  {
    initDialog( ( Window )null );
    setDialogEnabled( true );

    this.dialog.setModalityType( ModalityType.APPLICATION_MODAL );
    this.dialog.setVisible( true );

    return this.dialogResult;
  }

  /** activates / deactivates dialog options according to device status */
  public void updateFields()
  {
    updateFields( true );
  }

  public void writeProperties( final Properties properties )
  {
    properties.setProperty( NAME + ".port", ( String )this.portSelect.getSelectedItem() );
    properties.setProperty( NAME + ".portRate", ( String )this.portRateSelect.getSelectedItem() );
    properties.setProperty( NAME + ".source", ( String )this.sourceSelect.getSelectedItem() );
    properties.setProperty( NAME + ".speed", ( String )this.speedSelect.getSelectedItem() );
    properties.setProperty( NAME + ".size", ( String )this.sizeSelect.getSelectedItem() );
    properties.setProperty( NAME + ".ratio", String.valueOf( this.ratioSlider.getValue() ) );
    properties.setProperty( NAME + ".filter", this.filterEnable.isSelected() ? "true" : "false" );
    properties.setProperty( NAME + ".trigger", this.triggerEnable.isSelected() ? "true" : "false" );
    properties.setProperty( NAME + ".triggerType", ( String )this.triggerTypeSelect.getSelectedItem() );

    for ( int stage = 0; stage < this.triggerStages; stage++ )
    {
      properties.setProperty( NAME + ".triggerStage" + stage + "Level", ( String )this.triggerLevel[stage]
          .getSelectedItem() );
      properties.setProperty( NAME + ".triggerStage" + stage + "Delay", this.triggerDelay[stage].getText() );
      properties.setProperty( NAME + ".triggerStage" + stage + "Mode", ( String )this.triggerMode[stage]
          .getSelectedItem() );
      properties.setProperty( NAME + ".triggerStage" + stage + "Channel", ( String )this.triggerChannel[stage]
          .getSelectedItem() );

      final StringBuffer mask = new StringBuffer();
      for ( int i = 0; i < 32; i++ )
      {
        mask.append( this.triggerMask[stage][i].isSelected() ? "1" : "0" );
      }
      properties.setProperty( NAME + ".triggerStage" + stage + "Mask", mask.toString() );

      final StringBuffer value = new StringBuffer();
      for ( int i = 0; i < 32; i++ )
      {
        value.append( this.triggerValue[stage][i].isSelected() ? "1" : "0" );
      }
      properties.setProperty( NAME + ".triggerStage" + stage + "Value", value.toString() );

      properties.setProperty( NAME + ".triggerStage" + stage + "StartCapture",
          this.triggerStart[stage].isSelected() ? "true" : "false" );
    }

    final StringBuffer group = new StringBuffer();
    for ( int i = 0; i < 4; i++ )
    {
      group.append( this.channelGroup[i].isSelected() ? "1" : "0" );
    }
    properties.setProperty( NAME + ".channelGroup", group.toString() );
  }

  /**
   * Properly closes the dialog. This method makes sure timer and worker
   * thread are stopped before the dialog is closed.
   */
  private void close()
  {
    this.device.stop();
    this.dialog.setVisible( false );
  }

  /**
   * Creates an array of check boxes, adds it to the device controller and
   * returns it.
   * 
   * @param label
   *          label to use on device controller component
   * @return array of created check boxes
   */
  private JCheckBox[] createChannelList( final JPanel pane, final GridBagConstraints constraints )
  {
    final JCheckBox[] boxes = new JCheckBox[32];

    final Container container = new Container();
    container.setLayout( new GridLayout( 1, 32 ) );

    for ( int col = 31; col >= 0; col-- )
    {
      final JCheckBox box = new JCheckBox();
      box.setEnabled( false );
      container.add( box );
      if ( ( ( col % 8 ) == 0 ) && ( col > 0 ) )
      {
        container.add( new JLabel() );
      }
      boxes[col] = box;
    }

    pane.add( container, constraints );
    return ( boxes );
  }

  /**
   * Internal method that initializes a dialog and add this component to it.
   * 
   * @param frame
   *          owner of the dialog
   */
  private void initDialog( final Window frame )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.dialog != null ) && ( this.dialog.getOwner() != frame ) )
    {
      this.dialog.dispose();
      this.dialog = null;
    }
    // if no valid dialog exists, create one
    if ( this.dialog == null )
    {
      this.dialog = new JDialog( frame, "Capture" );
      this.dialog.getContentPane().add( this );
      this.dialog.setResizable( false );
      this.dialog.setSize( getPreferredSize() );
      // dialog.pack();
    }

    // sync dialog status with device
    updateFields();
  }

  private void selectByValue( final JComboBox box, final String value )
  {
    if ( value != null )
    {
      for ( int i = 0; i < box.getItemCount(); i++ )
      {
        if ( value.equals( ( String )box.getItemAt( i ) ) )
        {
          box.setSelectedIndex( i );
        }
      }
    }
  }

  private void selectByValue( final JSlider slider, final String value )
  {
    try
    {
      slider.setValue( Integer.valueOf( value ) );
    }
    catch ( NumberFormatException exception )
    {
      slider.setValue( 50 ); // XXX default value
    }
  }

  /**
   * Sets the enabled state of all configuration components of the dialog.
   * 
   * @param enable
   *          <code>true</code> to enable components, <code>false</code> to
   *          disable them
   */
  private void setDialogEnabled( final boolean enable )
  {
    this.triggerEnable.setEnabled( enable );
    this.captureButton.setEnabled( enable );
    this.portSelect.setEnabled( enable );
    this.portRateSelect.setEnabled( enable );
    this.numberSchemeSelect.setEnabled( enable );
    this.speedSelect.setEnabled( enable );
    this.sizeSelect.setEnabled( enable );
    updateFields( enable );
  }

  /**
   * Sets the enabled state of all available trigger check boxes and the ratio
   * select.
   * 
   * @param enable
   *          <code>true</code> to enable trigger configuration fields, <code>false</code> to disable them
   */
  private void setTriggerEnabled( final boolean enable )
  {
    final int channels = this.device.getAvailableChannelCount();
    final boolean complex = "Complex".equals( ( String )this.triggerTypeSelect.getSelectedItem() );
    if ( !complex )
    {
      this.triggerStageTabs.setSelectedIndex( 0 );
    }
    this.triggerTypeSelect.setEnabled( enable );
    this.ratioSlider.setEnabled( enable );

    for ( int stage = 0; stage < this.triggerStages; stage++ )
    {
      for ( int i = 0; i < channels; i++ )
      {
        this.triggerMask[stage][i].setEnabled( enable );
        this.triggerValue[stage][i].setEnabled( enable );
      }
      for ( int i = channels; i < 32; i++ )
      {
        this.triggerMask[stage][i].setEnabled( false );
        this.triggerValue[stage][i].setEnabled( false );
      }
      this.triggerStageTabs.setEnabledAt( stage, enable && ( ( stage == 0 ) || complex ) );
      this.triggerLevel[stage].setEnabled( enable && complex );
      this.triggerDelay[stage].setEnabled( enable );
      this.triggerMode[stage].setEnabled( enable );
      if ( enable && ( this.triggerMode[stage].getSelectedIndex() == 1 ) )
      {
        this.triggerChannel[stage].setEnabled( true );
      }
      else
      {
        this.triggerChannel[stage].setEnabled( false );
      }
      this.triggerStart[stage].setEnabled( enable && complex );
    }
  }

  /**
   * Extracts integers from strings regardless of trailing trash.
   * 
   * @param s
   *          string to be parsed
   * @return integer value, 0 if parsing fails
   */
  private int smartParseInt( final String s )
  {
    int val = 0;

    try
    {
      for ( int i = 1; i <= s.length(); i++ )
      {
        val = Integer.parseInt( s.substring( 0, i ) );
      }
    }
    catch ( final NumberFormatException E )
    {
    }

    return ( val );
  }

  /** writes the dialog settings to the device */
  private void updateDevice()
  {
    String value;

    // set clock source
    value = ( String )this.sourceSelect.getSelectedItem();
    if ( value.equals( "Internal" ) )
    {
      this.device.setClockSource( LogicSnifferDevice.CLOCK_INTERNAL );
    }
    else
    {
      if ( value.equals( "External / Rising" ) )
      {
        this.device.setClockSource( LogicSnifferDevice.CLOCK_EXTERNAL_RISING );
      }
      else
      {
        this.device.setClockSource( LogicSnifferDevice.CLOCK_EXTERNAL_FALLING );
      }
    }

    // set sample rate
    value = ( String )this.speedSelect.getSelectedItem();
    int f = smartParseInt( value );
    if ( value.indexOf( "M" ) > 0 )
    {
      f *= 1000000;
    }
    else if ( value.indexOf( "k" ) > 0 )
    {
      f *= 1000;
    }
    this.device.setRate( f );

    // set sample count
    value = ( String )this.sizeSelect.getSelectedItem();
    int s = smartParseInt( value );
    if ( value.indexOf( "K" ) > 0 )
    {
      s *= 1024;
    }
    this.device.setSize( s );

    // set before / after ratio
    double r = 1.0 - ( this.ratioSlider.getValue() / ( double )this.ratioSlider.getMaximum() );
    this.device.setRatio( r );

    // set filter
    this.device.setFilterEnabled( this.filterEnable.isSelected() );
    this.device.setRleEnabled( this.rleEnable.isSelected() );

    // set number scheme
    value = ( String )this.numberSchemeSelect.getSelectedItem();
    if ( value.equals( "Inside" ) )
    {
      this.device.setTestModeEnabled( false );
      this.device.setAltNumberSchemeEnabled( false );
    }
    else if ( value.equals( "Outside" ) )
    {
      this.device.setTestModeEnabled( false );
      this.device.setAltNumberSchemeEnabled( true );
    }
    else if ( value.equals( "Test Mode" ) )
    {
      this.device.setTestModeEnabled( true );
      this.device.setAltNumberSchemeEnabled( false );
    }

    // set trigger
    final boolean triggerEnabled = this.triggerEnable.isSelected();
    this.device.setTriggerEnabled( triggerEnabled );
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
        final int delay = smartParseInt( this.triggerDelay[stage].getText() );
        final int channel = this.triggerChannel[stage].getSelectedIndex();
        final boolean startCapture = this.triggerStart[stage].isSelected();
        if ( complex )
        {
          if ( this.triggerMode[stage].getSelectedIndex() == 0 )
          {
            this.device.setParallelTrigger( stage, m, v, level, delay, startCapture );
          }
          else
          {
            this.device.setSerialTrigger( stage, channel, m, v, level, delay, startCapture );
          }
        }
        else
        {
          if ( stage == 0 )
          {
            if ( this.triggerMode[stage].getSelectedIndex() == 0 )
            {
              this.device.setParallelTrigger( stage, m, v, 0, delay, true );
            }
            else
            {
              this.device.setSerialTrigger( stage, channel, m, v, 0, delay, true );
            }
          }
          else
          {
            // make sure stages > 0 will not interfere
            this.device.setParallelTrigger( stage, 0, 0, 3, 0, false );
          }
        }
      }
    }

    // set enabled channel groups
    int enabledChannels = 0;
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      if ( this.channelGroup[i].isSelected() )
      {
        enabledChannels |= 0xff << ( 8 * i );
      }
    }
    this.device.setEnabledChannels( enabledChannels );
  }

  /** activates / deactivates dialog options according to device status */
  private void updateFields( final boolean enable )
  {
    this.triggerEnable.setSelected( this.device.isTriggerEnabled() );
    setTriggerEnabled( this.device.isTriggerEnabled() );
    this.filterEnable.setEnabled( this.device.isFilterAvailable() && enable );
    for ( int i = 0; i < this.channelGroup.length; i++ )
    {
      this.channelGroup[i].setEnabled( enable && ( i < this.device.getAvailableChannelCount() / 8 ) );
    }
    this.speedSelect.setEnabled( this.device.getClockSource() == LogicSnifferDevice.CLOCK_INTERNAL );
  }

}

/* EOF */
