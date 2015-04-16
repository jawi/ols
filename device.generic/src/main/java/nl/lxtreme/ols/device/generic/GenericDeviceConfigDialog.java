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
package nl.lxtreme.ols.device.generic;


import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.NumberUtils;
import nl.lxtreme.ols.util.NumberUtils.*;
import nl.lxtreme.ols.util.swing.SpringLayoutUtils;
import nl.lxtreme.ols.util.swing.StandardActionFactory;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.SwingComponentUtils;
import nl.lxtreme.ols.util.swing.validation.JComponentInputVerifier;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.createRightAlignedLabel;


/**
 * Configuration dialog for supporting capturing raw or OLS capture file data.
 */
public class GenericDeviceConfigDialog extends JDialog implements Configurable, Closeable
{
  // CONSTANTS

  public static final String[] DATA_FORMATS = { "Raw", "OLS data format" };

  private static final String DEFAULT_DATA_FORMAT = DATA_FORMATS[0];

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JComboBox dataFormat;

  private JCheckBox timeDataPresent;
  private JTextField devicePath;
  private JTextField sampleRate;
  private JTextField sampleDepth;
  private JTextField sampleWidth;
  private JTextField channelCount;

  private boolean setupConfirmed;

  // CONSTRUCTORS

  /**
   * Creates a new GenericDeviceConfigDialog instance.
   *
   * @param aParent
   *          the parent window of this dialog, can be <code>null</code>.
   */
  public GenericDeviceConfigDialog( final Window aParent )
  {
    super( aParent, "Generic capture settings", ModalityType.DOCUMENT_MODAL );

    initDialog();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * Returns the number of channels in each sample.
   *
   * @return the channel count, >= 0.
   */
  public int getChannelCount()
  {
    return NumberUtils.safeParseInt( this.channelCount.getText(), 8 );
  }

  /**
   * @return the selected data format, as string value.
   */
  public String getDataFormat()
  {
    return ( String )this.dataFormat.getSelectedItem();
  }

  /**
   * Returns the path to the device, like <tt>/dev/ttyS0</tt> or similar.
   *
   * @return the device path, never <code>null</code>.
   */
  public String getDevicePath()
  {
    return this.devicePath.getText();
  }

  /**
   * @return the bitmask of the enabled channels.
   */
  public int getEnabledChannelsMask()
  {
    final int bits = 8 * getSampleWidth();
    final int width = ( int )( ( 1L << bits ) - 1 );
    return width;
  }

  /**
   * Returns the number of samples to take.
   *
   * @return the sample depth, >= 0.
   */
  public int getSampleDepth()
  {
    return NumberUtils.smartParseInt( this.sampleDepth.getText(), 1024 );
  }

  /**
   * Returns the sample rate of the generic device.
   *
   * @return the sample rate in Hertz (Hz), or -1 if not available.
   */
  public int getSampleRate()
  {
    if ( isTimingDataPresent() )
    {
      return NumberUtils.smartParseInt( this.sampleRate.getText(), UnitDefinition.SI, 1000000 );
    }
    return Ols.NOT_AVAILABLE;
  }

  /**
   * Returns the width (in bytes) of each sample.
   *
   * @return the sample width, in bytes, >= 0.
   */
  public int getSampleWidth()
  {
    return NumberUtils.safeParseInt( this.sampleWidth.getText(), 1 );
  }

  /**
   * Returns the number of channels in each sample.
   *
   * @return the channel count, >= 0.
   */
  public boolean isTimingDataPresent()
  {
    return this.timeDataPresent.isSelected();
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.dataFormat.setSelectedItem( aSettings.get( "dataFormat", ( String )this.dataFormat.getSelectedItem() ) );
    this.devicePath.setText( aSettings.get( "devicePath", this.devicePath.getText() ) );
    this.channelCount.setText( aSettings.get( "channelCount", this.channelCount.getText() ) );
    this.sampleDepth.setText( aSettings.get( "sampleDepth", this.sampleDepth.getText() ) );
    this.timeDataPresent.setSelected( aSettings.getBoolean( "timeDataPresent", this.timeDataPresent.isSelected() ) );
    this.sampleRate.setText( aSettings.get( "sampleRate", this.sampleRate.getText() ) );
    this.sampleWidth.setText( aSettings.get( "sampleWidth", this.sampleWidth.getText() ) );
  }

  /**
   * Shows this dialog on screen.
   *
   * @return <code>true</code> if this dialog is confirmed, <code>false</code>
   *         if it was cancelled.
   */
  public boolean showDialog()
  {
    this.setupConfirmed = false;

    setVisible( true );

    return this.setupConfirmed;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.put( "dataFormat", ( String )this.dataFormat.getSelectedItem() );
    aSettings.put( "devicePath", this.devicePath.getText() );
    aSettings.put( "channelCount", this.channelCount.getText() );
    aSettings.put( "sampleDepth", this.sampleDepth.getText() );
    aSettings.getBoolean( "timeDataPresent", this.timeDataPresent.isSelected() );
    aSettings.put( "sampleRate", this.sampleRate.getText() );
    aSettings.put( "sampleWidth", this.sampleWidth.getText() );
  }

  /**
   * Ensures the UI components are in the right state.
   *
   * @param aDataFormat
   *          the selected data format.
   */
  final void updateComponents( final String aDataFormat )
  {
    boolean rawDataSelected = DATA_FORMATS[0].equals( aDataFormat );
    // Raw data: user should enter more data...
    this.channelCount.setEnabled( rawDataSelected );
    this.sampleDepth.setEnabled( rawDataSelected );
    this.timeDataPresent.setEnabled( rawDataSelected );
    this.sampleRate.setEnabled( rawDataSelected && this.timeDataPresent.isSelected() );
    this.sampleWidth.setEnabled( rawDataSelected );
  }

  /**
   * Creates the contents of this dialog.
   *
   * @return a content pane, never <code>null</code>.
   */
  private JComponent createContents()
  {
    this.dataFormat = new JComboBox( DATA_FORMATS );
    this.dataFormat.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JComboBox cb = ( JComboBox )aEvent.getSource();
        updateComponents( ( String )cb.getSelectedItem() );
      }
    } );

    this.devicePath = new JTextField( 10 );
    this.devicePath.setToolTipText( "The path to the socket, file or pipe to read data from." );

    this.channelCount = new JTextField( 10 );
    this.channelCount.setText( "1" );
    this.channelCount.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid channel count!" ) );
    this.channelCount.setToolTipText( "The number of channels contained in the raw data." );

    this.sampleDepth = new JTextField( 10 );
    this.sampleDepth.setText( "256" );
    this.sampleDepth.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample depth!" ) );
    this.sampleDepth.setToolTipText( "The number of samples to read from the input source." );

    this.timeDataPresent = new JCheckBox();
    this.timeDataPresent.setToolTipText( "Whether or not the raw data represents samples or state data." );
    this.timeDataPresent.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox cb = ( JCheckBox )aEvent.getSource();
        GenericDeviceConfigDialog.this.sampleRate.setEnabled( cb.isSelected() );
      }
    } );

    this.sampleRate = new JTextField( 10 );
    this.sampleRate.setText( "1000000" );
    this.sampleRate.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample rate!" ) );
    this.sampleRate.setToolTipText( "The sample rate of the raw data, in Hertz." );

    this.sampleWidth = new JTextField( 10 );
    this.sampleWidth.setText( "1" );
    this.sampleWidth.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample width!" ) );
    this.sampleWidth.setToolTipText( "The number of bytes to read for each sample." );

    // Should cause the proper initial state to be selected...
    this.timeDataPresent.setSelected( true );
    this.dataFormat.setSelectedItem( DEFAULT_DATA_FORMAT );

    final JPanel result = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( result, "Acquisition settings" );

    result.add( createRightAlignedLabel( "Device path" ) );
    result.add( this.devicePath );

    result.add( createRightAlignedLabel( "Data format" ) );
    result.add( this.dataFormat );

    SpringLayoutUtils.addSeparator( result, null );

    result.add( createRightAlignedLabel( "Channel count" ) );
    result.add( this.channelCount );

    result.add( createRightAlignedLabel( "Time data present?" ) );
    result.add( this.timeDataPresent );

    result.add( createRightAlignedLabel( "Sample rate" ) );
    result.add( this.sampleRate );

    result.add( createRightAlignedLabel( "Sample depth" ) );
    result.add( this.sampleDepth );

    result.add( createRightAlignedLabel( "Sample width" ) );
    result.add( this.sampleWidth );

    SpringLayoutUtils.makeEditorGrid( result, 6, 6 );

    return result;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JComponent contents = createContents();
    final JButton closeButton = StandardActionFactory.createCloseButton();

    final JButton okButton = new JButton( "Ok" );
    okButton.setPreferredSize( closeButton.getPreferredSize() );
    okButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        GenericDeviceConfigDialog.this.setupConfirmed = true;
        close();
      }
    } );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, contents, buttonPane, okButton );
  }
}
