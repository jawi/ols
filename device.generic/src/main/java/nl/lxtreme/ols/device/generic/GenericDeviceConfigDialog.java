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

import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.api.UserSettings;
import nl.lxtreme.ols.util.NumberUtils;
import nl.lxtreme.ols.util.swing.SpringLayoutUtils;
import nl.lxtreme.ols.util.swing.StandardActionFactory;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.SwingComponentUtils;
import nl.lxtreme.ols.util.swing.validation.JComponentInputVerifier;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.createRightAlignedLabel;


/**
 * @author jawi
 */
public class GenericDeviceConfigDialog extends JDialog implements Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

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
    final int result = NumberUtils.smartParseInt( this.channelCount.getText(), 8 );
    return result;
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
   * @return
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
    final int result = NumberUtils.smartParseInt( this.sampleDepth.getText(), 1024 );
    return result;
  }

  /**
   * Returns the sample rate of the generic device.
   *
   * @return the sample rate in Hertz (Hz).
   */
  public int getSampleRate()
  {
    final int result = NumberUtils.smartParseInt( this.sampleRate.getText(), 1000000 );
    return result;
  }

  /**
   * Returns the width (in bytes) of each sample.
   *
   * @return the sample width, in bytes, >= 0.
   */
  public int getSampleWidth()
  {
    final int result = NumberUtils.smartParseInt( this.sampleWidth.getText(), 1 );
    return result;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.channelCount.setText( aSettings.get( "channelCount", this.channelCount.getText() ) );
    this.devicePath.setText( aSettings.get( "devicePath", this.devicePath.getText() ) );
    this.sampleDepth.setText( aSettings.get( "sampleDepth", this.sampleDepth.getText() ) );
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
    aSettings.put( "channelCount", this.channelCount.getText() );
    aSettings.put( "devicePath", this.devicePath.getText() );
    aSettings.put( "sampleDepth", this.sampleDepth.getText() );
    aSettings.put( "sampleRate", this.sampleRate.getText() );
    aSettings.put( "sampleWidth", this.sampleWidth.getText() );
  }

  /**
   * Creates the contents of this dialog.
   *
   * @return a content pane, never <code>null</code>.
   */
  private JComponent createContents()
  {
    this.channelCount = new JTextField( 10 );
    this.channelCount.setText( "1" );
    this.channelCount.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid channel count!" ) );

    this.devicePath = new JTextField( 10 );

    this.sampleDepth = new JTextField( 10 );
    this.sampleDepth.setText( "256" );
    this.sampleDepth.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample depth!" ) );

    this.sampleRate = new JTextField( 10 );
    this.sampleRate.setText( "1000000" );
    this.sampleRate.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample rate!" ) );

    this.sampleWidth = new JTextField( 10 );
    this.sampleWidth.setText( "1" );
    this.sampleWidth.setInputVerifier( JComponentInputVerifier.create( Integer.TYPE, "Invalid sample width!" ) );

    final JPanel result = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( result, "Acquisition settings" );

    result.add( createRightAlignedLabel( "Device path" ) );
    result.add( this.devicePath );

    result.add( createRightAlignedLabel( "Channel count" ) );
    result.add( this.channelCount );

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
