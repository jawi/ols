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
package nl.lxtreme.ols.device.generic;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.validation.*;

import org.osgi.service.prefs.*;


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
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void readPreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
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
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void writePreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
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
    this.channelCount.setInputVerifier( new NumberValidator( "Invalid channel count!" ) );

    this.devicePath = new JTextField( 10 );

    this.sampleDepth = new JTextField( 10 );
    this.sampleDepth.setText( "256" );
    this.sampleDepth.setInputVerifier( new NumberValidator( "Invalid sample depth!" ) );

    this.sampleRate = new JTextField( 10 );
    this.sampleRate.setText( "1000000" );
    this.sampleRate.setInputVerifier( new NumberValidator( "Invalid sample rate!" ) );

    this.sampleWidth = new JTextField( 10 );
    this.sampleWidth.setText( "1" );
    this.sampleWidth.setInputVerifier( new NumberValidator( "Invalid sample width!" ) );

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
   * @return
   */
  private JComponent createDialogButtonPanel()
  {
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

    return SwingComponentUtils.createButtonPane( new JButton[] { okButton, closeButton } );
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JComponent contents = createContents();
    final JComponent buttonPane = createDialogButtonPanel();

    SwingComponentUtils.setupDialogContentPane( this, contents, buttonPane );
  }
}
