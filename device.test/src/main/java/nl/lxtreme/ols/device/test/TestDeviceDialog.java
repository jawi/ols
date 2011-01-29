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
package nl.lxtreme.ols.device.test;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;

import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
@SuppressWarnings( "boxing" )
public class TestDeviceDialog extends JDialog implements Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  static final String[] DATA_FUNCTIONS = new String[] { "Sawtooth", "All zeros", "Sine", "odd-even", "0x55-0xAA",
      "Random", "I2C sample", "1-Wire sample" };
  static final Integer[] CHANNELS = new Integer[] { 1, 4, 8, 16, 32 };
  static final Integer[] DATA_LENGTH = new Integer[] { 16, 256, 1024, 4096, 8192, 16384, 32768, 65536, 131072 };

  // VARIABLES

  private boolean setupConfirmed;
  private String dataFunction;
  private int channels;
  private int dataLength;
  private JComboBox dataFunctionCombo;
  private JComboBox channelsCombo;
  private JComboBox dataLengthCombo;

  // CONSTRUCTORS

  /**
   * 
   */
  public TestDeviceDialog( final Window aParent )
  {
    super( aParent, "Test capture settings", ModalityType.DOCUMENT_MODAL );

    this.setupConfirmed = false;
    this.dataFunction = DATA_FUNCTIONS[6];
    this.channels = CHANNELS[2];
    this.dataLength = DATA_LENGTH[5];

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
   * @return the channels
   */
  public int getChannels()
  {
    return this.channels;
  }

  /**
   * @return the dataFunction
   */
  public String getDataFunction()
  {
    return this.dataFunction;
  }

  /**
   * @return the dataLength
   */
  public int getDataLength()
  {
    return this.dataLength;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void readPreferences( final Preferences aPrefs )
  {
    SwingComponentUtils.setSelectedIndex( this.channelsCombo, aPrefs.getInt( "channels", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.dataFunctionCombo, aPrefs.getInt( "dataFunction", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.dataLengthCombo, aPrefs.getInt( "dataLength", -1 ) );
  }

  /**
   * Shows this dialog on screen.
   * 
   * @return <code>true</code> if this dialog is confirmed, <code>false</code>
   *         if it was cancelled.
   */
  public boolean showDialog()
  {
    setVisible( true );
    return this.setupConfirmed;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void writePreferences( final Preferences aPrefs )
  {
    aPrefs.putInt( "channels", this.channelsCombo.getSelectedIndex() );
    aPrefs.putInt( "dataFunction", this.dataFunctionCombo.getSelectedIndex() );
    aPrefs.putInt( "dataLength", this.dataLengthCombo.getSelectedIndex() );
  }

  /**
   * @return
   */
  private JPanel createContents()
  {
    this.dataFunctionCombo = new JComboBox( DATA_FUNCTIONS );
    this.dataFunctionCombo.setSelectedItem( this.dataFunction );
    this.dataFunctionCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceDialog.this.dataFunction = ( String )aEvent.getItem();
      }
    } );

    this.channelsCombo = new JComboBox( CHANNELS );
    this.channelsCombo.setSelectedItem( this.channels );
    this.channelsCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceDialog.this.channels = ( Integer )aEvent.getItem();
      }
    } );

    this.dataLengthCombo = new JComboBox( DATA_LENGTH );
    this.dataLengthCombo.setSelectedItem( this.dataLength );
    this.dataLengthCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceDialog.this.dataLength = ( Integer )aEvent.getItem();
      }
    } );

    final Insets labelInsets = new Insets( 4, 4, 4, 2 );
    final Insets compInsets = new Insets( 4, 2, 4, 4 );

    final JPanel result = new JPanel( new GridBagLayout() );
    result.setBorder( BorderFactory.createEmptyBorder( 4, 0, 4, 0 ) );

    result.add( new JLabel( "Data function" ), //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, labelInsets, 0, 0 ) );
    result.add( this.dataFunctionCombo, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, compInsets, 0, 0 ) );

    result.add( new JLabel( "Channels" ), //
        new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, labelInsets, 0, 0 ) );
    result.add( this.channelsCombo, //
        new GridBagConstraints( 1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, compInsets, 0, 0 ) );

    result.add( new JLabel( "Data length" ), //
        new GridBagConstraints( 0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, labelInsets, 0, 0 ) );
    result.add( this.dataLengthCombo, //
        new GridBagConstraints( 1, 2, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, compInsets, 0, 0 ) );

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
        TestDeviceDialog.this.setupConfirmed = true;
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
