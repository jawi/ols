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
package nl.lxtreme.ols.device.demo;


import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * @author jawi
 */
@SuppressWarnings( "boxing" )
public class DemoDeviceDialog extends JDialog implements Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  static final Integer[] CHANNELS = new Integer[] { 1, 4, 8, 16, 32 };
  static final Integer[] DATA_LENGTH = new Integer[] { 16, 256, 1024, 4096, 8192, 16384, 32768, 65536, 131072, 262144,
      524288, 1048576, 2097152, 4194304, 8388608, 16777216 };
  static final Map<String, IDataGenerator> GENERATORS = new LinkedHashMap<String, IDataGenerator>();

  static
  {
    final Class<?>[] generators = { SawtoothDataGenerator.class, ZeroDataGenerator.class, SineDataGenerator.class,
        OddEvenGenerator.class, RandomDataGenerator.class, I2CGenerator.class, OneWireGenerator.class,
        ManchesterEncoder.class, ClockedCounterGenerator.class, StateDataGenerator.class, BurstGenerator.class };

    for ( Class<?> generator : generators )
    {
      try
      {
        IDataGenerator inst = ( IDataGenerator )generator.newInstance();
        GENERATORS.put( inst.getName(), inst );
      }
      catch ( Exception exception )
      {
        exception.printStackTrace();
      }
    }
  }

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
  public DemoDeviceDialog( final Window aParent )
  {
    super( aParent, "Test capture settings", ModalityType.DOCUMENT_MODAL );

    this.setupConfirmed = false;

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
  public IDataGenerator getDataGenerator()
  {
    return GENERATORS.get( this.dataFunction );
  }

  /**
   * @return the dataLength
   */
  public int getDataLength()
  {
    return this.dataLength;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.dataFunctionCombo.setSelectedIndex( aSettings.getInt( "dataFunction", 6 ) );
    this.channelsCombo.setSelectedIndex( aSettings.getInt( "channels", 2 ) );
    this.dataLengthCombo.setSelectedIndex( aSettings.getInt( "dataLength", 5 ) );
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
    aSettings.putInt( "channels", this.channelsCombo.getSelectedIndex() );
    aSettings.putInt( "dataFunction", this.dataFunctionCombo.getSelectedIndex() );
    aSettings.putInt( "dataLength", this.dataLengthCombo.getSelectedIndex() );
  }

  /**
   * Confirms and closes this dialog.
   */
  final void confirmAndCloseDialog()
  {
    this.setupConfirmed = true;

    // Make the selected information available for the outside...
    this.channels = ( Integer )this.channelsCombo.getSelectedItem();
    this.dataFunction = ( String )this.dataFunctionCombo.getSelectedItem();
    this.dataLength = ( Integer )this.dataLengthCombo.getSelectedItem();

    close();
  }

  /**
   * @return
   */
  private JPanel createContents()
  {
    Object[] generatorNames = GENERATORS.keySet().toArray();

    this.dataFunctionCombo = new JComboBox( generatorNames );
    this.channelsCombo = new JComboBox( CHANNELS );
    this.dataLengthCombo = new JComboBox( DATA_LENGTH );

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
        confirmAndCloseDialog();
      }
    } );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, contents, buttonPane, okButton );
  }

}
