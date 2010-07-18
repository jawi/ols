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
import java.awt.Dialog.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.devices.*;


/**
 * 
 */
public class TestDeviceController implements DeviceController
{
  // CONSTANTS

  private static final String[]  DATA_FUNCTIONS = new String[]
                                                { "Sawtooth", "All zeros", "Sine", "odd-even", "0x55-0xAA", "Random",
      "I2C sample"                             };
  private static final Integer[] CHANNELS       = new Integer[]
                                                { 1, 4, 8, 16, 32 };
  private static final Integer[] DATA_LENGTH    = new Integer[]
                                                { 16, 256, 1024, 4096, 8192, 16384, 32768, 65536, 131072 };

  // VARIABLES

  private boolean                setupConfirmed = false;
  private String                 dataFunction   = DATA_FUNCTIONS[0];
  private int                    channels       = CHANNELS[2];
  private int                    dataLength     = DATA_LENGTH[5];

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#captureData(nl.lxtreme.ols.api.devices.CaptureCallback)
   */
  @Override
  public void captureData( final CaptureCallback aCallback ) throws IOException
  {
    final Random rnd = new Random();

    final int[] data;
    int rate = 1000000000;

    if ( DATA_FUNCTIONS[6].equals( this.dataFunction ) )
    {
      data = new int[3260];
      rate = new I2CGenerator().writeBitStream( data, "Hello World; this is a sample I2C bit stream!" );
    }
    else
    {
      data = new int[this.dataLength];
      for ( int i = 0; i < data.length; i++ )
      {
        if ( DATA_FUNCTIONS[0].equals( this.dataFunction ) )
        {
          final int v = ( i / 8 ) & 0xff;
          data[i] = ( 255 - v ) | ( v << 8 ) | ( ( 255 - v ) << 16 ) | ( v << 24 );
        }
        else if ( DATA_FUNCTIONS[1].equals( this.dataFunction ) )
        {
          data[i] = 0x00;
        }
        else if ( DATA_FUNCTIONS[2].equals( this.dataFunction ) )
        {
          data[i] = ( int )( 128 + 128.0 * Math.sin( i / ( data.length / 8.0 ) ) );
        }
        else if ( DATA_FUNCTIONS[3].equals( this.dataFunction ) )
        {
          data[i] = ( i % 2 == 0 ) ? 0x55 : 0xAA;
        }
        else if ( DATA_FUNCTIONS[4].equals( this.dataFunction ) )
        {
          data[i] = ( i % 4 == 0 ) ? 0x55 : 0xAA;
        }
        else if ( DATA_FUNCTIONS[5].equals( this.dataFunction ) )
        {
          data[i] = rnd.nextInt();
        }
      }

    }
    final CapturedData capturedData = new CapturedData( data, 26, rate, this.channels, Integer.MAX_VALUE );
    aCallback.captureComplete( capturedData );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#getName()
   */
  @Override
  public String getName()
  {
    return "Test Device";
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#isSetup()
   */
  @Override
  public boolean isSetup()
  {
    return true;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.util.Properties)
   */
  @Override
  public void readProperties( final Properties aProperties )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#setupCapture()
   */
  @Override
  public boolean setupCapture() throws IOException
  {
    this.setupConfirmed = false;

    final JDialog dialog = new JDialog();
    dialog.setModalityType( ModalityType.APPLICATION_MODAL );
    dialog.setTitle( "Set up test capture" );
    dialog.setLayout( new BorderLayout() );

    final JPanel contentPane = new JPanel( new BorderLayout() );
    contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
    dialog.setContentPane( contentPane );

    final JPanel contents = createContents();
    contentPane.add( contents, BorderLayout.CENTER );

    final JPanel buttonPane = createDialogButtonPanel( dialog );
    contentPane.add( buttonPane, BorderLayout.PAGE_END );

    dialog.pack();
    dialog.setVisible( true );

    return this.setupConfirmed;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.util.Properties)
   */
  @Override
  public void writeProperties( final Properties aProperties )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @return
   */
  private JPanel createContents()
  {
    final JPanel result = new JPanel( new GridBagLayout() );

    final JComboBox dataFunctionCombo = new JComboBox( DATA_FUNCTIONS );
    dataFunctionCombo.setSelectedItem( this.dataFunction );
    dataFunctionCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceController.this.dataFunction = ( String )aEvent.getItem();
      }
    } );

    final JComboBox channelsCombo = new JComboBox( CHANNELS );
    channelsCombo.setSelectedItem( this.channels );
    channelsCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceController.this.channels = ( Integer )aEvent.getItem();
      }
    } );

    final JComboBox dataLengthCombo = new JComboBox( DATA_LENGTH );
    dataLengthCombo.setSelectedItem( this.dataLength );
    dataLengthCombo.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        TestDeviceController.this.dataLength = ( Integer )aEvent.getItem();
      }
    } );

    result.add( new JLabel( "Data function" ), new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0,
        GridBagConstraints.BASELINE_LEADING, GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );
    result.add( dataFunctionCombo, new GridBagConstraints( 1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.NORTHWEST,
        GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );

    result.add( new JLabel( "Channels" ), new GridBagConstraints( 0, 1, 1, 1, 0.0, 0.0,
        GridBagConstraints.BASELINE_LEADING, GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );
    result.add( channelsCombo, new GridBagConstraints( 1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.NORTHWEST,
        GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );

    result.add( new JLabel( "Data length" ), new GridBagConstraints( 0, 2, 1, 1, 0.0, 0.0,
        GridBagConstraints.BASELINE_LEADING, GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );
    result.add( dataLengthCombo, new GridBagConstraints( 1, 2, 1, 1, 1.0, 0.0, GridBagConstraints.NORTHWEST,
        GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 4, 4 ) );

    return result;
  }

  /**
   * @return
   */
  private JPanel createDialogButtonPanel( final JDialog aDialog )
  {
    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );

    buttonPane.add( Box.createHorizontalGlue() );

    final JButton closeButton = new JButton( "Close" );
    closeButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        TestDeviceController.this.setupConfirmed = false;
        aDialog.setVisible( false );
      }
    } );
    buttonPane.add( closeButton );

    final JButton okButton = new JButton( "Ok" );
    okButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        TestDeviceController.this.setupConfirmed = true;
        aDialog.setVisible( false );
      }
    } );
    buttonPane.add( okButton );
    return buttonPane;
  }
}

/* EOF */
