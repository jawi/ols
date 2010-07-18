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
package nl.lxtreme.ols.client.signal;


import java.awt.*;
import java.awt.Dialog.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;


/**
 * Stores diagram display settings and provides a dialog for changing them.
 * 
 * @version 0.6
 * @author Michael "Mr. Sump" Poppitz
 */
public class DiagramSettingsDialog extends JComponent implements ActionListener, Configurable, DiagramSettings
{
  // CONSTANTS

  private static final long   serialVersionUID = 1L;

  /** the user cancelled the dialog - all changes were discarded */
  public final static int     CANCEL           = 0;
  /** the user clicked ok - all changes were written to the settings */
  public final static int     OK               = 1;
  /** display a group in 8 channel logic level view (used in <code>groupSettings</code>) */
  public final static int     DISPLAY_CHANNELS = 1;
  /** display a group in a 8bit resolution scope view (used in <code>groupSettings</code>) */
  public final static int     DISPLAY_SCOPE    = 2;
  /** display a group in a 8bit hex value view (used in <code>groupSettings</code>) */
  public final static int     DISPLAY_BYTE     = 4;

  // VARIABLES

  /**
   * Display settings for each group.
   * Can be any combinations (ored) of the defined MODE_* values.
   */
  public int[]                groupSettings;
  private JDialog             dialog;
  private final JCheckBox[][] groupSettingBoxes;
  private int                 result;
  private final Color         signalColor;
  private final Color         triggerColor;
  private final Color         gridColor;
  private final Color         textColor;
  private final Color         timeColor;
  private final Color         groupBackgroundColor;
  private final Color         backgroundColor;
  private final Color         labelColor;
  private final Color[]       cursorColors;
  private final int           channelHeight;
  private final int           scopeHeight;

  // CONSTRUCTORS

  /**
   * Constructs diagram settings component.
   */
  public DiagramSettingsDialog()
  {
    super();

    // Based on color scheme "sleepyhollow";
    // <http://www.colorschemer.com/schemes/viewscheme.php?id=8379>

    // Not used: new Color( 0x40, 0x2c, 0x29 )
    this.backgroundColor = Color.WHITE;
    this.signalColor = new Color( 0x30, 0x4b, 0x75 );
    this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    this.textColor = new Color( 0x25, 0x25, 0x25 );
    this.timeColor = new Color( 0x25, 0x25, 0x25 );
    this.labelColor = new Color( 0x82, 0x87, 0x8f );
    this.cursorColors = makeColorPalette();

    this.channelHeight = 20;
    this.scopeHeight = 133;

    setLayout( new GridBagLayout() );
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    JPanel modePane = new JPanel();
    modePane.setLayout( new GridBagLayout() );
    modePane.setBorder( BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder( "Group Display Settings" ), BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    this.groupSettingBoxes = new JCheckBox[4][3];
    for ( int i = 0; i < 4; i++ )
    {
      modePane.add( new JLabel( "Group " + i + ": " ), createConstraints( 0, i, 1, 1, 0, 0 ) );

      this.groupSettingBoxes[i][0] = new JCheckBox();
      modePane.add( this.groupSettingBoxes[i][0], createConstraints( 1, i, 1, 1, 0, 0 ) );
      modePane.add( new JLabel( "Channels" ), createConstraints( 2, i, 1, 1, 0, 0 ) );

      this.groupSettingBoxes[i][1] = new JCheckBox();
      modePane.add( this.groupSettingBoxes[i][1], createConstraints( 3, i, 1, 1, 0, 0 ) );
      modePane.add( new JLabel( "Scope" ), createConstraints( 4, i, 1, 1, 0, 0 ) );

      this.groupSettingBoxes[i][2] = new JCheckBox();
      modePane.add( this.groupSettingBoxes[i][2], createConstraints( 5, i, 1, 1, 0, 0 ) );
      modePane.add( new JLabel( "Byte Value" ), createConstraints( 6, i, 1, 1, 0, 0 ) );
    }

    add( modePane, createConstraints( 0, 0, 2, 1, 0, 0 ) );

    JButton ok = new JButton( "Ok" );
    ok.addActionListener( this );
    add( ok, createConstraints( 0, 1, 1, 1, 0.5, 0 ) );
    JButton cancel = new JButton( "Cancel" );
    cancel.addActionListener( this );
    add( cancel, createConstraints( 1, 1, 1, 1, 0.5, 0 ) );

    this.groupSettings = new int[4];
    for ( int i = 0; i < this.groupSettings.length; i++ )
    {
      this.groupSettings[i] = DISPLAY_CHANNELS | DISPLAY_BYTE;
    }
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
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.fill = GridBagConstraints.BOTH;
    gbc.insets = new Insets( 4, 4, 4, 4 );
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
  public void actionPerformed( final ActionEvent e )
  {
    if ( e.getActionCommand().equals( "Ok" ) )
    {
      for ( int i = 0; i < 4; i++ )
      {
        this.groupSettings[i] = 0;
        for ( int j = 0; j < 3; j++ )
        {
          if ( this.groupSettingBoxes[i][j].isSelected() )
          {
            this.groupSettings[i] |= 1 << j;
          }
        }
      }
      this.result = OK;
    }
    this.dialog.setVisible( false );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getBackgroundColor()
   */
  public Color getBackgroundColor()
  {
    return this.backgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getChannelHeight()
   */
  @Override
  public int getChannelHeight()
  {
    return this.channelHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getCursorColor(int)
   */
  @Override
  public Color getCursorColor( final int aCursorIdx )
  {
    return this.cursorColors[aCursorIdx];
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGridColor()
   */
  public Color getGridColor()
  {
    return this.gridColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGroupBackgroundColor()
   */
  public Color getGroupBackgroundColor()
  {
    return this.groupBackgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGroupSettingBoxes()
   */
  public JCheckBox[][] getGroupSettingBoxes()
  {
    return this.groupSettingBoxes;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getLabelColor()
   */
  public Color getLabelColor()
  {
    return this.labelColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getScopeHeight()
   */
  @Override
  public int getScopeHeight()
  {
    return this.scopeHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getSignalColor()
   */
  public Color getSignalColor()
  {
    return this.signalColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getSignalHeight()
   */
  @Override
  public int getSignalHeight()
  {
    return getChannelHeight() - 4;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTextColor()
   */
  public Color getTextColor()
  {
    return this.textColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTimeColor()
   */
  public Color getTimeColor()
  {
    return this.timeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTriggerColor()
   */
  public Color getTriggerColor()
  {
    return this.triggerColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowByte(int)
   */
  public final boolean isShowByte( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettingsDialog.DISPLAY_BYTE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowChannels(int)
   */
  public final boolean isShowChannels( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettingsDialog.DISPLAY_CHANNELS ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowScope(int)
   */
  public final boolean isShowScope( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DiagramSettingsDialog.DISPLAY_SCOPE ) > 0 );
  }

  public void readProperties( final Properties properties )
  {
    String value;

    for ( int i = 0; i < 4; i++ )
    {
      value = properties.getProperty( "DiagramSettings.group" + i );
      if ( value != null )
      {
        this.groupSettings[i] = 0;
        if ( value.indexOf( "channels" ) >= 0 )
        {
          this.groupSettings[i] |= DISPLAY_CHANNELS;
        }
        if ( value.indexOf( "scope" ) >= 0 )
        {
          this.groupSettings[i] |= DISPLAY_SCOPE;
        }
        if ( value.indexOf( "byte" ) >= 0 )
        {
          this.groupSettings[i] |= DISPLAY_BYTE;
        }
      }
    }
    updateFields();
  }

  /**
   * Display the settings dialog.
   * If the user clicks ok, all changes are reflected in the properties of this object.
   * Otherwise changes are discarded.
   * 
   * @param frame
   *          parent frame (needed for creating a modal dialog)
   * @return <code>OK</code> when user accepted changes, <code>CANCEL</code> otherwise
   */
  public int showDialog( final Window frame )
  {
    initDialog( frame );
    updateFields();
    this.result = CANCEL;
    this.dialog.setVisible( true );
    return ( this.result );
  }

  public void writeProperties( final Properties properties )
  {
    for ( int i = 0; i < 4; i++ )
    {
      StringBuffer value = new StringBuffer();
      if ( ( this.groupSettings[i] & DISPLAY_CHANNELS ) != 0 )
      {
        value.append( "channels " );
      }
      if ( ( this.groupSettings[i] & DISPLAY_SCOPE ) != 0 )
      {
        value.append( "scope " );
      }
      if ( ( this.groupSettings[i] & DISPLAY_BYTE ) != 0 )
      {
        value.append( "byte " );
      }
      properties.setProperty( "DiagramSettings.group" + i, value.toString() );
    }
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
      this.dialog = new JDialog( frame, "Diagram Settings", ModalityType.APPLICATION_MODAL );
      this.dialog.getContentPane().add( this );
      this.dialog.pack();
      this.dialog.setResizable( false );
    }
  }

  /**
   * @param aI
   * @param aFreq1
   * @param aFreq2
   * @param aFreq3
   * @param aPhase1
   * @param aPhase2
   * @param aPhase3
   * @return
   */
  private Color makeColorGradient( final int aI, final double aFreq1, final double aFreq2, final double aFreq3,
      final double aPhase1, final double aPhase2, final double aPhase3 )
  {
    final int width = 127;
    final int center = 128;
    final int red = ( int )( Math.sin( aFreq1 * aI + aPhase1 ) * width + center );
    final int grn = ( int )( Math.sin( aFreq2 * aI + aPhase2 ) * width + center );
    final int blu = ( int )( Math.sin( aFreq3 * aI + aPhase3 ) * width + center );
    return new Color( red, grn, blu );
  }

  /**
   * @return
   */
  private Color[] makeColorPalette()
  {
    final Color[] result = new Color[10];
    for ( int i = 0; i < result.length; i++ )
    {
      result[i] = makeColorGradient( i, 0.3, 0.3, 0.3, 0.0, 2.0, 4.0 );
    }
    return result;
  }

  private void updateFields()
  {
    for ( int i = 0; i < 4; i++ )
    {
      for ( int j = 0; j < 3; j++ )
      {
        this.groupSettingBoxes[i][j].setSelected( ( this.groupSettings[i] & ( 1 << j ) ) > 0 );
      }
    }
  }

}
