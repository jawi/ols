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


/**
 * Stores the diagram labels and provides a dialog to change them.
 * 
 * @version 0.7
 * @author Frank Kunz
 */
public class DiagramLabelsDialog extends JComponent implements ActionListener, DiagramLabels
{
  // CONSTANTS

  private static final long  serialVersionUID = 1L;

  /** the user cancelled the dialog - all changes were discarded */
  public final static int    CANCEL           = 0;
  /** the user clicked ok - all changes were written to the settings */
  public final static int    OK               = 1;

  // VARIABLES

  private final String[]     diagramLabels;
  private JDialog            dialog;
  private final JTextField[] labelFields;
  private int                result;

  // CONSTRUCTORS

  /**
   * Constructs diagram labels component.
   */
  public DiagramLabelsDialog()
  {
    super();
    setLayout( new GridBagLayout() );
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    JPanel modePane = new JPanel();
    modePane.setLayout( new GridBagLayout() );
    modePane.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createTitledBorder( "Diagram Labels" ),
        BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) ) );

    this.labelFields = new JTextField[32];
    this.diagramLabels = new String[32];
    for ( int col = 0; col < 2; col++ )
    {
      for ( int row = 0; row < 16; row++ )
      {
        int num = 16 * col + row;
        modePane.add( new JLabel( "Channel " + num + ": " ), createConstraints( 2 * col, row, 1, 1, 0, 0 ) );
        this.labelFields[num] = new JTextField( 20 );
        modePane.add( this.labelFields[num], createConstraints( 2 * col + 1, row, 1, 1, 0, 0 ) );
        this.diagramLabels[num] = new String();
      }
    }
    add( modePane, createConstraints( 0, 0, 5, 1, 0, 0 ) );

    JButton ok = new JButton( "Ok" );
    ok.addActionListener( this );
    add( ok, createConstraints( 0, 1, 1, 1, 0.34, 0 ) );
    JButton cancel = new JButton( "Cancel" );
    cancel.addActionListener( this );
    add( cancel, createConstraints( 1, 1, 1, 1, 0.33, 0 ) );
    JButton clear = new JButton( "Clear" );
    clear.addActionListener( this );
    add( clear, createConstraints( 2, 1, 1, 1, 0.33, 0 ) );
  }

  // METHODS

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
      for ( int i = 0; i < 32; i++ )
      {
        this.diagramLabels[i] = new String( this.labelFields[i].getText() );
      }
      this.result = OK;
      this.dialog.setVisible( false );
    }
    else if ( e.getActionCommand().equals( "Clear" ) )
    {
      for ( int i = 0; i < 32; i++ )
      {
        this.labelFields[i].setText( "" );
      }
    }
    else
    {
      this.dialog.setVisible( false );
    }
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramLabels#getDiagramLabels()
   */
  public String[] getDiagramLabels()
  {
    return this.diagramLabels;
  }

  /**
   * Reads user settings from given properties.
   * Uses the property prefix "DiagramLabels".
   * @param properties
   *          properties to read settings from
   */
  public void readProperties( final String aNamespace, final Properties properties )
  {
    for ( int i = 0; i < 32; i++ )
    {
      if ( properties.containsKey( "DiagramLabels.channel" + i ) )
      {
        this.diagramLabels[i] = properties.getProperty( "DiagramLabels.channel" + i );
      }
    }
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

  /**
   * Writes user settings to given properties.
   * Uses the property prefix "DiagramLabels".
   * @param properties
   *          properties to write settings to
   */
  public void writeProperties( final String aNamespace, final Properties properties )
  {
    for ( int i = 0; i < 32; i++ )
    {
      properties.setProperty( "DiagramLabels.channel" + i, this.diagramLabels[i] );
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
      this.dialog = new JDialog( frame, "Diagram Labels", ModalityType.APPLICATION_MODAL );
      this.dialog.getContentPane().add( this );
      this.dialog.pack();
      this.dialog.setResizable( false );
    }
  }

  private void updateFields()
  {
    for ( int i = 0; i < 32; i++ )
    {
      this.labelFields[i].setText( this.diagramLabels[i] );
    }
  }
}
