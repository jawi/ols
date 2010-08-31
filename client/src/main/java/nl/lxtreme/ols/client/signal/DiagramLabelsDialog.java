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
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Stores the diagram labels and provides a dialog to change them.
 * 
 * @version 0.7
 * @author Frank Kunz
 */
public class DiagramLabelsDialog extends JDialog
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  private static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // VARIABLES

  private final DataContainer annotatedData;
  private final JTextField[] labelFields;
  private boolean result;

  // CONSTRUCTORS

  /**
   * Constructs diagram labels component.
   */
  public DiagramLabelsDialog( final Window aParent, final DataContainer aAnnotatedData )
  {
    super( aParent, "Diagram Labels", ModalityType.DOCUMENT_MODAL );

    this.annotatedData = aAnnotatedData;

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
    setContentPane( contentPane );

    final JPanel modePane = new JPanel( new GridBagLayout() );
    modePane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    this.labelFields = new JTextField[32];
    for ( int col = 0; col < 2; col++ )
    {
      for ( int row = 0; row < 16; row++ )
      {
        final int index = 16 * col + row;

        this.labelFields[index] = new JTextField( 20 );
        this.labelFields[index].setText( this.annotatedData.getChannelLabel( index ) );

        modePane.add( new JLabel( String.format( "Channel % 2d:", index ) ), //
            new GridBagConstraints( 2 * col, row, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
                GridBagConstraints.NONE, LABEL_INSETS, 0, 0 ) );
        modePane.add( this.labelFields[index], //
            new GridBagConstraints( 2 * col + 1, row, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
                GridBagConstraints.NONE, COMP_INSETS, 0, 0 ) );
      }
    }
    add( modePane, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, LABEL_INSETS,
            0, 0 ) );

    final JButton clear = new JButton( "Clear" );
    clear.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        clearAllLabels();
      }
    } );
    final JButton cancel = new JButton( "Cancel" );
    cancel.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        close( false );
      }
    } );
    final JButton ok = new JButton( "Ok" );
    ok.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        publishAllLabels();
        close( true );
      }
    } );
    // Make all buttons the same size...
    ok.setPreferredSize( cancel.getPreferredSize() );
    clear.setPreferredSize( cancel.getPreferredSize() );

    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );

    buttonPane.add( Box.createHorizontalGlue() );
    buttonPane.add( clear );
    buttonPane.add( Box.createHorizontalStrut( 16 ) );
    buttonPane.add( ok );
    buttonPane.add( Box.createHorizontalStrut( 8 ) );
    buttonPane.add( cancel );

    add( buttonPane, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL,
            LABEL_INSETS, 0, 0 ) );

    pack();
  }

  /**
   * Display the settings dialog. If the user clicks ok, all changes are
   * reflected in the properties of this object. Otherwise changes are
   * discarded.
   * 
   * @return <code>OK</code> when user accepted changes, <code>CANCEL</code>
   *         otherwise
   */
  public boolean showDialog()
  {
    setVisible( true );
    return ( this.result );
  }

  // METHODS

  /**
   * Clears all labels.
   */
  final void clearAllLabels()
  {
    for ( int i = 0; i < DataContainer.MAX_CHANNELS; i++ )
    {
      this.labelFields[i].setText( "" );
    }
  }

  /**
   * @param aDialogResult
   */
  final void close( final boolean aDialogResult )
  {
    this.result = aDialogResult;
    setVisible( false );
  }

  /**
   * Publishes all labels into the annotated data.
   */
  final void publishAllLabels()
  {
    for ( int i = 0; i < DataContainer.MAX_CHANNELS; i++ )
    {
      this.annotatedData.setChannelLabel( i, this.labelFields[i].getText() );
    }
  }

  /**
   * @param aChannelIdx
   * @param aText
   */
  final void setLabel( final int aChannelIdx, final String aText )
  {
    System.out.println( "Setting label " + aChannelIdx + " to '" + aText + "'" );
    this.annotatedData.setChannelLabel( aChannelIdx, aText );
  }
}
