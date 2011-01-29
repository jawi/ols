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
package nl.lxtreme.ols.client.diagram;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * Stores the diagram labels and provides a dialog to change them.
 * 
 * @version 0.7
 * @author Frank Kunz
 */
public class DiagramLabelsDialog extends JDialog implements Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  private static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // VARIABLES

  private final String[] channelLabels;
  private JTextField[] labelFields;
  private boolean result;

  // CONSTRUCTORS

  /**
   * Constructs diagram labels component.
   */
  public DiagramLabelsDialog( final Window aParent, final String[] aChannelLabels )
  {
    super( aParent, "Channel Labels", ModalityType.DOCUMENT_MODAL );

    this.result = false;
    this.channelLabels = aChannelLabels;

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
   * @return the channelLabels
   */
  public final String[] getChannelLabels()
  {
    return this.channelLabels;
  }

  /**
   * Display the settings dialog. If the user clicks ok, all changes are
   * reflected in the properties of this object. Otherwise changes are
   * discarded.
   * 
   * @return <code>true</code> when user accepted changes, <code>false</code>
   *         otherwise.
   */
  public boolean showDialog()
  {
    setVisible( true );
    return this.result;
  }

  /**
   * Clears all labels.
   */
  final void clearAllLabels()
  {
    for ( int i = 0; i < CapturedData.MAX_CHANNELS; i++ )
    {
      this.labelFields[i].setText( "" );
    }
  }

  /**
   * Publishes all labels into the annotated data.
   */
  final void publishAllLabels()
  {
    for ( int i = 0; i < CapturedData.MAX_CHANNELS; i++ )
    {
      final String newLabel = this.labelFields[i].getText();
      this.channelLabels[i] = DisplayUtils.isEmpty( newLabel ) ? null : newLabel.trim();
    }
  }

  /**
   * @param aChannelIdx
   * @param aText
   */
  final void setLabel( final int aChannelIdx, final String aText )
  {
    this.channelLabels[aChannelIdx] = aText;
  }

  /**
   * Creates the buttons pane.
   * 
   * @return a button pane, never <code>null</code>.
   */
  private JComponent createButtonPane()
  {
    final JButton clear = new JButton( "Clear" );
    clear.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        clearAllLabels();
      }
    } );

    final JButton ok = new JButton( "Ok" );
    ok.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        publishAllLabels();
        DiagramLabelsDialog.this.result = true;

        close();
      }
    } );

    final JButton cancel = StandardActionFactory.createCloseButton();

    return SwingComponentUtils.createButtonPane( new JButton[] { ok, cancel }, clear );
  }

  /**
   * Creates the label editors pane.
   * 
   * @return a label editor pane, never <code>null</code>.
   */
  private JPanel createLabelEditorsPane()
  {
    final JPanel modePane = new JPanel( new GridBagLayout() );

    this.labelFields = new JTextField[32];
    for ( int col = 0; col < 2; col++ )
    {
      for ( int row = 0; row < 16; row++ )
      {
        final int index = 16 * col + row;

        this.labelFields[index] = new JTextField( 20 );
        this.labelFields[index].setText( this.channelLabels[index] );

        modePane.add( new JLabel( String.format( "Channel % 2d:", Integer.valueOf( index ) ) ), //
            new GridBagConstraints( 2 * col, row, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE_LEADING,
                GridBagConstraints.NONE, LABEL_INSETS, 0, 0 ) );
        modePane.add( this.labelFields[index], //
            new GridBagConstraints( 2 * col + 1, row, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
                GridBagConstraints.NONE, COMP_INSETS, 0, 0 ) );
      }
    }

    return modePane;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JPanel editorsPane = createLabelEditorsPane();
    final JComponent buttonPane = createButtonPane();

    SwingComponentUtils.setupDialogContentPane( this, editorsPane, buttonPane );
  }
}
