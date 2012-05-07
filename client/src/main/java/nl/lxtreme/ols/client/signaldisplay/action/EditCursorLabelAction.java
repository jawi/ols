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
package nl.lxtreme.ols.client.signaldisplay.action;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an action to edit a cursor label.
 */
public class EditCursorLabelAction extends AbstractAction
{
  // INNER TYPES

  /**
   * Provides a Swing dialog for editing a cursor label.
   */
  static final class EditCursorDialog extends JDialog
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private JTextField labelEditor;

    boolean dialogResult = false;

    // CONSTRUCTORS

    /**
     * Creates a new EditCursorLabelAction.EditCursorDialog instance.
     */
    public EditCursorDialog( final int aCursorIdx, final String aLabel )
    {
      super( null /* owner */, ModalityType.DOCUMENT_MODAL );

      setTitle( String.format( "Edit label cursor %d", Integer.valueOf( aCursorIdx ) ) );

      initDialog( aCursorIdx, aLabel );
    }

    /**
     * Returns the new cursor label.
     * 
     * @return the cursor label, can be <code>null</code>.
     */
    public String getLabel()
    {
      return this.labelEditor.getText();
    }

    /**
     * Makes this dialog visible on screen and waits until it is dismissed.
     * 
     * @return <code>true</code> if the dialog is acknowledged by the user,
     *         <code>false</code> if it is cancelled by the user.
     */
    public boolean showDialog()
    {
      this.dialogResult = false;
      setVisible( true );
      return this.dialogResult;
    }

    /**
     * Initializes this dialog.
     */
    private void initDialog( final int aCursorIdx, final String aLabel )
    {
      JLabel label = new JLabel( String.format( "Cursor label %d", Integer.valueOf( aCursorIdx ) ) );
      this.labelEditor = new JTextField( aLabel, 10 );

      final JButton okButton = new JButton( "Ok" );
      okButton.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          EditCursorDialog.this.dialogResult = true;
          setVisible( false );
          dispose();
        }
      } );

      final JButton cancelButton = new JButton( "Cancel" );
      cancelButton.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          EditCursorDialog.this.dialogResult = false;
          setVisible( false );
          dispose();
        }
      } );

      JPanel editorPane = new JPanel( new SpringLayout() );
      editorPane.add( label );
      editorPane.add( this.labelEditor );

      SpringLayoutUtils.makeEditorGrid( editorPane, 10, 10 );

      JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, cancelButton );

      JPanel contentPane = new JPanel( new BorderLayout( 4, 4 ) );
      contentPane.add( editorPane, BorderLayout.CENTER );
      contentPane.add( buttonPane, BorderLayout.PAGE_END );

      setContentPane( contentPane );

      pack();
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;
  private final Cursor cursor;

  // CONSTRUCTORS

  /**
   * Creates a new EditCursorLabelAction instance.
   * 
   * @param aController
   *          the {@link SignalDiagramController} to use;
   * @param aCursor
   *          the cursor to edit the label for.
   */
  public EditCursorLabelAction( final SignalDiagramController aController, final Cursor aCursor )
  {
    super( "Edit label" );
    this.controller = aController;
    this.cursor = aCursor;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final int cursorIndex = this.cursor.getIndex();

    final EditCursorDialog dialog = new EditCursorDialog( cursorIndex, this.cursor.getLabel() );
    if ( dialog.showDialog() )
    {
      setCursorLabel( dialog.getLabel() );
    }
  }

  /**
   * Sets the cursor label to the one given.
   * 
   * @param aNewLabel
   *          the new cursor label to set.
   */
  private void setCursorLabel( final String aNewLabel )
  {
    final SignalDiagramModel model = this.controller.getSignalDiagramModel();
    model.setCursorLabel( this.cursor.getIndex(), aNewLabel );
  }
}
