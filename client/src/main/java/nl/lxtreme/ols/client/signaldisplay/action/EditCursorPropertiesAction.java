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
import nl.lxtreme.ols.client.signaldisplay.action.EditSignalElementPropertiesAction.JColorEditor;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.*;


/**
 * Provides an action to edit a cursor label.
 */
public class EditCursorPropertiesAction extends AbstractAction
{
  // INNER TYPES

  /**
   * Provides a Swing dialog for editing a cursor label.
   */
  static final class EditCursorDialog extends JDialog implements StatusAwareCloseableDialog
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final String defaultLabel;
    private final Color defaultColor;

    private JTextField labelEditor;
    private JColorEditor colorEditor;

    boolean dialogResult = false;

    // CONSTRUCTORS

    /**
     * Creates a new EditCursorLabelAction.EditCursorDialog instance.
     */
    public EditCursorDialog( final Window aParent, final Cursor aCursor )
    {
      super( aParent, ModalityType.APPLICATION_MODAL );

      setResizable( false );

      this.defaultLabel = aCursor.getLabel();
      this.defaultColor = aCursor.getColor();

      initDialog( aCursor );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void close()
    {
      setVisible( false );
      dispose();
    }

    /**
     * Returns the new cursor color.
     * 
     * @return the cursor color, can be <code>null</code>.
     */
    public Color getColor()
    {
      return this.colorEditor.getColor();
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
     * {@inheritDoc}
     */
    @Override
    public boolean setDialogStatus( final DialogStatus aStatus )
    {
      this.dialogResult = ( aStatus == DialogStatus.OK );
      return true;
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
     * Applies the default properties to the properties.
     */
    protected void applyDefaultProperties()
    {
      this.labelEditor.setText( this.defaultLabel );
      this.colorEditor.setColor( this.defaultColor );
    }

    /**
     * Initializes this dialog.
     */
    private void initDialog( final Cursor aCursor )
    {
      setTitle( "Edit cursor properties" );

      JLabel labelEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Label" );
      this.labelEditor = new JTextField( aCursor.getLabel(), 10 );

      JLabel colorEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Color" );
      this.colorEditor = new JColorEditor( aCursor.getColor() );

      final JButton okButton = StandardActionFactory.createOkButton();
      final JButton cancelButton = StandardActionFactory.createCancelButton();

      final JButton resetButton = new JButton( "Reset to defaults" );
      resetButton.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          applyDefaultProperties();
        }
      } );

      final JPanel resetButtonPanel = new JPanel();
      resetButtonPanel.add( resetButton, BorderLayout.LINE_END );

      JPanel editorPane = new JPanel( new SpringLayout() );
      editorPane.add( labelEditorLabel );
      editorPane.add( this.labelEditor );

      editorPane.add( colorEditorLabel );
      editorPane.add( this.colorEditor );

      editorPane.add( new JLabel( "" ) );
      editorPane.add( resetButtonPanel );

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
  public EditCursorPropertiesAction( final SignalDiagramController aController, final Cursor aCursor )
  {
    super( "Cursor Properties" );

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
    final EditCursorDialog dialog = new EditCursorDialog( SwingComponentUtils.getOwningWindow( aEvent ), this.cursor );
    if ( dialog.showDialog() )
    {
      final SignalDiagramModel model = this.controller.getViewModel();

      // XXX shouldn't this be set directly on the cursor?!!!
      model.setCursorLabel( this.cursor.getIndex(), dialog.getLabel() );
      model.setCursorColor( this.cursor.getIndex(), dialog.getColor() );
    }
  }
}
