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
import javax.swing.event.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.*;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.validation.*;


/**
 * Provides an action to edit the properties, such as label and color, of a
 * signal element.
 */
public class EditSignalElementPropertiesAction extends AbstractAction
{
  // INNER TYPES

  /**
   * Provides a Swing dialog for editing a signal element's label and color.
   */
  static final class EditPropertiesDialog extends JDialog implements StatusAwareCloseableDialog
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final String defaultLabel;
    private final Color defaultColor;
    private final int defaultHeight;
    private final int defaultSignalHeight;
    private final SignalAlignment defaultAlignment;

    private JTextField labelEditor;
    private JColorEditor colorEditor;
    private JTextField heightEditor;
    private JTextField signalHeightEditor;
    private JComboBox signalAlignmentEditor;

    boolean dialogResult = false;

    // CONSTRUCTORS

    /**
     * Creates a new {@link EditPropertiesDialog} instance.
     * 
     * @param aWindow
     *          the parent window for this dialog, can be <code>null</code>;
     * @param aElement
     *          the UI-element to edit the label for, cannot be
     *          <code>null</code>.
     */
    public EditPropertiesDialog( final Window aParent, final IUIElement aElement )
    {
      super( aParent, ModalityType.APPLICATION_MODAL );

      setResizable( false );

      this.defaultLabel = aElement.getLabel();
      this.defaultColor = aElement.getColor();
      this.defaultHeight = aElement.getHeight();

      if ( aElement instanceof SignalElement )
      {
        this.defaultSignalHeight = ( ( SignalElement )aElement ).getSignalHeight();
        this.defaultAlignment = ( ( SignalElement )aElement ).getSignalAlignment();
      }
      else
      {
        this.defaultSignalHeight = 0;
        this.defaultAlignment = null;
      }

      initDialog( aElement );
    }

    /**
     * Determines the title for this dialog based on the given signal element.
     * 
     * @param aElement
     *          the UI-element to determine the title for, cannot be
     *          <code>null</code>.
     * @return a title, never <code>null</code>.
     */
    private static String getTitle( final IUIElement aElement )
    {
      if ( aElement instanceof SignalElement )
      {
        SignalElement signalElement = ( SignalElement )aElement;
        if ( signalElement.isDigitalSignal() )
        {
          return "Edit channel properties";
        }
        else if ( signalElement.isAnalogSignal() )
        {
          return "Edit scope properties";
        }
        else if ( signalElement.isGroupSummary() )
        {
          return "Edit group summary properties";
        }
      }

      return "Edit signal group properties";
    }

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
     * @return
     */
    public Color getColor()
    {
      return this.colorEditor.getColor();
    }

    /**
     * Returns the new element height.
     * 
     * @return the new element height, in pixels.
     */
    public int getElementHeight()
    {
      int defaultValue = UIManager.getInt( UIManagerKeys.CHANNEL_HEIGHT );
      return NumberUtils.safeParseInt( this.heightEditor.getText(), defaultValue );
    }

    /**
     * Returns the new label.
     * 
     * @return the new label, can be <code>null</code>.
     */
    public String getLabel()
    {
      return this.labelEditor.getText();
    }

    /**
     * Returns the new signal height.
     * 
     * @return the new signal height, in pixels.
     */
    public SignalAlignment getSignalAlignment()
    {
      SignalAlignment result = ( SignalAlignment )this.signalAlignmentEditor.getSelectedItem();
      if ( result == null )
      {
        result = SignalAlignment.valueOf( UIManager.getString( UIManagerKeys.SIGNALVIEW_SIGNAL_ALIGNMENT ) );
      }
      return result;
    }

    /**
     * Returns the new signal height.
     * 
     * @return the new signal height, in pixels.
     */
    public int getSignalHeight()
    {
      int defaultValue = UIManager.getInt( UIManagerKeys.DIGITAL_SIGNAL_HEIGHT );
      return NumberUtils.safeParseInt( this.signalHeightEditor.getText(), defaultValue );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean setDialogStatus( final DialogStatus aStatus )
    {
      boolean isOkButton = ( aStatus == DialogStatus.OK );
      String text = this.labelEditor.getText();

      if ( isOkButton && ( ( text == null ) || "".equals( text ) ) )
      {
        JOptionPane.showMessageDialog( getParent(), "No label defined!", //
            "Invalid properties", JOptionPane.ERROR_MESSAGE );
        return false;
      }

      this.dialogResult = isOkButton;
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
      this.heightEditor.setText( "" + this.defaultHeight );
      this.signalHeightEditor.setText( "" + this.defaultSignalHeight );
      this.signalAlignmentEditor.setSelectedItem( this.defaultAlignment );
    }

    /**
     * Initializes this dialog.
     */
    private void initDialog( final IUIElement aElement )
    {
      setTitle( getTitle( aElement ) );

      final JButton okButton = StandardActionFactory.createOkButton();
      final JButton cancelButton = StandardActionFactory.createCancelButton();

      JLabel labelEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Label" );
      this.labelEditor = new JTextField( aElement.getLabel(), 10 );
      this.labelEditor.getDocument().addDocumentListener( new DocumentListener()
      {
        @Override
        public void changedUpdate( final DocumentEvent aEvent )
        {
          updateOkButton( aEvent );
        }

        @Override
        public void insertUpdate( final DocumentEvent aEvent )
        {
          updateOkButton( aEvent );
        }

        @Override
        public void removeUpdate( final DocumentEvent aEvent )
        {
          updateOkButton( aEvent );
        }

        private void updateOkButton( final DocumentEvent aEvent )
        {
          String text = EditPropertiesDialog.this.labelEditor.getText();
          okButton.setEnabled( ( text != null ) && !"".equals( text.trim() ) );
        }
      } );

      JLabel colorEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Color" );
      this.colorEditor = new JColorEditor( aElement.getColor() );

      JLabel heightEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Height" );
      this.heightEditor = new JTextField( "" + aElement.getHeight(), 10 );
      this.heightEditor.setInputVerifier( JComponentInputVerifier.create( Integer.class,
          "Invalid height! Must be a postive whole number." ) );

      JLabel signalHeightEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Signal height" );
      this.signalHeightEditor = new JTextField( "" + this.defaultSignalHeight, 10 );
      this.signalHeightEditor.setInputVerifier( JComponentInputVerifier.create( Integer.class,
          "Invalid height! Must be a postive whole number." ) );

      JLabel signalAlignmentEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Signal alignment" );
      this.signalAlignmentEditor = new JComboBox( SignalAlignment.values() );
      this.signalAlignmentEditor.setRenderer( new SignalAlignmentComboBoxRenderer() );
      this.signalAlignmentEditor.setSelectedItem( this.defaultAlignment );

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

      editorPane.add( heightEditorLabel );
      editorPane.add( this.heightEditor );

      if ( ( aElement instanceof SignalElement ) && ( ( SignalElement )aElement ).isDigitalSignal() )
      {
        editorPane.add( signalHeightEditorLabel );
        editorPane.add( this.signalHeightEditor );

        editorPane.add( signalAlignmentEditorLabel );
        editorPane.add( this.signalAlignmentEditor );
      }

      editorPane.add( new JLabel( "" ) );
      editorPane.add( resetButtonPanel );

      SpringLayoutUtils.makeEditorGrid( editorPane, 10, 10 );

      JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, cancelButton );

      JPanel contentPane = new JPanel( new BorderLayout( 4, 4 ) );
      contentPane.add( editorPane, BorderLayout.CENTER );
      contentPane.add( buttonPane, BorderLayout.PAGE_END );

      setContentPane( contentPane );

      getRootPane().setDefaultButton( okButton );

      pack();
    }
  }

  /**
   * Provides a color editor; showing as a label that has the selected color as
   * foreground, and can be clicked (if not read-only) to change the color.
   */
  static final class JColorEditor extends JTextField
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private volatile Color color;
    private volatile boolean readOnly;

    // CONSTRUCTORS

    /**
     * Creates a new {@link JColorEditor} instance.
     * 
     * @param aColor
     *          the initial color of this editor, cannot be <code>null</code>.
     */
    public JColorEditor( final Color aColor )
    {
      super( "" );
      setEditable( false );
      setColor( aColor );
    }

    /**
     * Creates a new {@link JColorEditor} instance.
     * 
     * @param aColor
     *          the initial color of this editor, cannot be <code>null</code>.
     */
    public JColorEditor( final String aColor )
    {
      super( "" );
      setEditable( false );
      setColor( aColor );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void addNotify()
    {
      try
      {
        addMouseListener( new MouseAdapter()
        {
          @Override
          public void mouseClicked( final MouseEvent aEvent )
          {
            if ( !isReadOnly() && ( aEvent.getClickCount() == 2 ) )
            {
              showEditDialog();
            }
          };
        } );
      }
      finally
      {
        super.addNotify();
      }
    }

    /**
     * Returns the current value of color.
     * 
     * @return the color
     */
    public final Color getColor()
    {
      return this.color;
    }

    /**
     * Returns the current value of readOnly.
     * 
     * @return the readOnly
     */
    public final boolean isReadOnly()
    {
      return this.readOnly;
    }

    /**
     * Sets color to the given value.
     * 
     * @param aColor
     *          the color to set.
     */
    public final void setColor( final Color aColor )
    {
      this.color = aColor;
      setBackground( this.color );
      setToolTipText( "#".concat( ColorUtils.toHexString( aColor ) ) );
    }

    /**
     * Sets color to the given value.
     * 
     * @param aColor
     *          the color (as hex triplet) to set.
     */
    public final void setColor( final String aColor )
    {
      setColor( ColorUtils.parseColor( aColor ) );
    }

    /**
     * Sets readOnly to the given value.
     * 
     * @param aReadOnly
     *          the readOnly to set.
     */
    public final void setReadOnly( final boolean aReadOnly )
    {
      this.readOnly = aReadOnly;
    }

    /**
     * Shows the editor dialog for changing the color of this editor.
     */
    protected void showEditDialog()
    {
      Color result = JColorChooser.showDialog( this, "Edit color", getColor() );
      if ( result != null )
      {
        setColor( result );
      }
    }
  }

  /**
   * Provides a signal alignment combobox renderer.
   */
  static final class SignalAlignmentComboBoxRenderer extends EnumItemRenderer<SignalAlignment>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final SignalAlignment aValue )
    {
      switch ( aValue )
      {
        case BOTTOM:
          return "Bottom";
        case TOP:
          return "Top";
        case CENTER:
          return "Center";
        default:
      }
      return super.getDisplayValue( aValue );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final IUIElement element;
  private final Point dialogLocation;
  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditSignalElementPropertiesAction} instance.
   * 
   * @param aController
   *          the controller to use;
   * @param aElement
   *          the UI-element to edit the label for;
   * @param aChannelLocation
   *          the location on screen of the channel to edit the label for.
   */
  public EditSignalElementPropertiesAction( final SignalDiagramController aController, final IUIElement aElement,
      final Point aChannelLocation )
  {
    super( "Element Properties" );

    this.controller = aController;
    this.element = aElement;
    this.dialogLocation = new Point( aChannelLocation.x + 15, aChannelLocation.y + 5 );

    setEnabled( this.element != null );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    EditPropertiesDialog dialog = new EditPropertiesDialog( SwingComponentUtils.getOwningWindow( aEvent ), this.element );
    dialog.setLocation( this.dialogLocation );

    if ( dialog.showDialog() )
    {
      this.element.setLabel( dialog.getLabel() );
      this.element.setColor( dialog.getColor() );
      this.element.setHeight( dialog.getElementHeight() );

      if ( ( this.element instanceof SignalElement ) )
      {
        SignalElement signalElement = ( SignalElement )this.element;
        if ( signalElement.isDigitalSignal() )
        {
          signalElement.setSignalHeight( dialog.getSignalHeight() );
          signalElement.setSignalAlignment( dialog.getSignalAlignment() );
        }
      }

      // Since the entire layout can be mixed up by the new label, we should
      // repaint the entire screen...
      this.controller.revalidateAll();
    }
  }
}
