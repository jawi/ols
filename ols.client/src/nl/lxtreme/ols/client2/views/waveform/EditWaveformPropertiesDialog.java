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
package nl.lxtreme.ols.client2.views.waveform;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.UIMgr.Alignment;
import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type.CHANNEL;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.validation.*;


/**
 * Provides a Swing dialog for editing a signal element's label and color.
 */
public class EditWaveformPropertiesDialog extends JDialog implements StatusAwareCloseableDialog
{
  // INNER TYPES

  /**
   * Provides a signal alignment combobox renderer.
   */
  static final class AlignmentComboBoxRenderer extends EnumItemRenderer<Alignment>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final Alignment aValue )
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

  private final String defaultLabel;
  private final Color defaultColor;
  private final int defaultHeight;
  private final int defaultSignalHeight;
  private final Alignment defaultAlignment;

  private JTextField labelEditor;
  private JColorEditor colorEditor;
  private JTextField heightEditor;
  private JTextField signalHeightEditor;
  private JComboBox signalAlignmentEditor;

  boolean dialogResult = false;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditWaveformPropertiesDialog} instance.
   * 
   * @param aWindow
   *          the parent window for this dialog, can be <code>null</code>;
   * @param aElement
   *          the UI-element to edit the label for, cannot be <code>null</code>.
   */
  public EditWaveformPropertiesDialog( final Window aParent, final WaveformElement aElement )
  {
    super( aParent, ModalityType.APPLICATION_MODAL );

    this.defaultLabel = aElement.getLabel();
    this.defaultColor = aElement.getColor();
    this.defaultHeight = aElement.getHeight();

    this.defaultSignalHeight = aElement.getSignalHeight();
    this.defaultAlignment = aElement.getAlignment();

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
  private static String getTitle( final WaveformElement aElement )
  {
    String name = "";
    switch ( aElement.getType() )
    {
      case ANALOG_SCOPE:
        name = "scope";
        break;
      case CHANNEL:
        name = "channel";
        break;
      case GROUP:
        name = "signal group";
        break;
      case GROUP_SUMMARY:
        name = "group summary";
        break;
    }
    return "Edit " + name + " properties";
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
    try
    {
      return Integer.parseInt( this.heightEditor.getText() );
    }
    catch ( NumberFormatException exception )
    {
      return UIManager.getInt( UIMgr.CHANNEL_HEIGHT );
    }
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
  public Alignment getAlignment()
  {
    Alignment result = ( Alignment )this.signalAlignmentEditor.getSelectedItem();
    if ( result == null )
    {
      result = Alignment.valueOf( UIManager.getString( UIMgr.SIGNALVIEW_SIGNAL_ALIGNMENT ) );
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
    try
    {
      return Integer.parseInt( this.signalHeightEditor.getText() );
    }
    catch ( NumberFormatException exception )
    {
      return UIManager.getInt( UIMgr.DIGITAL_SIGNAL_HEIGHT );
    }
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

  protected boolean validateSettings()
  {
    String text = this.labelEditor.getText();
    if ( isNullOrEmpty( text ) )
    {
      return false;
    }

    text = this.heightEditor.getText();
    if ( isNullOrEmpty( text ) )
    {
      return false;
    }

    int height;
    try
    {
      height = Integer.valueOf( text );
    }
    catch ( NumberFormatException exception1 )
    {
      return false;
    }

    if ( height < 1 )
    {
      return false;
    }

    int signalHeight = -1;
    if ( this.signalHeightEditor.isVisible() )
    {
      text = this.signalHeightEditor.getText();
      if ( isNullOrEmpty( text ) )
      {
        return false;
      }
      try
      {
        signalHeight = Integer.valueOf( text );
      }
      catch ( NumberFormatException exception )
      {
        return false;
      }

      if ( signalHeight < 1 )
      {
        return false;
      }
    }

    return signalHeight < height;
  }

  private static boolean isNullOrEmpty( String text )
  {
    return text == null || "".equals( text.trim() );
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog( final WaveformElement aElement )
  {
    setTitle( getTitle( aElement ) );
    setLocationRelativeTo( getParent() );
    setResizable( false );

    final JButton okButton = StandardActionFactory.createOkButton();
    JButton cancelButton = StandardActionFactory.createCancelButton();

    final DocumentListener inputValidator = new DocumentListener()
    {
      @Override
      public void insertUpdate( DocumentEvent aEvent )
      {
        okButton.setEnabled( validateSettings() );
      }

      @Override
      public void removeUpdate( DocumentEvent aEvent )
      {
        okButton.setEnabled( validateSettings() );
      }

      @Override
      public void changedUpdate( DocumentEvent aEvent )
      {
        okButton.setEnabled( validateSettings() );
      }
    };

    JLabel labelEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Label" );
    this.labelEditor = new JTextField( aElement.getLabel(), 10 );
    this.labelEditor.getDocument().addDocumentListener( inputValidator );

    JLabel colorEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Color" );
    this.colorEditor = new JColorEditor( aElement.getColor() );

    JLabel heightEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Height" );
    this.heightEditor = new JTextField( "" + aElement.getHeight(), 10 );
    this.heightEditor.setInputVerifier( JComponentInputVerifier.create( Integer.class,
        "Invalid height! Must be a postive whole number." ) );
    this.heightEditor.getDocument().addDocumentListener( inputValidator );

    JLabel signalHeightEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Signal height" );
    this.signalHeightEditor = new JTextField( "" + this.defaultSignalHeight, 10 );
    this.signalHeightEditor.setInputVerifier( JComponentInputVerifier.create( Integer.class,
        "Invalid height! Must be a postive whole number." ) );
    this.signalHeightEditor.getDocument().addDocumentListener( inputValidator );

    JLabel signalAlignmentEditorLabel = SwingComponentUtils.createRightAlignedLabel( "Signal alignment" );
    this.signalAlignmentEditor = new JComboBox( Alignment.values() );
    this.signalAlignmentEditor.setRenderer( new AlignmentComboBoxRenderer() );
    this.signalAlignmentEditor.setSelectedItem( this.defaultAlignment );

    JButton resetButton = new JButton( "Reset to defaults" );
    resetButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        applyDefaultProperties();
      }
    } );

    JPanel resetButtonPanel = new JPanel();
    resetButtonPanel.add( resetButton, BorderLayout.LINE_END );

    JPanel editorPane = new JPanel( new SpringLayout() );
    editorPane.add( labelEditorLabel );
    editorPane.add( this.labelEditor );

    editorPane.add( colorEditorLabel );
    editorPane.add( this.colorEditor );

    editorPane.add( heightEditorLabel );
    editorPane.add( this.heightEditor );

    if ( CHANNEL.equals( aElement.getType() ) )
    {
      editorPane.add( signalHeightEditorLabel );
      editorPane.add( this.signalHeightEditor );

      editorPane.add( signalAlignmentEditorLabel );
      editorPane.add( this.signalAlignmentEditor );
    }
    else
    {
      // Flag them invisible for validation routine...
      this.signalHeightEditor.setVisible( false );
      this.signalAlignmentEditor.setVisible( false );
    }

    editorPane.add( new JLabel( "" ) );
    editorPane.add( resetButtonPanel );

    SpringLayoutUtils.makeEditorGrid( editorPane, 10, 10 );

    JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, cancelButton );

    SwingComponentUtils.setupWindowContentPane( this, editorPane, buttonPane, okButton );
  }
}
