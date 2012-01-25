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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an action to edit the label of a signal element.
 */
public class EditSignalElementLabelAction extends AbstractAction
{
  // INNER TYPES

  /**
   * Provides a Swing dialog for editing a signal element's label.
   */
  static final class EditLabelDialog extends JDialog
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private JTextField labelEditor;

    boolean dialogResult = false;

    // CONSTRUCTORS

    /**
     * Creates a new {@link EditLabelDialog} instance.
     * 
     * @param aWindow
     *          the parent window for this dialog, can be <code>null</code>;
     * @param aSignalElement
     *          the signal element to edit the label for, cannot be
     *          <code>null</code>.
     */
    public EditLabelDialog( final Window aParent, final SignalElement aSignalElement )
    {
      super( aParent, ModalityType.DOCUMENT_MODAL );

      initDialog( aSignalElement );
    }

    /**
     * Determines the label shown in this dialog based on the given signal
     * element.
     * 
     * @param aSignalElement
     *          the signal element to determine the title for, cannot be
     *          <code>null</code>.
     * @return a label text, never <code>null</code>.
     */
    private static String getLabel( final SignalElement aSignalElement )
    {
      if ( aSignalElement.isDigitalSignal() )
      {
        Channel channel = aSignalElement.getChannel();
        return "Channel label " + channel.getIndex();
      }
      else if ( aSignalElement.isAnalogSignal() )
      {
        return "Analog signal";
      }

      return "Group summary";
    }

    /**
     * Determines the title for this dialog based on the given signal element.
     * 
     * @param aSignalElement
     *          the signal element to determine the title for, cannot be
     *          <code>null</code>.
     * @return a title, never <code>null</code>.
     */
    private static String getTitle( final SignalElement aSignalElement )
    {
      if ( aSignalElement.isDigitalSignal() )
      {
        Channel channel = aSignalElement.getChannel();
        return "Edit label channel " + channel.getIndex();
      }
      else if ( aSignalElement.isAnalogSignal() )
      {
        return "Edit scope label";
      }

      return "Edit group summary label";
    }

    /**
     * Returns the new channel label.
     * 
     * @return the channel label, can be <code>null</code>.
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
    private void initDialog( final SignalElement aSignalElement )
    {
      setTitle( getTitle( aSignalElement ) );

      JLabel label = new JLabel( getLabel( aSignalElement ) );
      this.labelEditor = new JTextField( aSignalElement.getLabel(), 10 );

      final JButton okButton = new JButton( "Ok" );
      okButton.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          EditLabelDialog.this.dialogResult = true;
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
          EditLabelDialog.this.dialogResult = false;
          setVisible( false );
          dispose();
        }
      } );

      // XXX use SpringLayout instead...
      JPanel editorPane = new JPanel( new GridLayout( 1, 2 ) );
      editorPane.add( label );
      editorPane.add( this.labelEditor );

      // XXX use button factory instead...
      JPanel buttonPane = new JPanel( new GridLayout( 1, 3 ) );
      buttonPane.add( new JLabel( " " ) );
      buttonPane.add( okButton );
      buttonPane.add( cancelButton );

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

  private final SignalElement signalElement;
  private final Point dialogLocation;
  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditSignalElementLabelAction} instance.
   * 
   * @param aController
   *          the controller to use;
   * @param aSignalElement
   *          the signal element to edit the label for;
   * @param aChannelLocation
   *          the location on screen of the channel to edit the label for.
   */
  public EditSignalElementLabelAction( final SignalDiagramController aController, final SignalElement aSignalElement,
      final Point aChannelLocation )
  {
    super( "Edit label" );
    this.controller = aController;
    this.signalElement = aSignalElement;
    this.dialogLocation = new Point( aChannelLocation.x + 15, aChannelLocation.y + 5 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    EditLabelDialog dialog = new EditLabelDialog( SwingComponentUtils.getOwningWindow( aEvent ), this.signalElement );
    dialog.setLocation( this.dialogLocation );

    if ( dialog.showDialog() )
    {
      this.signalElement.setLabel( dialog.getLabel() );

      // Since the entire layout can be mixed up by the new label, we should
      // repaint the entire screen...
      this.controller.recalculateDimensions();
    }
  }
}
