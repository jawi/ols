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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing.validation;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * @author jawi
 */
public class JComponentInputVerifier extends InputVerifier implements KeyListener
{
  // INNER TYPES

  /**
   * Provides a default validator that always returns <code>true</code>.
   */
  private static final class DefaultValidator implements IValidator
  {
    // METHODS

    /**
     * @see nl.lxtreme.ols.util.swing.validation.IValidator#validate(java.lang.Object)
     */
    @Override
    public boolean validate( final Object aValue )
    {
      return true;
    }
  }

  // CONSTANTS

  private static final String DEFAULT_MESSAGE = "Input invalid!";

  private static final byte[] ERROR_GIF_BYTES = { 71, 73, 70, 56, 57, 97, 16, 0, 16, 0, -77, 0, 0, -1, 127, 63, -8, 88,
      56, -1, 95, 63, -8, 56, 56, -33, 63, 63, -65, 63, 63, -104, 56, 56, 127, 63, 63, -1, -65, -65, -97, 127, 127, -1,
      -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33, -7, 4, 1, 0, 0, 11, 0, 44, 0, 0, 0, 0, 16, 0, 16, 0,
      0, 4, 84, 112, -55, 73, -85, -67, 120, -91, -62, 75, -54, -59, 32, 14, 68, 97, 92, 33, -96, 8, 65, -96, -104, 85,
      50, 0, 0, -94, 12, 10, 82, 126, 83, 26, -32, 57, 18, -84, 55, 96, 1, 69, -91, 3, 37, -12, -77, -35, -124, 74, 98,
      -64, 54, -96, -106, 78, -109, 4, 1, 55, 66, 32, 76, -68, -119, -127, 64, 46, -101, -94, 21, 67, -121, 99, 64, 91,
      18, -19, -125, 33, -100, -87, -37, 41, 17, 0, 59 };
  private static final ImageIcon ERROR_ICON = new ImageIcon( ERROR_GIF_BYTES );

  // VARIABLES

  private final String message;
  private final IValidator validator;
  private JDialog popup;

  // CONSTRUCTORS

  /**
   * Creates a new AbstractValidator instance.
   * 
   * @param aValidator
   *          the validator to use for verifying the input.
   */
  public JComponentInputVerifier( final IValidator aValidator )
  {
    this( DEFAULT_MESSAGE, aValidator );
  }

  /**
   * Creates a new AbstractValidator instance.
   * 
   * @param aMessage
   *          the message to use when validation fails;
   * @param aValidator
   *          the validator to use for verifying the input.
   */
  public JComponentInputVerifier( final String aMessage, final IValidator aValidator )
  {
    super();

    this.message = aMessage;
    this.validator = aValidator;
  }

  // METHODS

  /**
   * Creates an instance of this InputVerifier for the given type.
   * 
   * @param aType
   *          the type to create an input verifier for, cannot be
   *          <code>null</code>.
   * @return an input verifier instance, never <code>null</code>.
   */
  public static JComponentInputVerifier create( final Class<?> aType )
  {
    return create( aType, DEFAULT_MESSAGE );
  }

  /**
   * Creates an instance of this InputVerifier for the given type.
   * 
   * @param aType
   *          the type to create an input verifier for, cannot be
   *          <code>null</code>;
   * @param aMessage
   *          the message to display in case the verification failed.
   * @return an input verifier instance, never <code>null</code>.
   */
  public static JComponentInputVerifier create( final Class<?> aType, final String aMessage )
  {
    IValidator validator;
    if ( NumberValidator.isNumericType( aType ) )
    {
      validator = new NumberValidator( aType );
    }
    else
    {
      validator = new DefaultValidator();
    }
    return new JComponentInputVerifier( aMessage, validator );
  }

  /**
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public final void keyPressed( final KeyEvent aEvent )
  {
    ( ( Component )aEvent.getSource() ).removeKeyListener( this );

    if ( this.popup != null )
    {
      this.popup.setVisible( false );
      this.popup.dispose();
      this.popup = null;
    }
  }

  /**
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public final void keyReleased( final KeyEvent aEvent )
  {
    ( ( Component )aEvent.getSource() ).removeKeyListener( this );
  }

  /**
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public final void keyTyped( final KeyEvent aEvent )
  {
    ( ( Component )aEvent.getSource() ).removeKeyListener( this );
  }

  /**
   * @see javax.swing.InputVerifier#verify(javax.swing.JComponent)
   */
  @Override
  public final boolean verify( final JComponent aInput )
  {
    final Object value = getInputValue( aInput );
    if ( value == null )
    {
      // Unknown/empty values are assumed to be valid...
      return true;
    }

    boolean result = this.validator.validate( value );
    if ( !result )
    {
      if ( ( this.popup != null ) && ( this.popup.getOwner() != SwingComponentUtils.getOwningWindow( aInput ) ) )
      {
        this.popup.setVisible( false );
        this.popup.dispose();
        this.popup = null;
      }
      if ( this.popup == null )
      {
        this.popup = createMessagePopup( aInput );
      }

      // Allow the popup to be dismissed when a key is pressed...
      aInput.addKeyListener( this );

      // Signal that the input is incorrect...
      aInput.setBackground( Color.PINK );

      final Point point = aInput.getLocationOnScreen();
      final Dimension componentSize = aInput.getSize();

      this.popup.setSize( 0, 0 );
      this.popup.setLocation( point.x + ( ( int )componentSize.getWidth() / 2 ),
          point.y + ( ( int )componentSize.getHeight() / 2 ) );

      this.popup.pack();
      this.popup.setVisible( true );
    }
    else
    {
      aInput.setBackground( Color.WHITE );
    }

    return result;
  }

  /**
   * Tries to obtain the actual value of a given component.
   * 
   * @param aComponent
   *          the (Swing) component to get the value of, can be
   *          <code>null</code>.
   * @return the component's value, or <code>null</code> if this value is
   *         unknown.
   */
  @SuppressWarnings( "boxing" )
  protected Object getInputValue( final Component aComponent )
  {
    if ( aComponent instanceof JComboBox )
    {
      return ( ( JComboBox )aComponent ).getSelectedItem();
    }
    else if ( aComponent instanceof JTextField )
    {
      if ( ( SwingUtilities.getAncestorOfClass( JComboBox.class, aComponent ) == null )
          && ( SwingUtilities.getAncestorOfClass( JSpinner.class, aComponent ) == null ) )
      {
        return ( ( JTextField )aComponent ).getText();
      }
    }
    else if ( aComponent instanceof JSlider )
    {
      return ( ( JSlider )aComponent ).getValue();
    }
    else if ( aComponent instanceof JCheckBox )
    {
      return ( ( JCheckBox )aComponent ).isSelected();
    }
    return null;
  }

  /**
   * Returns the message to use when validation fails.
   * 
   * @return a message, might be <code>null</code>.
   */
  protected String getMessage()
  {
    return this.message;
  }

  /**
   * Creates a new message popup.
   * 
   * @return the error message popup, never <code>null</code>.
   */
  private JDialog createMessagePopup( final JComponent aComponent )
  {
    String message = getMessage();
    if ( ( message == null ) || "".equals( message.trim() ) )
    {
      message = DEFAULT_MESSAGE;
    }

    final JDialog result = new JDialog( SwingComponentUtils.getOwningWindow( aComponent ) );
    result.setFocusableWindowState( false );
    result.setUndecorated( true );

    final Container contentPane = result.getContentPane();
    contentPane.setLayout( new FlowLayout() );
    contentPane.add( new JLabel( ERROR_ICON ) );
    contentPane.add( new JLabel( message ) );

    return result;
  }

}
