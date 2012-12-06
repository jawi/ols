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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.editor;


import static javax.swing.SwingUtilities.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import org.osgi.service.metatype.*;


/**
 * Provides an input verifier for editor fields of a certain type.
 */
public class EditorInputVerifier extends InputVerifier
{
  // INNER TYPES

  protected static final class FadingBackgroundAnimator extends Timer implements ActionListener
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    private static final int DEFAULT_DELAY = 35;
    private static final int DEFAULT_FRAMES = 12;

    // VARIABLES

    private final Color color;
    private final Color originalBG;
    private final JComponent component;
    private final long startTime; // in nanos
    private final long animTime; // in nanos

    // CONSTRUCTORS

    /**
     * Creates a new {@link FadingBackgroundAnimator} instance.
     * 
     * @param aComponent
     *          the component to animate the background for, cannot be
     *          <code>null</code>.
     */
    public FadingBackgroundAnimator( final JComponent aComponent )
    {
      this( aComponent, DEFAULT_DELAY, DEFAULT_FRAMES );
    }

    /**
     * Creates a new {@link FadingBackgroundAnimator} instance.
     * 
     * @param aComponent
     *          the component to animate the background for, cannot be
     *          <code>null</code>;
     * @param aDelay
     *          the delay (in ms) to use;
     * @param aMaxFrames
     *          the maximum number of frames to use, > 0.
     */
    public FadingBackgroundAnimator( final JComponent aComponent, final int aDelay, final int aMaxFrames )
    {
      super( aDelay, null );

      this.component = aComponent;
      this.color = new Color( 0xff, 0x66, 0x66 ); // "salmon"
      this.originalBG = aComponent.getBackground();

      this.startTime = System.nanoTime();
      this.animTime = aDelay * aMaxFrames * 1000000; // convert to nanos

      addActionListener( this );

      setInitialDelay( 0 );
      start();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final Timer source = ( Timer )aEvent.getSource();
      final long relTime = System.nanoTime() - this.startTime;

      if ( relTime >= this.animTime )
      {
        source.stop();

        this.component.setBackground( this.originalBG );
      }
      else
      {
        // Use a spline to interpolate the color used in this animation; the
        // generic spline equation is:
        // x = b0*x0 + b1*x1 + b2*x2 + b3*x3
        // y = b0*y0 + b1*y1 + b2*y2 + b3*y3
        // in which (x0,y0) = (0,0) and (x3,y3) = (1,1) for our splines, so
        // this simplifies to:
        // x = b1*x1 + b2*x2 + b3
        // y = b1*x1 + b2*x2 + b3
        // We use factors of (x1,y1) = (1,0) and (x2,y2) = (1,0) to get a
        // kind of inverse exponential function...
        float t = relTime / ( float )this.animTime;
        float invT = 1.0f - t;

        float b1 = 3.0f * t * invT * invT;
        float b2 = 3.0f * t * t * invT;
        float b3 = t * t * t;

        float delta = ( b1 * 1.0f ) + ( b2 * 1.0f ) + b3;

        this.component.setBackground( interpolate( this.color, this.originalBG, delta ) );
      }
    }

    /**
     * Interpolates an intermediate color between two given colors.
     * 
     * @param aColor1
     *          the first color;
     * @param aColor2
     *          the second color;
     * @param aDelta
     *          the "delta" in which to determine the resulting color, &gt;= 0
     *          && &lt;=1.0f.
     * @return an intermediate color, never <code>null</code>.
     */
    private Color interpolate( final Color aColor1, final Color aColor2, final float aDelta )
    {
      float[] acomp = aColor2.getRGBComponents( null );
      float[] bcomp = aColor1.getRGBComponents( null );
      float[] ccomp = new float[4];

      for ( int i = 0; i < 4; i++ )
      {
        ccomp[i] = acomp[i] + ( ( bcomp[i] - acomp[i] ) * ( 1.0f - aDelta ) );
      }

      return new Color( ccomp[0], ccomp[1], ccomp[2], ccomp[3] );
    }
  }

  // CONSTANTS

  /**
   * The client property used to store the last valid input of a component so we
   * can revert back to it in case the current input isn't valid.
   */
  protected static final String PROPERTY_LAST_VALID_INPUT = "lastValidInput";

  // VARIABLES

  private final AttributeDefinition attributeDef;

  private FadingBackgroundAnimator animator;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditorInputVerifier} instance.
   * 
   * @param aAttributeDef
   *          the attribute definition to get the metadata from, cannot be
   *          <code>null</code>.
   */
  private EditorInputVerifier( final JComponent aComponent, final AttributeDefinition aAttributeDef )
  {
    this.attributeDef = aAttributeDef;

    // Put the initial value as "last" valid input...
    aComponent.putClientProperty( PROPERTY_LAST_VALID_INPUT, getInputValue( aComponent ) );
  }

  // METHODS

  /**
   * Installs this {@link EditorInputVerifier} as input verifier for the given
   * component.
   * 
   * @param aComponent
   *          the component for which to install the input verifier;
   * @param aAttributeDef
   *          the attribute definition to use for the input verifier.
   */
  public static void install( final JComponent aComponent, final AttributeDefinition aAttributeDef )
  {
    aComponent.setInputVerifier( new EditorInputVerifier( aComponent, aAttributeDef ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean shouldYieldFocus( final JComponent aInput )
  {
    // By default, any invalid components keep the focus until they are valid.
    // Instead, we want to restore the original value of the component and let
    // it loose its focus...
    if ( !verify( aInput ) )
    {
      // Restore former value...
      Object lastValidValue = aInput.getClientProperty( PROPERTY_LAST_VALID_INPUT );
      setInputValue( aInput, lastValidValue );

      if ( ( this.animator != null ) && this.animator.isRunning() )
      {
        this.animator.stop();
      }
      this.animator = new FadingBackgroundAnimator( aInput );

      // Give an audible feedback to denote we're reverting the input of the
      // user to something else...
      Toolkit.getDefaultToolkit().beep();
    }
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean verify( final JComponent aInput )
  {
    Object rawValue = getInputValue( aInput );
    String value = ( rawValue == null ) ? null : String.valueOf( rawValue );

    String validationResult = this.attributeDef.validate( value );
    if ( ( validationResult == null ) || "".equals( validationResult ) )
    {
      // Update the last valid input property to return to when validation fails
      // later on...
      aInput.putClientProperty( PROPERTY_LAST_VALID_INPUT, rawValue );
      return true;
    }

    return false;
  }

  /**
   * Tries to obtain the raw value of the given component.
   * 
   * @param aComponent
   *          the (Swing) component to get the value of, can be
   *          <code>null</code>.
   * @return the component's value, or <code>null</code> if its value is
   *         unknown.
   */
  protected final Object getInputValue( final JComponent aComponent )
  {
    Object result = null;
    if ( aComponent instanceof JComboBox )
    {
      result = ( ( JComboBox )aComponent ).getSelectedItem();
    }
    else if ( ( aComponent instanceof JTextField ) && noSpinnerOrComboBoxTextField( aComponent ) )
    {
      result = ( ( JTextField )aComponent ).getText();
    }
    else if ( aComponent instanceof JSpinner )
    {
      result = ( ( JSpinner )aComponent ).getValue();
    }
    else if ( aComponent instanceof JSlider )
    {
      result = Integer.toString( ( ( JSlider )aComponent ).getValue() );
    }
    else if ( aComponent instanceof JCheckBox )
    {
      result = Boolean.toString( ( ( JCheckBox )aComponent ).isSelected() );
    }
    return result;
  }

  /**
   * Tries to set the raw value of the given component.
   * 
   * @param aComponent
   *          the (Swing) component to get the value of, can be
   *          <code>null</code>;
   * @param aValue
   *          the value to set, can be <code>null</code>.
   */
  protected final void setInputValue( final JComponent aComponent, final Object aValue )
  {
    if ( aComponent instanceof JComboBox )
    {
      ( ( JComboBox )aComponent ).setSelectedItem( aValue );
    }
    else if ( ( aComponent instanceof JTextField ) && noSpinnerOrComboBoxTextField( aComponent ) )
    {
      ( ( JTextField )aComponent ).setText( ( aValue == null ) ? "" : String.valueOf( aValue ) );
    }
    else if ( aComponent instanceof JSpinner )
    {
      ( ( JSpinner )aComponent ).setValue( aValue );
    }
    else if ( aComponent instanceof JSlider )
    {
      final JSlider slider = ( JSlider )aComponent;

      int value = slider.getMinimum();
      if ( aValue != null )
      {
        if ( aValue instanceof Number )
        {
          value = ( ( Number )aValue ).intValue();
        }
        else
        {
          value = Integer.parseInt( String.valueOf( aValue ) );
        }
      }

      slider.setValue( value );
    }
    else if ( aComponent instanceof JCheckBox )
    {
      ( ( JCheckBox )aComponent ).setSelected( Boolean.parseBoolean( String.valueOf( aValue ) ) );
    }
  }

  /**
   * Tests whether the given field is actually the text field of a combobox or a
   * spinner component.
   * 
   * @param aComponent
   *          the component to test, can be <code>null</code>.
   * @return <code>true</code> if the given component is a "real" text field not
   *         part of a combobox or spinner, <code>false</code> otherwise.
   */
  private boolean noSpinnerOrComboBoxTextField( final JComponent aComponent )
  {
    return ( getAncestorOfClass( JComboBox.class, aComponent ) == null )
        && ( getAncestorOfClass( JSpinner.class, aComponent ) == null );
  }
}
