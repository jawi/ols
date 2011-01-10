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
package nl.lxtreme.ols.util.swing.validation;


import javax.swing.*;


/**
 * @author jawi
 */
public final class NumberValidator extends AbstractValidator
{
  // VARIABLES

  private final Class<? extends Number> type;

  // CONSTRUCTORS

  /**
   * @param aMessage
   */
  public NumberValidator( final String aMessage )
  {
    this( aMessage, Integer.TYPE );
  }

  /**
   * @param aMessage
   */
  public NumberValidator( final String aMessage, final Class<? extends Number> aType )
  {
    super( aMessage );
    this.type = aType;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.validation.AbstractValidator#doVerify(javax.swing.JComponent)
   */
  @Override
  protected boolean doVerify( final JComponent aInput )
  {
    if ( aInput instanceof JTextField )
    {
      final String inputText = ( ( JTextField )aInput ).getText();
      if ( ( inputText == null ) || inputText.trim().isEmpty() )
      {
        // Nothing given, this is allowed...
        return true;
      }

      if ( parse( inputText.trim() ) != null )
      {
        // A valid number was typed!
        return true;
      }
    }
    return false;
  }

  /**
   * Parses the given input to a number, if possible.
   * 
   * @param aInputText
   *          the text to parse as number, cannot be <code>null</code>.
   * @return the parsed number, can be <code>null</code> in case the given text
   *         could not be parsed.
   */
  protected final Number parse( final String aInputText )
  {
    try
    {
      final Number value;
      if ( Long.TYPE == this.type )
      {
        value = Long.parseLong( aInputText );
      }
      else if ( Short.TYPE == this.type )
      {
        value = Short.parseShort( aInputText );
      }
      else
      // if ( Integer.TYPE == this.type )
      {
        value = Integer.parseInt( aInputText );
      }

      return value;
    }
    catch ( NumberFormatException exception )
    {
      return null;
    }
  }
}
