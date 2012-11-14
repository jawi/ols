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
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a color editor; showing as a label that has the selected color as
 * foreground, and can be clicked (if not read-only) to change the color.
 */
public class JColorEditor extends JTextField
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
