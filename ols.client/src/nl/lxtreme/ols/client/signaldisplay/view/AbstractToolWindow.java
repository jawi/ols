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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;


/**
 * Provides a common base class for tool windows.
 * <p>
 * One distinct feature of this class is that all components that are added
 * automatically receive a smaller font size.
 * </p>
 */
public abstract class AbstractToolWindow extends AbstractViewLayer implements IToolWindow
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String id;

  private final float fontSize;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AbstractToolWindow} instance.
   * 
   * @param aController
   *          the signal diagram controller to use, cannot be <code>null</code>.
   */
  public AbstractToolWindow( final String aID, final SignalDiagramController aController )
  {
    super( aController );

    this.id = aID;

    Font font = getFont();
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    this.fontSize = font.getSize2D() * 0.9f;
  }

  // METHODS

  /**
   * Updates the font size for all given components and all of their sub
   * components.
   * 
   * @param aFontSize
   *          the new font size to apply;
   * @param aComponents
   *          the component(s) to update.
   */
  private static void updateFontSize( final float aFontSize, final Component... aComponents )
  {
    for ( Component component : aComponents )
    {
      Font cFont = component.getFont();
      if ( cFont != null )
      {
        component.setFont( cFont.deriveFont( aFontSize ) );
      }

      if ( component instanceof Container )
      {
        updateFontSize( aFontSize, ( ( Container )component ).getComponents() );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon getIcon()
  {
    return null; // XXX
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final String getId()
  {
    return this.id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void addImpl( final Component aComp, final Object aConstraints, final int aIndex )
  {
    // Override the font settings...
    updateFontSize( this.fontSize, aComp );

    super.addImpl( aComp, aConstraints, aIndex );
  }
}
