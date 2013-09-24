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
package nl.lxtreme.ols.client.about;


import java.awt.*;
import java.net.*;

import javax.swing.*;

import nl.lxtreme.ols.client.icons.*;


/**
 * Denotes the panel with the general information about this client.
 */
final class AboutPane extends JPanel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ABOUT_MESSAGE = //
  "<html><body><h3>%s</h3>" //
      + "<p>\u00A9 Copyright 2006-2010 Michael Poppitz<br>" //
      + "\u00A9 Copyright 2010-2013 J.W. Janssen<br><br></p>" //
      + "<p>This software is released under the GNU GPLv2 license.<br><br></p>" //
      + "<p>Version: %s<br><br></p>" //
      + "<p>For more information see:</p>" //
      + "<ul>" //
      + "<li><a href='http://ols.lxtreme.nl/'>http://ols.lxtreme.nl</a>;</li>" //
      + "<li><a href='https://github.com/jawi/ols/wiki/FAQ'>https://github.com/jawi/ols/wiki/FAQ</a>;</li>" //
      + "<li><a href='http://dangerousprototypes.com/open-logic-sniffer'>http://dangerousprototypes.com/open-logic-sniffer</a>;</li>" //
      + "<li><a href='http://www.gadgetfactory.net/gf/project/butterflylogic'>http://www.gadgetfactory.net/gf/project/butterflylogic</a>;</li>" //
      + "<li><a href='http://www.sump.org/projects/analyzer'>http://www.sump.org/projects/analyzer</a>.</li>" //
      + "</ul></body></html>";

  // CONSTRUCTORS

  /**
   * Creates a new {@link AboutPane} instance.
   */
  private AboutPane()
  {
    super( new GridBagLayout() );
  }

  // METHODS

  /**
   * Factory method for creating an instance of this {@link AboutPane}.
   * 
   * @return a new {@link AboutPane} instance, never <code>null</code>.
   */
  public static AboutPane create( final String aName, final String aVersion )
  {
    AboutPane result = new AboutPane();
    result.buildPanel( aName, aVersion );
    return result;
  }

  /**
   * Builds this panel.
   * 
   * @param aName
   *          the name to display;
   * @param aVersion
   *          the version to display.
   */
  void buildPanel( final String aName, final String aVersion )
  {
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    final URL url = IconLocator.class.getResource( IconLocator.LOGO );
    final JLabel iconLabel = new JLabel( new ImageIcon( url ) );
    iconLabel.setBackground( Color.WHITE );

    final JLabel aboutMsg = new JLabel( String.format( ABOUT_MESSAGE, aName, aVersion ) );

    add( iconLabel, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, //
            new Insets( 0, 0, 5, 0 ), 0, 0 ) );

    add( aboutMsg, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, //
            new Insets( 5, 10, 5, 10 ), 0, 0 ) );
  }
}
