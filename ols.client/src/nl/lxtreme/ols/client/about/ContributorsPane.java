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

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Denotes the panel with the information about the contributors of this client.
 */
final class ContributorsPane extends JPanel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String[][] CONTRIBUTORS = { //
  { "Dangerous Prototypes", "Providing the forum and OLS hardware" }, //
      { "Michael Poppitz", "Provided the initial basis of this client" }, //
      { "Mario Schrenk", "Provided the JTAG decoder tool" }, //
      { "Ansgar Kueckes", "Provided the ASM45 decoder tool" }, //
      { "Alan Burlison", "Tested support for this client on Solaris" }, //
      { "Piotr Pawluczuk", "Provided the current artwork/icons" }, //
      { "Anneke Dijkstra", "Provided the main logo" }, //
  // { "Marius Jonsson", "UI/User interaction design" }, //
  };

  // CONSTRUCTORS

  /**
   * Creates a new {@link ContributorsPane} instance.
   */
  private ContributorsPane()
  {
    super( new BorderLayout() );
  }

  // METHODS

  /**
   * Factory method for creating a new {@link ContributorsPane} instance.
   * 
   * @return a new {@link ContributorsPane} instance, never <code>null</code>.
   */
  public static ContributorsPane create()
  {
    final ContributorsPane result = new ContributorsPane();
    result.buildPanel();
    return result;
  }

  /**
   * Builds this panel.
   */
  private void buildPanel()
  {
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    JPanel contributorsPane = new JPanel( new SpringLayout() );
    for ( String[] contributor : CONTRIBUTORS )
    {
      contributorsPane.add( new JLabel( contributor[0] ) );
      contributorsPane.add( new JLabel( contributor[1] ) );
    }
    SpringLayoutUtils.makeCompactGrid( contributorsPane, CONTRIBUTORS.length, 2, 0, 0, 10, 6 );

    JLabel intro = new JLabel(
        "<html><body><p>People who have contributed to this client (in no particular order):</p></body></html>" );
    JLabel outro = new JLabel(
        "<html><body><p>... and of course, all people who contributed by issuing bug reports,<br>" + //
            "testing public beta's or otherwise participated in the development<br>of this client.</p></body></html>" );

    JPanel contentPane = new JPanel( new GridBagLayout() );

    contentPane.add( intro, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, //
            new Insets( 12, 0, 12, 0 ), 0, 0 ) );
    contentPane.add( contributorsPane, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, //
            new Insets( 12, 0, 12, 0 ), 0, 0 ) );
    contentPane.add( outro, //
        new GridBagConstraints( 0, 2, 1, 1, 1.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, //
            new Insets( 6, 0, 12, 0 ), 0, 0 ) );

    add( contentPane, BorderLayout.NORTH );
  }
}
