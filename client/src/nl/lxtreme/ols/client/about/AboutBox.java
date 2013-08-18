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
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.*;


/**
 * Provides an about box dialog.
 */
public class AboutBox extends JDialog implements Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new AboutBox instance.
   */
  public AboutBox( final String aName, final String aVersion )
  {
    super( SwingComponentUtils.getCurrentWindow(), "About " + aName, ModalityType.APPLICATION_MODAL );

    final JTabbedPane tabbedPane = new JTabbedPane();
    tabbedPane.add( "About", AboutPane.create( aName, aVersion ) );
    tabbedPane.add( "Contributors", ContributorsPane.create() );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( StandardActionFactory.createCloseButton() );

    final JPanel contentPane = new JPanel( new GridBagLayout() );

    contentPane.add( tabbedPane, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, //
            new Insets( 0, 0, 5, 0 ), 0, 0 ) );
    contentPane.add( buttonPane, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 0.0, GridBagConstraints.SOUTH, GridBagConstraints.HORIZONTAL,
            new Insets( 0, 0, 5, 0 ), 0, 0 ) );

    setContentPane( contentPane );

    pack();

    setLocationRelativeTo( getOwner() );
    setResizable( false );
  }

  // METHODS

  /**
   * Closes this dialog and disposes it.
   */
  public final void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * @see java.awt.Dialog#show()
   */
  public void showDialog()
  {
    setVisible( true );
  }
}
