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
package nl.lxtreme.ols.tool.base;


import java.awt.*;
import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * Provides a base tool dialog.
 */
public abstract class BaseToolDialog extends JDialog implements ToolDialog, Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  protected static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  protected static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // CONSTRUCTORS

  /**
   * Creates a new BaseToolDialog instance that is document modal.
   * 
   * @param aOwner
   *          the owning window of this dialog;
   * @param aName
   *          the name of this dialog.
   */
  protected BaseToolDialog( final Window aOwner, final String aTitle )
  {
    this( aOwner, aTitle, Dialog.ModalityType.DOCUMENT_MODAL );
  }

  /**
   * Creates a new BaseToolDialog instance that is document modal.
   * 
   * @param aOwner
   *          the owning window of this dialog;
   * @param aName
   *          the name of this dialog;
   * @param aModalityType
   *          the modality type.
   */
  protected BaseToolDialog( final Window aOwner, final String aTitle, final ModalityType aModalityType )
  {
    super( aOwner, aTitle, aModalityType );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#showDialog(nl.lxtreme.ols.api.data.DataContainer)
   */
  @Override
  public void showDialog( final DataContainer aData )
  {
    setVisible( true );
  }

  /**
   * Convenience method to create a close button that closes this dialog.
   * 
   * @return a button with a {@link CloseAction} instance, never
   *         <code>null</code>.
   */
  protected final JButton createCloseButton()
  {
    return StandardActionFactory.createCloseButton();
  }
}
