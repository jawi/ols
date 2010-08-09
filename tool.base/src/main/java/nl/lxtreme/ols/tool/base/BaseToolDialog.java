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
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;


/**
 * @author jawi
 */
public abstract class BaseToolDialog extends JDialog implements ToolDialog, Configurable
{
  // INNER TYPES

  /**
   * 
   */
  protected final class CloseAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * 
     */
    public CloseAction()
    {
      super( "Close" );
      putValue( SHORT_DESCRIPTION, "Closes this dialog" );
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      close();
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  protected static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  protected static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // VARIABLES

  private transient volatile AnnotatedData analysisData;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  protected BaseToolDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName, Dialog.ModalityType.DOCUMENT_MODAL );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#showDialog(nl.lxtreme.ols.api.data.AnnotatedData)
   */
  @Override
  public boolean showDialog( final AnnotatedData aData )
  {
    this.analysisData = aData;

    setVisible( true );

    // always return true...
    return true;
  }

  /**
   * Closes this dialog.
   */
  protected void close()
  {
    setVisible( false );
  }

  /**
   * @return the analysisData
   */
  protected final AnnotatedData getAnalysisData()
  {
    return this.analysisData;
  }
}
