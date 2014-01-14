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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;
import java.io.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "save as" functionality for projects.
 */
public class SaveAsAction extends SaveAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SaveAs";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SaveAsAction} instance.
   */
  public SaveAsAction()
  {
    super( ID, IconLocator.ICON_SAVE_PROJECT, "Save as ...", "Save the current project or data under a different name" );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_S, InputEvent.SHIFT_MASK ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_A ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 4 );
  }

  // METHODS

  @Override
  protected boolean showFileChooserDialogNeeded( File aFile )
  {
    // Always should the file chooser dialog...
    return true;
  }
}

/* EOF */
