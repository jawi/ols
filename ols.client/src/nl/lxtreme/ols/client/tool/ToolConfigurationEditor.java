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
package nl.lxtreme.ols.client.tool;


import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;

import org.osgi.service.metatype.*;


/**
 * Provides a generic editor for a tool configuration.
 */
final class ToolConfigurationEditor extends JDialog implements StatusAwareCloseableDialog
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ToolConfigPanel configPanel;
  private DialogStatus status;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolConfigurationEditor} instance.
   */
  public ToolConfigurationEditor( final Window aParent, final ObjectClassDefinition aOCD,
      final AcquisitionDataInfo aDataInfo, final Map<Object, Object> aSettings )
  {
    super( aParent, ModalityType.APPLICATION_MODAL );

    setTitle( aOCD.getName() );

    // Avoid the dialog to be resized automatically...
    getRootPane().putClientProperty( "unmanaged", Boolean.TRUE );

    this.configPanel = new ToolConfigPanel( aOCD, aDataInfo, aSettings );

    final JButton closeButton = StandardActionFactory.createCloseButton();
    final JButton okButton = StandardActionFactory.createOkButton();

    final JComponent buttonBar = SwingComponentUtils.createButtonPane( okButton, closeButton );

    SwingComponentUtils.setupDialogContentPane( this, this.configPanel, buttonBar, closeButton );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * @return the configuration properties, never <code>null</code>.
   */
  public Dictionary<Object, Object> getProperties()
  {
    return this.configPanel.getProperties();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean setDialogStatus( final DialogStatus aStatus )
  {
    if ( ( aStatus == DialogStatus.OK ) && !this.configPanel.areSettingsValid() )
    {
      return false;
    }
    this.status = aStatus;
    return true;
  }

  /**
   * Shows the dialog and waits until it is closed.
   * 
   * @return <code>true</code> if the dialog was closed with "Ok",
   *         <code>false</code> otherwise.
   */
  public boolean showDialog()
  {
    setVisible( true );
    return this.status == DialogStatus.OK;
  }
}
