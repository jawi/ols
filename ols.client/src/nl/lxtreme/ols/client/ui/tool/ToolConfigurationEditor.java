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
package nl.lxtreme.ols.client.ui.tool;


import java.awt.*;
import java.beans.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.*;

import org.osgi.service.metatype.*;


/**
 * Provides a generic editor for a tool configuration.
 */
final class ToolConfigurationEditor extends JDialog implements PropertyChangeListener, StatusAwareDialog
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ToolConfigPanel configPanel;
  private final JButton closeButton;
  private final JButton okButton;

  private volatile DialogStatus status;

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
    this.configPanel.addPropertyChangeListener( this );

    this.closeButton = StandardActionFactory.createCloseButton();
    this.okButton = StandardActionFactory.createOkButton();

    final JComponent buttonBar = SwingComponentUtils.createButtonPane( this.okButton, this.closeButton );

    SwingComponentUtils.setupDialogContentPane( this, this.configPanel, buttonBar, this.closeButton );
  }

  // METHODS

  /**
   * @return <code>true</code> if the settings of this tool are valid,
   *         <code>false</code> otherwise.
   */
  public boolean areSettingsValid()
  {
    return this.configPanel.areSettingsValid();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DialogStatus getDialogStatus()
  {
    return this.status;
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
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    // Update the state of the buttons to indicate what's wrong...
    this.closeButton.setEnabled( true );
    this.okButton.setEnabled( areSettingsValid() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setDialogStatus( final DialogStatus aStatus )
  {
    this.status = aStatus;
  }
}
