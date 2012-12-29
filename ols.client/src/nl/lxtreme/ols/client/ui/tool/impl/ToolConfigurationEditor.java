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
package nl.lxtreme.ols.client.ui.tool.impl;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.client.ui.editor.*;

import org.osgi.service.metatype.*;


/**
 * Provides a generic editor for a tool configuration.
 */
final class ToolConfigurationEditor extends BaseConfigurationEditor
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String pid;
  private final AcquisitionDataInfo acquisitionDataInfo;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolConfigurationEditor} instance.
   */
  public ToolConfigurationEditor( final Window aParent, final String aTitle, final String aPID,
      final AcquisitionDataInfo aAcquisitionDataInfo )
  {
    super( aParent, aTitle );

    this.pid = aPID;
    this.acquisitionDataInfo = aAcquisitionDataInfo;
  }

  // METHODS

  /**
   * Factory method for creating a new {@link ToolConfigurationEditor} dialog.
   * 
   * @param aParent
   *          the parent window to use;
   * @param aOCD
   *          the object-class definition to use;
   * @param aSettings
   *          the (initial) settings to use;
   * @param aDataInfo
   *          the acquisition data information to use.
   * @return a new {@link ToolConfigurationEditor} instance, never
   *         <code>null</code>.
   */
  public static ToolConfigurationEditor create( final Window aParent, final ObjectClassDefinition aOCD,
      final Map<Object, Object> aSettings, final AcquisitionDataInfo aDataInfo )
  {
    ToolConfigurationEditor result = new ToolConfigurationEditor( aParent, aOCD.getName(), aOCD.getID(), aDataInfo );
    result.initDialog( aOCD, aSettings );
    return result;
  }

  /**
   * Returns the current value of pid.
   * 
   * @return the pid
   */
  public String getPid()
  {
    return this.pid;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected EditorPanel createEditorPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aSettings )
  {
    return ToolConfigPanel.create( aOCD, aSettings, this.acquisitionDataInfo );
  }
}
