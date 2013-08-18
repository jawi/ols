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
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;

import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "run"-action for any analysis/decoder tool.
 */
public class RunToolAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "RunTool.";

  // VARIABLES

  private final String toolName;
  private final ToolCategory category;

  // CONSTRUCTORS

  /**
   * Creates a new RunAnalysisToolAction instance.
   * 
   * @param aController
   *          the controller to use for this action;
   * @param aToolName
   *          the name of the tool to invoke in this action;
   * @param aCategory
   *          the category of the tool.
   */
  public RunToolAction( final ClientController aController, final String aToolName, final ToolCategory aCategory )
  {
    super( getID( aToolName ), aController, aToolName, "Run ".concat( aToolName ) );

    this.toolName = aToolName;
    this.category = aCategory;
  }

  // METHODS

  /**
   * Returns the action identifier for the given tool name.
   * 
   * @param aToolName
   *          a tool name, cannot be <code>null</code>.
   * @return a tool action identifier, never <code>null</code>.
   */
  public static final String getID( final String aToolName )
  {
    if ( aToolName == null )
    {
      throw new IllegalArgumentException( "Tool name cannot be null!" );
    }
    return ID.concat( aToolName );
  }

  /**
   * Returns the action identifier for the given tool.
   * 
   * @param aTool
   *          a tool instance, cannot be <code>null</code>.
   * @return a tool action identifier, never <code>null</code>.
   */
  public static final String getID( final Tool<?> aTool )
  {
    if ( aTool == null )
    {
      throw new IllegalArgumentException( "Tool cannot be null!" );
    }
    return getID( aTool.getName() );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );
    getController().invokeTool( this.toolName, owner );
  }

  /**
   * Returns the current value of category.
   * 
   * @return the tool category, never <code>null</code>.
   */
  public ToolCategory getCategory()
  {
    return this.category;
  }
}

/* EOF */
