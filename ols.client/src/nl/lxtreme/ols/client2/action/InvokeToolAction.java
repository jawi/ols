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
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Provides a "run"-action for any analysis/decoder tool.
 */
public class InvokeToolAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "RunTool.";

  private static final Logger LOG = Logger.getLogger( InvokeToolAction.class.getName() );

  // VARIABLES

  private final Tool<?> tool;

  // CONSTRUCTORS

  /**
   * Creates a new {@link InvokeToolAction} instance.
   * 
   * @param aTool
   *          the tool to invoke in this action;
   */
  public InvokeToolAction( Tool<?> aTool )
  {
    super( getID( aTool ) );

    this.tool = aTool;

    String toolName = aTool.getName();

    putValue( NAME, toolName );
    putValue( SHORT_DESCRIPTION, "Invoke ".concat( toolName ) );
    putValue( MENU_NAME, ClientConstants.TOOL_MENU );
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
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    if ( !client.hasAcquiredData() && !ToolCategory.OTHER.equals( getCategory() ) )
    {
      JOptionPane.showMessageDialog( client, "No data to invoke tool on!", "Error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.log( Level.INFO, "Invoking tool: \"{0}\" ...", this.tool.getName() );
    }

    final ToolContext context = client.createToolContext();

    this.tool.invoke( client, context );
  }

  /**
   * Returns the current value of category.
   * 
   * @return the tool category, never <code>null</code>.
   */
  public ToolCategory getCategory()
  {
    return this.tool.getCategory();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return getID( this.tool );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( ToolCategory.OTHER.equals( getCategory() ) || aClient.hasAcquiredData() );
  }
}

/* EOF */
