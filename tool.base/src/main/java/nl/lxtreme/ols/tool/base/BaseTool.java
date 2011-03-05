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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a base implementation for tools showing a dialog.
 */
public abstract class BaseTool<DIALOG extends JDialog & ToolDialog> implements Tool
{
  // VARIABLES

  private final String name;
  private final Category category;
  private DIALOG dialog;

  // CONSTRUCTORS

  /**
   * Creates a new BaseTool instance.
   * 
   * @param aCategory
   *          the category of this tool;
   * @param aName
   *          the name of this tool (to show in the UI).
   * @throws IllegalArgumentException
   *           in case the given category was <code>null</code> or the given
   *           name was <code>null</code> or empty.
   */
  protected BaseTool( final Category aCategory, final String aName )
  {
    if ( aCategory == null )
    {
      throw new IllegalArgumentException( "Category cannot be null!" );
    }
    this.category = aCategory;

    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }
    this.name = aName;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof BaseTool<?> ) )
    {
      return false;
    }

    final BaseTool<?> other = ( BaseTool<?> )aObject;
    if ( this.name == null )
    {
      if ( other.name != null )
      {
        return false;
      }
    }
    else if ( !this.name.equals( other.name ) )
    {
      return false;
    }

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final Category getCategory()
  {
    return this.category;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final String getName()
  {
    return this.name;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( this.name == null ) ? 0 : this.name.hashCode() );
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void process( final Window aOwner, final DataContainer aData, final ToolContext aContext,
      final AnalysisCallback aCallback )
  {
    // check if dialog exists with different owner and dispose if so...
    if ( this.dialog != null )
    {
      this.dialog.dispose();
      this.dialog = null;
    }

    // if no valid dialog exists, create one
    if ( this.dialog == null )
    {
      this.dialog = createDialog( aOwner, aContext, getName() );
    }
    else
    {
      // Reset the dialog to its initial state...
      this.dialog.reset();
    }

    doProcess( aData, aContext, aCallback );
  }

  /**
   * Factory method for creating a tool dialog.
   * 
   * @param aOwner
   *          the owning window of the newly created tool dialog, can be
   *          <code>null</code>;
   * @param aContext
   *          the tool context in which this tool is to be called, never
   *          <code>null</code>;
   * @param aName
   *          the name of the tool dialog to create, never <code>null</code>.
   * @return a new tool dialog, never <code>null</code>.
   */
  protected abstract DIALOG createDialog( final Window aOwner, ToolContext aContext, final String aName );

  /**
   * Does the actual processing of data.
   * 
   * @param aData
   *          the captured data to process in this tool, cannot be
   *          <code>null</code>;
   * @param aContext
   *          the tool context to use during the processing, can be
   *          <code>null</code>;
   * @param aCallback
   *          the callback to report the status of the tool to, cannot be
   *          <code>null</code>.
   */
  protected void doProcess( final DataContainer aData, final ToolContext aContext, final AnalysisCallback aCallback )
  {
    this.dialog.showDialog( aData );
  }

  /**
   * Returns the dialog that is currently available.
   * 
   * @return the current dialog, can be <code>null</code>.
   */
  protected final DIALOG getDialog()
  {
    return this.dialog;
  }
}
