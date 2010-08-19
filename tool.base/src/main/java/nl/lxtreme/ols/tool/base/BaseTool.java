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
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a base implementation for tools showing a dialog.
 */
public abstract class BaseTool<DIALOG extends JDialog & ToolDialog> implements Tool, Configurable
{
  // VARIABLES

  private final String name;
  private DIALOG dialog;

  // CONSTRUCTORS

  /**
   * Creates a new BaseTool instance.
   * 
   * @param aName
   *          the name of this tool (to show in the UI).
   */
  public BaseTool( final String aName )
  {
    this.name = aName;
  }

  // METHODS

  /**
   * @see java.lang.Object#equals(java.lang.Object)
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
   * @see nl.lxtreme.ols.api.tools.Tool#getName()
   */
  @Override
  public final String getName()
  {
    return this.name;
  }

  /**
   * @see java.lang.Object#hashCode()
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
   * @see nl.lxtreme.ols.api.tools.Tool#process(java.awt.Frame,
   *      nl.lxtreme.ols.api.data.AnnotatedData,
   *      nl.lxtreme.ols.api.tools.ToolContext,
   *      nl.lxtreme.ols.api.tools.AnalysisCallback)
   */
  @Override
  public final void process( final Window aOwner, final AnnotatedData aData, final ToolContext aContext,
      final AnalysisCallback aCallback )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.dialog != null ) && ( this.dialog.getOwner() != aOwner ) )
    {
      this.dialog.dispose();
      this.dialog = null;
    }

    // if no valid dialog exists, create one
    if ( this.dialog == null )
    {
      this.dialog = createDialog( aOwner, getName(), aData, aContext, aCallback );
    }
    else
    {
      // Reset the dialog to its initial state...
      this.dialog.reset();
    }

    doProcess( aData, aContext, aCallback );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(String,
   *      java.util.Properties)
   */
  @Override
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    if ( this.dialog instanceof Configurable )
    {
      ( ( Configurable )this.dialog ).readProperties( aNamespace, aProperties );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(String,
   *      java.util.Properties)
   */
  @Override
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    if ( this.dialog instanceof Configurable )
    {
      ( ( Configurable )this.dialog ).writeProperties( aNamespace, aProperties );
    }
  }

  /**
   * @param aOwner
   * @param aName
   * @param aData
   * @param aContext
   * @param aCallback
   * @return
   */
  protected abstract DIALOG createDialog( final Window aOwner, final String aName, final AnnotatedData aData,
      final ToolContext aContext, final AnalysisCallback aCallback );

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
  protected void doProcess( final AnnotatedData aData, final ToolContext aContext, final AnalysisCallback aCallback )
  {
    this.dialog.showDialog( aData );
  }

  /**
   * @return
   */
  protected final DIALOG getDialog()
  {
    return this.dialog;
  }
}
