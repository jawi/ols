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

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;

/**
 * @author jawi
 *
 */
public abstract class BaseTool implements Tool, Configurable
{
  // VARIABLES

  private final String name;

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
    if ( ( aObject == null ) || !(aObject instanceof BaseTool) )
    {
      return false;
    }

    final BaseTool other = ( BaseTool )aObject;
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
  public void process( final Frame aParentFrame, final AnnotatedData aData, final ToolContext aContext, final AnalysisCallback aCallback )
  {
    setupTool( aParentFrame );

    doProcess( aData, aContext );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(String, java.util.Properties)
   */
  @Override
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    // NO-op
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(String, java.util.Properties)
   */
  @Override
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    // NO-op
  }

  /**
   * Does the actual processing of data.
   * <p>
   * By default, this method does <tt>getToolWorker().execute()</tt> to start
   * the tool worker in the background. Override this method to do something
   * different, for example, to wrap the tool worker in a UI-dialog.
   * </p>
   * 
   * @param aData
   *          the captured data to process in this tool;
   * @param aContext
   *          the tool context to use during the processing.
   */
  protected abstract void doProcess( final AnnotatedData aData, final ToolContext aContext );

  /**
   * Sets up this tool, for example by creating a dialog.
   * 
   * @param aFrame
   *          the (parent) frame to use when creating a modal dialog.
   */
  protected void setupTool( final Frame aFrame )
  {
    // NO-op
  }
}
