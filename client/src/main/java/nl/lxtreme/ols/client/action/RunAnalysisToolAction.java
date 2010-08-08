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
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;


/**
 * 
 */
public class RunAnalysisToolAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RunTool";

  // VARIABLES

  private final Host host;
  private final Tool tool;
  private final Project project;

  // CONSTRUCTORS

  /**
   * @param aID
   * @param aName
   * @param aDescription
   */
  public RunAnalysisToolAction( final Host aHost, final Tool aTool, final Project aProject )
  {
    super( ID + aTool.getName(), aTool.getName(), "Run " + aTool.getName() );

    this.host = aHost;
    this.tool = aTool;
    this.project = aProject;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Frame owner = ( Frame )HostUtils.getOwningWindow( aEvent );

    if ( this.tool instanceof Configurable )
    {
      this.project.addConfigurable( ( Configurable )this.tool );
    }

    // Make sure the tool is started from the EDT...
    SwingUtilities.invokeLater( new Runnable()
    {
      private final Host host = RunAnalysisToolAction.this.host;
      private final Tool tool = RunAnalysisToolAction.this.tool;

      /**
       * @see java.lang.Runnable#run()
       */
      @Override
      public void run()
      {
        final AnnotatedData data = this.host.getAnnotatedData();
        final ToolContext toolContext = null;

        this.tool.process( owner, data, toolContext, this.host );
      }
    } );
  }
}

/* EOF */
