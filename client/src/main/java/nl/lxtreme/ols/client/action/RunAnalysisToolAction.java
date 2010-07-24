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
  private final PropertyManager propertyManager;

  // CONSTRUCTORS

  /**
   * @param aID
   * @param aName
   * @param aDescription
   */
  public RunAnalysisToolAction( final Host aHost, final Tool aTool, final PropertyManager aPropertyManager )
  {
    super( ID + aTool.getName(), aTool.getName(), "Run " + aTool.getName() );

    this.host = aHost;
    this.tool = aTool;
    this.propertyManager = aPropertyManager;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Frame owner = ( Frame )HostUtils.getOwningWindow( aEvent );

    // Make sure the tool is started from the EDT...
    SwingUtilities.invokeLater( new Runnable()
    {
      private final Host host = RunAnalysisToolAction.this.host;
      private final Tool tool = RunAnalysisToolAction.this.tool;
      private final PropertyManager propertyManager = RunAnalysisToolAction.this.propertyManager;

      /**
       * @see java.lang.Runnable#run()
       */
      @Override
      public void run()
      {
        final CapturedData capturedData = this.host.getCapturedData();
        final ToolContext toolContext = null;

        if ( this.tool instanceof Configurable )
        {
          ( ( Configurable )this.tool ).readProperties( this.propertyManager.getProperties() );
        }

        this.tool.process( owner, capturedData, toolContext, this.host );

        if ( this.tool instanceof Configurable )
        {
          ( ( Configurable )this.tool ).writeProperties( this.propertyManager.getProperties() );
        }
      }
    } );
  }
}

/* EOF */
