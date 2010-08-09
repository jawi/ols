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
package nl.lxtreme.ols.client.osgi;


import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * 
 */
public class ToolTracker extends ServiceTracker
{
  // VARIABLES

  private final Host      host;
  private final Project project;
  private final JMenu     menu;
  private final JMenuItem noItemItem;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aReference
   * @param aCustomizer
   */
  public ToolTracker( final BundleContext aContext, final Host aHost, final Project aProject, final JMenu aMenu )
  {
    super( aContext, Tool.class.getName(), null );

    this.host = aHost;
    this.project = aProject;
    this.menu = aMenu;

    this.noItemItem = new JMenuItem( "No tools found" );
    this.noItemItem.setEnabled( false );

    this.menu.add( this.noItemItem );
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final Tool devCtrl = ( Tool )this.context.getService( aReference );

    SwingUtilities.invokeLater( new Runnable()
    {
      public void run()
      {
        addMenuItem( devCtrl );
      }
    } );

    return devCtrl;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference, java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final Tool devCtrl = ( Tool )aService;

    SwingUtilities.invokeLater( new Runnable()
    {
      public void run()
      {
        removeMenuItem( devCtrl );
      }
    } );
  }

  /**
   * @param aTool
   */
  final void addMenuItem( final Tool aTool )
  {
    // We're adding one, so, there's at least one device available...
    this.menu.remove( this.noItemItem );

    final JMenuItem menuItem = createMenuItem( aTool );

    this.menu.add( menuItem );

    updateMenuState( aTool, menuItem, true /* aAdded */);
  }

  /**
   * @param aTool
   */
  final void removeMenuItem( final Tool aTool )
  {
    final String name = aTool.getName();

    JMenuItem menuItem = null;
    for ( int i = 0; i < this.menu.getItemCount(); i++ )
    {
      final JMenuItem comp = this.menu.getItem( i );
      if ( name.equals( comp.getName() ) )
      {
        menuItem = comp;
        break;
      }
    }

    if ( menuItem != null )
    {
      this.menu.remove( menuItem );
    }

    updateMenuState( aTool, menuItem, false /* aAdded */);
  }

  /**
   * @param aTool
   * @return
   */
  private JMenuItem createMenuItem( final Tool aTool )
  {
    final JMenuItem menuItem = new JMenuItem( new RunAnalysisToolAction( this.host, aTool, this.project ) );
    menuItem.setName( aTool.getName() );
    return menuItem;
  }

  /**
   * @param aTool
   * @param aMenuItem
   * @param aAdded
   */
  private void updateMenuState( final Tool aTool, final JMenuItem aMenuItem, final boolean aAdded )
  {
    if ( !aAdded )
    {
      if ( this.menu.getItemCount() == 0 )
      {
        // We've removed the last one...
        this.menu.add( this.noItemItem );
      }
    }

    this.menu.revalidate();
    this.menu.repaint();
  }

}

/* EOF */
