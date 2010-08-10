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

import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class DeviceControllerTracker extends ServiceTracker
{
  // VARIABLES

  private final Host host;
  private final JMenu menu;
  private final JMenuItem noItemItem;
  private final ButtonGroup deviceGroup;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aWindow
   */
  public DeviceControllerTracker( final BundleContext aContext, final Host aHost, final JMenu aMenu )
  {
    super( aContext, DeviceController.class.getName(), null );

    this.host = aHost;
    this.menu = aMenu;

    this.noItemItem = new JMenuItem( "No Devices found" );
    this.noItemItem.setEnabled( false );

    this.menu.add( this.noItemItem );

    this.deviceGroup = new ButtonGroup();
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final DeviceController devCtrl = ( DeviceController )this.context.getService( aReference );

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
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final DeviceController devCtrl = ( DeviceController )aService;

    SwingUtilities.invokeLater( new Runnable()
    {
      public void run()
      {
        removeMenuItem( devCtrl );
      }
    } );
  }

  /**
   * @param aDevController
   */
  final void addMenuItem( final DeviceController aDevController )
  {
    // We're adding one, so, there's at least one device available...
    this.menu.remove( this.noItemItem );

    final JMenuItem menuItem = createMenuItem( aDevController );

    this.deviceGroup.add( menuItem );
    this.menu.add( menuItem );

    updateMenuState( aDevController, menuItem, true /* aAdded */);
  }

  /**
   * @param aDevController
   */
  final void removeMenuItem( final DeviceController aDevController )
  {
    final String name = aDevController.getName();

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
      this.deviceGroup.remove( menuItem );
      this.menu.remove( menuItem );
    }

    updateMenuState( aDevController, menuItem, false /* aAdded */);
  }

  /**
   * @param aDevController
   * @return
   */
  private JRadioButtonMenuItem createMenuItem( final DeviceController aDevController )
  {
    final JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem( new SelectDeviceAction( this.host, aDevController ) );
    menuItem.setName( aDevController.getName() );
    return menuItem;
  }

  /**
   * @param aDevController
   * @param aMenuItem
   * @param aAdded
   */
  private void updateMenuState( final DeviceController aDevController, final JMenuItem aMenuItem, final boolean aAdded )
  {
    if ( aAdded )
    {
      // Always select the first added device...
      if ( this.menu.getItemCount() == 1 )
      {
        aMenuItem.setSelected( true );

        this.host.setCurrentDeviceController( aDevController );
      }
    }
    else
    {
      if ( this.menu.getItemCount() == 0 )
      {
        // We've removed the last one...
        this.menu.add( this.noItemItem );
      }

      if ( this.host.getCurrentDeviceController() == aDevController )
      {
        this.host.setCurrentDeviceController( null );
      }
    }

    this.menu.revalidate();
    this.menu.repaint();
  }
}
