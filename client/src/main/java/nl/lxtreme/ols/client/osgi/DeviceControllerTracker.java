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
import nl.lxtreme.ols.client.Host.MainFrame;
import nl.lxtreme.ols.client.action.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class DeviceControllerTracker extends ServiceTracker
{
  // VARIABLES

  private final MainFrame mainFrame;
  private final JMenu menu;
  private final JMenuItem noItemItem;
  private final ButtonGroup deviceGroup;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aWindow
   */
  public DeviceControllerTracker( final BundleContext aContext, final MainFrame aFrame, final JMenu aMenu )
  {
    super( aContext, DeviceController.class.getName(), null );

    this.mainFrame = aFrame;
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
    // Determine where in the menu we should add the menu item, this way, we
    // can make the menu appear consistent...
    final int idx = determineMenuItemIndex( menuItem );

    this.deviceGroup.add( menuItem );
    this.menu.add( menuItem, idx );

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
    final JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem( new SelectDeviceAction( this.mainFrame,
        aDevController ) );
    menuItem.setName( aDevController.getName() );
    return menuItem;
  }

  /**
   * Determines the index in the menu where the given menu item should be
   * inserted.
   * 
   * @param aMenuItem
   *          the menu item to add, cannot be <code>null</code>.
   * @return the position in the menu to add the given menu item, -1 if the menu
   *         item should be added as last item.
   */
  private int determineMenuItemIndex( final JMenuItem aMenuItem )
  {
    int idx = -1;
    for ( int i = 0; ( idx < 0 ) && ( i < this.menu.getItemCount() ); i++ )
    {
      final String nameA = this.menu.getItem( i ).getText();
      final int comparison = aMenuItem.getText().compareTo( nameA );
      if ( comparison < 0 )
      {
        idx = i;
      }
    }
    return idx;
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

        this.mainFrame.setCurrentDeviceController( aDevController );
      }
    }
    else
    {
      if ( this.menu.getItemCount() == 0 )
      {
        // We've removed the last one...
        this.menu.add( this.noItemItem );
      }

      if ( this.mainFrame.getCurrentDeviceController() == aDevController )
      {
        this.mainFrame.setCurrentDeviceController( null );
      }
    }

    this.menu.revalidate();
    this.menu.repaint();
  }
}
