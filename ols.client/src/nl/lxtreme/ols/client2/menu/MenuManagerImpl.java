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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.menu;


import static nl.lxtreme.ols.client2.Client.*;
import static nl.lxtreme.ols.client2.ClientConstants.*;
import static nl.lxtreme.ols.client2.action.ManagedAction.*;

import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.api.*;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.osgi.service.log.*;


/**
 * Provides a manager for keeping menu's in sync with the current set of
 * services.
 */
public class MenuManagerImpl implements MenuManager
{
  // VARIABLES

  private final List<JMenu> menus;
  private final Map<JMenu, ButtonGroup> groups;
  // Injected by Felix DM...
  private volatile LogService log;
  // Locally managed...
  private volatile JMenuBar menuBar;

  // CONSTRUCTORS

  /**
   * Creates a new MenuManagerImpl instance.
   */
  public MenuManagerImpl()
  {
    this.menus = new ArrayList<JMenu>();
    this.groups = new HashMap<JMenu, ButtonGroup>();
  }

  // METHODS

  /**
   * Adds a managed action to this menu manager.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aAction
   *          the managed action to add, cannot be <code>null</code>.
   */
  public void addAction( final ManagedAction aAction )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        Map<String, ?> props = aAction.getProperties();

        String menuName = ( String )props.get( MENU_NAME );
        for ( JMenu menu : menus )
        {
          if ( menuName.equals( menu.getClientProperty( MENU_NAME ) ) )
          {
            synchronized ( menu.getTreeLock() )
            {
              addMenuItem( menu, aAction, props );
            }
            return;
          }
        }
      }
    } );
  }

  /**
   * Adds the given component provider to this controller, and does this
   * synchronously on the EDT.
   * <p>
   * This method is called by the Dependency Manager.
   * </p>
   * 
   * @param aProvider
   *          the component provider, cannot be <code>null</code>.
   */
  public void addMenu( final ComponentProvider aProvider )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JMenuBar menuBar = getMenuBar();
        if ( menuBar != null )
        {
          final JMenu menu = ( JMenu )aProvider.getComponent();
          synchronized ( menuBar.getTreeLock() )
          {
            menuBar.add( menu );
          }

          aProvider.addedToContainer();

          menuBar.revalidate();
          menuBar.repaint();
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public JMenuBar getMenuBar()
  {
    return this.menuBar;
  }

  /**
   * Removes a managed action from this menu manager.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aAction
   *          the managed action to remove, cannot be <code>null</code>.
   */
  public void removeAction( final ManagedAction aAction )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        Map<String, ?> props = aAction.getProperties();

        String menuName = ( String )props.get( ManagedAction.MENU_NAME );
        for ( JMenu menu : menus )
        {
          if ( menuName.equals( menu.getClientProperty( MENU_NAME ) ) )
          {
            synchronized ( menu.getTreeLock() )
            {
              removeMenuItem( menu, aAction, props );
            }
            return;
          }
        }
      }
    } );
  }

  /**
   * Removes the given component provider from this controller, and does this
   * synchronously on the EDT.
   * <p>
   * This method is called by the Dependency Manager.
   * </p>
   * 
   * @param aProvider
   *          the menu component provider, cannot be <code>null</code>.
   */
  public void removeMenu( final ComponentProvider aProvider )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JMenuBar menuBar = getMenuBar();
        if ( menuBar != null )
        {
          aProvider.removedFromContainer();

          synchronized ( menuBar.getTreeLock() )
          {
            menuBar.remove( aProvider.getComponent() );
          }

          menuBar.revalidate();
          menuBar.repaint();
        }
      }
    } );
  }

  final void createMenuBar()
  {
    this.menuBar = new JMenuBar();

    JMenu fileMenu = this.menuBar.add( new JMenu( "File" ) );
    fileMenu.putClientProperty( MENU_NAME, FILE_MENU );
    fileMenu.setMnemonic( 'F' );

    this.menus.add( fileMenu );

    fileMenu.addSeparator();

    JMenu exportMenu = ( JMenu )fileMenu.add( new JMenu( "Export ..." ) );
    exportMenu.putClientProperty( MENU_NAME, EXPORT_MENU );
    exportMenu.putClientProperty( MENU_ORDER, 6 );
    exportMenu.putClientProperty( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
    exportMenu.setMnemonic( 'e' );

    this.menus.add( exportMenu );

    if ( !isMacOS() )
    {
      JMenu editMenu = this.menuBar.add( new JMenu( "Edit" ) );
      editMenu.putClientProperty( MENU_NAME, EDIT_MENU );
      editMenu.setMnemonic( 'E' );

      this.menus.add( editMenu );
    }

    JMenu captureMenu = this.menuBar.add( new JMenu( "Capture" ) );
    captureMenu.putClientProperty( MENU_NAME, CAPTURE_MENU );
    captureMenu.setMnemonic( 'C' );

    this.menus.add( captureMenu );

    JMenu deviceMenu = ( JMenu )captureMenu.add( new JMenu( "Device" ) );
    deviceMenu.putClientProperty( MENU_NAME, DEVICE_MENU );
    deviceMenu.putClientProperty( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
    deviceMenu.setMnemonic( 'D' );

    this.menus.add( deviceMenu );

    JMenu diagramMenu = this.menuBar.add( new JMenu( "Diagram" ) );
    diagramMenu.putClientProperty( MENU_NAME, DIAGRAM_MENU );
    diagramMenu.setMnemonic( 'D' );

    this.menus.add( diagramMenu );

    JMenu cursorsMenu = ( JMenu )diagramMenu.add( new JMenu( "Cursors" ) );
    cursorsMenu.putClientProperty( MENU_NAME, CURSORS_MENU );
    cursorsMenu.putClientProperty( MENU_ORDER, 10 );
    cursorsMenu.putClientProperty( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
    cursorsMenu.putClientProperty( MENU_SEPARATOR_BELOW, Boolean.TRUE );
    cursorsMenu.setMnemonic( 'C' );

    this.menus.add( cursorsMenu );

    JMenu toolsMenu = this.menuBar.add( new JMenu( "Tools" ) );
    toolsMenu.putClientProperty( MENU_NAME, TOOL_MENU );
    toolsMenu.setMnemonic( 'T' );

    this.menus.add( toolsMenu );

    JMenu windowMenu = this.menuBar.add( new JMenu( "Window" ) );
    windowMenu.putClientProperty( MENU_NAME, WINDOW_MENU );
    windowMenu.setMnemonic( 'W' );

    this.menus.add( windowMenu );

    // Add two items that remain constant for the remainder of the lifetime of
    // this client...
    windowMenu.add( new JMenuItem( StandardActionFactory.createCloseAction() ) );
    windowMenu.add( new JMenuItem( new MinimizeWindowAction() ) );

    windowMenu.addSeparator();

    JMenu viewMenu = ( JMenu )windowMenu.add( new JMenu( "Show View" ) );
    viewMenu.putClientProperty( MENU_NAME, VIEW_MENU );

    this.menus.add( viewMenu );

    JMenu helpMenu = this.menuBar.add( new JMenu( "Help" ) );
    helpMenu.putClientProperty( MENU_NAME, HELP_MENU );
    helpMenu.setMnemonic( 'H' );

    this.menus.add( helpMenu );
  }

  /**
   * Called by Felix DM when starting this component.
   * 
   * @param aComponent
   *          the component definition, never <code>null</code>.
   */
  void start( Component aComponent ) throws Exception
  {
    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        createMenuBar();
      }
    } );
  }

  /**
   * Called by Felix DM when stopping this component.
   * 
   * @param aComponent
   *          the component definition, never <code>null</code>.
   */
  void stop( Component aComponent ) throws Exception
  {
    SwingUtilities.invokeAndWait( new Runnable()
    {
      @Override
      public void run()
      {
        menuBar.removeAll();
        menuBar = null;
      }
    } );
  }

  /**
   * Adds a given managed action as menu item to a given menu.
   * 
   * @param aMenu
   * @param aAction
   * @param aProperties
   */
  private void addMenuItem( JMenu aMenu, ManagedAction aAction, Map<String, ?> aProperties )
  {
    JMenuItem item;
    if ( Boolean.TRUE.equals( aProperties.get( MENU_GROUPED ) ) )
    {
      ButtonGroup group = this.groups.get( aMenu );
      if ( group == null )
      {
        group = new ButtonGroup();
        this.groups.put( aMenu, group );
      }

      item = new JRadioButtonMenuItem( aAction );
      group.add( item );
    }
    else if ( Boolean.TRUE.equals( aProperties.get( MENU_CHECKBOX ) ) )
    {
      item = new JCheckBoxMenuItem( aAction );
    }
    else
    {
      item = new JMenuItem( aAction );
    }

    item.putClientProperty( "id", aAction.getId() );
    for ( Map.Entry<String, ?> entry : aProperties.entrySet() )
    {
      item.putClientProperty( entry.getKey(), entry.getValue() );
    }

    aMenu.add( item );

    this.log.log( LogService.LOG_INFO, "Adding menu item: " + item.getText() + " (" + aAction.getId() + ")" );

    // Make sure the order is correct...
    sortMenu( aMenu );
  }

  /**
   * Removes a managed action from the given menu.
   * 
   * @param aMenu
   * @param aAction
   * @param aProperties
   */
  private void removeMenuItem( JMenu aMenu, ManagedAction aAction, Map<String, ?> aProperties )
  {
    final String id = aAction.getId();

    int index = -1;
    for ( int i = aMenu.getItemCount(); i >= 0; i-- )
    {
      JMenuItem item = aMenu.getItem( i );
      if ( item != null && id.equals( item.getClientProperty( "id" ) ) )
      {
        index = i;
        break;
      }
    }

    if ( index >= 0 )
    {
      // Found it...
      JMenuItem item = aMenu.getItem( index );

      this.log.log( LogService.LOG_INFO, "Removing menu item: " + item.getText() + " (" + aAction.getId() + ")" );

      if ( Boolean.TRUE.equals( item.getClientProperty( MENU_SEPARATOR_BELOW ) ) )
      {
        aMenu.remove( index + 1 );
      }

      aMenu.remove( index );

      if ( Boolean.TRUE.equals( item.getClientProperty( MENU_SEPARATOR_ABOVE ) ) )
      {
        aMenu.remove( index - 1 ); // XXX or -0?
      }
    }
  }

  /**
   * @param aMenu
   *          the menu to sort, cannot be <code>null</code>.
   */
  private void sortMenu( JMenu aMenu )
  {
    List<JMenuItem> comps = new ArrayList<JMenuItem>();

    int count = aMenu.getItemCount();
    for ( int i = 0; i < count; i++ )
    {
      JMenuItem comp = aMenu.getItem( i );
      if ( comp != null )
      {
        comps.add( comp );
      }
    }

    Collections.sort( comps, new Comparator<JMenuItem>()
    {
      @Override
      public int compare( JMenuItem aO1, JMenuItem aO2 )
      {
        Integer order1 = ( Integer )aO1.getClientProperty( MENU_ORDER );
        Integer order2 = ( Integer )aO2.getClientProperty( MENU_ORDER );

        if ( order1 != null )
        {
          if ( order2 != null )
          {
            return order1.compareTo( order2 );
          }
          else
          {
            return -1;
          }
        }
        else
        {
          if ( order2 != null )
          {
            return 1;
          }
        }

        String name1 = aO1.getText();
        String name2 = aO2.getText();

        return name1.compareTo( name2 );
      }

    } );

    aMenu.removeAll();

    // Add items & separators in correct order...
    for ( JMenuItem comp : comps )
    {
      if ( Boolean.TRUE.equals( comp.getClientProperty( MENU_SEPARATOR_ABOVE ) ) )
      {
        aMenu.addSeparator();
      }

      aMenu.add( comp );

      if ( Boolean.TRUE.equals( comp.getClientProperty( MENU_SEPARATOR_BELOW ) ) )
      {
        aMenu.addSeparator();
      }
    }
  }
}
