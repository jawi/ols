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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client.ui.action.*;
import nl.lxtreme.ols.client.ui.action.manager.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.log.*;

import com.jidesoft.docking.*;


/**
 * Provides a factory for creating menu bars and tool bars.
 */
public class MenuBarFactory
{
  // INNER TYPES

  /**
   * Provides an adapter class for {@link MenuListener} allowing a menu to be
   * recreated each time it is selected.
   */
  static abstract class AbstractMenuBuilder implements MenuListener
  {
    // VARIABLES

    private final ButtonGroup group = new ButtonGroup();

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void menuCanceled( final MenuEvent aEvent )
    {
      // No-op
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void menuDeselected( final MenuEvent aEvent )
    {
      // No-op
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void menuSelected( final MenuEvent aEvent )
    {
      // Build the menu dynamically...
      final JMenu menu = ( JMenu )aEvent.getSource();

      String[] names = getMenuItemNames();
      if ( names.length == 0 )
      {
        for ( int i = 0, size = menu.getItemCount(); i < size; i++ )
        {
          final JMenuItem item = menu.getItem( i );
          if ( item instanceof AbstractButton )
          {
            this.group.remove( item );
          }
          menu.remove( item );
        }

        JMenuItem noDevicesItem = new JMenuItem( getNoItemsName() );
        noDevicesItem.setEnabled( false );

        menu.add( noDevicesItem );
      }
      else
      {
        names = removeObsoleteMenuItems( menu, names );
        for ( String name : names )
        {
          try
          {
            final JMenuItem menuItem = createMenuItem( name );
            if ( menuItem != null )
            {
              this.group.add( menuItem );
              menu.add( menuItem );
            }
          }
          catch ( Exception exception )
          {
            LogService log = Client.getInstance().getLogService();
            log.log( LogService.LOG_WARNING, "Exception thrown while creating menu item!", exception );
          }
        }
      }
    }

    /**
     * Factory method for creating a menu item for the given name.
     * 
     * @param aName
     *          the name of the menu item, never <code>null</code>.
     * @return a new menu item instance, never <code>null</code>.
     */
    protected abstract JMenuItem createMenuItem( String aName );

    /**
     * Returns all names of menu items.
     * 
     * @return an array of menu item names, never <code>null</code>.
     */
    protected abstract String[] getMenuItemNames();

    /**
     * Returns the name to display in case no other menu items are available.
     * 
     * @return a 'no items' menu item name, never <code>null</code>.
     */
    protected abstract String getNoItemsName();

    /**
     * Returns whether or not the given menu item is "persistent", i.e., it
     * should not be removed automagically from the menu.
     * 
     * @param aMenuItem
     *          the menu item to test, cannot be <code>null</code>.
     * @return <code>true</code> if the menu item is persistent,
     *         <code>false</code> otherwise.
     */
    private boolean isPersistentMenuItem( final JMenuItem aMenuItem )
    {
      final Object isPersistent = aMenuItem.getClientProperty( PERSISTENT_MENU_ITEM_KEY );
      return Boolean.TRUE.equals( isPersistent );
    }

    /**
     * Removes all obsolete menu items from the given menu, meaning that all
     * items that are not persistent and are not contained in the given list of
     * menu items are removed.
     * 
     * @param aMenu
     *          the menu to remove the obsolete items from;
     * @param aMenuItems
     *          the menu items that should either remain or be added to the
     *          menu.
     * @return an array of menu items that are to be added to the given menu.
     */
    private String[] removeObsoleteMenuItems( final JMenu aMenu, final String[] aMenuItems )
    {
      List<String> result = new ArrayList<String>( Arrays.asList( aMenuItems ) );
      // Remove all obsolete menu items from the menu...
      for ( int i = aMenu.getItemCount() - 1; i >= 0; i-- )
      {
        final JMenuItem menuItem = aMenu.getItem( i );
        if ( menuItem == null )
        {
          // Not a menu item; simply ignore it and continue...
          continue;
        }

        final String itemText = menuItem.getText();
        if ( !result.contains( itemText ) && !isPersistentMenuItem( menuItem ) )
        {
          // Remove this menu item from the menu; it is obsolete...
          aMenu.remove( i );
        }
        else
        {
          // Remove the checked item; it should not be (re)added to the menu...
          result.remove( itemText );
        }
      }

      return result.toArray( new String[result.size()] );
    }
  }

  /**
   * Provides a builder for building the cursors menu upon selection of the
   * menu.
   */
  static final class CursorMenuBuilder extends AbstractMenuBuilder
  {
    // VARIABLES

    private final ActionManager actionManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link CursorMenuBuilder} instance.
     */
    public CursorMenuBuilder( final ActionManager aActionManager )
    {
      this.actionManager = aActionManager;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aName )
    {
      try
      {
        int idx = Integer.parseInt( aName );
        if ( idx >= 0 )
        {
          final Action action = this.actionManager.getAction( GotoNthCursorAction.getID( idx ) );
          return new JMenuItem( action );
        }
      }
      catch ( NumberFormatException exception )
      {
        // Ignore...
      }
      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      final CursorController controller = Client.getInstance().getCursorController();

      final List<String> result = new ArrayList<String>();
      for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
      {
        final Cursor cursor = controller.getCursor( i );
        if ( ( cursor != null ) && cursor.isDefined() )
        {
          result.add( Integer.toString( cursor.getIndex() ) );
        }
      }
      return result.toArray( new String[result.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No cursors set.";
    }
  }

  /**
   * Provides a builder for building the devices menu upon selection of the
   * menu.
   */
  static final class DeviceMenuBuilder extends AbstractMenuBuilder
  {
    // VARIABLES

    private final ActionManager actionManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link DeviceMenuBuilder} instance.
     */
    public DeviceMenuBuilder( final ActionManager aActionManager )
    {
      this.actionManager = aActionManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aDeviceName )
    {
      final Action action = this.actionManager.getAction( SelectDeviceAction.getID( aDeviceName ) );
      action.putValue( Action.SELECTED_KEY, isDeviceToBeSelected( aDeviceName ) );
      return new JRadioButtonMenuItem( action );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      final DeviceController controller = Client.getInstance().getDeviceController();
      return controller.getDeviceNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No devices.";
    }

    /**
     * Returns whether or not the given device name is to be selected in the
     * menu.
     * 
     * @param aDeviceName
     *          the name of the device to test.
     * @return {@link Boolean#TRUE} if the device is to be selected,
     *         {@link Boolean#FALSE} otherwise.
     */
    private Boolean isDeviceToBeSelected( final String aDeviceName )
    {
      final DeviceController controller = Client.getInstance().getDeviceController();
      return Boolean.valueOf( aDeviceName.equals( controller.getSelectedDeviceName() ) );
    }
  }

  /**
   * Provides a builder for building the export menu upon selection of the menu.
   */
  static final class ExportMenuBuilder extends AbstractMenuBuilder
  {
    // VARIABLES

    private final ActionManager actionManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ExportMenuBuilder} instance.
     */
    public ExportMenuBuilder( final ActionManager aActionManager )
    {
      this.actionManager = aActionManager;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aExporterName )
    {
      final Action action = this.actionManager.getAction( ExportAction.getID( aExporterName ) );
      return new JMenuItem( action );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      return Client.getInstance().getImportExportController().getExporterNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No exporters.";
    }
  }

  /**
   * Provides a builder for building the tools menu upon selection of the menu.
   */
  static final class ToolMenuBuilder extends AbstractMenuBuilder
  {
    // VARIABLES

    private final ActionManager actionManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ToolMenuBuilder} instance.
     */
    public ToolMenuBuilder( final ActionManager aActionManager )
    {
      this.actionManager = aActionManager;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aToolName )
    {
      return new JMenuItem( this.actionManager.getAction( RunToolAction.getID( aToolName ) ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      return Client.getInstance().getToolController().getToolNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No tools.";
    }
  }

  /**
   * Provides a builder for selecting the tool windows.
   */
  static final class ToolWindowMenuBuilder extends AbstractMenuBuilder implements ActionListener
  {
    // VARIABLES

    private final DockingManager dockingManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ToolWindowMenuBuilder} instance.
     */
    public ToolWindowMenuBuilder( final DockingManager aDockingManager )
    {
      this.dockingManager = aDockingManager;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      JMenuItem menuItem = ( JMenuItem )aEvent.getSource();
      this.dockingManager.activateFrame( menuItem.getName() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aName )
    {
      JMenuItem menuItem = new JMenuItem( aName );
      menuItem.setName( aName );
      menuItem.addActionListener( this );
      return menuItem;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      List<String> names = this.dockingManager.getAllFrameNames();
      return names.toArray( new String[names.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No tool windows";
    }
  }

  /**
   * Provides a builder for building the window menu upon selection of the menu.
   */
  static final class WindowMenuBuilder extends AbstractMenuBuilder
  {
    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected JMenuItem createMenuItem( final String aWindowName )
    {
      return new JMenuItem( new FocusWindowAction( aWindowName ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getMenuItemNames()
    {
      final Window[] windows = Window.getWindows();
      final List<String> titles = new ArrayList<String>();
      for ( Window window : windows )
      {
        if ( window.isDisplayable() )
        {
          titles.add( FocusWindowAction.getTitle( window ) );
        }
      }
      return titles.toArray( new String[titles.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getNoItemsName()
    {
      return "No windows.";
    }
  }

  // CONSTANTS

  private static final String PERSISTENT_MENU_ITEM_KEY = "persistentMenuItem";

  // METHODS

  /**
   * Creates the menu bar with all menu's.
   * 
   * @return the menu bar instance, never <code>null</code>.
   */
  public JMenuBar createMenuBar( final MainFrame aMainFrame )
  {
    final ActionManager actionManager = Client.getInstance().getActionManager();

    JMenuBar menuBar = new JMenuBar();

    JMenu exportMenu = new JMenu( "Export" );
    exportMenu.setMnemonic( 'E' );
    exportMenu.addMenuListener( new ExportMenuBuilder( actionManager ) );

    final JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( 'F' );
    menuBar.add( fileMenu );

    fileMenu.add( actionManager.getAction( NewProjectAction.ID ) );
    fileMenu.add( actionManager.getAction( OpenProjectAction.ID ) );
    fileMenu.add( actionManager.getAction( SaveProjectAction.ID ) );
    fileMenu.add( actionManager.getAction( SaveProjectAsAction.ID ) );
    fileMenu.addSeparator();
    fileMenu.add( actionManager.getAction( OpenDataFileAction.ID ) );
    fileMenu.add( actionManager.getAction( SaveDataFileAction.ID ) );
    fileMenu.addSeparator();
    fileMenu.add( exportMenu );

    if ( Platform.needsExitMenuItem() )
    {
      fileMenu.add( new JSeparator() );
      fileMenu.add( actionManager.getAction( ExitAction.ID ) );
    }

    if ( Platform.needsPreferencesMenuItem() )
    {
      final JMenu editMenu = menuBar.add( new JMenu( "Edit" ) );
      editMenu.setMnemonic( 'E' );
      editMenu.add( actionManager.getAction( ShowPreferencesDialogAction.ID ) );
    }

    JMenu captureMenu = menuBar.add( new JMenu( "Capture" ) );
    captureMenu.setMnemonic( 'C' );

    JMenu deviceMenu = new JMenu( "Device" );
    deviceMenu.setMnemonic( 'D' );
    deviceMenu.addMenuListener( new DeviceMenuBuilder( actionManager ) );

    captureMenu.add( actionManager.getAction( CaptureAction.ID ) );
    captureMenu.add( actionManager.getAction( RepeatCaptureAction.ID ) );
    captureMenu.add( actionManager.getAction( CancelCaptureAction.ID ) );
    captureMenu.addSeparator();
    captureMenu.add( deviceMenu );

    final JMenu diagramMenu = menuBar.add( new JMenu( "Diagram" ) );
    diagramMenu.setMnemonic( 'D' );

    diagramMenu.add( actionManager.getAction( ZoomInAction.ID ) );
    diagramMenu.add( actionManager.getAction( ZoomOutAction.ID ) );
    diagramMenu.add( actionManager.getAction( ZoomOriginalAction.ID ) );
    diagramMenu.add( actionManager.getAction( ZoomAllAction.ID ) );
    diagramMenu.addSeparator();
    diagramMenu.add( actionManager.getAction( GotoTriggerAction.ID ) );
    diagramMenu.addSeparator();
    diagramMenu.add( new JCheckBoxMenuItem( actionManager.getAction( SetCursorModeAction.ID ) ) );
    diagramMenu.add( new JCheckBoxMenuItem( actionManager.getAction( SetCursorSnapModeAction.ID ) ) );
    diagramMenu.add( actionManager.getAction( DeleteAllCursorsAction.ID ) );
    diagramMenu.add( actionManager.getAction( GotoFirstCursorAction.ID ) );
    diagramMenu.add( actionManager.getAction( GotoLastCursorAction.ID ) );

    JMenu cursorsMenu = new JMenu( "Cursors" );
    cursorsMenu.setMnemonic( 'C' );
    cursorsMenu.addMenuListener( new CursorMenuBuilder( actionManager ) );
    diagramMenu.add( cursorsMenu );

    diagramMenu.addSeparator();
    diagramMenu.add( actionManager.getAction( RemoveAnnotationsAction.ID ) );

    JMenu toolsMenu = menuBar.add( new JMenu( "Tools" ) );
    toolsMenu.setMnemonic( 'T' );
    toolsMenu.add( new JCheckBoxMenuItem( actionManager.getAction( SetMeasurementModeAction.ID ) ) ) //
        .putClientProperty( PERSISTENT_MENU_ITEM_KEY, Boolean.TRUE );
    toolsMenu.addSeparator();
    toolsMenu.addMenuListener( new ToolMenuBuilder( actionManager ) );

    JMenu windowMenu = menuBar.add( new JMenu( "Window" ) );
    windowMenu.setMnemonic( 'W' );

    JMenu toolWindowMenu = new JMenu( "Show Tool Window" );
    toolWindowMenu.setMnemonic( 'T' );
    toolWindowMenu.putClientProperty( PERSISTENT_MENU_ITEM_KEY, Boolean.TRUE );
    toolWindowMenu.addMenuListener( new ToolWindowMenuBuilder( aMainFrame.getDockingManager() ) );

    windowMenu.add( new JMenuItem( StandardActionFactory.createCloseAction() ) ) //
        .putClientProperty( PERSISTENT_MENU_ITEM_KEY, Boolean.TRUE );
    windowMenu.add( new JMenuItem( new MinimizeWindowAction() ) ) //
        .putClientProperty( PERSISTENT_MENU_ITEM_KEY, Boolean.TRUE );

    windowMenu.addSeparator();

    windowMenu.add( toolWindowMenu );

    windowMenu.addSeparator();

    windowMenu.addMenuListener( new WindowMenuBuilder() );

    final JMenu helpMenu = menuBar.add( new JMenu( "Help" ) );
    helpMenu.setMnemonic( 'H' );
    helpMenu.add( actionManager.getAction( ShowBundlesAction.ID ) );

    if ( Platform.needsAboutMenuItem() )
    {
      helpMenu.addSeparator();
      helpMenu.add( actionManager.getAction( HelpAboutAction.ID ) );
    }

    return menuBar;
  }

  /**
   * Creates the tool bar for quick access to the various functions.
   * 
   * @return a toolbar, never <code>null</code>.
   */
  public JToolBar createToolBar()
  {
    final ActionManager actionManager = Client.getInstance().getActionManager();

    final JToolBar toolBar = new JToolBar();
    toolBar.setOrientation( SwingConstants.HORIZONTAL );
    toolBar.setRollover( true );
    toolBar.setFloatable( false );

    toolBar.add( actionManager.getAction( OpenProjectAction.ID ) );
    toolBar.add( actionManager.getAction( SaveProjectAction.ID ) );
    toolBar.addSeparator();

    toolBar.add( actionManager.getAction( CaptureAction.ID ) );
    toolBar.add( actionManager.getAction( CancelCaptureAction.ID ) );
    toolBar.add( actionManager.getAction( RepeatCaptureAction.ID ) );
    toolBar.addSeparator();

    toolBar.add( actionManager.getAction( ZoomInAction.ID ) );
    toolBar.add( actionManager.getAction( ZoomOutAction.ID ) );
    toolBar.add( actionManager.getAction( ZoomOriginalAction.ID ) );
    toolBar.add( actionManager.getAction( ZoomAllAction.ID ) );
    toolBar.addSeparator();

    toolBar.add( actionManager.getAction( GotoTriggerAction.ID ) );
    for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
    {
      toolBar.add( actionManager.getAction( GotoNthCursorAction.getID( c ) ) );
    }

    return toolBar;
  }
}
