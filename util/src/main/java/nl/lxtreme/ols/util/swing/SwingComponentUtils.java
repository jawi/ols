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
package nl.lxtreme.ols.util.swing;


import java.awt.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * Provides some utility methods for use with Swing components.
 */
public final class SwingComponentUtils
{
  // CONSTANTS

  /**
   * Creates a new {@link SwingComponentUtils} instance, never used.
   */
  private SwingComponentUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Convenience method to create a key mask.
   * 
   * @param aKeyStroke
   *          the key stroke to create a key mask for;
   * @param aMasks
   *          the (optional) mask modifiers to use.
   * @return a keystroke instance.
   */
  public static final KeyStroke createKeyMask( final int aKeyStroke, final int... aMasks )
  {
    int modifiers = 0;
    for ( int aMask : aMasks )
    {
      modifiers |= aMask;
    }
    return KeyStroke.getKeyStroke( aKeyStroke, modifiers );
  }

  /**
   * Convenience method to create a key mask for menu's.
   * 
   * @param aKeyStroke
   *          the key stroke to create a menu key mask for;
   * @param aMasks
   *          the (optional) mask modifiers to use.
   * @return a keystroke instance.
   */
  public static final KeyStroke createMenuKeyMask( final int aKeyStroke, final int... aMasks )
  {
    int modifiers = getMenuShortcutKeyMask();
    for ( int aMask : aMasks )
    {
      modifiers |= aMask;
    }
    return KeyStroke.getKeyStroke( aKeyStroke, modifiers );
  }

  /**
   * Returns the key mask of the menu shortcut key.
   * 
   * @return a key mask, >= 0.
   */
  public static final int getMenuShortcutKeyMask()
  {
    return Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
  }

  /**
   * Tries to find the owning window for the AWT-event's source.
   * 
   * @param aEvent
   *          the AWT event to find the owning window for, may be
   *          <code>null</code>.
   * @return the owning window, or <code>null</code> if no such window could be
   *         found, or a <code>null</code> event was given.
   */
  public static final Window getOwningWindow( final AWTEvent aEvent )
  {
    Window owner = null;
    if ( ( aEvent != null ) && ( aEvent.getSource() instanceof Component ) )
    {
      owner = getOwningWindow( ( Component )aEvent.getSource() );
    }
    return owner;
  }

  /**
   * Tries to find the owning window for the given component.
   * 
   * @param aComponent
   *          the AWT event to find the owning window for, may be
   *          <code>null</code>.
   * @return the owning window, or <code>null</code> if no such window could be
   *         found, or a <code>null</code> component was given.
   */
  public static final Window getOwningWindow( final Component aComponent )
  {
    if ( aComponent == null )
    {
      return null;
    }

    Window owner = SwingUtilities.getWindowAncestor( aComponent );
    if ( owner == null )
    {
      final KeyboardFocusManager kbdFocusManager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
      owner = kbdFocusManager.getFocusedWindow();
      if ( owner == null )
      {
        owner = kbdFocusManager.getActiveWindow();
      }
    }
    return owner;
  }

  /**
   * Sets the selected item of the given checkbox to the value given, unless
   * this value is <code>null</code>.
   * 
   * @param aCheckBox
   *          the checkbox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>.
   */
  public static void setSelected( final JCheckBox aCheckBox, final Object aValue )
  {
    if ( aCheckBox == null )
    {
      throw new IllegalArgumentException( "CheckBox cannot be null!" );
    }

    if ( aValue != null )
    {
      aCheckBox.setSelected( "true".equalsIgnoreCase( String.valueOf( aValue ) ) );
    }
  }

  /**
   * Sets the selected item of the given combobox to the value given, unless
   * this value is <code>null</code>.
   * 
   * @param aComboBox
   *          the combobox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>.
   */
  public static void setSelectedItem( final JComboBox aComboBox, final Object aValue )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }

    if ( aValue != null )
    {
      aComboBox.setSelectedItem( aValue );
    }
  }

  /**
   * Sets the selected item of the given combobox to the value given, unless
   * this value is <code>null</code> in which case a default value is set.
   * 
   * @param aComboBox
   *          the combobox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>;
   * @param aDefault
   *          the default value to set in case the given value was
   *          <code>null</code>.
   */
  public static void setSelectedItem( final JComboBox aComboBox, final Object aValue, final Object aDefault )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }
    if ( aDefault == null )
    {
      throw new IllegalArgumentException( "Default value cannot be null!" );
    }

    if ( aValue != null )
    {
      aComboBox.setSelectedItem( aValue );
    }
    else
    {
      aComboBox.setSelectedItem( aDefault );
    }
  }

  /**
   * Shows a file-open selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileOpenDialog( final Window aOwner,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    return showFileOpenDialog( aOwner, null, aFileFilters );
  }

  /**
   * Shows a file-open selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in;
   * @param aCurrentDirectory
   *          the working directory to start the dialog in, can be
   *          <code>null</code>.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileOpenDialog( final Window aOwner, final String aCurrentDirectory,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    if ( HostUtils.isMacOSX() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner, "Open file", FileDialog.LOAD );
      }
      else
      {
        dialog = new FileDialog( ( Frame )aOwner, "Open file", FileDialog.LOAD );
      }
      if ( aCurrentDirectory != null )
      {
        dialog.setDirectory( aCurrentDirectory );
      }

      dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );

      dialog.setVisible( true );
      final String selectedFile = dialog.getFile();
      return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
    }
    else
    {
      final JFileChooser dialog = new JFileChooser();
      if ( aCurrentDirectory != null )
      {
        dialog.setCurrentDirectory( new File( aCurrentDirectory ) );
      }

      for ( javax.swing.filechooser.FileFilter filter : aFileFilters )
      {
        dialog.addChoosableFileFilter( filter );
      }

      if ( dialog.showOpenDialog( aOwner ) == JFileChooser.APPROVE_OPTION )
      {
        return dialog.getSelectedFile();
      }

      return null;
    }
  }

  /**
   * Shows a file-save selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileSaveDialog( final Window aOwner,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    return showFileSaveDialog( aOwner, null, aFileFilters );
  }

  /**
   * Shows a file-save selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in;
   * @param aCurrentDirectory
   *          the working directory to start the dialog in, can be
   *          <code>null</code>.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileSaveDialog( final Window aOwner, final String aCurrentDirectory,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    if ( HostUtils.isMacOSX() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner, "Save file", FileDialog.SAVE );
      }
      else
      {
        dialog = new FileDialog( ( Frame )aOwner, "Save file", FileDialog.SAVE );
      }
      if ( aCurrentDirectory != null )
      {
        dialog.setDirectory( aCurrentDirectory );
      }

      dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );

      dialog.setVisible( true );
      final String selectedFile = dialog.getFile();
      return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
    }
    else
    {
      final JFileChooser dialog = new JFileChooser();
      if ( aCurrentDirectory != null )
      {
        dialog.setCurrentDirectory( new File( aCurrentDirectory ) );
      }

      for ( javax.swing.filechooser.FileFilter filter : aFileFilters )
      {
        dialog.addChoosableFileFilter( filter );
      }

      if ( dialog.showSaveDialog( aOwner ) == JFileChooser.APPROVE_OPTION )
      {
        return dialog.getSelectedFile();
      }

      return null;
    }
  }

  /**
   * Shows a file selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileSelectionDialog( final Window aOwner,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    return showFileSelectionDialog( aOwner, null, aFileFilters );
  }

  /**
   * Shows a file selection dialog for the current working directory.
   * 
   * @param aOwner
   *          the owning window to show the dialog in;
   * @param aCurrentDirectory
   *          the working directory to start the dialog in, can be
   *          <code>null</code>.
   * @return the selected file, or <code>null</code> if the user aborted the
   *         dialog.
   */
  public static final File showFileSelectionDialog( final Window aOwner, final String aCurrentDirectory,
      final javax.swing.filechooser.FileFilter... aFileFilters )
  {
    if ( HostUtils.isMacOSX() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner );
      }
      else
      {
        dialog = new FileDialog( ( Frame )aOwner );
      }
      if ( aCurrentDirectory != null )
      {
        dialog.setDirectory( aCurrentDirectory );
      }

      dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );

      dialog.setVisible( true );
      final String selectedFile = dialog.getFile();
      return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
    }
    else
    {
      final JFileChooser dialog = new JFileChooser();
      if ( aCurrentDirectory != null )
      {
        dialog.setCurrentDirectory( new File( aCurrentDirectory ) );
      }

      for ( javax.swing.filechooser.FileFilter filter : aFileFilters )
      {
        dialog.addChoosableFileFilter( filter );
      }

      dialog.setVisible( true );
      return dialog.getSelectedFile();
    }
  }
}
