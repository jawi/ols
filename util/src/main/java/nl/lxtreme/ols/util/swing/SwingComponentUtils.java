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
import java.util.*;

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
   * "Fixates" the preferred width of the given label to the given text.
   * 
   * @param aLabel
   *          the label to fixate, cannot be <code>null</code>;
   * @param aMinimalText
   *          the text to use as minimal width indicator.
   */
  public static final void fixLabelWidth( final JLabel aLabel, final String aMinimalText )
  {
    final FontMetrics fm = aLabel.getFontMetrics( aLabel.getFont() );
    final int height = fm.getHeight();

    aLabel.setPreferredSize( new Dimension( fm.stringWidth( aMinimalText ), height ) );
  }

  /**
   * Tries to find the current focused window.
   * 
   * @return the current focused window, or <code>null</code> if no such window
   *         could be found.
   */
  public static final Window getCurrentWindow()
  {
    Window owner;
    final KeyboardFocusManager kbdFocusManager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
    owner = kbdFocusManager.getFocusedWindow();
    if ( owner == null )
    {
      owner = kbdFocusManager.getActiveWindow();
    }
    return owner;
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
    if ( aEvent != null )
    {
      final Object source = aEvent.getSource();
      if ( source instanceof Component )
      {
        owner = getOwningWindow( ( Component )source );
      }
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
      owner = getCurrentWindow();
    }
    return owner;
  }

  /**
   * Returns whether the given component is "actively" shown in screen, that is,
   * it or any of its ancestors is focused.
   * 
   * @param aComponent
   *          the component to determine whether it is actively shown on screen,
   *          may be <code>null</code>.
   * @return <code>true</code> if the given component is actively shown,
   *         <code>false</code> otherwise.
   */
  public static final boolean isActivelyShown( final Component aComponent )
  {
    final KeyboardFocusManager kbdFocusManager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
    final Window owner = kbdFocusManager.getFocusedWindow();

    return ( ( aComponent != null ) && ( owner != null ) && ( ( owner == aComponent ) || owner
        .isAncestorOf( aComponent ) ) );
  }

  /**
   * Tries to load/restore the window state of the given window.
   * 
   * @param aNamespace
   *          the namespace to use for the window state;
   * @param aProperties
   *          the properties to read from;
   * @param aWindow
   *          the window to load the state for.
   */
  public static void loadWindowState( final String aNamespace, final Properties aProperties, final Window aWindow )
  {
    // Special case: for FileDialog/JFileChooser we also should restore the
    // properties...
    loadFileDialogState( aNamespace, aProperties, aWindow );

    try
    {
      final String xPos = aProperties.getProperty( aNamespace + ".xPos" );
      final String yPos = aProperties.getProperty( aNamespace + ".yPos" );
      if ( ( xPos != null ) && ( yPos != null ) )
      {
        aWindow.setLocation( Integer.valueOf( xPos ), Integer.valueOf( yPos ) );
      }
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }

    if ( isNonResizableWindow( aWindow ) )
    {
      // In case the window cannot be resized, don't restore its width &
      // height...
      return;
    }

    try
    {
      final String width = aProperties.getProperty( aNamespace + ".width" );
      final String height = aProperties.getProperty( aNamespace + ".height" );
      if ( ( width != null ) && ( height != null ) )
      {
        aWindow.setSize( Integer.valueOf( width ), Integer.valueOf( height ) );
      }
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }
  }

  /**
   * Registers the keystroke of the given action as "command" of the given
   * component.
   * <p>
   * This code is based on the Sulky-tools, found at
   * &lt;http://github.com/huxi/sulky&gt;.
   * </p>
   * 
   * @param aComponent
   *          the component that should react on the keystroke, cannot be
   *          <code>null</code>;
   * @param aAction
   *          the action of the keystroke, cannot be <code>null</code>;
   * @param aCommandName
   *          the name of the command to register the keystore under.
   */
  public static void registerKeystroke( final JComponent aComponent, final Action aAction, final String aCommandName )
  {
    final KeyStroke keyStroke = ( KeyStroke )aAction.getValue( Action.ACCELERATOR_KEY );
    if ( keyStroke == null )
    {
      return;
    }

    InputMap inputMap = aComponent.getInputMap( JComponent.WHEN_IN_FOCUSED_WINDOW );
    ActionMap actionMap = aComponent.getActionMap();
    inputMap.put( keyStroke, aCommandName );
    actionMap.put( aCommandName, aAction );

    inputMap = aComponent.getInputMap( JComponent.WHEN_FOCUSED );
    Object value = inputMap.get( keyStroke );
    if ( value != null )
    {
      inputMap.put( keyStroke, aCommandName );
    }

    inputMap = aComponent.getInputMap( JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT );
    value = inputMap.get( keyStroke );
    if ( value != null )
    {
      inputMap.put( keyStroke, aCommandName );
    }
  }

  /**
   * Saves the window state to the given properties map.
   * 
   * @param aNamespace
   *          the namespace to use for the window state;
   * @param aProperties
   *          the properties to fill;
   * @param aWindow
   *          the window to save the state for.
   */
  public static void saveWindowState( final String aNamespace, final Properties aProperties, final Window aWindow )
  {
    // Special case: for FileDialog/JFileChooser we also store the properties...
    saveFileDialogState( aNamespace, aProperties, aWindow );

    final Point location = aWindow.getLocation();
    aProperties.put( aNamespace + ".xPos", Integer.toString( location.x ) );
    aProperties.put( aNamespace + ".yPos", Integer.toString( location.y ) );

    if ( isNonResizableWindow( aWindow ) )
    {
      // In case the window cannot be resized, don't restore its width &
      // height...
      return;
    }

    final Dimension dims = aWindow.getSize();
    aProperties.put( aNamespace + ".width", Integer.toString( dims.width ) );
    aProperties.put( aNamespace + ".height", Integer.toString( dims.height ) );
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
      boolean value = false;
      if ( aValue instanceof Boolean )
      {
        value = ( ( Boolean )aValue ).booleanValue();
      }
      else
      {
        value = "true".equalsIgnoreCase( String.valueOf( aValue ) );
      }
      aCheckBox.setSelected( value );
    }
  }

  /**
   * Sets the selected item of the given combobox to the value given, unless
   * this index is <code>null</code>.
   * 
   * @param aComboBox
   *          the combobox to set, cannot be <code>null</code>;
   * @param aIndex
   *          the index to set, may be <code>null</code>.
   */
  public static void setSelectedIndex( final JComboBox aComboBox, final Object aIndex )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }

    if ( aIndex != null )
    {
      final int idx = NumberUtils.smartParseInt( String.valueOf( aIndex ) );
      aComboBox.setSelectedIndex( idx );
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

    if ( ( aValue != null ) && !"null".equals( aValue ) )
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

    if ( ( aValue != null ) && !"null".equals( aValue ) )
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

      try
      {
        dialog.setVisible( true );
        final String selectedFile = dialog.getFile();
        return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
      }
      finally
      {
        dialog.dispose();
      }
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

      File result = null;
      if ( dialog.showOpenDialog( aOwner ) == JFileChooser.APPROVE_OPTION )
      {
        result = dialog.getSelectedFile();
      }

      return result;
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

      try
      {
        dialog.setVisible( true );
        final String selectedFile = dialog.getFile();
        return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
      }
      finally
      {
        dialog.dispose();
      }
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

      try
      {
        dialog.setVisible( true );
        final String selectedFile = dialog.getFile();
        return selectedFile == null ? null : new File( dialog.getDirectory(), selectedFile );
      }
      finally
      {
        dialog.dispose();
      }
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

  /**
   * @param aContainer
   * @param aComponentClass
   * @return
   */
  private static Component findComponent( final Container aContainer, final Class<? extends Component> aComponentClass )
  {
    Component result = null;

    final int cnt = aContainer.getComponentCount();
    for ( int i = 0; ( result == null ) && ( i < cnt ); i++ )
    {
      final Component comp = aContainer.getComponent( i );
      if ( aComponentClass.equals( comp.getClass() ) )
      {
        result = comp;
      }
      else if ( comp instanceof Container )
      {
        result = findComponent( ( Container )comp, aComponentClass );
      }
    }
    return result;
  }

  /**
   * Returns whether the given window is non-resizable.
   * 
   * @param aWindow
   *          the window to determine whether it can be resized or not, may be
   *          <code>null</code>.
   * @return <code>true</code> if the given window is not resizable,
   *         <code>false</code> otherwise.
   */
  private static boolean isNonResizableWindow( final Window aWindow )
  {
    if ( aWindow instanceof Dialog )
    {
      if ( !( ( Dialog )aWindow ).isResizable() )
      {
        return true;
      }
    }

    if ( aWindow instanceof Frame )
    {
      if ( !( ( Frame )aWindow ).isResizable() )
      {
        return true;
      }
    }

    return false;
  }

  /**
   * Checks whether the given window is either a FileDialog, or contains a
   * JFileChooser component. If so, its current directory is stored in the given
   * properties.
   * 
   * @param aNamespace
   *          the name space to use;
   * @param aProperties
   *          the properties to store the found directory in;
   * @param aWindow
   *          the window to check for.
   */
  private static void loadFileDialogState( final String aNamespace, final Properties aProperties, final Window aWindow )
  {
    final String propKey = aNamespace + ".dir";

    if ( aWindow instanceof FileDialog )
    {
      final String dir = aProperties.getProperty( propKey );
      if ( dir != null )
      {
        ( ( FileDialog )aWindow ).setDirectory( dir );
      }
    }
    else if ( aWindow instanceof JDialog )
    {
      final Container contentPane = ( ( JDialog )aWindow ).getContentPane();
      final JFileChooser fileChooser = ( JFileChooser )findComponent( contentPane, JFileChooser.class );
      if ( fileChooser != null )
      {
        final String dir = aProperties.getProperty( propKey );
        if ( dir != null )
        {
          fileChooser.setCurrentDirectory( new File( dir ) );
        }
      }
    }
  }

  /**
   * Checks whether the given window is either a FileDialog, or contains a
   * JFileChooser component. If so, its current directory is restored from the
   * given properties.
   * 
   * @param aNamespace
   *          the name space to use;
   * @param aProperties
   *          the properties to get the directory from;
   * @param aWindow
   *          the window to check for.
   */
  private static void saveFileDialogState( final String aNamespace, final Properties aProperties, final Window aWindow )
  {
    final String propKey = aNamespace + ".dir";

    if ( aWindow instanceof FileDialog )
    {
      final String dir = ( ( FileDialog )aWindow ).getDirectory();
      if ( dir != null )
      {
        aProperties.put( propKey, dir );
      }
    }
    else if ( aWindow instanceof JDialog )
    {
      final Container contentPane = ( ( JDialog )aWindow ).getContentPane();
      final JFileChooser fileChooser = ( JFileChooser )findComponent( contentPane, JFileChooser.class );
      if ( fileChooser != null )
      {
        final File dir = fileChooser.getCurrentDirectory();
        if ( dir != null )
        {
          aProperties.put( propKey, dir.getAbsolutePath() );
        }
      }
    }
  }
}
