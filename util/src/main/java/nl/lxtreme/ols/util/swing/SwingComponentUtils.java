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

import org.osgi.service.prefs.*;


/**
 * Provides some utility methods for use with Swing components.
 */
public final class SwingComponentUtils
{
  // CONSTANTS

  private static final int DIALOG_PADDING = 6;

  private static final int BUTTONS_PADDING_TOP = 3;
  private static final int BUTTONS_PADDING_BOTTOM = 3;
  private static final int BUTTONS_PADDING_LEFT = 0;
  private static final int BUTTONS_PADDING_RIGHT = 0;

  private static final int BUTTONS_SPACING_DEFAULT = 8;

  public static final int LEFT_FACING = 1;
  public static final int RIGHT_FACING = -1;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SwingComponentUtils} instance, never used.
   */
  private SwingComponentUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates a button pane in which the given buttons are neatly aligned with
   * proper spacings.
   * 
   * @param aDefaultButton
   *          the default button, will be placed as last button;
   * @param aButtons
   *          the other buttons to add to the created button pane, will be added
   *          in the given order.
   * @return the button pane, never <code>null</code>.
   */
  public static JComponent createButtonPane( final JButton aDefaultButton, final JButton... aButtons )
  {
    return createButtonPane( new JButton[] { aDefaultButton }, aButtons );
  }

  /**
   * Creates a button pane in which the given buttons are neatly aligned with
   * proper spacings.
   * 
   * @param aDefaultButton
   *          the default button, will be placed as last button;
   * @param aButtons
   *          the other buttons to add to the created button pane, will be added
   *          in the given order.
   * @return the button pane, never <code>null</code>.
   */
  public static JComponent createButtonPane( final JButton[] aDefaultButtons, final JButton... aButtons )
  {
    if ( ( aDefaultButtons == null ) || ( aDefaultButtons.length < 1 ) )
    {
      throw new IllegalArgumentException( "Need at least one default button!" );
    }

    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );
    buttonPane.setBorder( BorderFactory.createEmptyBorder( BUTTONS_PADDING_TOP, BUTTONS_PADDING_LEFT,
        BUTTONS_PADDING_BOTTOM, BUTTONS_PADDING_RIGHT ) );

    buttonPane.add( Box.createHorizontalGlue() );

    int width = 1;
    int height = 1;

    for ( final JButton button : aDefaultButtons )
    {
      width = Math.max( width, button.getPreferredSize().width );
      height = Math.max( height, button.getPreferredSize().height );
    }

    if ( ( aButtons != null ) && ( aButtons.length > 0 ) )
    {
      for ( JButton button : aButtons )
      {
        width = Math.max( width, button.getPreferredSize().width );

        buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );
        buttonPane.add( button );
      }

      buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );

      final Dimension newDims = new Dimension( width, height );
      for ( JButton button : aButtons )
      {
        button.setPreferredSize( newDims );
      }
    }

    for ( final JButton button : aDefaultButtons )
    {
      buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );

      button.setPreferredSize( new Dimension( width, height ) );

      // Add the default button as last one...
      buttonPane.add( button );
    }

    return buttonPane;
  }

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
   * Creates a JLabel which text is right aligned.
   * 
   * @param aText
   *          the text of the JLabel to create, may be <code>null</code>.
   * @return a JLabel instance, never <code>null</code>.
   */
  public static final JLabel createRightAlignedLabel( final String aText )
  {
    final JLabel label = new JLabel( aText );
    label.setHorizontalAlignment( SwingConstants.RIGHT );
    return label;
  }

  /**
   * Draws a single arrow head
   * 
   * @param aG
   *          the canvas to draw on;
   * @param aXpos
   *          the X position of the arrow head;
   * @param aYpos
   *          the (center) Y position of the arrow head;
   * @param aFactor
   *          +1 to have a left-facing arrow head, -1 to have a right-facing
   *          arrow head;
   * @param aArrowWidth
   *          the total width of the arrow head;
   * @param aArrowHeight
   *          the total height of the arrow head.
   */
  public static final void drawArrowHead( final Graphics2D aG, final int aXpos, final int aYpos, final int aFactor,
      final int aArrowWidth, final int aArrowHeight )
  {
    final double halfHeight = aArrowHeight / 2.0;
    final int x1 = aXpos + ( aFactor * aArrowWidth );
    final int y1 = ( int )Math.ceil( aYpos - halfHeight );
    final int y2 = ( int )Math.floor( aYpos + halfHeight );

    final Polygon arrowHead = new Polygon();
    arrowHead.addPoint( aXpos, aYpos );
    arrowHead.addPoint( x1, y1 );
    arrowHead.addPoint( x1, y2 );

    aG.fill( arrowHead );
  }

  /**
   * Draws a double headed arrow of 8x8.
   * 
   * @param aG
   *          the canvas to draw on;
   * @param aX1
   *          the starting X position of the arrow;
   * @param aY
   *          the starting Y position of the arrow;
   * @param aX2
   *          the ending X position of the arrow.
   */
  public static final void drawDoubleHeadedArrow( final Graphics aG, final int aX1, final int aY, final int aX2 )
  {
    drawDoubleHeadedArrow( aG, aX1, aY, aX2, aY );
  }

  /**
   * Draws a double headed arrow of 8x8.
   * 
   * @param aG
   *          the canvas to draw on;
   * @param aX1
   *          the starting X position of the arrow;
   * @param aY1
   *          the starting Y position of the arrow;
   * @param aX2
   *          the ending X position of the arrow;
   * @param aY2
   *          the ending Y position of the arrow.
   */
  public static final void drawDoubleHeadedArrow( final Graphics aG, final int aX1, final int aY1, final int aX2,
      final int aY2 )
  {
    drawDoubleHeadedArrow( aG, aX1, aY1, aX2, aY2, 8, 8 );
  }

  /**
   * Draws a double headed arrow with arrow heads of a given width and height.
   * 
   * @param aG
   *          the canvas to draw on;
   * @param aX1
   *          the starting X position of the arrow;
   * @param aY1
   *          the starting Y position of the arrow;
   * @param aX2
   *          the ending X position of the arrow;
   * @param aY2
   *          the ending Y position of the arrow;
   * @param aArrowWidth
   *          the total width of the arrow head;
   * @param aArrowHeight
   *          the total height of the arrow head.
   */
  public static final void drawDoubleHeadedArrow( final Graphics aG, final int aX1, final int aY1, final int aX2,
      final int aY2, final int aArrowWidth, final int aArrowHeight )
  {
    final Graphics2D g2d = ( Graphics2D )aG.create();

    final int lineWidth = Math.abs( aX2 - aX1 );
    final int threshold = ( 2 * aArrowWidth ) + 2;
    try
    {
      int x1 = aX1;
      int x2 = aX2;

      if ( lineWidth > threshold )
      {
        drawArrowHead( g2d, aX1, aY1, LEFT_FACING, aArrowWidth, aArrowHeight );
        // why x2 needs to be shifted by one pixel is beyond me...
        drawArrowHead( g2d, aX2 + 1, aY2, RIGHT_FACING, aArrowWidth, aArrowHeight );

        x1 += aArrowWidth - 1;
        x2 -= aArrowWidth + 1;
      }

      g2d.drawLine( x1, aY1, x2, aY2 );
    }
    finally
    {
      g2d.dispose();
    }
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
  public static void loadWindowState( final Preferences aProperties, final Window aWindow )
  {
    // Special case: for FileDialog/JFileChooser we also should restore the
    // properties...
    loadFileDialogState( aProperties, aWindow );

    try
    {
      final int xPos = aProperties.getInt( "winXpos", -1 );
      final int yPos = aProperties.getInt( "winYpos", -1 );
      if ( ( xPos >= 0 ) && ( yPos >= 0 ) )
      {
        aWindow.setLocation( xPos, yPos );
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
      final int width = aProperties.getInt( "winWidth", -1 );
      final int height = aProperties.getInt( "winHeight", -1 );
      if ( ( width >= 0 ) && ( height >= 0 ) )
      {
        aWindow.setSize( width, height );
      }
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }
  }

  /**
   * Parses the given color-string into a valid Color instance.
   * <p>
   * A color-string has the following form: <tt>[#]rrggbb</tt> where <tt>rr</tt>, <tt>gg</tt> and <tt>bb</tt> are the hexadecimal color values for red,
   * green and blue. The string may optionally start with a hashpound sign.
   * </p>
   * 
   * @param aColor
   *          the color string to parse as color, cannot be <code>null</code>.
   * @return the Color-instance matching the given color, never
   *         <code>null</code>.
   */
  public static final Color parseColor( final String aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Color cannot be null!" );
    }

    String color = aColor.trim();
    if ( color.startsWith( "#" ) )
    {
      color = color.substring( 1 );
    }

    try
    {
      final int colorValue = Integer.parseInt( color, 16 );
      return new Color( ( colorValue >> 16 ) & 0xFF, ( colorValue >> 8 ) & 0xFF, colorValue & 0xFF );
    }
    catch ( NumberFormatException exception )
    {
      throw new IllegalArgumentException( "Given string does NOT represent a valid color!" );
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
  public static void saveWindowState( final Preferences aProperties, final Window aWindow )
  {
    // Special case: for FileDialog/JFileChooser we also store the properties...
    saveFileDialogState( aProperties, aWindow );

    final Point location = aWindow.getLocation();
    aProperties.put( "winXpos", Integer.toString( location.x ) );
    aProperties.put( "winYpos", Integer.toString( location.y ) );

    if ( isNonResizableWindow( aWindow ) )
    {
      // In case the window cannot be resized, don't restore its width &
      // height...
      return;
    }

    final Dimension dims = aWindow.getSize();
    aProperties.put( "winWidth", Integer.toString( dims.width ) );
    aProperties.put( "winHeight", Integer.toString( dims.height ) );
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
  public static void setSelectedIndex( final JComboBox aComboBox, final int aIndex )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }

    if ( aIndex >= 0 )
    {
      aComboBox.setSelectedIndex( aIndex );
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
   * Sets up the given dialog's content pane by setting its border to provide a
   * good default spacer, and setting the content pane to the given components.
   * 
   * @param aDialog
   *          the dialog to setup, cannot be <code>null</code>;
   * @param aCenterComponent
   *          the component that should appear at the center of the dialog;
   * @param aButtonPane
   *          the component that should appear at the bottom of the dialog
   *          (typically the buttons).
   */
  public static void setupDialogContentPane( final JDialog aDialog, //
      final Component aCenterComponent, final Component aButtonPane )
  {
    final JPanel contentPane = new JPanel( new BorderLayout() );
    contentPane.setBorder( BorderFactory.createEmptyBorder( DIALOG_PADDING, DIALOG_PADDING, //
        DIALOG_PADDING, DIALOG_PADDING ) );

    contentPane.add( aCenterComponent, BorderLayout.CENTER );
    contentPane.add( aButtonPane, BorderLayout.PAGE_END );

    aDialog.setContentPane( contentPane );
    aDialog.pack();
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

      if ( ( aFileFilters != null ) && ( aFileFilters.length > 0 ) )
      {
        dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );
      }

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

      if ( ( aFileFilters != null ) && ( aFileFilters.length > 0 ) )
      {
        dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );
      }

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

      if ( ( aFileFilters != null ) && ( aFileFilters.length > 0 ) )
      {
        dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );
      }

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
   * Returns the given color instance as a string.
   * 
   * @param aColor
   *          the color to return as a string value, cannot be <code>null</code>
   *          .
   * @return the string representing the given color.
   * @see #parseColor(String)
   */
  public static final String toString( final Color aColor )
  {
    final StringBuilder sb = new StringBuilder();
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getRed() ) ) );
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getGreen() ) ) );
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getBlue() ) ) );
    return sb.toString();
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
  private static void loadFileDialogState( final Preferences aProperties, final Window aWindow )
  {
    final String propKey = "lastDirectory";

    if ( aWindow instanceof FileDialog )
    {
      final String dir = aProperties.get( propKey, null );
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
        final String dir = aProperties.get( propKey, null );
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
  private static void saveFileDialogState( final Preferences aProperties, final Window aWindow )
  {
    final String propKey = "lastDirectory";

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
