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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing;


import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.StandardActionFactory.*;

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
   * Asks the user for confirmation.
   * 
   * @param aWindow
   *          the parent window of the confirmation dialog;
   * @param aMessage
   *          the message to display in the confirmation dialog.
   * @return <code>true</code> if the user acknowledged the confirmation,
   *         <code>false</code> otherwise.
   */
  public static boolean askConfirmation( final Window aWindow, final String aMessage )
  {
    return askConfirmation( aWindow, aMessage, "Continue?" );
  }

  /**
   * Asks the user for confirmation.
   * 
   * @param aWindow
   *          the parent window of the confirmation dialog;
   * @param aMessage
   *          the message to display in the confirmation dialog;
   * @param aTitle
   *          the title to display in the confirmation dialog.
   * @return <code>true</code> if the user acknowledged the confirmation,
   *         <code>false</code> otherwise.
   */
  public static boolean askConfirmation( final Window aWindow, final String aMessage, final String aTitle )
  {
    return JOptionPane.showConfirmDialog( aWindow, aMessage, aTitle, JOptionPane.YES_NO_OPTION,
        JOptionPane.WARNING_MESSAGE ) == JOptionPane.YES_OPTION;
  }

  /**
   * Creates a button pane in which the given buttons are neatly aligned with
   * proper spacings.
   * 
   * @param aButtons
   *          the buttons to add to the created button pane, will be added in
   *          the given order.
   * @return the button pane, never <code>null</code>.
   */
  public static JComponent createButtonPane( final JButton... aButtons )
  {
    if ( ( aButtons == null ) || ( aButtons.length < 1 ) )
    {
      throw new IllegalArgumentException( "Need at least one button!" );
    }

    // we want equally sized buttons, so we are recording preferred sizes while
    // adding the buttons and set them to the maximum afterwards...
    int width = 1;
    int height = 1;

    ArrayList<JButton> buttons = new ArrayList<JButton>();
    ArrayList<JButton> stdButtons = new ArrayList<JButton>();

    for ( JButton button : aButtons )
    {
      width = Math.max( width, button.getPreferredSize().width );
      height = Math.max( height, button.getPreferredSize().height );

      Action action = button.getAction();
      if ( ( action != null )
          && ( ( action instanceof CloseAction ) || ( action instanceof OkAction ) || ( action instanceof CancelAction ) ) )
      {
        stdButtons.add( button );
      }
      else
      {
        buttons.add( button );
      }
    }

    final Dimension newDims = new Dimension( width, height );

    final JPanel buttonPane = new JPanel();
    buttonPane.setLayout( new BoxLayout( buttonPane, BoxLayout.LINE_AXIS ) );
    buttonPane.setBorder( BorderFactory.createEmptyBorder( BUTTONS_PADDING_TOP, BUTTONS_PADDING_LEFT,
        BUTTONS_PADDING_BOTTOM, BUTTONS_PADDING_RIGHT ) );

    buttonPane.add( Box.createHorizontalGlue() );

    // everything added; let's set all sizes
    for ( final JButton button : buttons )
    {
      button.setPreferredSize( newDims );

      buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );
      buttonPane.add( button );
    }

    // everything added; let's set all sizes
    for ( final JButton button : stdButtons )
    {
      button.setPreferredSize( newDims );

      buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );
      buttonPane.add( button );
    }

    buttonPane.add( Box.createHorizontalStrut( BUTTONS_SPACING_DEFAULT ) );

    return buttonPane;
  }

  /**
   * Creates a channel selector combobox, where only a valid channel can be
   * selected.
   * 
   * @param aChannelCount
   *          the number of channels to include in the combobox options;
   * @return a combobox with channel selector options.
   */
  public static JComboBox createChannelSelector( final int aChannelCount )
  {
    return internalCreateChannelSelector( aChannelCount, -1, false /* aAddUnusedOption */);
  }

  /**
   * Creates a channel selector combobox, where only a valid channel can be
   * selected.
   * 
   * @param aChannelCount
   *          the number of channels to include in the combobox options;
   * @param aDefaultSelectedIndex
   *          the default selected index for the created combobox.
   * @return a combobox with channel selector options.
   */
  public static JComboBox createChannelSelector( final int aChannelCount, final int aDefaultSelectedIndex )
  {
    return internalCreateChannelSelector( aChannelCount, aDefaultSelectedIndex, false /* aAddUnusedOption */);
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
   * Creates a channel selector combobox, where optionally a channel can be
   * selected.
   * 
   * @param aChannelCount
   *          the number of channels to include in the combobox options.
   * @return a combobox with channel selector options.
   */
  public static JComboBox createOptionalChannelSelector( final int aChannelCount )
  {
    return internalCreateChannelSelector( aChannelCount, -1, true /* aAddUnusedOption */);
  }

  /**
   * Creates a channel selector combobox, where optionally a channel can be
   * selected.
   * 
   * @param aChannelCount
   *          the number of channels to include in the combobox options;
   * @param aDefaultSelectedIndex
   *          the default selected index for the created combobox.
   * @return a combobox with channel selector options.
   */
  public static JComboBox createOptionalChannelSelector( final int aChannelCount, final int aDefaultSelectedIndex )
  {
    return internalCreateChannelSelector( aChannelCount, aDefaultSelectedIndex, true /* aAddUnusedOption */);
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
   * Closes and disposes a given {@link Window}.
   * 
   * @param aWindow
   *          the window to close, if <code>null</code>, this method doesn't do
   *          anything.
   */
  public static void dispose( final Window aWindow )
  {
    if ( aWindow == null )
    {
      return;
    }
    if ( aWindow.isVisible() )
    {
      aWindow.setVisible( false );
    }
    aWindow.dispose();
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
   * Convenience method for searching above the given component in the component
   * hierarchy and returns the first object of the given type it finds, or
   * <code>null</code> if no such parent was found.
   * <p>
   * The reason this method exists is for tidyness of the calling code, as no
   * longer a explicit cast is needed.
   * 
   * @param aType
   *          the type of the parent to find, cannot be <code>null</code>;
   * @param aComponent
   *          the component to search in the hierarchy, cannot be
   *          <code>null</code>.
   * @return the requested ancestor, or <code>null</code> if not found.
   * @see SwingUtilities#getAncestorOfClass(Class, Component)
   */
  @SuppressWarnings( "unchecked" )
  public static <T> T getAncestorOfClass( final Class<T> aType, final Component aComponent )
  {
    if ( ( aComponent == null ) || ( aType == null ) )
    {
      return null;
    }

    Container parent = aComponent.getParent();
    while ( ( parent != null ) && !( aType.isInstance( parent ) ) )
    {
      parent = parent.getParent();
    }

    return ( T )parent;
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
   * Returns the deepest visible descendent Component of <code>parent</code>
   * that contains the location <code>x</code>, <code>y</code>. If
   * <code>parent</code> does not contain the specified location, then
   * <code>null</code> is returned. If <code>parent</code> is not a container,
   * or none of <code>parent</code>'s visible descendents contain the specified
   * location, <code>parent</code> is returned.
   * 
   * @param aParent
   *          the root component to begin the search
   * @param aXpos
   *          the x target location
   * @param aYpos
   *          the y target location
   */
  public static JComponent getDeepestComponentAt( final Component aParent, final int aXpos, final int aYpos )
  {
    return ( JComponent )SwingUtilities.getDeepestComponentAt( aParent, aXpos, aYpos );
  }

  /**
   * Returns the deepest visible descendent Component of <code>parent</code>
   * that contains the location <code>x</code>, <code>y</code>. If
   * <code>parent</code> does not contain the specified location, then
   * <code>null</code> is returned. If <code>parent</code> is not a container,
   * or none of <code>parent</code>'s visible descendents contain the specified
   * location, <code>parent</code> is returned.
   * 
   * @param aParent
   *          the root component to begin the search
   * @param aXpos
   *          the x target location
   * @param aYpos
   *          the y target location
   */
  public static JComponent getDeepestComponentAt( final MouseEvent aEvent )
  {
    return getDeepestComponentAt( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
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
   * Returns the string width for a given {@link Font} and string.
   * 
   * @param aFont
   *          the font to create the string width;
   * @param aString
   *          the string to get the width for.
   * @return a string width, >= 0.
   */
  public static int getStringWidth( final Font aFont, final String aString )
  {
    final FontMetrics frc = createFontMetrics( aFont );
    return SwingUtilities.computeStringWidth( frc, aString );
  }

  /**
   * Returns the string width for the default label font and string.
   * 
   * @param aString
   *          the string to get the width for.
   * @return a string width, >= 0.
   */
  public static int getStringWidth( final String aString )
  {
    return getStringWidth( UIManager.getFont( "Label.font" ), aString );
  }

  /**
   * Similar as to {@link SwingUtilities#invokeLater(Runnable)}, but does a
   * check first if the current running thread is already the EDT. If so, it
   * will directly call the {@link Runnable#run()} method, otherwise leave it up
   * to {@link SwingUtilities#invokeLater(Runnable)} to invoke it on a later
   * moment.
   * 
   * @param aRunnable
   *          the runnable to call on the EDT, cannot be <code>null</code>.
   */
  public static void invokeOnEDT( final Runnable aRunnable )
  {
    // System.out.println( "invokeOnEDT called with " + aRunnable );
    if ( SwingUtilities.isEventDispatchThread() )
    {
      aRunnable.run();
    }
    else
    {
      SwingUtilities.invokeLater( aRunnable );
    }
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
      final int width = aProperties.getInt( "winWidth", -1 );
      final int height = aProperties.getInt( "winHeight", -1 );

      if ( ( xPos >= 0 ) && ( yPos >= 0 ) && ( width >= 0 ) && ( height >= 0 ) )
      {
        aWindow.setBounds( xPos, yPos, width, height );
      }
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }
  }

  /**
   * Registers a given keystroke to invoke a given action on the given
   * component.
   * 
   * @param aComponent
   *          the component to register the keystroke for;
   * @param aKeyStroke
   *          the keystroke (as plain char) to register;
   * @param aAction
   *          the action to invoke when the keystroke is typed.
   */
  public static void registerKeyBinding( final JComponent aComponent, final char aKey, final Action aAction )
  {
    registerKeyBinding( aComponent, KeyStroke.getKeyStroke( aKey ), aAction );
  }

  /**
   * Registers a given keystroke to invoke a given action on the given
   * component.
   * 
   * @param aComponent
   *          the component to register the keystroke for;
   * @param aKeyStroke
   *          the keystroke to register;
   * @param aAction
   *          the action to invoke when the keystroke is typed.
   */
  public static void registerKeyBinding( final JComponent aComponent, final KeyStroke aKeyStroke, final Action aAction )
  {
    final String name = "KeyBinding.".concat( aKeyStroke.toString() );
    aComponent.getInputMap( JComponent.WHEN_IN_FOCUSED_WINDOW ).put( aKeyStroke, name );
    aComponent.getActionMap().put( name, aAction );
  }

  /**
   * Registers a given keystroke to invoke a given action on the given
   * component.
   * 
   * @param aComponent
   *          the component to register the keystroke for;
   * @param aKeyStroke
   *          the keystroke (as String) to register;
   * @param aAction
   *          the action to invoke when the keystroke is typed.
   */
  public static void registerKeyBinding( final JComponent aComponent, final String aKeyStroke, final Action aAction )
  {
    registerKeyBinding( aComponent, KeyStroke.getKeyStroke( aKeyStroke ), aAction );
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
   * @param aProperties
   *          the properties to fill;
   * @param aWindow
   *          the window to save the state for.
   */
  public static void saveWindowState( final Preferences aProperties, final Window aWindow )
  {
    // Special case: for FileDialog/JFileChooser we also store the properties...
    saveFileDialogState( aProperties, aWindow );

    final Rectangle bounds = aWindow.getBounds();
    aProperties.put( "winXpos", Integer.toString( bounds.x ) );
    aProperties.put( "winYpos", Integer.toString( bounds.y ) );
    aProperties.put( "winWidth", Integer.toString( bounds.width ) );
    aProperties.put( "winHeight", Integer.toString( bounds.height ) );
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
  public static void setupWindowContentPane( final Window aDialog, //
      final Component aCenterComponent, final Component aButtonPane )
  {
    setupWindowContentPane( aDialog, aCenterComponent, aButtonPane, null );
  }

  /**
   * Sets up the given window content pane by setting its border to provide a
   * good default spacer, and setting the content pane to the given components.
   * 
   * @param aWindow
   *          the window to setup, cannot be <code>null</code>;
   * @param aCenterComponent
   *          the component that should appear at the center of the dialog;
   * @param aButtonPane
   *          the component that should appear at the bottom of the dialog
   * @param defaultButton
   *          the default button for this dialog; can be null for "none".
   * @see javax.swing.JRootPane#setDefaultButton(javax.swing.JButton)
   */
  public static void setupWindowContentPane( final Window aWindow, final Component aCenterComponent,
      final Component aButtonPane, final JButton defaultButton )
  {
    final JPanel contentPane = new JPanel( new BorderLayout() );
    contentPane.setBorder( BorderFactory.createEmptyBorder( DIALOG_PADDING, DIALOG_PADDING, //
        DIALOG_PADDING, DIALOG_PADDING ) );

    contentPane.add( aCenterComponent, BorderLayout.CENTER );
    contentPane.add( aButtonPane, BorderLayout.PAGE_END );

    if ( aWindow instanceof JDialog )
    {
      ( ( JDialog )aWindow ).setContentPane( contentPane );
      ( ( JDialog )aWindow ).getRootPane().setDefaultButton( defaultButton );
    }
    else if ( aWindow instanceof JFrame )
    {
      ( ( JFrame )aWindow ).setContentPane( contentPane );
      ( ( JFrame )aWindow ).getRootPane().setDefaultButton( defaultButton );
    }
    aWindow.pack();
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
    final String currentWorkingDir = null; // System.getProperty( "user.dir" );
    return showFileOpenDialog( aOwner, currentWorkingDir, aFileFilters );
  }

  /**
   * Shows a file-open selection dialog for the given working directory.
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
    if ( isMacOS() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner, "Open file", FileDialog.LOAD );
      }
      else if ( aOwner instanceof Frame )
      {
        dialog = new FileDialog( ( Frame )aOwner, "Open file", FileDialog.LOAD );
      }
      else
      {
        throw new InternalError( "Unknown owner window type!" );
      }
      dialog.setDirectory( aCurrentDirectory );

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
      dialog.setCurrentDirectory( ( aCurrentDirectory == null ) ? null : new File( aCurrentDirectory ) );

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
    final String currentWorkingDir = System.getProperty( "user.dir" );
    return showFileSaveDialog( aOwner, currentWorkingDir, aFileFilters );
  }

  /**
   * Shows a file-save selection dialog for the given working directory.
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
    if ( isMacOS() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner, "Save file", FileDialog.SAVE );
      }
      else if ( aOwner instanceof Frame )
      {
        dialog = new FileDialog( ( Frame )aOwner, "Save file", FileDialog.SAVE );
      }
      else
      {
        throw new InternalError( "Unknown owner window type!" );
      }
      dialog.setDirectory( aCurrentDirectory );

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
      dialog.setCurrentDirectory( ( aCurrentDirectory == null ) ? null : new File( aCurrentDirectory ) );

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
    final String currentWorkingDir = System.getProperty( "user.dir" );
    return showFileSelectionDialog( aOwner, currentWorkingDir, aFileFilters );
  }

  /**
   * Shows a file selection dialog for the given working directory.
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
    if ( isMacOS() )
    {
      final FileDialog dialog;
      if ( aOwner instanceof Dialog )
      {
        dialog = new FileDialog( ( Dialog )aOwner );
      }
      else if ( aOwner instanceof Frame )
      {
        dialog = new FileDialog( ( Frame )aOwner );
      }
      else
      {
        throw new InternalError( "Unknown owner window type!" );
      }
      dialog.setDirectory( aCurrentDirectory );

      if ( ( aFileFilters != null ) && ( aFileFilters.length > 0 ) )
      {
        dialog.setFilenameFilter( new FilenameFilterAdapter( aFileFilters ) );
      }

      try
      {
        dialog.setVisible( true );
        final String selectedFile = dialog.getFile();
        return ( selectedFile == null ) ? null : new File( dialog.getDirectory(), selectedFile );
      }
      finally
      {
        dialog.dispose();
      }
    }
    else
    {
      final JFileChooser dialog = new JFileChooser();
      dialog.setCurrentDirectory( ( aCurrentDirectory == null ) ? null : new File( aCurrentDirectory ) );

      for ( javax.swing.filechooser.FileFilter filter : aFileFilters )
      {
        dialog.addChoosableFileFilter( filter );
      }

      dialog.setVisible( true );
      return dialog.getSelectedFile();
    }
  }

  /**
   * Converts a given font to a font-clause that can be used in a CSS-file.
   * 
   * @param aFont
   *          the font convert to CSS, cannot be <code>null</code>.
   * @return a CSS clause for the given font, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given font was <code>null</code>.
   */
  public static String toCssString( final Font aFont )
  {
    if ( aFont == null )
    {
      throw new IllegalArgumentException( "Parameter Font cannot be null!" );
    }

    final StringBuilder sb = new StringBuilder( "font: " );
    if ( aFont.isItalic() )
    {
      sb.append( "italic " );
    }
    if ( aFont.isBold() )
    {
      sb.append( "bold " );
    }
    sb.append( aFont.getSize() ).append( "pt " );
    sb.append( '"' ).append( aFont.getFontName() ).append( "\", " );
    sb.append( '"' ).append( aFont.getPSName() ).append( "\";" );
    return sb.toString();
  }

  /**
   * Creates (in a rather clumsy way) the font metrics for a given {@link Font}.
   * 
   * @param aFont
   *          the font instance to create the font metrics for, cannot be
   *          <code>null</code>.
   * @return a font metrics, never <code>null</code>.
   */
  private static FontMetrics createFontMetrics( final Font aFont )
  {
    BufferedImage img = new BufferedImage( 1, 1, BufferedImage.TYPE_INT_ARGB );
    Graphics canvas = img.getGraphics();

    try
    {
      return canvas.getFontMetrics( aFont );
    }
    finally
    {
      canvas.dispose();
      canvas = null;
      img = null;
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
   * Creates a channel selector combobox.
   * 
   * @param aChannelCount
   *          the number of channels to include in the combobox options;
   * @param aDefaultSelectedindex
   *          the default selected index;
   * @param aAddUnusedOption
   *          <code>true</code> to add "unused" as first option,
   *          <code>false</code> to omit this option.
   * @return a combobox with channel selector options.
   */
  private static JComboBox internalCreateChannelSelector( final int aChannelCount, final int aDefaultSelectedindex,
      final boolean aAddUnusedOption )
  {
    int modelSize = Math.max( 0, Math.min( 32, aChannelCount ) );
    if ( aAddUnusedOption )
    {
      modelSize++;
    }

    final String dataChannels[] = new String[modelSize];

    int i = 0;
    if ( aAddUnusedOption )
    {
      dataChannels[i++] = "Unused";
    }
    for ( ; i < modelSize; i++ )
    {
      final int index = aAddUnusedOption ? i - 1 : i;
      dataChannels[i] = String.format( "Channel %d", Integer.valueOf( index ) );
    }

    int selectedIndex = aDefaultSelectedindex < 0 ? 0 : aDefaultSelectedindex % modelSize;

    final JComboBox result = new JComboBox( dataChannels );
    result.setSelectedIndex( selectedIndex );
    return result;
  }

  /**
   * Returns whether the current host's operating system is Mac OS.
   * 
   * @return <code>true</code> if running on Mac OS, <code>false</code>
   *         otherwise.
   */
  private static boolean isMacOS()
  {
    final String osName = System.getProperty( "os.name", "" ).toLowerCase();
    return osName.startsWith( "mac os" );
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
