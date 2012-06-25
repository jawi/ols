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
package nl.lxtreme.ols.client;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;
import static nl.lxtreme.ols.util.swing.SpringLayoutUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides the preferences dialog.
 */
public class PreferencesDialog extends JDialog implements StatusAwareCloseableDialog
{
  // INNER TYPES

  /**
   * Provides a custom combobox model for the current color schemes.
   */
  final class ColorSchemeModel extends AbstractListModel implements ComboBoxModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private volatile int selected = -1;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      java.util.List<String> schemes = PreferencesDialog.this.colorSchemeManager.getColorSchemes();
      if ( ( aIndex < 0 ) || ( aIndex >= schemes.size() ) )
      {
        return null;
      }
      return schemes.get( aIndex );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getSelectedItem()
    {
      return getElementAt( this.selected );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSize()
    {
      return PreferencesDialog.this.colorSchemeManager.getColorSchemeCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      java.util.List<String> schemes = PreferencesDialog.this.colorSchemeManager.getColorSchemes();
      this.selected = schemes.indexOf( aItem );
    }
  }

  /**
   * Provides a combobox renderer for ColorScheme values.
   */
  static final class SignalAlignmentRenderer extends EnumItemRenderer<SignalAlignment>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    @Override
    protected String getDisplayValue( final SignalAlignment aValue )
    {
      switch ( aValue )
      {
        case BOTTOM:
          return "Bottom";
        case CENTER:
          return "Center";
        case TOP:
          return "Top";
      }
      return super.getDisplayValue( aValue );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final UIColorSchemeManager colorSchemeManager;

  private volatile boolean dialogResult;

  private JCheckBox mouseWheelZooms;
  private JCheckBox cursorSnapToEdge;
  private JCheckBox showGroupSummary;
  private JCheckBox showAnalogScope;
  private JCheckBox showToolWindows;
  private JCheckBox showChannelIndexes;
  private JComboBox annotationAlignment;
  private JComboBox signalAlignment;
  private JComboBox colorScheme;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PreferencesDialog} instance.
   * 
   * @param aParent
   *          the parent of the preferences window, can be <code>null</code>;
   * @param aColorSchemeManager
   *          the color scheme manager to use, cannot be <code>null</code>.
   */
  public PreferencesDialog( final Window aParent, final UIColorSchemeManager aColorSchemeManager )
  {
    super( aParent, "", ModalityType.APPLICATION_MODAL );

    this.colorSchemeManager = aColorSchemeManager;

    initDialog();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();

    if ( this.dialogResult )
    {
      applyNewPreferences();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean setDialogStatus( final DialogStatus aStatus )
  {
    this.dialogResult = ( aStatus == DialogStatus.OK );
    return true;
  }

  /**
   * Display the bundles dialog.
   * 
   * @return always <code>true</code>.
   */
  public boolean showDialog()
  {
    this.dialogResult = false;

    setVisible( true );

    return this.dialogResult;
  }

  /**
   * Applies all (new) preferences.
   */
  private void applyNewPreferences()
  {
    UIManager.put( MOUSEWHEEL_ZOOM_DEFAULT, Boolean.valueOf( this.mouseWheelZooms.isSelected() ) );
    UIManager.put( SNAP_CURSORS_DEFAULT, Boolean.valueOf( this.cursorSnapToEdge.isSelected() ) );
    UIManager.put( GROUP_SUMMARY_VISIBLE_DEFAULT, Boolean.valueOf( this.showGroupSummary.isSelected() ) );
    UIManager.put( ANALOG_SCOPE_VISIBLE_DEFAULT, Boolean.valueOf( this.showAnalogScope.isSelected() ) );
    UIManager.put( SHOW_TOOL_WINDOWS_DEFAULT, Boolean.valueOf( this.showToolWindows.isSelected() ) );
    UIManager.put( CHANNELLABELS_SHOW_CHANNEL_INDEX, Boolean.valueOf( this.showChannelIndexes.isSelected() ) );

    UIManager.put( SIGNALVIEW_SIGNAL_ALIGNMENT, this.signalAlignment.getSelectedItem().toString() );
    UIManager.put( SIGNALVIEW_ANNOTATION_ALIGNMENT, this.annotationAlignment.getSelectedItem().toString() );

    String colorScheme = ( String )this.colorScheme.getSelectedItem();
    if ( colorScheme != null )
    {
      UIManager.put( COLOR_SCHEME, colorScheme );

      Properties props = this.colorSchemeManager.getColorScheme( colorScheme );
      if ( props != null )
      {
        for ( Object key : props.keySet() )
        {
          Object value = props.get( key );
          UIManager.put( key, value );
        }
      }
    }
  }

  /**
   * @return a tab pane for the general settings, never <code>null</code>.
   */
  private JComponent createGeneralSettingsTab()
  {
    JPanel pane = new JPanel( new SpringLayout() );

    addSeparator( pane, "Look and feel defaults" );

    this.mouseWheelZooms = new JCheckBox();
    this.mouseWheelZooms
        .setToolTipText( "Whether the mouse wheel by default zooms in or out, or scrolls the view. Will be applied immediately." );
    this.mouseWheelZooms.setSelected( UIManager.getBoolean( MOUSEWHEEL_ZOOM_DEFAULT ) );

    pane.add( createRightAlignedLabel( "Mouse wheel zooms?" ) );
    pane.add( this.mouseWheelZooms );

    this.cursorSnapToEdge = new JCheckBox();
    this.cursorSnapToEdge
        .setToolTipText( "Whether or not cursors by default snap to signal edges. Will be applied immediately." );
    this.cursorSnapToEdge.setSelected( UIManager.getBoolean( SNAP_CURSORS_DEFAULT ) );

    pane.add( createRightAlignedLabel( "Snap cursor to edge?" ) );
    pane.add( this.cursorSnapToEdge );

    this.showGroupSummary = new JCheckBox();
    this.showGroupSummary
        .setToolTipText( "Whether or not the group (byte) summary is shown by default for each acquisition. Will be applied after an acquisition." );
    this.showGroupSummary.setSelected( UIManager.getBoolean( GROUP_SUMMARY_VISIBLE_DEFAULT ) );

    pane.add( createRightAlignedLabel( "Show group summary?" ) );
    pane.add( this.showGroupSummary );

    this.showAnalogScope = new JCheckBox();
    this.showAnalogScope
        .setToolTipText( "Whether or not the analog scope is shown by default for each acquisition. Will be applied after an acquisition." );
    this.showAnalogScope.setSelected( UIManager.getBoolean( ANALOG_SCOPE_VISIBLE_DEFAULT ) );

    pane.add( createRightAlignedLabel( "Show analog scope?" ) );
    pane.add( this.showAnalogScope );

    this.showChannelIndexes = new JCheckBox();
    this.showChannelIndexes
        .setToolTipText( "Whether or not channel indexes are shown beside the labels. Will be applied immediately." );
    this.showChannelIndexes.setSelected( UIManager.getBoolean( CHANNELLABELS_SHOW_CHANNEL_INDEX ) );

    pane.add( createRightAlignedLabel( "Show channel indexes?" ) );
    pane.add( this.showChannelIndexes );

    this.showToolWindows = new JCheckBox();
    this.showToolWindows
        .setToolTipText( "Whether or not the tool windows are shown by default. Will be applied after a restart." );
    this.showToolWindows.setSelected( UIManager.getBoolean( SHOW_TOOL_WINDOWS_DEFAULT ) );

    pane.add( createRightAlignedLabel( "Show tool windows?" ) );
    pane.add( this.showToolWindows );

    this.signalAlignment = new JComboBox( SignalAlignment.values() );
    this.signalAlignment.setSelectedItem( getSignalAlignment( SIGNALVIEW_SIGNAL_ALIGNMENT ) );
    this.signalAlignment
        .setToolTipText( "The vertical alignment of the signals itself. Will be applied after an acquisition." );
    this.signalAlignment.setRenderer( new SignalAlignmentRenderer() );

    pane.add( createRightAlignedLabel( "Signal alignment" ) );
    pane.add( this.signalAlignment );

    this.annotationAlignment = new JComboBox( SignalAlignment.values() );
    this.annotationAlignment.setSelectedItem( getSignalAlignment( SIGNALVIEW_ANNOTATION_ALIGNMENT ) );
    this.annotationAlignment.setToolTipText( "The vertical aligment of the annotations. Will be applied immediately." );
    this.annotationAlignment.setRenderer( new SignalAlignmentRenderer() );

    pane.add( createRightAlignedLabel( "Annotation alignment" ) );
    pane.add( this.annotationAlignment );

    addSeparator( pane, "Color scheme" );

    this.colorScheme = new JComboBox( new ColorSchemeModel() );
    this.colorScheme.setSelectedItem( UIManager.getString( COLOR_SCHEME ) );
    this.colorScheme.setToolTipText( "What color scheme is to be used. Will be applied immediately." );

    pane.add( createRightAlignedLabel( "Default scheme" ) );
    pane.add( this.colorScheme );

    makeEditorGrid( pane, 10, 10 );
    return pane;
  }

  /**
   * Returns the {@link SignalAlignment} for the given key from the
   * {@link UIManager}.
   * 
   * @param aKey
   * @return
   */
  private SignalAlignment getSignalAlignment( final String aKey )
  {
    String value = UIManager.getString( aKey );
    if ( value != null )
    {
      return SignalAlignment.valueOf( value.toUpperCase() );
    }
    return SignalAlignment.CENTER;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    setTitle( "Preferences" );

    JButton okButton = StandardActionFactory.createOkButton();
    JButton cancelButton = StandardActionFactory.createCancelButton();

    JComponent buttonPane = SwingComponentUtils.createButtonPane( okButton, cancelButton );

    JPanel contentPane = new JPanel( new BorderLayout( 4, 4 ) );
    contentPane.add( createGeneralSettingsTab(), BorderLayout.CENTER );
    contentPane.add( buttonPane, BorderLayout.PAGE_END );

    setContentPane( contentPane );

    pack();
  }
}
