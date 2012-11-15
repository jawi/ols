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


import static nl.lxtreme.ols.client.signaldisplay.view.UIManagerKeys.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static nl.lxtreme.ols.client.editor.EditorPanel.*;

import java.awt.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.editor.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.cm.*;
import org.osgi.service.metatype.*;


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
      java.util.List<String> schemes = getColorSchemeManager().getColorSchemes();
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
      return getColorSchemeManager().getColorSchemeCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      java.util.List<String> schemes = getColorSchemeManager().getColorSchemes();
      this.selected = schemes.indexOf( aItem );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final UIColorSchemeManager colorSchemeManager;
  private final ObjectClassDefinition ocd;
  private final Configuration config;
  private final EditorPanel editorPanel;
  private final JComboBox colorScheme;

  private boolean dialogResult;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PreferencesDialog} instance.
   * 
   * @param aParent
   *          the parent of the preferences window, can be <code>null</code>;
   * @param aColorSchemeManager
   *          the color scheme manager to use, cannot be <code>null</code>.
   * @param aOcd
   */
  @SuppressWarnings( "unchecked" )
  public PreferencesDialog( final Window aParent, final UIColorSchemeManager aColorSchemeManager,
      final Configuration aConfiguration, final ObjectClassDefinition aOCD )
  {
    super( aParent, aOCD.getName(), ModalityType.APPLICATION_MODAL );

    this.colorSchemeManager = aColorSchemeManager;
    this.config = aConfiguration;
    this.ocd = aOCD;

    // Avoid the dialog to be resized automatically...
    getRootPane().putClientProperty( "unmanaged", Boolean.TRUE );

    JButton okButton = StandardActionFactory.createOkButton();
    JButton cancelButton = StandardActionFactory.createCancelButton();

    JComponent buttonPane = createButtonPane( okButton, cancelButton );

    this.colorScheme = new JComboBox( new ColorSchemeModel() );
    this.colorScheme.setToolTipText( "What color scheme is to be used. Will be applied immediately." );

    Dictionary<Object, Object> settings = aConfiguration.getProperties();
    this.editorPanel = new EditorPanel( this.ocd, settings );

    // Adjust the editor panel a bit to suite our needs...
    SpringLayoutUtils.addSeparator( this.editorPanel, "Color scheme" );

    this.editorPanel.add( createRightAlignedLabel( "Color scheme" ) );
    this.editorPanel.add( this.colorScheme );

    SpringLayoutUtils.makeEditorGrid( this.editorPanel, PADDING, PADDING, PADDING, PADDING );

    setupDialogContentPane( this, this.editorPanel, buttonPane, cancelButton );

    pack();
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
      try
      {
        applyNewPreferences();
      }
      catch ( IOException exception )
      {
        JErrorDialog.showDialog( getOwner(), "Failed to apply preferences!", exception );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean setDialogStatus( final DialogStatus aStatus )
  {
    boolean settingsValid = this.editorPanel.areSettingsValid();
    this.dialogResult = settingsValid && ( aStatus == DialogStatus.OK );
    return settingsValid || ( aStatus == DialogStatus.CANCEL );
  }

  /**
   * Display the bundles dialog.
   * 
   * @return always <code>true</code>.
   */
  @SuppressWarnings( "unchecked" )
  public boolean showDialog()
  {
    this.dialogResult = false;

    // The properties are in string format; so we need to do some conversions
    // prior to applying them to our components...
    Dictionary<Object, Object> properties = this.config.getProperties();

    // Apply values to our components...
    // @formatter:off
//    this.mouseWheelZooms.setSelected( getBoolean( properties, MOUSEWHEEL_ZOOM_DEFAULT ) );
//    this.cursorSnapToEdge.setSelected( getBoolean( properties, SNAP_CURSORS_DEFAULT ) );
//    this.showGroupSummary.setSelected( getBoolean( properties, GROUP_SUMMARY_VISIBLE_DEFAULT ) );
//    this.showAnalogScope.setSelected( getBoolean( properties, ANALOG_SCOPE_VISIBLE_DEFAULT ) );
//    this.showChannelIndexes.setSelected( getBoolean( properties, CHANNELLABELS_SHOW_CHANNEL_INDEX ) );
//    this.retainAnnotations.setSelected( getBoolean( properties, RETAIN_ANNOTATIONS_WITH_RECAPTURE ) );
//    this.showToolWindows.setSelected( getBoolean( properties, SHOW_TOOL_WINDOWS_DEFAULT ) );
//
//    this.signalAlignment.setSelectedItem( getSignalAlignment( properties, SIGNALVIEW_SIGNAL_ALIGNMENT ) );
//    this.annotationAlignment.setSelectedItem( getSignalAlignment( properties, SIGNALVIEW_ANNOTATION_ALIGNMENT ) );
//    this.colorScheme.setSelectedItem( getString( properties, COLOR_SCHEME ) );
    // @formatter:on

    setVisible( true );

    return this.dialogResult;
  }

  /**
   * @return the current color scheme manager, never <code>null</code>.
   */
  final UIColorSchemeManager getColorSchemeManager()
  {
    return this.colorSchemeManager;
  }

  /**
   * Applies the new color scheme by copying its colors to the given dictionary.
   * 
   * @param colorScheme
   *          the name of the new color scheme to apply;
   * @param dictionary
   *          the dictionary to fill with the new color scheme.
   */
  private void applyNewColorScheme( final String colorScheme, final Dictionary<Object, Object> dictionary )
  {
    dictionary.put( COLOR_SCHEME, colorScheme );

    Properties props = getColorSchemeManager().getColorScheme( colorScheme );
    if ( props == null )
    {
      return;
    }

    for ( Object key : props.keySet() )
    {
      Object value = props.get( key );
      if ( value instanceof Color )
      {
        dictionary.put( key, ColorUtils.toHexString( ( Color )value ) );
      }
    }
  }

  /**
   * Applies all (new) preferences by updating the <em>original</em>
   * configuration object (from the ConfigurationAdmin service) with the changed
   * preferences. The reason for this is that the changes will now be persisted
   * automatically.
   */
  @SuppressWarnings( "unchecked" )
  private void applyNewPreferences() throws IOException
  {
    Dictionary<Object, Object> properties = this.config.getProperties();
    if ( properties == null )
    {
      properties = new Hashtable<Object, Object>();
    }

    // The properties are in string format; so we need to do some conversions
    // prior to persisting them...
    // @formatter:off
//    properties.put( MOUSEWHEEL_ZOOM_DEFAULT, Boolean.toString( this.mouseWheelZooms.isSelected() ) );
//    properties.put( SNAP_CURSORS_DEFAULT, Boolean.toString( this.cursorSnapToEdge.isSelected() ) );
//    properties.put( GROUP_SUMMARY_VISIBLE_DEFAULT, Boolean.toString( this.showGroupSummary.isSelected() ) );
//    properties.put( ANALOG_SCOPE_VISIBLE_DEFAULT, Boolean.toString( this.showAnalogScope.isSelected() ) );
//    properties.put( SHOW_TOOL_WINDOWS_DEFAULT, Boolean.toString( this.showToolWindows.isSelected() ) );
//    properties.put( CHANNELLABELS_SHOW_CHANNEL_INDEX, Boolean.toString( this.showChannelIndexes.isSelected() ) );
//    properties.put( RETAIN_ANNOTATIONS_WITH_RECAPTURE, Boolean.toString( this.retainAnnotations.isSelected() ) );
//
//    properties.put( SIGNALVIEW_SIGNAL_ALIGNMENT, String.valueOf( this.signalAlignment.getSelectedItem() ) );
//    properties.put( SIGNALVIEW_ANNOTATION_ALIGNMENT, String.valueOf( this.annotationAlignment.getSelectedItem() ) );
//
//    String colorScheme = ( String )this.colorScheme.getSelectedItem();
//    if ( colorScheme != null )
//    {
//      // Remove old color scheme, as it might contain keys that aren't defined
//      // in the new scheme...
//      purgeOldColorScheme( properties );
//
//      applyNewColorScheme( colorScheme, properties );
//    }
    // @formatter:on

    // Update the configuration, so it will be persisted...
    this.config.update( properties );
  }

  /**
   * Purges the current color scheme from the given dictionary.
   * 
   * @param dictionary
   *          the dictionary to purge any existing color scheme from.
   */
  private void purgeOldColorScheme( final Dictionary<Object, Object> dictionary )
  {
    String oldColorScheme = ( String )dictionary.get( COLOR_SCHEME );
    Properties props = getColorSchemeManager().getColorScheme( oldColorScheme );
    if ( props == null )
    {
      return;
    }

    for ( Object key : props.keySet() )
    {
      Object value = props.get( key );
      if ( value instanceof Color )
      {
        dictionary.put( key, "" );
      }
    }
  }
}
