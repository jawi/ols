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
import static nl.lxtreme.ols.util.swing.SpringLayoutUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.SignalDiagramModel.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.cm.*;


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
  private final Configuration config;

  private final JCheckBox mouseWheelZooms;
  private final JCheckBox cursorSnapToEdge;
  private final JCheckBox showGroupSummary;
  private final JCheckBox showAnalogScope;
  private final JCheckBox showToolWindows;
  private final JCheckBox showChannelIndexes;
  private final JCheckBox retainAnnotations;
  private final JComboBox annotationAlignment;
  private final JComboBox signalAlignment;
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
   */
  public PreferencesDialog( final Window aParent, final UIColorSchemeManager aColorSchemeManager,
      final Configuration aConfiguration )
  {
    super( aParent, "", ModalityType.APPLICATION_MODAL );

    this.colorSchemeManager = aColorSchemeManager;
    this.config = aConfiguration;

    // @formatter:off
    this.mouseWheelZooms = new JCheckBox();
    this.mouseWheelZooms.setToolTipText( "Whether the mouse wheel by default zooms in or out, or scrolls the view. Will be applied immediately." );

    this.cursorSnapToEdge = new JCheckBox();
    this.cursorSnapToEdge.setToolTipText( "Whether or not cursors by default snap to signal edges. Will be applied immediately." );

    this.showGroupSummary = new JCheckBox();
    this.showGroupSummary.setToolTipText( "Whether or not the group (byte) summary is shown by default for each acquisition. Will be applied after an acquisition." );

    this.showAnalogScope = new JCheckBox();
    this.showAnalogScope.setToolTipText( "Whether or not the analog scope is shown by default for each acquisition. Will be applied after an acquisition." );

    this.showChannelIndexes = new JCheckBox();
    this.showChannelIndexes.setToolTipText( "Whether or not channel indexes are shown beside the labels. Will be applied immediately." );

    this.retainAnnotations = new JCheckBox();
    this.retainAnnotations.setToolTipText( "Whether or not annotations should be retained after a recapture. Will be applied immediately." );
    
    this.showToolWindows = new JCheckBox();
    this.showToolWindows.setToolTipText( "Whether or not the tool windows are shown by default. Will be applied after a restart." );

    this.signalAlignment = new JComboBox( SignalAlignment.values() );
    this.signalAlignment.setToolTipText( "The vertical alignment of the signals itself. Will be applied after an acquisition." );
    this.signalAlignment.setRenderer( new SignalAlignmentRenderer() );

    this.annotationAlignment = new JComboBox( SignalAlignment.values() );
    this.annotationAlignment.setToolTipText( "The vertical aligment of the annotations. Will be applied immediately." );
    this.annotationAlignment.setRenderer( new SignalAlignmentRenderer() );

    this.colorScheme = new JComboBox( new ColorSchemeModel() );
    this.colorScheme.setToolTipText( "What color scheme is to be used. Will be applied immediately." );
    // @formatter:on

    buildDialog();
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
    this.dialogResult = ( aStatus == DialogStatus.OK );
    return true;
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
    this.mouseWheelZooms.setSelected( getBoolean( properties, MOUSEWHEEL_ZOOM_DEFAULT ) );
    this.cursorSnapToEdge.setSelected( getBoolean( properties, SNAP_CURSORS_DEFAULT ) );
    this.showGroupSummary.setSelected( getBoolean( properties, GROUP_SUMMARY_VISIBLE_DEFAULT ) );
    this.showAnalogScope.setSelected( getBoolean( properties, ANALOG_SCOPE_VISIBLE_DEFAULT ) );
    this.showChannelIndexes.setSelected( getBoolean( properties, CHANNELLABELS_SHOW_CHANNEL_INDEX ) );
    this.retainAnnotations.setSelected( getBoolean( properties, RETAIN_ANNOTATIONS_WITH_RECAPTURE ) );
    this.showToolWindows.setSelected( getBoolean( properties, SHOW_TOOL_WINDOWS_DEFAULT ) );

    this.signalAlignment.setSelectedItem( getSignalAlignment( properties, SIGNALVIEW_SIGNAL_ALIGNMENT ) );
    this.annotationAlignment.setSelectedItem( getSignalAlignment( properties, SIGNALVIEW_ANNOTATION_ALIGNMENT ) );
    this.colorScheme.setSelectedItem( getString( properties, COLOR_SCHEME ) );

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
    properties.put( MOUSEWHEEL_ZOOM_DEFAULT, Boolean.toString( this.mouseWheelZooms.isSelected() ) );
    properties.put( SNAP_CURSORS_DEFAULT, Boolean.toString( this.cursorSnapToEdge.isSelected() ) );
    properties.put( GROUP_SUMMARY_VISIBLE_DEFAULT, Boolean.toString( this.showGroupSummary.isSelected() ) );
    properties.put( ANALOG_SCOPE_VISIBLE_DEFAULT, Boolean.toString( this.showAnalogScope.isSelected() ) );
    properties.put( SHOW_TOOL_WINDOWS_DEFAULT, Boolean.toString( this.showToolWindows.isSelected() ) );
    properties.put( CHANNELLABELS_SHOW_CHANNEL_INDEX, Boolean.toString( this.showChannelIndexes.isSelected() ) );
    properties.put( RETAIN_ANNOTATIONS_WITH_RECAPTURE, Boolean.toString( this.retainAnnotations.isSelected() ) );

    properties.put( SIGNALVIEW_SIGNAL_ALIGNMENT, String.valueOf( this.signalAlignment.getSelectedItem() ) );
    properties.put( SIGNALVIEW_ANNOTATION_ALIGNMENT, String.valueOf( this.annotationAlignment.getSelectedItem() ) );

    String colorScheme = ( String )this.colorScheme.getSelectedItem();
    if ( colorScheme != null )
    {
      // Remove old color scheme, as it might contain keys that aren't defined
      // in the new scheme...
      purgeOldColorScheme( properties );

      applyNewColorScheme( colorScheme, properties );
    }

    // Update the configuration, so it will be persisted...
    this.config.update( properties );
  }

  /**
   * Builds this dialog by adding all components to it.
   */
  private void buildDialog()
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

  /**
   * @return a tab pane for the general settings, never <code>null</code>.
   */
  private JComponent createGeneralSettingsTab()
  {
    JPanel pane = new JPanel( new SpringLayout() );

    addSeparator( pane, "Look and feel defaults" );

    pane.add( createRightAlignedLabel( "Mouse wheel zooms?" ) );
    pane.add( this.mouseWheelZooms );

    pane.add( createRightAlignedLabel( "Snap cursor to edge?" ) );
    pane.add( this.cursorSnapToEdge );

    pane.add( createRightAlignedLabel( "Show group summary?" ) );
    pane.add( this.showGroupSummary );

    pane.add( createRightAlignedLabel( "Show analog scope?" ) );
    pane.add( this.showAnalogScope );

    pane.add( createRightAlignedLabel( "Show channel indexes?" ) );
    pane.add( this.showChannelIndexes );

    pane.add( createRightAlignedLabel( "Retain annotations?" ) );
    pane.add( this.retainAnnotations );

    pane.add( createRightAlignedLabel( "Show tool windows?" ) );
    pane.add( this.showToolWindows );

    pane.add( createRightAlignedLabel( "Signal alignment" ) );
    pane.add( this.signalAlignment );

    pane.add( createRightAlignedLabel( "Annotation alignment" ) );
    pane.add( this.annotationAlignment );

    addSeparator( pane, "Color scheme" );

    pane.add( createRightAlignedLabel( "Default scheme" ) );
    pane.add( this.colorScheme );

    makeEditorGrid( pane, 10, 10 );
    return pane;
  }

  /**
   * Returns the boolean value for the given value representation.
   * 
   * @param aProperties
   *          the properties to retrieve the boolean from, may be
   *          <code>null</code>;
   * @param aKey
   *          the key under which the property should be retrieved, cannot be
   *          <code>null</code>.
   * @return a boolean representation for the given value, defaults to
   *         <code>false</code>.
   */
  private static boolean getBoolean( final Dictionary<Object, Object> aProperties, final String aKey )
  {
    if ( aProperties == null )
    {
      return false;
    }

    Object value = aProperties.get( aKey );
    if ( value instanceof Boolean )
    {
      return ( ( Boolean )value ).booleanValue();
    }
    return Boolean.parseBoolean( String.valueOf( value ) );
  }

  /**
   * Returns the string value for the given value representation.
   * 
   * @param aProperties
   *          the properties to retrieve the string from, may be
   *          <code>null</code>;
   * @param aKey
   *          the key under which the property should be retrieved, cannot be
   *          <code>null</code>.
   * @return a boolean representation for the given value, defaults to
   *         <code>""</code> (an empty string).
   */
  private static String getString( final Dictionary<Object, Object> aProperties, final String aKey )
  {
    if ( aProperties == null )
    {
      return "";
    }

    Object value = aProperties.get( aKey );
    return String.valueOf( value );
  }

  /**
   * Returns the {@link SignalAlignment} for the given value representation.
   * 
   * @param aProperties
   *          the properties to retrieve the boolean from, may be
   *          <code>null</code>;
   * @param aKey
   *          the key under which the property should be retrieved, cannot be
   *          <code>null</code>.
   * @return a {@link SignalAlignment} value, defaults to
   *         {@link SignalAlignment#CENTER}.
   */
  private static SignalAlignment getSignalAlignment( final Dictionary<Object, Object> aProperties, final String aKey )
  {
    SignalAlignment result = SignalAlignment.CENTER;
    if ( aProperties != null )
    {
      Object value = aProperties.get( aKey );
      if ( value != null )
      {
        result = SignalAlignment.valueOf( value.toString().toUpperCase() );
      }
    }
    return result;
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
