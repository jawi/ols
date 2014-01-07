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
package nl.lxtreme.ols.client2.prefs;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;
import static nl.lxtreme.ols.util.swing.SpringLayoutUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;
import nl.lxtreme.ols.client2.colorscheme.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


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

    private final String[] schemes = colorSchemeProvider.getColorSchemeNames();

    private volatile int selected = -1;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      if ( ( aIndex < 0 ) || ( aIndex >= schemes.length ) )
      {
        return null;
      }
      return this.schemes[aIndex];
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
      return this.schemes.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      this.selected = Arrays.binarySearch( this.schemes, aItem );
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

  private final AtomicBoolean dialogResult;

  private JCheckBox mouseWheelZooms;
  private JCheckBox cursorSnapToEdge;
  private JCheckBox showGroupSummary;
  private JCheckBox showAnalogScope;
  private JCheckBox showToolWindows;
  private JCheckBox showChannelIndexes;
  private JCheckBox retainAnnotations;
  private JCheckBox autoCenterCapture;
  private JComboBox annotationAlignment;
  private JComboBox signalAlignment;
  private JComboBox colorScheme;

  // Injected by Felix DM...
  private volatile ColorSchemeProvider colorSchemeProvider;
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService logService;
  // Locally managed...
  private volatile Configuration config;
  private volatile Component component;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PreferencesDialog} instance.
   * 
   * @param aParent
   *          the parent of the preferences window, can be <code>null</code>;
   * @param aColorSchemeProvider
   *          the color scheme manager to use, cannot be <code>null</code>.
   */
  public PreferencesDialog( Window aParent )
  {
    super( aParent, "", ModalityType.MODELESS );

    this.dialogResult = new AtomicBoolean( false );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    if ( this.component != null )
    {
      // All changes are persisted automatically...
      DependencyManager dm = this.component.getDependencyManager();
      dm.remove( this.component );
      this.component = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean setDialogStatus( final DialogStatus aStatus )
  {
    this.dialogResult.set( aStatus == DialogStatus.OK );
    return true;
  }

  /**
   * Display the bundles dialog.
   */
  public void showDialog()
  {
    DependencyManager dm = new DependencyManager( FrameworkUtil.getBundle( getClass() ).getBundleContext() );
    this.component = dm.createComponent();

    this.component.setImplementation( this ) //
        .add( dm.createServiceDependency() //
            .setService( LogService.class ) //
            .setInstanceBound( true ) //
            .setRequired( false ) ) //
        .add( dm.createServiceDependency() //
            .setService( ConfigurationAdmin.class ) //
            .setInstanceBound( true ) //
            .setRequired( true ) ) //
        .add( dm.createServiceDependency() //
            .setService( ColorSchemeProvider.class ) //
            .setInstanceBound( true ) //
            .setRequired( true ) );
    dm.add( this.component );
  }

  /**
   * Initializes this dialog, should be called from the EDT.
   */
  @SuppressWarnings( "unchecked" )
  final void initDialog()
  {
    this.dialogResult.set( false );
    // The properties are in string format; so we need to do some conversions
    // prior to applying them to our components...
    Dictionary<Object, Object> properties = this.config.getProperties();

    // @formatter:off
    this.mouseWheelZooms = new JCheckBox();
    this.mouseWheelZooms.setToolTipText( "Whether the mouse wheel by default zooms in or out, or scrolls the view. Will be applied immediately." );
    this.mouseWheelZooms.setSelected( getBoolean( properties.get( MOUSEWHEEL_ZOOM_DEFAULT ) ) );

    this.cursorSnapToEdge = new JCheckBox();
    this.cursorSnapToEdge.setToolTipText( "Whether or not cursors by default snap to signal edges. Will be applied immediately." );
    this.cursorSnapToEdge.setSelected( getBoolean( properties.get( SNAP_CURSORS_DEFAULT ) ) );

    this.showGroupSummary = new JCheckBox();
    this.showGroupSummary.setToolTipText( "Whether or not the group (byte) summary is shown by default for each acquisition. Will be applied after an acquisition." );
    this.showGroupSummary.setSelected( getBoolean( properties.get( GROUP_SUMMARY_VISIBLE_DEFAULT ) ) );

    this.showAnalogScope = new JCheckBox();
    this.showAnalogScope.setToolTipText( "Whether or not the analog scope is shown by default for each acquisition. Will be applied after an acquisition." );
    this.showAnalogScope.setSelected( getBoolean( properties.get( ANALOG_SCOPE_VISIBLE_DEFAULT ) ) );

    this.showChannelIndexes = new JCheckBox();
    this.showChannelIndexes.setToolTipText( "Whether or not channel indexes are shown beside the labels. Will be applied immediately." );
    this.showChannelIndexes.setSelected( getBoolean( properties.get( CHANNELLABELS_SHOW_CHANNEL_INDEX ) ) );

    this.retainAnnotations = new JCheckBox();
    this.retainAnnotations.setToolTipText( "Whether or not annotations should be retained after a recapture. Will be applied immediately." );
    this.retainAnnotations.setSelected( getBoolean( properties.get( RETAIN_ANNOTATIONS_WITH_RECAPTURE ) ) );

    this.showToolWindows = new JCheckBox();
    this.showToolWindows.setToolTipText( "Whether or not the tool windows are shown by default. Will be applied after a restart." );
    this.showToolWindows.setSelected( getBoolean( properties.get( SHOW_TOOL_WINDOWS_DEFAULT ) ) );

    this.autoCenterCapture = new JCheckBox();
    this.autoCenterCapture.setToolTipText( "Whether or not to auto-center the diagram to the trigger after a capture. Will be applied immediately." );
    this.autoCenterCapture.setSelected( getBoolean( properties.get( AUTO_CENTER_TO_TRIGGER_AFTER_CAPTURE ) ) );

    this.signalAlignment = new JComboBox( SignalAlignment.values() );
    this.signalAlignment.setToolTipText( "The vertical alignment of the signals itself. Will be applied after an acquisition." );
    this.signalAlignment.setSelectedItem( getSignalAlignment( properties.get( SIGNALVIEW_SIGNAL_ALIGNMENT ) ) );
    this.signalAlignment.setRenderer( new SignalAlignmentRenderer() );

    this.annotationAlignment = new JComboBox( SignalAlignment.values() );
    this.annotationAlignment.setToolTipText( "The vertical aligment of the annotations. Will be applied immediately." );
    this.annotationAlignment.setSelectedItem( getSignalAlignment( properties.get( SIGNALVIEW_ANNOTATION_ALIGNMENT ) ) );
    this.annotationAlignment.setRenderer( new SignalAlignmentRenderer() );

    this.colorScheme = new JComboBox( new ColorSchemeModel() );
    this.colorScheme.setToolTipText( "What color scheme is to be used. Will be applied immediately." );
    this.colorScheme.setSelectedItem( String.valueOf( properties.get( COLOR_SCHEME ) ) );
    // @formatter:on

    buildDialog();
  }

  /**
   * Called upon start of this component by the dependency manager.
   */
  protected void start() throws IOException
  {
    this.config = this.configAdmin.getConfiguration( UIManagerConfigurator.PID );

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        initDialog();

        setVisible( true );
      }
    } );
  }

  /**
   * Called upon stop of this component by the dependency manager.
   */
  protected void stop()
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        setVisible( false );
        dispose();

        if ( dialogResult.get() )
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
    } );
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
    this.logService.log( LogService.LOG_INFO, "Applying updated color scheme..." );

    dictionary.put( COLOR_SCHEME, colorScheme );

    Properties props = this.colorSchemeProvider.getColorScheme( colorScheme );
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
    this.logService.log( LogService.LOG_INFO, "Applying updated preferences..." );

    // The properties are in string format; so we need to do some conversions
    // prior to persisting them...
    Dictionary<Object, Object> properties = this.config.getProperties();
    properties.put( MOUSEWHEEL_ZOOM_DEFAULT, Boolean.toString( this.mouseWheelZooms.isSelected() ) );
    properties.put( SNAP_CURSORS_DEFAULT, Boolean.toString( this.cursorSnapToEdge.isSelected() ) );
    properties.put( GROUP_SUMMARY_VISIBLE_DEFAULT, Boolean.toString( this.showGroupSummary.isSelected() ) );
    properties.put( ANALOG_SCOPE_VISIBLE_DEFAULT, Boolean.toString( this.showAnalogScope.isSelected() ) );
    properties.put( SHOW_TOOL_WINDOWS_DEFAULT, Boolean.toString( this.showToolWindows.isSelected() ) );
    properties.put( CHANNELLABELS_SHOW_CHANNEL_INDEX, Boolean.toString( this.showChannelIndexes.isSelected() ) );
    properties.put( RETAIN_ANNOTATIONS_WITH_RECAPTURE, Boolean.toString( this.retainAnnotations.isSelected() ) );
    properties.put( AUTO_CENTER_TO_TRIGGER_AFTER_CAPTURE, Boolean.toString( this.autoCenterCapture.isSelected() ) );

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

    pane.add( createRightAlignedLabel( "Auto-center on trigger?" ) );
    pane.add( this.autoCenterCapture );

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
   * @param aValue
   *          the value to parse as boolean, can be <code>null</code>.
   * @return a boolean representation for the given value, defaults to
   *         <code>false</code>.
   */
  private boolean getBoolean( final Object aValue )
  {
    if ( aValue instanceof Boolean )
    {
      return ( ( Boolean )aValue ).booleanValue();
    }
    return Boolean.parseBoolean( String.valueOf( aValue ) );
  }

  /**
   * Returns the {@link SignalAlignment} for the given value representation.
   * 
   * @param aValue
   *          the value parse as {@link SignalAlignment}, can be
   *          <code>null</code>.
   * @return a {@link SignalAlignment} value, defaults to
   *         {@link SignalAlignment#CENTER}.
   */
  private SignalAlignment getSignalAlignment( final Object aValue )
  {
    if ( aValue == null )
    {
      return SignalAlignment.CENTER;
    }
    return SignalAlignment.valueOf( aValue.toString().toUpperCase() );
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
    Properties props = this.colorSchemeProvider.getColorScheme( oldColorScheme );
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
