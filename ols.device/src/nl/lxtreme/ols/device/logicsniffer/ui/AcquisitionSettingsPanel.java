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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.logicsniffer.ui;


import static nl.lxtreme.ols.common.Ols.*;
import static nl.lxtreme.ols.device.logicsniffer.ui.OcdHelper.*;
import static nl.lxtreme.ols.util.swing.editor.EditorUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static org.sump.device.logicsniffer.ConfigDialogHelper.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.plaf.basic.*;

import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.device.logicsniffer.profile.*;
import nl.lxtreme.ols.device.logicsniffer.profile.DeviceProfile.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.editor.*;

import org.osgi.service.metatype.*;


/**
 * Provides a panel for the main acquisition settings.
 */
public class AcquisitionSettingsPanel extends JPanel implements DeviceProfileChangedListener
{
  // INNER TYPES

  /**
   * Renders a binary size.
   */
  static final class BinarySizeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof Integer )
      {
        value = SizeUnit.format( ( ( Integer )aValue ).doubleValue() );
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a binary size.
   */
  static final class CaptureSpeedComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof Number )
      {
        value = FrequencyUnit.format( ( ( Number )value ).doubleValue() );
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Renders a clock source.
   */
  static final class ClockSourceComboBoxRenderer extends EnumItemRenderer<CaptureClockSource>
  {
    private static final long serialVersionUID = 1L;

    /**
     * @see nl.lxtreme.ols.util.swing.component.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final CaptureClockSource aValue )
    {
      switch ( aValue )
      {
        case INTERNAL:
          return "Internal";
        case EXTERNAL_FALLING:
          return "External / Rising";
        case EXTERNAL_RISING:
          return "External / Falling";
      }
      return super.getDisplayValue( aValue );
    }
  }

  /**
   * Renders a numbering scheme.
   */
  static final class NumberSchemeComboBoxRenderer extends EnumItemRenderer<NumberingScheme>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final NumberingScheme aValue )
    {
      switch ( aValue )
      {
        case DEFAULT:
          return "Default";
        case INSIDE:
          return "Inside";
        case OUTSIDE:
          return "Outside";
      }
      return super.getDisplayValue( aValue );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String RLE_WARNING = "<html><body><b>The last channel will always<br/>be low when RLE is enabled!</b></body></html>";

  // VARIABLES

  private final List<JComponent> components;

  private JComboBox numberScheme;
  private JComboBox clockSource;
  private JComboBox sampleRate;
  private JCheckBox[] enabledChannelGroups;
  private JComboBox size;
  private JCheckBox useMaxSize;
  private JCheckBox testModeEnabled;
  private JCheckBox filterEnabled;
  private JCheckBox rleEnabled;
  private JLabel rleWarning;

  private JTextField channelGroupValue; // not visible...

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionSettingsPanel} instance.
   */
  public AcquisitionSettingsPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    super( new SpringLayout() );

    this.components = new ArrayList<JComponent>();

    initPanel( aOCD, aInitialValues );
    buildPanel();

    // disable all components...
    enableComponents( false );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void deviceProfileChanged( final DeviceProfile aProfile )
  {
    enableComponents( aProfile != null );

    if ( aProfile != null )
    {
      // Noise filter supported?
      updateCheckBoxState( this.filterEnabled, aProfile.isNoiseFilterSupported() );
      // RLE supported?
      updateCheckBoxState( this.rleEnabled, aProfile.isRleSupported() );
      // Test mode supported at all?
      updateCheckBoxState( this.testModeEnabled, aProfile.isTestModeSupported() );

      // Enable the supported number of channel groups...
      updateChannelGroups( this.enabledChannelGroups, aProfile );

      // Update the capture speeds...
      updateCaptureSpeedComboBoxModel( this.sampleRate, aProfile );
      // Update the capture sizes...
      updateComboBoxModel( this.size, aProfile.getCaptureSizes() );
      // Update the capture clock sources...
      updateComboBoxModel( this.clockSource, aProfile.getCaptureClock() );
      // Update the numbering schemes...
      updateComboBoxModel( this.numberScheme, aProfile.getChannelNumberingSchemes() );
    }
  }

  /**
   * @param aConfiguration
   *          the configuration map to fill with the configuration settings of
   *          this panel, never <code>null</code>.
   * @return the given configuration, never <code>null</code>.
   */
  public Map<Object, Object> getConfiguration( final Map<Object, Object> aConfiguration )
  {
    aConfiguration.putAll( new EditorUtils().getComponentValues( this.components ) );
    return aConfiguration;
  }

  /**
   * Calculates the channel group mask value based on the current chosen values.
   */
  final void calculateChannelGroupValue()
  {
    int value = 0;
    for ( JCheckBox element : this.enabledChannelGroups )
    {
      if ( element.isSelected() )
      {
        value |= 0xFF;
      }
      value <<= 8;
    }
    this.channelGroupValue.setText( Integer.toString( value ) );
  }

  /**
   * Shows a warning about the last channel when RLE-mode is enabled.
   */
  final void showOrHideRleWarning()
  {
    this.rleWarning.setVisible( this.rleEnabled.isSelected() );
  }

  /**
   * Updates the size selection combobox according to the max-value selection.
   */
  final void updateSizeSelection()
  {
    this.size.setEnabled( !this.useMaxSize.isSelected() );
    if ( this.useMaxSize.isSelected() )
    {
      // Always select the highest value...
      this.size.setSelectedIndex( 0 );
    }
  }

  /**
   * Updates the channel groups according to the chosen sample rate.
   */
  final void updateEnabledChannelGroups()
  {
    Integer value = ( Integer )this.sampleRate.getSelectedItem();
    boolean ddrMode = ( value == null ) || ( value.intValue() <= 100000000 );

    this.enabledChannelGroups[0].setEnabled( true );
    this.enabledChannelGroups[1].setEnabled( true );
    this.enabledChannelGroups[2].setEnabled( ddrMode );
    this.enabledChannelGroups[3].setEnabled( ddrMode );

    if ( ddrMode )
    {
      this.enabledChannelGroups[2].setSelected( false );
      this.enabledChannelGroups[3].setSelected( false );
    }
  }

  /**
   * Builds this panel by placing all its components.
   */
  private void buildPanel()
  {
    SpringLayoutUtils.addSeparator( this, "Acquisition settings" );

    add( createRightAlignedLabel( "Number scheme" ) );
    add( this.numberScheme );

    add( createRightAlignedLabel( "Sampling Clock" ) );
    add( this.clockSource );

    add( createRightAlignedLabel( "Sampling Rate" ) );
    add( this.sampleRate );

    SpringLayoutUtils.addSeparator( this, "" );

    JPanel groupsPanel = new JPanel( new GridLayout( 1, 4 ) );
    for ( JCheckBox element : this.enabledChannelGroups )
    {
      groupsPanel.add( element );
    }

    add( createRightAlignedLabel( "Channel Groups" ) );
    add( groupsPanel );

    add( createRightAlignedLabel( "Recording Size" ) );
    add( this.useMaxSize );
    add( new JLabel() );
    add( this.size );

    SpringLayoutUtils.addSeparator( this, "Options" );

    add( createRightAlignedLabel( "Test mode" ) );
    add( this.testModeEnabled );

    add( createRightAlignedLabel( "Noise Filter" ) );
    add( this.filterEnabled );

    add( createRightAlignedLabel( "Run Length Encoding" ) );
    add( this.rleEnabled );

    add( new JLabel( "" ) );
    add( this.rleWarning );

    SpringLayoutUtils.makeEditorGrid( this, 10, 10 );
  }

  /**
   * @param aValue
   *          <code>true</code> to enable all components, <code>false</code> to
   *          disable them.
   */
  private void enableComponents( final boolean aValue )
  {
    for ( JCheckBox cg : this.enabledChannelGroups )
    {
      cg.setEnabled( aValue );
    }

    this.filterEnabled.setEnabled( aValue );
    this.useMaxSize.setEnabled( aValue );
    this.numberScheme.setEnabled( aValue );
    this.rleEnabled.setEnabled( aValue );
    this.rleWarning.setEnabled( aValue );
    this.size.setEnabled( aValue );
    this.clockSource.setEnabled( aValue );
    this.sampleRate.setEnabled( aValue );
    this.testModeEnabled.setEnabled( aValue );
  }

  /**
   * Initializes the components that should be placed on this panel.
   * 
   * @param aOCD
   * @param aInitialValues
   */
  private void initPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    EditorUtils editorUtils = new EditorUtils();

    this.numberScheme = new JComboBox();
    this.numberScheme.setRenderer( new NumberSchemeComboBoxRenderer() );
    wireEditor( this.numberScheme, aOCD, "numberScheme", aInitialValues );

    this.clockSource = new JComboBox();
    this.clockSource.setRenderer( new ClockSourceComboBoxRenderer() );
    wireEditor( this.clockSource, aOCD, "clockSource", aInitialValues );

    this.sampleRate = new JComboBox();
    this.sampleRate.setRenderer( new CaptureSpeedComboBoxRenderer() );
    wireEditor( this.sampleRate, aOCD, "sampleRate", aInitialValues );
    this.sampleRate.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateEnabledChannelGroups();
      }
    } );

    AttributeDefinition ad = getAttributeDefinition( aOCD, "enabledChannels" );
    String deflt = String.valueOf( editorUtils.getDefaultValue( ad, aInitialValues.get( "enabledChannels" ) ) );
    int defaultValue = Integer.parseInt( deflt );

    this.enabledChannelGroups = new JCheckBox[MAX_BLOCKS];
    for ( int i = 0; i < this.enabledChannelGroups.length; i++ )
    {
      boolean selected = ( ( defaultValue >> ( i * 8 ) ) & 0xFF ) != 0;

      this.enabledChannelGroups[i] = new JCheckBox( Integer.toString( i ) );
      this.enabledChannelGroups[i].setSelected( selected );
      this.enabledChannelGroups[i].addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          calculateChannelGroupValue();
        }
      } );
    }

    this.channelGroupValue = new JTextField();
    this.channelGroupValue.putClientProperty( PROPERTY_ATTRIBUTE, ad );

    this.size = new JComboBox();
    this.size.setRenderer( new BinarySizeComboBoxRenderer() );
    wireEditor( this.size, aOCD, "size", aInitialValues );

    this.useMaxSize = new JCheckBox( "Automatic (maximum)" );
    this.useMaxSize.setSelected( false );
    this.useMaxSize.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateSizeSelection();
      }
    } );

    this.testModeEnabled = createEditor( aOCD, "testModeEnabled", aInitialValues );
    this.filterEnabled = createEditor( aOCD, "filterEnabled", aInitialValues );
    this.rleEnabled = createEditor( aOCD, "rleEnabled", aInitialValues );
    this.rleEnabled.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        showOrHideRleWarning();
      }
    } );

    this.rleWarning = new JLabel( RLE_WARNING );

    this.components
        .addAll( Arrays.<JComponent> asList( this.numberScheme, this.clockSource, this.sampleRate,
            this.channelGroupValue, this.size, this.useMaxSize, this.testModeEnabled, this.filterEnabled,
            this.rleEnabled ) );

    // Ensure the initial values are correct...
    calculateChannelGroupValue();
    updateSizeSelection();
    updateEnabledChannelGroups();
    showOrHideRleWarning();

    editorUtils.applyComponentProperties( this.components );
  }

  /**
   * @param aComponent
   * @param aOCD
   * @param aPropertyName
   * @param aInitialValues
   */
  private void wireEditor( final JComboBox aComponent, final ObjectClassDefinition aOCD, final String aPropertyName,
      final Map<Object, Object> aInitialValues )
  {
    AttributeDefinition ad = getAttributeDefinition( aOCD, aPropertyName );
    aComponent.setSelectedItem( new EditorUtils().getDefaultValue( ad, aInitialValues.get( aPropertyName ) ) );
    aComponent.putClientProperty( PROPERTY_ATTRIBUTE, ad );
  }
}
