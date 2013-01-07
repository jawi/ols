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
package nl.lxtreme.ols.client.ui.tool.impl;


import static nl.lxtreme.ols.client.ui.ClientSwingUtil.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.editor.*;
import org.osgi.service.metatype.*;


/**
 * Provides a panel for configuring tools.
 */
final class ToolConfigPanel extends EditorPanel implements Constants, IEditorProvider
{
  // INNER TYPES

  /**
   * Provides a renderer for channel labels.
   */
  static final class ChannelLabelRenderer extends DefaultListCellRenderer
  {
    // VARIABLES

    private final JComboBox result;
    private final boolean addUnusedOption;
    private final AttributeDefinition attributeDef;
    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ChannelLabelRenderer} instance.
     * 
     * @param aResult
     * @param aAddUnusedOption
     * @param aAttributeDef
     */
    ChannelLabelRenderer( final JComboBox aResult, final boolean aAddUnusedOption,
        final AttributeDefinition aAttributeDef )
    {
      this.result = aResult;
      this.addUnusedOption = aAddUnusedOption;
      this.attributeDef = aAttributeDef;
    }

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      final String[] optionLabels = this.attributeDef.getOptionLabels();

      int idx = aIndex;
      if ( idx < 0 )
      {
        // When rendering the combobox in collapsed state, the given index is
        // -1, use the selected index from the model instead...
        idx = this.result.getSelectedIndex();
      }
      if ( this.addUnusedOption )
      {
        idx--;
      }

      Object value;
      Integer v = ( Integer )aValue;
      if ( v.intValue() < 0 )
      {
        value = "Unused";
      }
      else
      {
        value = String.format( "Channel %d", ( Integer )aValue );
      }
      if ( ( optionLabels != null ) && ( idx >= 0 ) && ( idx < optionLabels.length ) )
      {
        value = optionLabels[idx];
      }

      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final AcquisitionDataInfo acquisitionDataInfo;

  private JCheckBox decodeEntireTimeline;
  private JComboBox decodeMarkerA;
  private JComboBox decodeMarkerB;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolConfigPanel} instance.
   * 
   * @param aDataInfo
   *          the acquisition data information.
   */
  private ToolConfigPanel( final ObjectClassDefinition aOCD, final AcquisitionDataInfo aDataInfo )
  {
    super( aOCD );

    this.acquisitionDataInfo = aDataInfo;
  }

  // METHODS

  /**
   * Factory method for creating a new {@link ToolConfigPanel} instance.
   * 
   * @param aOCD
   *          the object class definition containing the attribute definitions
   *          to create editors for, cannot be <code>null</code>;
   * @param aSettings
   *          the (optional) settings to use as initial values for the editor
   *          components, may be <code>null</code>;
   * @param aTitle
   *          the (optional) title to display above the editors, may be
   *          <code>null</code>;
   * @param aProvider
   *          the (optional) editor provider to supply custom editors for
   *          unmanaged attributes, may be <code>null</code>.
   * @return a new, initialized {@link ToolConfigPanel} instance, never
   *         <code>null</code>.
   */
  public static ToolConfigPanel create( final ObjectClassDefinition aOCD, final Dictionary<Object, Object> aSettings,
      final AcquisitionDataInfo aContext )
  {
    return create( aOCD, asMap( aSettings ), aContext );
  }

  /**
   * Factory method for creating a new {@link ToolConfigPanel} instance.
   * 
   * @param aOCD
   *          the object class definition containing the attribute definitions
   *          to create editors for, cannot be <code>null</code>;
   * @param aSettings
   *          the (optional) settings to use as initial values for the editor
   *          components, may be <code>null</code>;
   * @param aTitle
   *          the (optional) title to display above the editors, may be
   *          <code>null</code>;
   * @param aProvider
   *          the (optional) editor provider to supply custom editors for
   *          unmanaged attributes, may be <code>null</code>.
   * @return a new, initialized {@link ToolConfigPanel} instance, never
   *         <code>null</code>.
   */
  public static ToolConfigPanel create( final ObjectClassDefinition aOCD, final Map<Object, Object> aSettings,
      final AcquisitionDataInfo aContext )
  {
    ToolConfigPanel result = new ToolConfigPanel( aOCD, aContext );

    Map<Object, Object> settings = aSettings;
    if ( settings == null )
    {
      settings = Collections.emptyMap();
    }

    result.initPanel( settings, "Settings", null /* aProvider */);
    return result;
  }

  /**
   * @return <code>true</code> if the settings are valid, <code>false</code>
   *         otherwise.
   */
  @Override
  public boolean areSettingsValid()
  {
    boolean result = super.areSettingsValid();
    if ( !result )
    {
      return false;
    }

    if ( !this.decodeEntireTimeline.isSelected() )
    {
      // Check whether either one of the markers is defined...
      int indexA = this.decodeMarkerA.getSelectedIndex();
      int indexB = this.decodeMarkerB.getSelectedIndex();
      if ( indexA == indexB )
      {
        return false;
      }
    }

    return true;
  }

  /**
   * @return the properties with the current values, never <code>null</code>.
   */
  @Override
  public Properties getProperties()
  {
    Properties result = super.getProperties();
    result.put( PROPERTY_DECODE_ENTIRE_TIMELINE, Boolean.valueOf( this.decodeEntireTimeline.isSelected() ) );
    result.put( PROPERTY_DECODE_MARKER_A, getMarkerValue( this.decodeMarkerA ) );
    result.put( PROPERTY_DECODE_MARKER_B, getMarkerValue( this.decodeMarkerB ) );
    return result;
  }

  /**
   * Updates the states of the components denoting the decode context.
   */
  final void updateContextState()
  {
    boolean enabled = !this.decodeEntireTimeline.isSelected();
    this.decodeMarkerA.setEnabled( enabled );
    this.decodeMarkerB.setEnabled( enabled );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void initPanel( final Map<Object, Object> aSettings, final String aTitle, final IEditorProvider aProvider )
  {
    SpringLayoutUtils.addSeparator( this, "Context" );

    add( createRightAlignedLabel( "Decode region?" ) );
    this.decodeEntireTimeline = ( JCheckBox )add( new JCheckBox( "", isDecodeAll( aSettings ) ) );
    this.decodeEntireTimeline.setEnabled( this.acquisitionDataInfo.hasDefinedCursors() );
    this.decodeEntireTimeline.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateContextState();
      }
    } );

    Cursor[] cursors = this.acquisitionDataInfo.getCursors();

    add( createRightAlignedLabel( "Marker A" ) );
    this.decodeMarkerA = ( JComboBox )add( createOptionalCursorSelector( cursors, getIndexMarkerA( cursors, aSettings ) ) );

    add( createRightAlignedLabel( "Marker B" ) );
    this.decodeMarkerB = ( JComboBox )add( createOptionalCursorSelector( cursors, getIndexMarkerB( cursors, aSettings ) ) );

    // set default settings...
    updateContextState();

    super.initPanel( aSettings, "Settings", this );
  }

  @Override
  public JComponent createEditor( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    // For integer values ending with "Idx" create a channel selector
    // instead...
    int type = aAttributeDef.getType();
    if ( ( type == AttributeDefinition.INTEGER ) && aAttributeDef.getID().endsWith( "Idx" ) )
    {
      int initialSelectedIdx = getInitialSelectedIdx( aAttributeDef, aInitialValue );
      return createChannelSelector( aAttributeDef, initialSelectedIdx );
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void wireChangeListeners( final PropertyChangeListener aListener )
  {
    super.wireChangeListeners( aListener );

    new EditorUtils().wireChangeListeners( aListener,
        Arrays.<JComponent> asList( this.decodeEntireTimeline, this.decodeMarkerA, this.decodeMarkerB ) );
  }

  /**
   * Finds the index of the cursor.
   * 
   * @param aCursors
   * @param aMarkerValue
   * @param aDefault
   * @return
   */
  private int findIndex( final Cursor[] aCursors, final Object aMarkerValue, final int aDefault )
  {
    if ( aMarkerValue instanceof Number )
    {
      long timestamp = ( ( Number )aMarkerValue ).longValue();
      if ( timestamp < 0L )
      {
        return 0;
      }
      int j = 0;
      for ( Cursor cursor : aCursors )
      {
        if ( cursor.isDefined() )
        {
          j++;
          if ( cursor.getTimestamp() == timestamp )
          {
            return j; // zero == "Unused"...
          }
        }
      }
    }
    return aDefault;
  }

  /**
   * @return the index of the first marker, >= 0 or -1.
   */
  private int getIndexMarkerA( final Cursor[] aCursors, final Map<Object, Object> aSettings )
  {
    Object value = aSettings.get( Constants.PROPERTY_DECODE_MARKER_A );
    return findIndex( aCursors, value, 1 );
  }

  /**
   * @return the index of the second marker, >= 0 or -1.
   */
  private int getIndexMarkerB( final Cursor[] aCursors, final Map<Object, Object> aSettings )
  {
    Object value = aSettings.get( Constants.PROPERTY_DECODE_MARKER_B );
    return findIndex( aCursors, value, 2 );
  }

  /**
   * @param aComboBox
   * @return
   */
  private Long getMarkerValue( final JComboBox aComboBox )
  {
    Object selectedValue = aComboBox.getSelectedItem();
    if ( ( selectedValue instanceof Cursor ) && ( ( Cursor )selectedValue ).isDefined() )
    {
      return Long.valueOf( ( ( Cursor )selectedValue ).getTimestamp() );
    }
    return Long.valueOf( -1L );
  }

  /**
   * @return <code>true</code> if the entire timeline is to be decoded,
   *         <code>false</code> otherwise.
   */
  private boolean isDecodeAll( final Map<Object, Object> aSettings )
  {
    Object value = aSettings.get( Constants.PROPERTY_DECODE_ENTIRE_TIMELINE );
    if ( value == null )
    {
      return true;
    }
    return Boolean.TRUE.equals( value );
  }

  /**
   * Creates a channel selector component.
   * 
   * @param aAttributeDef
   *          the {@link AttributeDefinition} to create a channel selector for;
   * @param aInitialSelectedIdx
   *          the initial selected index of the selector.
   * @return a channel selector component, never <code>null</code>.
   */
  private JComboBox createChannelSelector( final AttributeDefinition aAttributeDef, final int aInitialSelectedIdx )
  {
    final int bitmask = this.acquisitionDataInfo.getEnabledChannelMask();
    final int channelCount = this.acquisitionDataInfo.getChannelCount();
    // add an unused option if the attribute is not required...
    final boolean addUnusedOption = "".equals( aAttributeDef.validate( null ) );

    int modelSize = Math.max( 0, Math.min( Integer.SIZE, channelCount ) );
    if ( addUnusedOption )
    {
      modelSize++;
    }

    final Vector<Integer> dataChannels = new Vector<Integer>( modelSize );

    if ( addUnusedOption )
    {
      dataChannels.add( Integer.valueOf( -1 ) );
    }
    for ( int mask = 1, i = 0; i < Integer.SIZE; i++, mask <<= 1 )
    {
      // Fixes #115: disabled channel groups...
      if ( ( bitmask & mask ) != 0 )
      {
        dataChannels.add( Integer.valueOf( i ) );
      }
    }

    final JComboBox result = new JComboBox( dataChannels );
    int selectedIndex = ( aInitialSelectedIdx < 0 ) ? 0 : ( aInitialSelectedIdx % modelSize );
    result.setSelectedIndex( selectedIndex );
    result.setRenderer( new ChannelLabelRenderer( result, addUnusedOption, aAttributeDef ) );

    return result;
  }

  /**
   * Tries to determine the default initial selected index for a channel
   * selector.
   * 
   * @param aAttributeDef
   * @param aInitialValue
   * @return an index, < 0 if no index could be determined, otherwise, >= 0.
   */
  private int getInitialSelectedIdx( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    int idx = -1;
    try
    {
      if ( aInitialValue != null )
      {
        idx = Integer.parseInt( aInitialValue.toString() );
      }
      else
      {
        String[] defaults = aAttributeDef.getDefaultValue();
        if ( ( defaults != null ) && ( defaults.length > 0 ) )
        {
          idx = Integer.parseInt( defaults[0] );
        }
      }
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }
    return idx;
  }
}
