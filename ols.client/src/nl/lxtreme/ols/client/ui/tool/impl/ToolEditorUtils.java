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


import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.editor.*;

import org.osgi.service.metatype.*;


/**
 * Provides some utility methods for creating editors.
 */
final class ToolEditorUtils extends EditorUtils
{
  // VARIABLES

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

  // VARIABLES

  private final AcquisitionDataInfo dataInfo;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolEditorUtils} instance.
   */
  public ToolEditorUtils( final AcquisitionDataInfo aDataInfo )
  {
    this.dataInfo = aDataInfo;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected JComponent createNumericEditor( final AttributeDefinition aAttributeDef, final String aInitialValue )
  {
    // For integer values ending with "Idx" create a channel selector instead...
    int type = aAttributeDef.getType();
    if ( ( type == AttributeDefinition.INTEGER ) && aAttributeDef.getID().endsWith( "Idx" ) )
    {
      int initialSelectedIdx = getInitialSelectedIdx( aAttributeDef, aInitialValue );
      return createChannelSelector( aAttributeDef, initialSelectedIdx );
    }

    return super.createNumericEditor( aAttributeDef, aInitialValue );
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
    final int bitmask = this.dataInfo.getEnabledChannelMask();
    final int channelCount = this.dataInfo.getChannelCount();
    final boolean addUnusedOption = !isRequired( aAttributeDef );

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

    if ( hasMetaTags( aAttributeDef ) )
    {
      parseMetaTags( result, aAttributeDef );
    }

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
  private int getInitialSelectedIdx( final AttributeDefinition aAttributeDef, final String aInitialValue )
  {
    int idx = -1;
    try
    {
      if ( aInitialValue != null )
      {
        idx = Integer.parseInt( aInitialValue );
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
