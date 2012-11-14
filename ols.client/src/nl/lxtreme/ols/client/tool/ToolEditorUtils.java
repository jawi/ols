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
package nl.lxtreme.ols.client.tool;


import java.awt.*;
import java.util.*;
import javax.swing.*;

import nl.lxtreme.ols.client.editor.*;
import org.osgi.service.metatype.*;


/**
 * Provides some utility methods for creating editors.
 */
final class ToolEditorUtils extends EditorUtils
{
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
  protected JComponent createNumericEditor( final int aType, final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    // For integer values ending with "Idx" create a channel selector instead...
    if ( ( aType == AttributeDefinition.INTEGER ) && aAttributeDef.getID().endsWith( "Idx" ) )
    {
      return createChannelSelector( aAttributeDef, aInitialValue );
    }

    return super.createNumericEditor( aType, aAttributeDef, aInitialValue );
  }

  /**
   * Creates a channel selector component.
   * 
   * @param aAttributeDef
   *          the {@link AttributeDefinition} to create a channel selector for;
   * @param aContext
   *          the tool context to use.
   * @return a channel selector component, never <code>null</code>.
   */
  private JComboBox createChannelSelector( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    final int bitmask = this.dataInfo.getEnabledChannelMask();
    final int channelCount = Integer.bitCount( bitmask );
    final boolean addUnusedOption = !isRequired( aAttributeDef );
    int defaultSelectedIndex = getDefaultIntValue( aAttributeDef, aInitialValue );

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
    int selectedIndex = ( defaultSelectedIndex < 0 ) ? 0 : ( defaultSelectedIndex % modelSize );
    result.setSelectedIndex( selectedIndex );
    result.setRenderer( new DefaultListCellRenderer()
    {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
          final boolean aIsSelected, final boolean aCellHasFocus )
      {
        final String[] optionLabels = aAttributeDef.getOptionLabels();

        int idx = aIndex;
        if ( idx < 0 )
        {
          // When rendering the combobox in collapsed state, the given index is
          // -1, use the selected index from the model instead...
          idx = result.getSelectedIndex();
        }
        if ( addUnusedOption )
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
    } );

    if ( hasMetaTags( aAttributeDef ) )
    {
      parseMetaTags( result, aAttributeDef );
    }

    return result;
  }
}
