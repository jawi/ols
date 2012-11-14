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
package nl.lxtreme.ols.client.editor;


import java.awt.*;
import java.util.*;
import javax.swing.*;

import org.osgi.service.metatype.*;


/**
 * Provides some utility methods for creating editors.
 */
public class EditorUtils
{
  // CONSTANTS

  public static final String PROPERTY_READONLY = "readonly";
  public static final String PROPERTY_EDITABLE = "editable";
  public static final String PROPERTY_LISTENER = "listener";

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditorUtils} instance.
   */
  public EditorUtils()
  {
    // Not intended for instantiation.
  }

  // METHODS

  /**
   * Creates an editor component for the given {@link AttributeDefinition}.
   * 
   * @param aAttributeDef
   * @param aInitialValue
   * @return a editor component, can be <code>null</code> in case no editor
   *         component could be created.
   */
  public JComponent createEditor( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    JComponent result = null;

    int type = aAttributeDef.getType();
    switch ( type )
    {
      case AttributeDefinition.BOOLEAN:
      {
        result = createBooleanEditor( type, aAttributeDef, aInitialValue );
        break;
      }

      case AttributeDefinition.BYTE:
      case AttributeDefinition.CHARACTER:
      case AttributeDefinition.DOUBLE:
      case AttributeDefinition.FLOAT:
      case AttributeDefinition.INTEGER:
      case AttributeDefinition.SHORT:
      {
        result = createNumericEditor( type, aAttributeDef, aInitialValue );
        break;
      }

      case AttributeDefinition.STRING:
      {
        result = createStringEditor( type, aAttributeDef, aInitialValue );
        break;
      }
    }

    // Set the name of attribute definition...
    result.setName( aAttributeDef.getID() );

    return result;
  }

  /**
   * @param aAttributeDef
   * @return
   */
  public boolean hasMetaTags( final AttributeDefinition aAttributeDef )
  {
    String description = aAttributeDef.getDescription();
    if ( description == null )
    {
      return false;
    }
    int closeCurlyIdx = description.lastIndexOf( '}' );
    if ( closeCurlyIdx < 0 )
    {
      return false;
    }
    int openCurlyIdx = description.indexOf( '{' );
    if ( openCurlyIdx < 0 )
    {
      return false;
    }
    return ( ( closeCurlyIdx - openCurlyIdx ) > 0 );
  }

  /**
   * Creates a boolean editor component.
   * 
   * @param aType
   * @param aAttributeDef
   * @param aContext
   * @param aInitialValue
   * @return
   */
  protected JCheckBox createBooleanEditor( final int aType, final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    JCheckBox result = new JCheckBox( "", getDefaultBooleanValue( aAttributeDef, aInitialValue ) );
    if ( hasMetaTags( aAttributeDef ) )
    {
      parseMetaTags( result, aAttributeDef );
    }
    return result;
  }

  /**
   * Creates an editor component for an enumeration defined by the given
   * {@link AttributeDefinition}.
   * 
   * @param aAttributeDef
   * @param aContext
   * @param aInitialValue
   * @return
   */
  protected JComboBox createEnumEditor( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    final boolean isRequired = isRequired( aAttributeDef );
    String[] optionValues = aAttributeDef.getOptionValues();
    if ( !isRequired )
    {
      int length = optionValues.length;
      optionValues = Arrays.copyOf( optionValues, length + 1 );
      System.arraycopy( optionValues, 0, optionValues, 1, length );
      optionValues[0] = "";
    }
    int defaultSelectedIndex = getDefaultIntValue( aAttributeDef, aInitialValue );

    final JComboBox result = new JComboBox( optionValues );
    if ( ( defaultSelectedIndex >= 0 ) && ( defaultSelectedIndex < optionValues.length ) )
    {
      result.setSelectedIndex( defaultSelectedIndex );
    }
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
        if ( !isRequired )
        {
          idx--;
        }

        Object value = aValue;
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

  /**
   * Creates a numeric editor component.
   * 
   * @param aType
   * @param aAttributeDef
   * @param aContext
   * @param aInitialValue
   * @return
   */
  protected JComponent createNumericEditor( final int aType, final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    String[] optionValues = aAttributeDef.getOptionValues();
    String[] optionLabels = aAttributeDef.getOptionLabels();
    if ( ( ( optionValues != null ) && ( optionValues.length > 0 ) )
        || ( ( optionLabels != null ) && ( optionLabels.length > 0 ) ) )
    {
      return createEnumEditor( aAttributeDef, aInitialValue );
    }

    JTextField editor = new JTextField( Integer.toString( getDefaultIntValue( aAttributeDef, aInitialValue ) ) );

    if ( hasMetaTags( aAttributeDef ) )
    {
      parseMetaTags( editor, aAttributeDef );
    }

    return editor;
  }

  /**
   * @param aType
   * @param aAttributeDef
   * @param aContext
   * @param aInitialValue
   * @return
   */
  protected JComponent createStringEditor( final int aType, final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    String[] optionValues = aAttributeDef.getOptionValues();
    String[] optionLabels = aAttributeDef.getOptionLabels();
    if ( ( ( optionValues != null ) && ( optionValues.length > 0 ) )
        || ( ( optionLabels != null ) && ( optionLabels.length > 0 ) ) )
    {
      return createEnumEditor( aAttributeDef, aInitialValue );
    }

    JTextField editor = new JTextField( getDefaultStringValue( aAttributeDef, aInitialValue ) );

    if ( hasMetaTags( aAttributeDef ) )
    {
      parseMetaTags( editor, aAttributeDef );
    }

    return editor;
  }

  /**
   * @param aAttributeDef
   * @return
   */
  protected final boolean getDefaultBooleanValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    boolean defaultValue;
    if ( aInitialValue != null )
    {
      defaultValue = Boolean.parseBoolean( String.valueOf( aInitialValue ) );
    }
    else
    {
      String[] defaults = aAttributeDef.getDefaultValue();
      defaultValue = ( defaults != null ) && ( defaults.length > 0 ) && Boolean.parseBoolean( defaults[0] );
    }
    return defaultValue;
  }

  /**
   * @param aAttributeDef
   * @return
   */
  protected final int getDefaultIntValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    int defaultValue = -1;

    try
    {
      if ( aInitialValue != null )
      {
        defaultValue = Integer.parseInt( String.valueOf( aInitialValue ) );
      }
      else
      {
        String[] defaultValues = aAttributeDef.getDefaultValue();
        if ( ( defaultValues != null ) && ( defaultValues.length > 0 ) )
        {
          defaultValue = Integer.parseInt( defaultValues[0] );
        }
      }
    }
    catch ( NumberFormatException ignored )
    {
      // Ignored...
    }

    return defaultValue;
  }

  /**
   * @param aAttributeDef
   * @return
   */
  protected final String getDefaultStringValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    String defaultValue = "";
    if ( aInitialValue != null )
    {
      defaultValue = String.valueOf( aInitialValue );
    }
    else
    {
      String[] defaults = aAttributeDef.getDefaultValue();
      if ( ( defaults != null ) && ( defaults.length > 0 ) )
      {
        defaultValue = String.valueOf( defaults[0] );
      }
    }
    return defaultValue;
  }

  /**
   * @param aAttributeDef
   * @return
   */
  protected final String[] getTags( final AttributeDefinition aAttributeDef )
  {
    String description = aAttributeDef.getDescription();
    int openCurlyIdx = description.indexOf( '{' );
    int closeCurlyIdx = description.lastIndexOf( '}' );
    if ( ( openCurlyIdx < 0 ) || ( closeCurlyIdx < 0 ) )
    {
      return new String[0];
    }

    description = description.substring( openCurlyIdx + 1, closeCurlyIdx );
    String[] parts = description.split( "\\s*[=,]\\s*" );
    return parts;
  }

  /**
   * Determines if the given attribute definition is required or not.
   * 
   * @param aAttributeDef
   *          the {@link AttributeDefinition} to check.
   * @return <code>true</code> if the given {@link AttributeDefinition} is
   *         required, <code>false</code> otherwise.
   */
  protected final boolean isRequired( final AttributeDefinition aAttributeDef )
  {
    String validationResult = aAttributeDef.validate( null );
    return !"".equals( validationResult );
  }

  /**
   * Parses the meta tags found in the description field of the given
   * {@link AttributeDefinition} and places them as client properties on the
   * given component.
   * 
   * @param aAttributeDef
   * @param aResult
   */
  protected final void parseMetaTags( final JComponent aComponent, final AttributeDefinition aAttributeDef )
  {
    String[] tags = getTags( aAttributeDef );
    for ( int i = 0; i < tags.length; i++ )
    {
      String tag = tags[i];
      if ( "readonly".equals( tag ) )
      {
        Boolean value = Boolean.valueOf( tags[++i] );
        aComponent.putClientProperty( PROPERTY_READONLY, value );
      }
      else if ( "editable".equals( tag ) )
      {
        Boolean value = Boolean.valueOf( tags[++i] );
        aComponent.putClientProperty( PROPERTY_EDITABLE, value );
      }
      else if ( "listen".equals( tag ) || "listener".equals( tag ) )
      {
        String value = tags[++i];
        Object curValue = aComponent.getClientProperty( PROPERTY_LISTENER );
        if ( curValue == null )
        {
          aComponent.putClientProperty( PROPERTY_LISTENER, new String[] { value } );
        }
        else
        {
          String[] newValue = Arrays.copyOf( ( String[] )curValue, ( ( String[] )curValue ).length + 1 );
          newValue[newValue.length - 1] = value;
          aComponent.putClientProperty( PROPERTY_LISTENER, newValue );
        }
      }
    }
  }
}
