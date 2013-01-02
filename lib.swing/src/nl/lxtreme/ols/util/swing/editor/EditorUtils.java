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
package nl.lxtreme.ols.util.swing.editor;


import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;

import org.osgi.service.metatype.*;


/**
 * Provides some utility methods for creating editors.
 */
public class EditorUtils
{
  // CONSTANTS

  public static final String PROPERTY_ATTRIBUTE = "ad";
  public static final String PROPERTY_READONLY = "readonly";
  public static final String PROPERTY_EDITABLE = "editable";
  public static final String PROPERTY_LISTENER = "listener";

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditorUtils} instance.
   */
  public EditorUtils()
  {
    // Nop
  }

  // METHODS

  /**
   * Applies component properties and wires all listeners (if any).
   * 
   * @param aEditorComponents
   *          the collection with editor components.
   */
  public void applyComponentProperties( final Iterable<JComponent> aEditorComponents )
  {
    // Create an index on the component's name...
    Map<String, JComponent> nameIndex = new HashMap<String, JComponent>();
    for ( JComponent comp : aEditorComponents )
    {
      String name;

      AttributeDefinition ad = ( AttributeDefinition )comp.getClientProperty( PROPERTY_ATTRIBUTE );
      if ( ad != null )
      {
        // Set the name of attribute definition...
        name = ad.getID();

        comp.setName( name );

        parseMetaTags( comp, ad );

        // Keep track of invalid inputs...
        EditorInputVerifier.install( comp, ad );

        // Set the (optional) description as tool tip...
        String desc = getDescription( ad );
        if ( ( desc != null ) && !"".equals( desc.trim() ) )
        {
          comp.setToolTipText( desc );
        }
      }

      nameIndex.put( comp.getName(), comp );
    }

    // Process the component's properties...
    for ( JComponent comp : aEditorComponents )
    {
      Object value = comp.getClientProperty( PROPERTY_READONLY );
      if ( Boolean.TRUE.equals( value ) )
      {
        comp.setEnabled( false );
      }

      value = comp.getClientProperty( PROPERTY_EDITABLE );
      if ( Boolean.TRUE.equals( value ) )
      {
        if ( comp instanceof JComboBox )
        {
          ( ( JComboBox )comp ).setEditable( true );
        }
        else if ( comp instanceof JTextComponent )
        {
          ( ( JTextComponent )comp ).setEditable( true );
        }
      }

      value = comp.getClientProperty( PROPERTY_LISTENER );
      if ( value != null )
      {
        for ( String descriptor : ( String[] )value )
        {
          addListener( nameIndex, comp, descriptor );
        }
      }
    }
  }

  /**
   * Creates an editor component for the given {@link AttributeDefinition}.
   * 
   * @param aAttributeDef
   *          the attribute definition to base the editor component on, cannot
   *          be <code>null</code>;
   * @param aInitialValue
   *          the (optional) initial value to apply, can be <code>null</code>.
   * @return a editor component, can be <code>null</code> in case no editor
   *         component could be created.
   */
  public JComponent createEditor( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    JComponent result = null;

    String[] optionValues = aAttributeDef.getOptionValues();
    String[] optionLabels = aAttributeDef.getOptionLabels();

    if ( ( ( optionValues != null ) && ( optionValues.length > 0 ) )
        || ( ( optionLabels != null ) && ( optionLabels.length > 0 ) ) )
    {
      result = createFixedChoiceEditor( aAttributeDef, aInitialValue );
    }
    else
    {
      int type = aAttributeDef.getType();
      switch ( type )
      {
        case AttributeDefinition.BOOLEAN:
        {
          Boolean initialValue = getDefaultBooleanValue( aAttributeDef, aInitialValue );
          result = createBooleanEditor( aAttributeDef, initialValue.booleanValue() );
          break;
        }

        case AttributeDefinition.CHARACTER:
        {
          Character initialValue = getDefaultCharacterValue( aAttributeDef, aInitialValue );
          result = createStringEditor( aAttributeDef, initialValue.toString() );
          break;
        }

        case AttributeDefinition.BYTE:
        case AttributeDefinition.DOUBLE:
        case AttributeDefinition.FLOAT:
        case AttributeDefinition.INTEGER:
        case AttributeDefinition.SHORT:
        {
          String initialValue = getDefaultStringValue( aAttributeDef, aInitialValue );
          result = createNumericEditor( aAttributeDef, initialValue );
          break;
        }

        case AttributeDefinition.PASSWORD:
        {
          String initialValue = getDefaultStringValue( aAttributeDef, aInitialValue );
          result = createPasswordEditor( aAttributeDef, initialValue );
          break;
        }

        case AttributeDefinition.STRING:
        {
          String initialValue = getDefaultStringValue( aAttributeDef, aInitialValue );
          result = createStringEditor( aAttributeDef, initialValue );
          break;
        }
      }
    }

    // Store for later use...
    result.putClientProperty( PROPERTY_ATTRIBUTE, aAttributeDef );

    return result;
  }

  /**
   * @param aComponent
   * @return
   */
  @SuppressWarnings( "boxing" )
  public Object getComponentValue( final Component aComponent )
  {
    Object value = null;
    if ( aComponent instanceof AbstractButton )
    {
      value = ( ( AbstractButton )aComponent ).isSelected();
    }
    else if ( aComponent instanceof JComboBox )
    {
      value = ( ( JComboBox )aComponent ).getSelectedItem();
    }
    else if ( aComponent instanceof JTextComponent )
    {
      value = ( ( JTextComponent )aComponent ).getText();
    }
    else if ( aComponent instanceof JList )
    {
      value = ( ( JList )aComponent ).getSelectedIndex();
    }
    else if ( aComponent instanceof JSlider )
    {
      value = ( ( JSlider )aComponent ).getValue();
    }
    else if ( aComponent instanceof JSpinner )
    {
      value = ( ( JSpinner )aComponent ).getValue();
    }
    return value;
  }

  /**
   * @param aEditorComponents
   * @return a map of component name to component value, never <code>null</code>
   *         .
   */
  public Map<String, Object> getComponentValues( final Iterable<JComponent> aEditorComponents )
  {
    Map<String, Object> result = new HashMap<String, Object>();
    for ( JComponent comp : aEditorComponents )
    {
      String key = comp.getName();
      if ( key == null )
      {
        AttributeDefinition ad = ( AttributeDefinition )comp.getClientProperty( PROPERTY_ATTRIBUTE );
        if ( ad != null )
        {
          key = ad.getID();
        }
      }
      Object value = getComponentValue( comp );

      result.put( key, value );
    }
    return result;
  }

  /**
   * Returns the default value for the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to use, cannot be <code>null</code>;
   * @param aInitialValue
   *          the initial value, if defined, will have precedence on the default
   *          defined in the given attribute definition. May be
   *          <code>null</code>.
   * @return a default value, can be <code>null</code>.
   */
  public Object getDefaultValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    String[] optionValues = aAttributeDef.getOptionValues();
    String[] optionLabels = aAttributeDef.getOptionLabels();

    if ( ( ( optionValues != null ) && ( optionValues.length > 0 ) )
        || ( ( optionLabels != null ) && ( optionLabels.length > 0 ) ) )
    {
      return getDefaultStringValue( aAttributeDef, aInitialValue );
    }
    else
    {
      int type = aAttributeDef.getType();
      switch ( type )
      {
        case AttributeDefinition.BOOLEAN:
          return getDefaultBooleanValue( aAttributeDef, aInitialValue );

        case AttributeDefinition.CHARACTER:
          return getDefaultCharacterValue( aAttributeDef, aInitialValue );

        case AttributeDefinition.BYTE:
        case AttributeDefinition.DOUBLE:
        case AttributeDefinition.FLOAT:
        case AttributeDefinition.INTEGER:
        case AttributeDefinition.SHORT:
          return getDefaultNumericValue( aAttributeDef, aInitialValue );

        default:
          return getDefaultStringValue( aAttributeDef, aInitialValue );
      }
    }
  }

  /**
   * Returns the (optional) description defined in the given attribute
   * definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to obtain the description from, cannot be
   *          <code>null</code>.
   * @return a description, can be <code>null</code>.
   */
  public String getDescription( final AttributeDefinition aAttributeDef )
  {
    String description = aAttributeDef.getDescription();
    if ( ( description == null ) || "".equals( description.trim() ) )
    {
      return "";
    }

    int openCurlyIdx = description.indexOf( '{' );
    int closeCurlyIdx = description.lastIndexOf( '}' );
    if ( ( openCurlyIdx < 0 ) || ( closeCurlyIdx < 0 ) )
    {
      return description;
    }

    StringBuilder sb = new StringBuilder();
    if ( openCurlyIdx > 0 )
    {
      sb.append( description.substring( 0, openCurlyIdx ) );
    }
    sb.append( description.substring( closeCurlyIdx + 1 ) );
    return sb.toString();
  }

  /**
   * Returns whether or not the given attribute definition has meta-tags in its
   * description field.
   * 
   * @param aAttributeDef
   *          the attribute definition to test, cannot be <code>null</code>.
   * @return <code>true</code> if the given attribute definition has meta tags
   *         in its description field, <code>false</code> otherwise.
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
   * Parses the meta tags found in the description field of the given
   * {@link AttributeDefinition} and places them as client properties on the
   * given component.
   * 
   * @param aAttributeDef
   * @param aResult
   */
  public void parseMetaTags( final JComponent aComponent, final AttributeDefinition aAttributeDef )
  {
    boolean readOnly = false;
    boolean editable = false;

    String[] tags = getTags( aAttributeDef );
    for ( int i = 0; i < tags.length; i++ )
    {
      String tag = tags[i];
      if ( PROPERTY_READONLY.equals( tag ) )
      {
        readOnly = Boolean.parseBoolean( tags[++i] );
      }
      else if ( PROPERTY_EDITABLE.equals( tag ) )
      {
        editable = Boolean.parseBoolean( tags[++i] );
      }
      else if ( PROPERTY_LISTENER.equals( tag ) )
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

    aComponent.putClientProperty( PROPERTY_READONLY, Boolean.valueOf( readOnly ) );
    aComponent.putClientProperty( PROPERTY_EDITABLE, Boolean.valueOf( editable ) );
  }

  /**
   * Tries to apply a default selection to the given combobox component.
   * 
   * @param aComboBox
   *          the combobox to apply the selection to;
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the (optional) initial value to use.
   */
  protected void applyDefaultSelection( final JComboBox aComboBox, final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    final ComboBoxModel model = aComboBox.getModel();

    int selectedIdx = -1;

    // Try to determine the type of this initial value...
    String value = getDefaultStringValue( aAttributeDef, aInitialValue );
    if ( value.matches( "\\d+" ) )
    {
      // Integer value lets use it as starter for the index...
      int idx = Integer.parseInt( value );
      if ( ( idx >= 0 ) && ( idx < model.getSize() ) )
      {
        // Presume it is a valid index...
        selectedIdx = idx;
      }
    }

    if ( selectedIdx < 0 )
    {
      // Try to match based on actual model value...
      for ( int i = 0; ( selectedIdx < 0 ) && ( i < model.getSize() ); i++ )
      {
        String modelValue = String.valueOf( model.getElementAt( i ) );
        if ( modelValue.equals( value ) )
        {
          selectedIdx = i;
        }
      }
    }

    // Apply the actual selection...
    if ( selectedIdx >= 0 )
    {
      aComboBox.setSelectedIndex( selectedIdx );
    }
  }

  /**
   * Creates a boolean editor component.
   * 
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the (optional) initial value to use.
   * @return an editor component, never <code>null</code>.
   */
  protected JCheckBox createBooleanEditor( final AttributeDefinition aAttributeDef, final boolean aInitialValue )
  {
    return new JCheckBox( "", aInitialValue );
  }

  /**
   * Creates an editor component for a fixed number of options.
   * 
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the initial value to use.
   * @return an editor component, never <code>null</code>.
   */
  protected JComboBox createFixedChoiceEditor( final AttributeDefinition aAttributeDef, final Object aInitialValue )
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

    final JComboBox result = new JComboBox( optionValues );
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
    // Try to make a default selection...
    applyDefaultSelection( result, aAttributeDef, aInitialValue );

    return result;
  }

  /**
   * Creates a numeric editor component.
   * 
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the initial value to use.
   * @return an editor component, never <code>null</code>.
   */
  protected JComponent createNumericEditor( final AttributeDefinition aAttributeDef, final String aInitialValue )
  {
    return new JTextField( aInitialValue );
  }

  /**
   * Creates a password editor component.
   * 
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the initial value to use.
   * @return an editor component, never <code>null</code>.
   */
  protected JComponent createPasswordEditor( final AttributeDefinition aAttributeDef, final String aInitialValue )
  {
    return new JPasswordField( aInitialValue );
  }

  /**
   * Creates a string editor component.
   * 
   * @param aAttributeDef
   *          the attribute definition to use;
   * @param aInitialValue
   *          the initial value to use.
   * @return an editor component, never <code>null</code>.
   */
  protected JComponent createStringEditor( final AttributeDefinition aAttributeDef, final String aInitialValue )
  {
    return new JTextField( aInitialValue );
  }

  /**
   * Returns the default boolean value for the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to retrieve the boolean default for,
   *          cannot be <code>null</code>;
   * @param aInitialValue
   *          the (optional) initial value to interpret as boolean value.
   * @return a boolean value, defaults to <code>false</code>.
   */
  protected final Boolean getDefaultBooleanValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    Boolean defaultValue;
    if ( aInitialValue != null )
    {
      defaultValue = Boolean.valueOf( String.valueOf( aInitialValue ) );
    }
    else
    {
      String[] defaults = aAttributeDef.getDefaultValue();
      defaultValue = Boolean.valueOf( ( defaults != null ) && ( defaults.length > 0 )
          && Boolean.parseBoolean( defaults[0] ) );
    }
    return defaultValue;
  }

  /**
   * Returns the default character value for the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to retrieve the character default for,
   *          cannot be <code>null</code>;
   * @param aInitialValue
   *          the (optional) initial value to interpret as character value.
   * @return a character value, defaults to <code>' '</code>.
   */
  protected final Character getDefaultCharacterValue( final AttributeDefinition aAttributeDef,
      final Object aInitialValue )
  {
    Character defaultValue;

    String value;
    if ( aInitialValue != null )
    {
      value = String.valueOf( aInitialValue );
    }
    else
    {
      String[] defaults = aAttributeDef.getDefaultValue();
      if ( ( defaults != null ) && ( defaults.length > 0 ) )
      {
        value = defaults[0];
      }
      else
      {
        value = "";
      }
    }

    if ( value.length() > 0 )
    {
      defaultValue = Character.valueOf( value.charAt( 0 ) );
    }
    else
    {
      defaultValue = Character.valueOf( ' ' );
    }

    return defaultValue;
  }

  /**
   * Returns the default number value for the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to retrieve the number default for,
   *          cannot be <code>null</code>;
   * @param aInitialValue
   *          the (optional) initial value to interpret as number value.
   * @return a number value, defaults to <code>0</code>.
   */
  protected final Number getDefaultNumericValue( final AttributeDefinition aAttributeDef, final Object aInitialValue )
  {
    Number defaultValue;

    String value;
    if ( aInitialValue != null )
    {
      value = String.valueOf( aInitialValue );
    }
    else
    {
      String[] defaults = aAttributeDef.getDefaultValue();
      if ( ( defaults != null ) && ( defaults.length > 0 ) )
      {
        value = defaults[0];
      }
      else
      {
        value = "0";
      }
    }

    switch ( aAttributeDef.getType() )
    {
      case AttributeDefinition.BYTE:
        defaultValue = Byte.valueOf( value );
        break;
      case AttributeDefinition.DOUBLE:
        defaultValue = Double.valueOf( value );
        break;
      case AttributeDefinition.FLOAT:
        defaultValue = Float.valueOf( value );
        break;
      case AttributeDefinition.SHORT:
        defaultValue = Short.valueOf( value );
        break;

      case AttributeDefinition.INTEGER:
      default:
        defaultValue = Integer.valueOf( value );
        break;
    }

    return defaultValue;
  }

  /**
   * Returns the default string value for the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to retrieve the string default for,
   *          cannot be <code>null</code>;
   * @param aInitialValue
   *          the (optional) initial value to interpret as string value.
   * @return a string value, defaults to <code>""</code>.
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
   * @param aIndex
   * @param aComponent
   * @param aDescriptor
   */
  private void addListener( final Map<String, JComponent> aIndex, final JComponent aComponent, final String aDescriptor )
  {
    if ( aComponent instanceof AbstractButton )
    {
      final AbstractButton button = ( AbstractButton )aComponent;
      final String[] parts = aDescriptor.split( "\\s*;\\s*" );

      button.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          for ( String part : parts )
          {
            final boolean aInvert = part.startsWith( "!" );
            final JComponent target = aIndex.get( aInvert ? part.substring( 1 ) : part );
            if ( target != null )
            {
              boolean value = button.isSelected();
              if ( aInvert )
              {
                value = !value;
              }
              target.setEnabled( value );
            }
          }
        }
      } );

      for ( String part : parts )
      {
        final boolean aInvert = part.startsWith( "!" );
        final JComponent target = aIndex.get( aInvert ? part.substring( 1 ) : part );
        if ( target != null )
        {
          boolean value = button.isSelected();
          if ( aInvert )
          {
            value = !value;
          }
          target.setEnabled( value );
        }
      }
    }
    else if ( aComponent instanceof JComboBox )
    {
      final JComboBox combobox = ( JComboBox )aComponent;
      final String[] parts = aDescriptor.split( "\\s*:\\s*" );

      combobox.addActionListener( new ActionListener()
      {
        @Override
        public void actionPerformed( final ActionEvent aEvent )
        {
          final int index = combobox.getSelectedIndex();
          if ( ( index >= 0 ) && ( index < parts.length ) )
          {
            String part = parts[index];
            final boolean aInvert = part.startsWith( "!" );
            final JComponent target = aIndex.get( aInvert ? part.substring( 1 ) : part );
            if ( target != null )
            {
              boolean value = aInvert ? false : true;
              target.setEnabled( value );
            }
          }
        }
      } );

      final int index = combobox.getSelectedIndex();
      if ( ( index >= 0 ) && ( index < parts.length ) )
      {
        String part = parts[index];
        final boolean aInvert = part.startsWith( "!" );
        final JComponent target = aIndex.get( aInvert ? part.substring( 1 ) : part );
        if ( target != null )
        {
          boolean value = aInvert ? false : true;
          target.setEnabled( value );
        }
      }
    }
    else
    {
      throw new RuntimeException( "Cannot add listener to component: " + aComponent );
    }
  }

  /**
   * Returns all meta tags from the given attribute definition.
   * 
   * @param aAttributeDef
   *          the attribute definition to get all meta tags from, cannot be
   *          <code>null</code>.
   * @return an array of meta tags, never <code>null</code>.
   */
  private String[] getTags( final AttributeDefinition aAttributeDef )
  {
    String description = aAttributeDef.getDescription();
    if ( ( description == null ) || "".equals( description.trim() ) )
    {
      return new String[0];
    }

    int openCurlyIdx = description.indexOf( '{' );
    int closeCurlyIdx = description.lastIndexOf( '}' );
    if ( ( openCurlyIdx < 0 ) || ( closeCurlyIdx < 0 ) || ( closeCurlyIdx < openCurlyIdx ) )
    {
      return new String[0];
    }

    description = description.substring( openCurlyIdx + 1, closeCurlyIdx );
    return description.split( "\\s*[=,]\\s*" );
  }
}
