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
package nl.lxtreme.ols.client.ui.editor;


import static nl.lxtreme.ols.client.ui.editor.EditorUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.*;
import javax.swing.text.*;

import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.metatype.*;


/**
 * Provides a generic editor panel.
 */
public class EditorPanel extends JPanel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final int PADDING = 6;

  // VARIABLES

  private final ObjectClassDefinition ocd;
  private final Map<AttributeDefinition, JComponent> components;

  // CONSTRUCTORS

  /**
   * Creates a new {@link EditorPanel} instance.
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
   */
  public EditorPanel( final ObjectClassDefinition aOCD, final Dictionary<Object, Object> aSettings,
      final String aTitle, final IEditorProvider aProvider )
  {
    this( aOCD, asMap( aSettings ), aTitle, aProvider );
  }

  /**
   * Creates a new {@link EditorPanel} instance.
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
   */
  public EditorPanel( final ObjectClassDefinition aOCD, final Properties aSettings, final String aTitle,
      final IEditorProvider aProvider )
  {
    super( new SpringLayout() );

    this.ocd = aOCD;
    this.components = new HashMap<AttributeDefinition, JComponent>();

    Map<Object, Object> settings = aSettings;
    if ( settings == null )
    {
      settings = Collections.emptyMap();
    }

    initPanel( settings, aTitle, aProvider );
  }

  // METHODS

  /**
   * Converts a given {@link Dictionary} to a {@link Map}.
   * 
   * @param aValue
   *          the dictionary to convert, can be <code>null</code>.
   * @return a map representation of the given {@link Dictionary}, or an empty
   *         map is the given value was <code>null</code>.
   */
  @SuppressWarnings( "rawtypes" )
  private static Properties asMap( final Dictionary aValue )
  {
    Properties result = new Properties();
    if ( aValue != null )
    {
      Enumeration keys = aValue.keys();
      while ( keys.hasMoreElements() )
      {
        Object key = keys.nextElement();
        result.put( key, aValue.get( key ) );
      }
    }
    return result;
  }

  /**
   * @return <code>true</code> if the settings are valid, <code>false</code>
   *         otherwise.
   */
  public boolean areSettingsValid()
  {
    for ( Entry<AttributeDefinition, JComponent> entry : this.components.entrySet() )
    {
      String value = String.valueOf( getComponentValue( entry.getValue() ) );

      String validationResult = entry.getKey().validate( value );
      if ( ( validationResult != null ) && !"".equals( validationResult ) )
      {
        return false;
      }
    }

    return true;
  }

  /**
   * @return the properties with the current values, never <code>null</code>.
   */
  public final Properties getProperties()
  {
    Properties result = new Properties();
    for ( Entry<AttributeDefinition, JComponent> entry : this.components.entrySet() )
    {
      Object value = getComponentValue( entry.getValue() );

      if ( value != null )
      {
        result.put( entry.getKey().getID(), String.valueOf( value ) );
      }
    }
    return result;
  }

  /**
   * Initializes this panel.
   * 
   * @param aSettings
   *          the settings to use, cannot be <code>null</code>;
   * @param aTitle
   *          the (optional) title to display above all editors;
   * @param aProvider
   *          the editor provider to use, can be <code>null</code>.
   */
  final void initPanel( final Map<Object, Object> aSettings, final String aTitle, final IEditorProvider aProvider )
  {
    if ( ( aTitle != null ) && !"".equals( aTitle.trim() ) )
    {
      SpringLayoutUtils.addSeparator( this, aTitle );
    }

    final EditorUtils editorUtils = new EditorUtils();

    AttributeDefinition[] ads = this.ocd.getAttributeDefinitions( ObjectClassDefinition.ALL );
    for ( AttributeDefinition ad : ads )
    {
      Object initialValue = editorUtils.getDefaultValue( ad, aSettings.get( ad.getID() ) );

      JComponent comp = editorUtils.createEditor( ad, initialValue );
      if ( !isManagedComponent( comp ) && ( aProvider != null ) )
      {
        comp = aProvider.createEditor( ad, initialValue );
      }

      if ( comp != null )
      {
        add( createRightAlignedLabel( ad.getName() ) );
        add( comp );

        this.components.put( ad, comp );
      }
    }

    applyComponentProperties( this.components.values() );

    SpringLayoutUtils.makeEditorGrid( this, PADDING, PADDING, PADDING, PADDING );
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
   * @param aComponents
   */
  private void applyComponentProperties( final Collection<JComponent> aComponents )
  {
    // Create an index on the component's name...
    Map<String, JComponent> nameIndex = new HashMap<String, JComponent>();
    for ( JComponent comp : aComponents )
    {
      nameIndex.put( comp.getName(), comp );
    }

    // Process the component's properties...
    for ( JComponent comp : aComponents )
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
   * @param aComponent
   * @return
   */
  @SuppressWarnings( "boxing" )
  private Object getComponentValue( final Component aComponent )
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
   * Returns whether the given component is managed or not.
   * 
   * @param aComponent
   *          the component to test, can be <code>null</code>.
   * @return <code>true</code> if the given component is a managed component,
   *         <code>false</code> otherwise.
   */
  private boolean isManagedComponent( final JComponent aComponent )
  {
    if ( aComponent == null )
    {
      return false;
    }

    Object managed = aComponent.getClientProperty( PROPERTY_MANAGED );
    return ( managed == null ) || Boolean.TRUE.equals( managed );
  }
}
