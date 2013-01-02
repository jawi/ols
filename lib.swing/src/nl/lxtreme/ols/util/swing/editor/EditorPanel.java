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


import static nl.lxtreme.ols.util.swing.editor.EditorUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.metatype.*;


/**
 * Provides a generic editor panel.
 */
public class EditorPanel extends JPanel
{
  // INNER TYPES

  /**
   * Converts the various UI-change events to single property change events.
   */
  protected final class ChangeReflector implements ActionListener, ChangeListener, ListSelectionListener,
      DocumentListener
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      firePropertyChangeEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void changedUpdate( final DocumentEvent aEvent )
    {
      firePropertyChangeEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void insertUpdate( final DocumentEvent aEvent )
    {
      firePropertyChangeEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeUpdate( final DocumentEvent aEvent )
    {
      firePropertyChangeEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stateChanged( final ChangeEvent aEvent )
    {
      firePropertyChangeEvent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void valueChanged( final ListSelectionEvent aEvent )
    {
      if ( !aEvent.getValueIsAdjusting() )
      {
        firePropertyChangeEvent();
      }
    }

    /**
     * 
     */
    private void firePropertyChangeEvent()
    {
      firePropertyChange( "changed", null, this );
    }
  }

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
  protected EditorPanel( final ObjectClassDefinition aOCD )
  {
    super( new SpringLayout() );

    this.ocd = aOCD;
    this.components = new HashMap<AttributeDefinition, JComponent>();
  }

  // METHODS

  /**
   * Factory method for creating a new {@link EditorPanel} instance.
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
   * @return a new, initialized {@link EditorPanel} instance, never
   *         <code>null</code>.
   */
  public static EditorPanel create( final ObjectClassDefinition aOCD, final Dictionary<Object, Object> aSettings,
      final String aTitle, final IEditorProvider aProvider )
  {
    return create( aOCD, asMap( aSettings ), aTitle, aProvider );
  }

  /**
   * Factory method for creating a new {@link EditorPanel} instance.
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
   * @return a new, initialized {@link EditorPanel} instance, never
   *         <code>null</code>.
   */
  public static EditorPanel create( final ObjectClassDefinition aOCD, final Map<Object, Object> aSettings,
      final String aTitle, final IEditorProvider aProvider )
  {
    EditorPanel result = new EditorPanel( aOCD );

    Map<Object, Object> settings = aSettings;
    if ( settings == null )
    {
      settings = Collections.emptyMap();
    }

    result.initPanel( settings, aTitle, aProvider );
    return result;
  }

  /**
   * Converts a given {@link Dictionary} to a {@link Map}.
   * 
   * @param aValue
   *          the dictionary to convert, can be <code>null</code>.
   * @return a map representation of the given {@link Dictionary}, or an empty
   *         map is the given value was <code>null</code>.
   */
  @SuppressWarnings( "rawtypes" )
  protected static Map<Object, Object> asMap( final Dictionary aValue )
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
    EditorUtils editorUtils = new EditorUtils();

    for ( Entry<AttributeDefinition, JComponent> entry : this.components.entrySet() )
    {
      String value = String.valueOf( editorUtils.getComponentValue( entry.getValue() ) );

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
  public Properties getProperties()
  {
    Properties result = new Properties();
    result.putAll( new EditorUtils().getComponentValues( this.components.values() ) );
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
  protected void initPanel( final Map<Object, Object> aSettings, final String aTitle, final IEditorProvider aProvider )
  {
    if ( ( aTitle != null ) && !"".equals( aTitle.trim() ) )
    {
      SpringLayoutUtils.addSeparator( this, aTitle );
    }

    final EditorUtils editorUtils = createEditorUtils();

    AttributeDefinition[] ads = this.ocd.getAttributeDefinitions( ObjectClassDefinition.ALL );
    for ( AttributeDefinition ad : ads )
    {
      Object initialValue = editorUtils.getDefaultValue( ad, aSettings.get( ad.getID() ) );

      JComponent comp = null;
      if ( aProvider != null )
      {
        comp = aProvider.createEditor( ad, initialValue );
      }
      if ( comp == null )
      {
        comp = editorUtils.createEditor( ad, initialValue );
      }

      if ( comp != null )
      {
        // Store for later use...
        comp.putClientProperty( PROPERTY_ATTRIBUTE, ad );

        add( createRightAlignedLabel( ad.getName() ) );
        add( comp );

        this.components.put( ad, comp );
      }
    }

    editorUtils.applyComponentProperties( this.components.values() );

    wireChangeListeners( new ChangeReflector() );

    SpringLayoutUtils.makeEditorGrid( this, PADDING, PADDING, PADDING, PADDING );
  }

  /**
   * Wires all components on this panel to fire a {@link PropertyChangeEvent} in
   * case their value changes.
   */
  protected void wireChangeListeners( final ChangeReflector changeReflector )
  {
    for ( JComponent comp : this.components.values() )
    {
      if ( comp instanceof AbstractButton )
      {
        ( ( AbstractButton )comp ).addActionListener( changeReflector );
      }
      else if ( comp instanceof JComboBox )
      {
        ( ( JComboBox )comp ).addActionListener( changeReflector );
      }
      else if ( comp instanceof JTextComponent )
      {
        ( ( JTextComponent )comp ).getDocument().addDocumentListener( changeReflector );
      }
      else if ( comp instanceof JList )
      {
        ( ( JList )comp ).addListSelectionListener( changeReflector );
      }
      else if ( comp instanceof JSlider )
      {
        ( ( JSlider )comp ).addChangeListener( changeReflector );
      }
      else if ( comp instanceof JSpinner )
      {
        ( ( JSpinner )comp ).addChangeListener( changeReflector );
      }
    }
  }

  /**
   * Factory method for creating new {@link EditorUtils} instances.
   * 
   * @return a new instance of {@link EditorUtils}, never <code>null</code>.
   */
  private EditorUtils createEditorUtils()
  {
    return new EditorUtils();
  }
}
