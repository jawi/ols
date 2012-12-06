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
package nl.lxtreme.ols.client.ui.tool;


import static nl.lxtreme.ols.client.ui.ClientSwingUtil.*;
import static nl.lxtreme.ols.client.ui.editor.EditorUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.common.acquisition.Cursor;

import org.osgi.service.metatype.*;


/**
 * Provides a panel for configuring tools.
 */
final class ToolConfigPanel extends JPanel implements Constants
{
  // INNER TYPES

  private class ChangeReflector implements ActionListener, ChangeListener, ListSelectionListener, DocumentListener
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

  // VARIABLES

  private final ObjectClassDefinition ocd;
  private final Map<AttributeDefinition, JComponent> components;

  private JCheckBox decodeEntireTimeline;
  private JComboBox decodeMarkerA;
  private JComboBox decodeMarkerB;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolConfigPanel} instance.
   * 
   * @param aSettings
   */
  public ToolConfigPanel( final ObjectClassDefinition aOCD, final AcquisitionDataInfo aContext,
      final Map<Object, Object> aSettings )
  {
    super( new SpringLayout() );

    this.ocd = aOCD;

    this.components = new HashMap<AttributeDefinition, JComponent>();

    initPanel( aContext, aSettings );
  }

  // METHODS

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
  public Dictionary<Object, Object> getProperties()
  {
    Hashtable<Object, Object> result = new Hashtable<Object, Object>();
    for ( Entry<AttributeDefinition, JComponent> entry : this.components.entrySet() )
    {
      Object value = getComponentValue( entry.getValue() );

      if ( value != null )
      {
        result.put( entry.getKey().getID(), value );
      }
    }
    result.put( PROPERTY_DECODE_ENTIRE_TIMELINE, Boolean.valueOf( this.decodeEntireTimeline.isSelected() ) );
    result.put( PROPERTY_DECODE_MARKER_A, getMarkerValue( this.decodeMarkerA ) );
    result.put( PROPERTY_DECODE_MARKER_B, getMarkerValue( this.decodeMarkerB ) );
    return result;
  }

  /**
   * Initializes this panel.
   * 
   * @param aContext
   * @param aSettings
   */
  final void initPanel( final AcquisitionDataInfo aContext, final Map<Object, Object> aSettings )
  {
    SpringLayoutUtils.addSeparator( this, "Context" );

    add( createRightAlignedLabel( "Decode all?" ) );
    this.decodeEntireTimeline = ( JCheckBox )add( new JCheckBox( "", isDecodeAll( aSettings ) ) );
    this.decodeEntireTimeline.setEnabled( aContext.hasDefinedCursors() );
    this.decodeEntireTimeline.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateContextState();
      }
    } );

    add( createRightAlignedLabel( "Marker A" ) );
    this.decodeMarkerA = ( JComboBox )add( createOptionalCursorSelector( aContext.getCursors(),
        getIndexMarkerA( aContext.getCursors(), aSettings ) ) );

    add( createRightAlignedLabel( "Marker B" ) );
    this.decodeMarkerB = ( JComboBox )add( createOptionalCursorSelector( aContext.getCursors(),
        getIndexMarkerB( aContext.getCursors(), aSettings ) ) );

    // set default settings...
    updateContextState();

    SpringLayoutUtils.addSeparator( this, "Settings" );

    final ToolEditorUtils editorUtils = new ToolEditorUtils( aContext );

    AttributeDefinition[] ads = this.ocd.getAttributeDefinitions( ObjectClassDefinition.ALL );
    for ( AttributeDefinition ad : ads )
    {
      Object initialValue = aSettings.get( ad.getID() );

      JComponent comp = editorUtils.createEditor( ad, initialValue );
      if ( comp != null )
      {
        add( createRightAlignedLabel( ad.getName() ) );
        add( comp );

        this.components.put( ad, comp );
      }
    }

    applyComponentProperties( this.components.values() );

    wireChangeListeners();

    SpringLayoutUtils.makeEditorGrid( this, 6, 4, 6, 6 );
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
   * @return the index of the first marker, >= 0 or -1.
   */
  private int getIndexMarkerA( final Cursor[] aCursors, final Map<Object, Object> aSettings )
  {
    Object value = aSettings.get( Constants.PROPERTY_DECODE_MARKER_A );
    int idx = findIndex( aCursors, value, 1 );
    System.out.println( "IndexMarkerA = " + idx );
    return idx;
  }

  /**
   * @return the index of the second marker, >= 0 or -1.
   */
  private int getIndexMarkerB( final Cursor[] aCursors, final Map<Object, Object> aSettings )
  {
    Object value = aSettings.get( Constants.PROPERTY_DECODE_MARKER_B );
    int idx = findIndex( aCursors, value, 2 );
    System.out.println( "IndexMarkerB = " + idx );
    return idx;
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
   * Wires all components on this panel to fire a {@link PropertyChangeEvent} in
   * case their value changes.
   */
  private void wireChangeListeners()
  {
    final ChangeReflector changeReflector = new ChangeReflector();

    this.decodeEntireTimeline.addActionListener( changeReflector );
    this.decodeMarkerA.addActionListener( changeReflector );
    this.decodeMarkerB.addActionListener( changeReflector );

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
}
