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
package nl.lxtreme.ols.client.ui;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.editor.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareDialog;

import org.osgi.service.metatype.*;


/**
 * Provides the preferences dialog.
 */
public class PreferencesDialog extends JDialog implements StatusAwareDialog
{
  // INNER TYPES

  /**
   * Provides a custom combobox model for the current color schemes.
   */
  static final class ColorSchemeModel extends AbstractListModel implements ComboBoxModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private volatile int selected = -1;
    private final ColorSchemeManager colorSchemeManager;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ColorSchemeModel} instance.
     */
    public ColorSchemeModel( final ColorSchemeManager aColorSchemeManager )
    {
      this.colorSchemeManager = aColorSchemeManager;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      java.util.List<String> schemes = getColorSchemeManager().getColorSchemes();
      if ( ( aIndex < 0 ) || ( aIndex >= schemes.size() ) )
      {
        return null;
      }
      return schemes.get( aIndex );
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
      return getColorSchemeManager().getColorSchemeCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      java.util.List<String> schemes = getColorSchemeManager().getColorSchemes();
      this.selected = schemes.indexOf( aItem );
    }

    /**
     * @return the current color scheme manager, never <code>null</code>.
     */
    private ColorSchemeManager getColorSchemeManager()
    {
      return this.colorSchemeManager;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ObjectClassDefinition ocd;
  private final EditorPanel editorPanel;

  private DialogStatus status;

  // CONSTRUCTORS

  /**
   * Creates a new {@link PreferencesDialog} instance.
   * 
   * @param aParent
   *          the parent of the preferences window, can be <code>null</code>;
   * @param aColorSchemeManager
   *          the color scheme manager to use, cannot be <code>null</code>;
   * @param aOCD
   *          the {@link ObjectClassDefinition} to use as main configuration
   *          part, cannot be <code>null</code>;
   * @param aConfiguration
   *          the configuration values to use, cannot be <code>null</code>.
   */
  public PreferencesDialog( final Window aParent, final ColorSchemeManager aColorSchemeManager,
      final ObjectClassDefinition aOCD, final Properties aConfiguration )
  {
    super( aParent, aOCD.getName(), ModalityType.APPLICATION_MODAL );

    this.ocd = aOCD;

    // Avoid the dialog to be resized automatically...
    getRootPane().putClientProperty( "unmanaged", Boolean.TRUE );

    this.editorPanel = new EditorPanel( this.ocd, aConfiguration, "Preferences", new IEditorProvider()
    {
      @Override
      public JComponent createEditor( final AttributeDefinition aAttributeDefinition, final Object aInitialValue )
      {
        if ( "colorScheme".equals( aAttributeDefinition.getID() ) )
        {
          JComboBox colorScheme = new JComboBox( new ColorSchemeModel( aColorSchemeManager ) );
          colorScheme.setSelectedItem( aInitialValue );
          colorScheme.setToolTipText( "What color scheme is to be used. Will be applied immediately." );

          return colorScheme;
        }
        return null;
      }
    } );

    JButton okButton = StandardActionFactory.createOkButton();
    JButton cancelButton = StandardActionFactory.createCancelButton();

    JComponent buttonPane = createButtonPane( okButton, cancelButton );

    setupDialogContentPane( this, this.editorPanel, buttonPane, cancelButton );

    pack();
  }

  // METHODS

  /**
   * @return <code>true</code> if the properties are valid, <code>false</code>
   *         otherwise.
   */
  public boolean arePropertiesValid()
  {
    return this.editorPanel.areSettingsValid();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DialogStatus getDialogStatus()
  {
    return this.status;
  }

  /**
   * Returns the (possibly changed) preferences.
   * 
   * @return the current preferences, after the dialog is closed this will yield
   *         the preferences adjusted by the user.
   */
  public Properties getProperties()
  {
    return this.editorPanel.getProperties();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setDialogStatus( final DialogStatus aStatus )
  {
    this.status = aStatus;
  }
}
