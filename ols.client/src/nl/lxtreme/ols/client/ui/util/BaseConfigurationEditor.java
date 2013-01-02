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
package nl.lxtreme.ols.client.ui.util;


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareDialog;
import nl.lxtreme.ols.util.swing.editor.*;

import org.osgi.service.metatype.*;


/**
 * Provides a default configuration editor.
 * <p>
 * Users of this class are obliged to create a subclass from it and call
 * {@link #initDialog(ObjectClassDefinition, Map)} on the resulting object
 * yourself.
 * </p>
 */
public abstract class BaseConfigurationEditor extends JDialog implements ConfigurationEditor, ActionListener,
    PropertyChangeListener, StatusAwareDialog
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String TITLE_SETTINGS = "Settings";

  // VARIABLES

  private final EventListenerList eventListeners;

  private EditorPanel configPanel;
  private JButton closeButton;
  private JButton okButton;

  private volatile DialogStatus status;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BaseConfigurationEditor} instance.
   * 
   * @param aParent
   *          the parent of this dialog;
   * @param aTitle
   *          the title of this dialog.
   */
  protected BaseConfigurationEditor( final Window aParent, final String aTitle )
  {
    this( aParent, aTitle, ModalityType.MODELESS );
  }

  /**
   * Creates a new {@link BaseConfigurationEditor} instance.
   * 
   * @param aParent
   *          the parent of this dialog;
   * @param aTitle
   *          the title of this dialog;
   * @param aModalityType
   *          the modality type to use for this editor dialog.
   */
  protected BaseConfigurationEditor( final Window aParent, final String aTitle, final ModalityType aModalityType )
  {
    super( aParent, aTitle, aModalityType );

    this.eventListeners = new EventListenerList();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Object source = aEvent.getSource();
    if ( source == this.closeButton )
    {
      fireDialogCancelledEvent();

      fireConfigurationDiscardedEvent();
    }
    else if ( source == this.okButton )
    {
      fireDialogConfirmedEvent();

      if ( areSettingsValid() )
      {
        fireConfigurationAcknowledgedEvent();
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addConfigurationChangeListener( final ConfigurationChangeListener aListener )
  {
    this.eventListeners.add( ConfigurationChangeListener.class, aListener );
  }

  /**
   * Adds a given dialog listener to this dialog.
   */
  public void addDialogStateListener( final DialogStateListener aListener )
  {
    this.eventListeners.add( DialogStateListener.class, aListener );
  }

  /**
   * @return <code>true</code> if the settings of this tool are valid,
   *         <code>false</code> otherwise.
   */
  public boolean areSettingsValid()
  {
    return this.configPanel.areSettingsValid();
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
   * Returns the current value of PID.
   * 
   * @return the PID, cannot be <code>null</code>.
   */
  public abstract String getPid();

  /**
   * @return the configuration properties, never <code>null</code>.
   */
  public Dictionary<Object, Object> getProperties()
  {
    return this.configPanel.getProperties();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    // Update the state of the buttons to indicate what's wrong...
    this.closeButton.setEnabled( true );
    this.okButton.setEnabled( areSettingsValid() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeConfigurationChangeListener( final ConfigurationChangeListener aListener )
  {
    this.eventListeners.remove( ConfigurationChangeListener.class, aListener );
  }

  /**
   * Removes a given dialog listener from this dialog.
   */
  public void removeDialogStateListener( final DialogStateListener aListener )
  {
    this.eventListeners.remove( DialogStateListener.class, aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setDialogStatus( final DialogStatus aStatus )
  {
    this.status = aStatus;
  }

  /**
   * Factory method for creating a new {@link EditorPanel} instance.
   * 
   * @param aOCD
   *          the object-class definition to use;
   * @param aSettings
   *          the (initial) settings to use.
   * @return a new {@link EditorPanel} instance, never <code>null</code>.
   */
  protected EditorPanel createEditorPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aSettings )
  {
    return EditorPanel.create( aOCD, aSettings, TITLE_SETTINGS, null );
  }

  /**
   * Fires a dialog state change event to all interested listeners.
   * 
   * @param aState
   *          the new dialog state, cannot be <code>null</code>.
   */
  protected void fireDialogStateChangeEvent( final DialogStatus aState )
  {
    DialogStateListener[] listeners = this.eventListeners.getListeners( DialogStateListener.class );
    for ( DialogStateListener listener : listeners )
    {
      listener.onStateChanged( aState );
    }
  }

  /**
   * Initializes this dialog by creating and placing its contents.
   * 
   * @param aOCD
   *          the object-class definition to use;
   * @param aSettings
   *          the (initial) settings to use.
   */
  protected void initDialog( final ObjectClassDefinition aOCD, final Map<Object, Object> aSettings )
  {
    this.configPanel = createEditorPanel( aOCD, aSettings );
    this.configPanel.addPropertyChangeListener( this );

    this.closeButton = StandardActionFactory.createCloseButton();
    this.closeButton.addActionListener( this );
    this.okButton = StandardActionFactory.createOkButton();
    this.okButton.addActionListener( this );

    final JComponent buttonBar = SwingComponentUtils.createButtonPane( this.okButton, this.closeButton );

    SwingComponentUtils.setupDialogContentPane( this, this.configPanel, buttonBar, this.closeButton );
  }

  /**
   * Fires an event to all {@link ConfigurationChangeListener}s that the
   * configuration is acknowledged.
   */
  private void fireConfigurationAcknowledgedEvent()
  {
    ConfigurationChangeListener[] listeners = this.eventListeners.getListeners( ConfigurationChangeListener.class );
    for ( ConfigurationChangeListener listener : listeners )
    {
      listener.onConfigurationAcknowledged( getPid(), getProperties() );
    }
  }

  /**
   * Fires an event to all {@link ConfigurationChangeListener}s that the
   * configuration is to be discarded.
   */
  private void fireConfigurationDiscardedEvent()
  {
    ConfigurationChangeListener[] listeners = this.eventListeners.getListeners( ConfigurationChangeListener.class );
    for ( ConfigurationChangeListener listener : listeners )
    {
      listener.onConfigurationDiscarded();
    }
  }

  /**
   * Fires a dialog cancel event to all interested listeners.
   */
  private void fireDialogCancelledEvent()
  {
    fireDialogStateChangeEvent( DialogStatus.CANCEL );
  }

  /**
   * Fires a dialog confirm event to all interested listeners.
   */
  private void fireDialogConfirmedEvent()
  {
    fireDialogStateChangeEvent( DialogStatus.OK );
  }
}
