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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing.editor.util;


import java.awt.event.*;
import java.beans.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Provides a generic change listener for most common {@link JComponent}s, such
 * as {@link JButton}, {@link JCheckBox}, {@link JComboBox} and so on.
 */
public class GenericComponentChangeAdapter implements ActionListener, ChangeListener, ListSelectionListener,
    DocumentListener
{
  // CONSTANTS

  public static final String PROPERTY_NAME = "changed";

  // VARIABLES

  private final PropertyChangeListener listener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GenericComponentChangeAdapter} instance.
   * 
   * @param aListener
   *          the listener to pass all changes to, cannot be <code>null</code>.
   */
  public GenericComponentChangeAdapter( final PropertyChangeListener aListener )
  {
    if ( aListener == null )
    {
      throw new IllegalArgumentException( "Listener cannot be null!" );
    }
    this.listener = aListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    firePropertyChangeEvent( aEvent.getSource() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void changedUpdate( final DocumentEvent aEvent )
  {
    firePropertyChangeEvent( aEvent.getDocument() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void insertUpdate( final DocumentEvent aEvent )
  {
    firePropertyChangeEvent( aEvent.getDocument() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeUpdate( final DocumentEvent aEvent )
  {
    firePropertyChangeEvent( aEvent.getDocument() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stateChanged( final ChangeEvent aEvent )
  {
    firePropertyChangeEvent( aEvent.getSource() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void valueChanged( final ListSelectionEvent aEvent )
  {
    if ( !aEvent.getValueIsAdjusting() )
    {
      firePropertyChangeEvent( aEvent.getSource() );
    }
  }

  /**
   * Fires a {@link PropertyChangeEvent} to the contained listener.
   */
  private void firePropertyChangeEvent( final Object aSource )
  {
    this.listener.propertyChange( new PropertyChangeEvent( aSource, PROPERTY_NAME, //
        Long.valueOf( -1L ), Long.valueOf( System.currentTimeMillis() ) ) );
  }
}
