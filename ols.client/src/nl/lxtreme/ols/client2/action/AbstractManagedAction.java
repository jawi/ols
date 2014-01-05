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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Abstract base class for {@link ManagedAction}.
 */
public abstract class AbstractManagedAction extends AbstractAction implements ManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String id;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AbstractManagedAction} instance.
   */
  protected AbstractManagedAction( String aID )
  {
    this.id = aID;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return this.id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, ?> getProperties()
  {
    Map<String, Object> properties = new HashMap<String, Object>();
    for ( Object key : getKeys() )
    {
      properties.put( key.toString(), getValue( key.toString() ) );
    }
    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    // Nop
  }

  protected final Client getClient( ActionEvent aEvent )
  {
    return ( Client )SwingComponentUtils.getOwningWindow( aEvent );
  }
}
