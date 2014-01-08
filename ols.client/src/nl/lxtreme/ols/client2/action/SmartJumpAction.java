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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.Client.JumpDirection;
import nl.lxtreme.ols.client2.Client.JumpType;


/**
 * Provides a "smart jump" action, allowing to navigate to the next/previous
 * edge, cursor or annotation.
 */
public class SmartJumpAction extends AbstractManagedAction
{
  // INNER TYPES

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JumpDirection direction;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SmartJumpAction} instance.
   */
  public SmartJumpAction( JumpDirection aDirection )
  {
    super( getID( aDirection ) );

    this.direction = aDirection;

    putValue( NAME, getTitle( aDirection ) );
    putValue( LONG_DESCRIPTION, getDescription( aDirection ) );
  }

  // METHODS

  /**
   * @return
   */
  public static String getJumpLeftID()
  {
    return getID( JumpDirection.LEFT );
  }

  /**
   * @return
   */
  public static String getJumpRightID()
  {
    return getID( JumpDirection.RIGHT );
  }

  /**
   * @param aDirection
   * @return
   */
  private static String getDescription( final JumpDirection aDirection )
  {
    return String.format( "<html>Jumps to the %1$s event. Use<br/>SHIFT to jump to %1$s cursor;<br/>"
        + "CTRL to jump to %1$s edge, and<br/>ALT to jump to %1$s annotation.</html>",
        ( aDirection == JumpDirection.LEFT ? "previous" : "next" ) );
  }

  /**
   * @param aDirection
   * @return
   */
  private static String getID( final JumpDirection aDirection )
  {
    return String.format( "SmartJump%s", aDirection.name() );
  }

  /**
   * @param aDirection
   * @return
   */
  private static String getTitle( final JumpDirection aDirection )
  {
    return String.format( "Smart jump %s", aDirection.name().toLowerCase() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    // What do we need to jump to?
    JumpType type = null;
    if ( ( aEvent.getModifiers() & InputEvent.SHIFT_MASK ) != 0 )
    {
      // Cursor...
      type = JumpType.CURSOR;
    }
    else if ( ( aEvent.getModifiers() & InputEvent.CTRL_MASK ) != 0 )
    {
      // Signal edge...
      type = JumpType.SIGNAL_EDGE;
    }
    else if ( ( aEvent.getModifiers() & InputEvent.ALT_MASK ) != 0 )
    {
      // Annotation...
      type = JumpType.ANNOTATION;
    }

    if ( type != null )
    {
      Client client = getClient( aEvent );
      client.smartJump( type, this.direction );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() );
  }
}
