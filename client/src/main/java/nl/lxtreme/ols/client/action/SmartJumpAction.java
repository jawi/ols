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
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Provides a "smart jump" action, allowing to navigate to the next/previous
 * edge, cursor or annotation.
 */
public class SmartJumpAction extends BaseAction
{
  // INNER TYPES

  /**
   * Denotes the direction in which a smart jump should be performed.
   */
  public static enum JumpDirection
  {
    LEFT, RIGHT;

    public boolean isLeft()
    {
      return this == LEFT;
    }

    public boolean isRight()
    {
      return this == RIGHT;
    }
  }

  /**
   * Denotes the type of jump that should be performed.
   */
  public static enum JumpType
  {
    CURSOR, SIGNAL_EDGE, ANNOTATION;
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JumpDirection direction;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SmartJumpAction} instance.
   */
  public SmartJumpAction( final JumpDirection aDirection, final ClientController aController )
  {
    super( getID( aDirection ), aController, getTitle( aDirection ), getDescription( aDirection ) );

    this.direction = aDirection;
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
      SignalDiagramController controller = getSignalDiagramController();
      controller.smartJump( getSignalDiagramModel().getSelectedChannelIndex(), type, this.direction );
    }
  }

  /**
   * @return
   */
  private SignalDiagramController getSignalDiagramController()
  {
    return getController().getSignalDiagramController();
  }

  /**
   * @return a {@link SignalDiagramModel}, never <code>null</code>.
   */
  private SignalDiagramModel getSignalDiagramModel()
  {
    return getSignalDiagramController().getViewModel();
  }
}
