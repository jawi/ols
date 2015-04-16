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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.SignalElementType;


/**
 * Provides an action to set a channel either as visible or invisible.
 */
public class SetSignalElementVisibilityAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalElement signalElement;
  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetSignalElementVisibilityAction} instance.
   *
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to hide/show, cannot be <code>null</code>.
   */
  public SetSignalElementVisibilityAction( final SignalDiagramController aController, final SignalElement aSignalElement )
  {
    this.signalElement = aSignalElement;
    this.controller = aController;

    putValue( Action.NAME, getLabel( aSignalElement.getType(), aSignalElement.isEnabled() ) );
  }

  // METHODS

  /**
   * Determines the label for this action.
   *
   * @param aType
   *          the signal element to determine the label of this action for,
   *          cannot be <code>null</code>;
   * @param aVisible
   *          <code>true</code> if the signal element is currently visible,
   *          <code>false</code> if it is currently invisible.
   * @return a label, never <code>null</code>.
   */
  private static String getLabel( final SignalElementType aType, final boolean aVisible )
  {
    String prefix = aVisible ? "Hide" : "Show";

    String suffix;
    if ( aType == SignalElementType.DIGITAL_SIGNAL )
    {
      prefix = aVisible ? "Disable" : "Enable";
      suffix = "digital signal";
    }
    else if ( aType == SignalElementType.ANALOG_SIGNAL )
    {
      suffix = "analog signal";
    }
    else if ( aType == SignalElementType.GROUP_SUMMARY )
    {
      suffix = "group summary";
    }
    else
    {
      throw new InternalError( "Unknown signal element?!" );
    }

    return String.format( "%s %s", prefix, suffix );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    this.signalElement.setEnabled( !this.signalElement.isEnabled() );

    if ( this.signalElement.isDigitalSignal() )
    {
      // Digital signal stays on screen; so we can redraw this easily...
      this.controller.repaintSignalElement( this.signalElement );
    }
    else
    {
      // Group summary and analog scope are really hidden, so we need to repaint
      // the entire frame...
      this.controller.revalidateAll();
    }
  }
}
