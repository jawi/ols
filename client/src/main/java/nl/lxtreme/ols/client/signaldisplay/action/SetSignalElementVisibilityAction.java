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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


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
    super();

    this.signalElement = aSignalElement;
    this.controller = aController;

    putValue( Action.NAME, getLabel( aSignalElement ) );
    putValue( Action.SELECTED_KEY, Boolean.valueOf( aSignalElement.isEnabled() ) );
  }

  // METHODS

  /**
   * Determines the label for this action.
   * 
   * @param aSignalElement
   *          the signal element to determine the label of this action for,
   *          cannot be <code>null</code>.
   * @return a label, never <code>null</code>.
   */
  private static String getLabel( final SignalElement aSignalElement )
  {
    String prefix = "Show";
    if ( aSignalElement.isEnabled() )
    {
      prefix = "Hide";
    }

    String suffix;
    if ( aSignalElement.isDigitalSignal() )
    {
      Channel channel = aSignalElement.getChannel();
      suffix = "channel " + channel.getIndex();
    }
    else if ( aSignalElement.isAnalogSignal() )
    {
      suffix = "analog signal";
    }
    else if ( aSignalElement.isGroupSummary() )
    {
      suffix = "group summary";
    }
    else if ( aSignalElement.isSignalGroup() )
    {
      suffix = "digital signals";
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
    final JCheckBoxMenuItem menuitem = ( JCheckBoxMenuItem )aEvent.getSource();

    this.signalElement.setEnabled( menuitem.getState() );

    // TODO: this can be made smarter...
    this.controller.getSignalDiagram().repaint( 25L );
  }
}
