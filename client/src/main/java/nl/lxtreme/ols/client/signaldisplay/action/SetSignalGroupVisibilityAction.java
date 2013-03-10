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
 * Provides an action to show/hide a all channels, group summary or analog scope
 * in a signal group.
 */
public class SetSignalGroupVisibilityAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ElementGroup signalGroup;
  private final SignalDiagramController controller;
  private final SignalElementType elementType;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetSignalGroupVisibilityAction} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to hide/show, cannot be <code>null</code>;
   * @param aType
   *          the signal element type to show/hide.
   */
  public SetSignalGroupVisibilityAction( final SignalDiagramController aController, final SignalElement aSignalElement,
      final SignalElementType aType )
  {
    if ( !aSignalElement.isSignalGroup() )
    {
      throw new IllegalArgumentException();
    }

    this.signalGroup = aSignalElement.getGroup();
    this.controller = aController;
    this.elementType = aType;

    String suffix;
    boolean visible;
    if ( aType == SignalElementType.DIGITAL_SIGNAL )
    {
      suffix = "digital signals";
      visible = this.signalGroup.isShowDigitalSignals();
    }
    else if ( aType == SignalElementType.ANALOG_SIGNAL )
    {
      suffix = "analog signal";
      visible = this.signalGroup.isShowAnalogSignal();
    }
    else if ( aType == SignalElementType.GROUP_SUMMARY )
    {
      suffix = "group summary";
      visible = this.signalGroup.isShowGroupSummary();
    }
    else
    {
      throw new InternalError( "Unknown signal element?!" );
    }

    String prefix = visible ? "Hide" : "Show";

    putValue( Action.NAME, prefix.concat( " " ).concat( suffix ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    if ( this.elementType == SignalElementType.DIGITAL_SIGNAL )
    {
      this.signalGroup.setShowDigitalSignals( !this.signalGroup.isShowDigitalSignals() );
    }
    else if ( this.elementType == SignalElementType.ANALOG_SIGNAL )
    {
      this.signalGroup.setShowAnalogSignal( !this.signalGroup.isShowAnalogSignal() );
    }
    else if ( this.elementType == SignalElementType.GROUP_SUMMARY )
    {
      this.signalGroup.setShowGroupSummary( !this.signalGroup.isShowGroupSummary() );
    }

    // Signal group: entire display could be changed...
    this.controller.revalidateAll();
  }
}
