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

  private final ElementGroup group;
  private final SignalDiagramController controller;
  private final SignalElementType elementType;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetSignalGroupVisibilityAction} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aGroup
   *          the group for which to hide/show the elements, cannot be
   *          <code>null</code>;
   * @param aType
   *          the signal element type to show/hide.
   */
  public SetSignalGroupVisibilityAction( final SignalDiagramController aController, final ElementGroup aGroup,
      final SignalElementType aType )
  {
    this.group = aGroup;
    this.controller = aController;
    this.elementType = aType;

    String suffix;
    boolean visible;
    if ( aType == SignalElementType.DIGITAL_SIGNAL )
    {
      suffix = "digital signals";
      visible = this.group.isShowDigitalSignals();
    }
    else if ( aType == SignalElementType.ANALOG_SIGNAL )
    {
      suffix = "analog signal";
      visible = this.group.isShowAnalogSignal();
    }
    else if ( aType == SignalElementType.GROUP_SUMMARY )
    {
      suffix = "group summary";
      visible = this.group.isShowGroupSummary();
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
      this.group.setShowDigitalSignals( !this.group.isShowDigitalSignals() );
    }
    else if ( this.elementType == SignalElementType.ANALOG_SIGNAL )
    {
      this.group.setShowAnalogSignal( !this.group.isShowAnalogSignal() );
    }
    else if ( this.elementType == SignalElementType.GROUP_SUMMARY )
    {
      this.group.setShowGroupSummary( !this.group.isShowGroupSummary() );
    }

    // Signal group: entire display could be changed...
    this.controller.revalidateAll();
  }
}
