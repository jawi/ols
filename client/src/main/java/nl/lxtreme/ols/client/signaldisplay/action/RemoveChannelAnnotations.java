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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 * Provides an action to edit the label of a signal element.
 */
public class RemoveChannelAnnotations extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalElement signalElement;
  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link RemoveChannelAnnotations} instance.
   *
   * @param aController
   *          the controller to use;
   * @param aSignalElement
   *          the signal element to edit the label for.
   */
  public RemoveChannelAnnotations( final SignalDiagramController aController, final SignalElement aSignalElement )
  {
    super( "Remove annotations" );

    this.controller = aController;
    this.signalElement = aSignalElement;

    setEnabled( ( this.signalElement != null ) && this.signalElement.isDigitalSignal() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getChannel().clearAnnotations();

    // Repaint the affected area...
    this.controller.repaintSignalElement( this.signalElement );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled()
  {
    return super.isEnabled() && hasAnnotations();
  }

  /**
   * Returns the channel of the contained signal element.
   *
   * @return a channel, never <code>null</code>.
   */
  private Channel getChannel()
  {
    return this.signalElement.getChannel();
  }

  /**
   * Determines whether or not there are annotations available for the signal
   * element.
   *
   * @return <code>true</code> if there are any annotations available for the
   *         channel, <code>false</code> otherwise.
   */
  private boolean hasAnnotations()
  {
    return !getChannel().getAnnotations().isEmpty();
  }
}
