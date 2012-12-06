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
package nl.lxtreme.ols.client.ui.signaldisplay.signalelement;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides an action to edit the label of a signal element.
 */
public class RemoveChannelAnnotations extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final int channelIndex;
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
    this.channelIndex = aSignalElement.getChannel().getIndex();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    // Clear the annotations for the channel...
    getAnnotationData().clear( this.channelIndex );

    // Repaint the affected area...
    getSignalDiagram().repaintSignalElement( this.signalElement );
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
   * Returns the {@link SignalDiagramComponent} instance.
   * 
   * @return the signal diagram component, never <code>null</code>.
   */
  private SignalDiagramComponent getSignalDiagram()
  {
    return this.controller.getSignalDiagram();
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
    return getAnnotationData().hasAnnotations( DataAnnotation.class, this.channelIndex );
  }

  /**
   * @return the {@link AnnotationData} for the current session, never
   *         <code>null</code>.
   */
  private AnnotationData getAnnotationData()
  {
    final Session session = Client.getInstance().getSession();
    return session.getAnnotationData();
  }
}
