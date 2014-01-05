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
package nl.lxtreme.ols.client.view.waveform;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.view.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a waveform view of {@link AcquisitionData}, in which data is shown
 * graphically as rows.
 */
public class WaveformView extends BaseView
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES
  
  private final SignalDiagramController signalDiagramController;
  
  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  public WaveformView( SignalDiagramController aDiagramController, ViewController aController, ViewModel aModel )
  {
    super( aController, aModel );

    this.signalDiagramController = aDiagramController;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void initialize()
  {
    this.signalDiagramController.initialize();

    add( new JScrollPane( this.signalDiagramController.getSignalDiagram() ), BorderLayout.CENTER );
  }
}
