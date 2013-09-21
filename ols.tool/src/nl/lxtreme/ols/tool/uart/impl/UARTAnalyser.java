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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.uart.impl;


import java.awt.*;

import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import org.osgi.framework.*;


/**
 * Provides an UART/RS-232 analysis tool.
 */
public class UARTAnalyser implements Tool<UARTDataSet>
{
  // VARIABLES

  private volatile BundleContext context;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public UARTAnalyserTask createToolTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    return new UARTAnalyserTask( aContext, aProgressListener, aAnnotationListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolCategory getCategory()
  {
    return ToolCategory.DECODER;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "UART analyser ...";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke( final Window aParent, final ToolContext aContext )
  {
    new UARTProtocolAnalysisDialog( aParent, aContext, this.context, this ).showDialog();
  }
}

/* EOF */
