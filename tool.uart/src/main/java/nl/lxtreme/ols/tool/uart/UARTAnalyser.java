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
package nl.lxtreme.ols.tool.uart;


import java.awt.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * Provides an UART/RS-232 analysis tool.
 */
public class UARTAnalyser extends BaseAsyncTool<UARTProtocolAnalysisDialog, UARTDataSet, UARTAnalyserWorker>
{
  // CONSTRUCTORS

  /**
   * Creates a new UARTAnalyser instance.
   */
  public UARTAnalyser()
  {
    super( Category.DECODER, "UART analyser ..." );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected UARTProtocolAnalysisDialog createDialog( final Window aOwner, final ToolContext aContext, final String aName )
  {
    return new UARTProtocolAnalysisDialog( aOwner, getName(), aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected UARTAnalyserWorker createToolWorker( final DataContainer aData, final ToolContext aContext )
  {
    return new UARTAnalyserWorker( aData, aContext );
  }
}

/* EOF */
