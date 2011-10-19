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
package nl.lxtreme.ols.tool.spi;


import java.awt.*;
import java.beans.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * Provides a SPI analyser tool.
 */
public class SPIAnalyser extends BaseAsyncTool<SPIProtocolAnalysisDialog, SPIDataSet, SPIAnalyserWorker>
{
  // CONSTRUCTORS

  /**
   * Creates a new SPIAnalyser instance.
   */
  public SPIAnalyser()
  {
    super( ToolCategory.DECODER, "SPI analyser ..." );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected SPIProtocolAnalysisDialog createDialog( final Window aOwner, final ToolContext aContext, final String aName )
  {
    return new SPIProtocolAnalysisDialog( aOwner, aName, aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected SPIAnalyserWorker createToolWorker( final DataContainer aData, final ToolContext aContext )
  {
    return new SPIAnalyserWorker( aData, aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onPropertyChange( final PropertyChangeEvent aEvent )
  {
    final String name = aEvent.getPropertyName();
    final Object value = aEvent.getNewValue();

    if ( SPIAnalyserWorker.PROPERTY_AUTO_DETECT_MODE.equals( name ) )
    {
      getDialog().setAutoDetectSPIMode( ( SPIMode )value );
    }
  }
}

/* EOF */
