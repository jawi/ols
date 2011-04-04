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
package nl.lxtreme.ols.tool.i2c;


import java.awt.*;
import java.beans.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * Provides an I2C analyser tool.
 */
public class I2CAnalyser extends BaseAsyncTool<I2CProtocolAnalysisDialog, I2CDataSet, I2CAnalyserWorker>
{
  // CONSTRUCTORS

  /**
   * Creates a new I2CAnalyser instance.
   */
  public I2CAnalyser()
  {
    super( Category.DECODER, "I2C protocol analyser ..." );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected I2CProtocolAnalysisDialog createDialog( final Window aOwner, final ToolContext aContext, final String aName )
  {
    return new I2CProtocolAnalysisDialog( aOwner, getName(), aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected I2CAnalyserWorker createToolWorker( final DataContainer aData, final ToolContext aContext )
  {
    return new I2CAnalyserWorker( aData, aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onPropertyChange( final PropertyChangeEvent aEvent )
  {
    final String name = aEvent.getPropertyName();
    final Object value = aEvent.getNewValue();

    if ( I2CAnalyserWorker.PROPERTY_AUTO_DETECT_SCL.equals( name ) )
    {
      getDialog().setAutoDetectSCL( ( String )value );
    }
    else if ( I2CAnalyserWorker.PROPERTY_AUTO_DETECT_SDA.equals( name ) )
    {
      getDialog().setAutoDetectSDA( ( String )value );
    }
  }
}

/* EOF */
