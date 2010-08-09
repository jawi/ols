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
    super( "I2C protocol analyser ..." );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.BaseTool#createDialog(java.awt.Window,
   *      java.lang.String, nl.lxtreme.ols.api.data.AnnotatedData,
   *      nl.lxtreme.ols.api.tools.ToolContext,
   *      nl.lxtreme.ols.api.tools.AnalysisCallback)
   */
  @Override
  protected I2CProtocolAnalysisDialog createDialog( final Window aOwner, final String aName, final AnnotatedData aData,
      final ToolContext aContext, final AnalysisCallback aCallback )
  {
    return new I2CProtocolAnalysisDialog( aOwner, getName() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#createToolWorker(nl.lxtreme.ols.api.data.AnnotatedData)
   */
  @Override
  protected I2CAnalyserWorker createToolWorker( final AnnotatedData aData )
  {
    return new I2CAnalyserWorker( aData );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#onPropertyChange(java.beans.PropertyChangeEvent)
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

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#onToolWorkerDone(java.lang.Object)
   */
  @Override
  protected void onToolWorkerDone( final I2CDataSet aAnalysisResult )
  {
    getDialog().createReport( aAnalysisResult );
  }
}

/* EOF */
