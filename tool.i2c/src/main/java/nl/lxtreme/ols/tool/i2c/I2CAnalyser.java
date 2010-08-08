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
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * 
 */
public class I2CAnalyser extends BaseAsyncTool<I2CDataSet, I2CAnalyserWorker>
{
  // VARIABLES

  private I2CProtocolAnalysisDialog dialog;

  // CONSTRUCTORS

  /**
   * 
   */
  public I2CAnalyser()
  {
    super( "I2C protocol analyser" );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.BaseTool#readProperties(java.lang.String, java.util.Properties)
   */
  @Override
  public void readProperties( final String aNamespace, final Properties aProperties )
  {
    this.dialog.readProperties( aNamespace, aProperties );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseTool#writeProperties(java.lang.String, java.util.Properties)
   */
  @Override
  public void writeProperties( final String aNamespace, final Properties aProperties )
  {
    this.dialog.writeProperties( aNamespace, aProperties );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#createToolWorker(nl.lxtreme.ols.api.data.AnnotatedData)
   */
  @Override
  protected I2CAnalyserWorker createToolWorker( final AnnotatedData aData )
  {
    return new I2CAnalyserWorker( aData, this.dialog );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#doProcess(nl.lxtreme.ols.api.data.AnnotatedData,
   *      nl.lxtreme.ols.api.tools.ToolContext)
   */
  @Override
  protected void doProcess( final AnnotatedData aData, final ToolContext aContext )
  {
    this.dialog.showDialog( aData, getToolWorker() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#propertyChange(java.beans.PropertyChangeEvent)
   */
  @Override
  protected void propertyChange( final PropertyChangeEvent aEvent )
  {
    final String name = aEvent.getPropertyName();
    final Object value = aEvent.getNewValue();

    if ( I2CAnalyserWorker.PROPERTY_AUTO_DETECT_SCL.equals( name ) )
    {
      this.dialog.setAutoDetectSCL( ( String )value );
    }
    else if ( I2CAnalyserWorker.PROPERTY_AUTO_DETECT_SDA.equals( name ) )
    {
      this.dialog.setAutoDetectSDA( ( String )value );
    }
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#setupTool(java.awt.Frame)
   */
  @Override
  protected void setupTool( final Frame aFrame )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.dialog != null ) && ( this.dialog.getOwner() != aFrame ) )
    {
      this.dialog.dispose();
      this.dialog = null;
    }

    // if no valid dialog exists, create one
    if ( this.dialog == null )
    {
      this.dialog = new I2CProtocolAnalysisDialog( aFrame, getName() );
    }
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncTool#toolWorkerDone(java.util.List)
   */
  @Override
  protected void toolWorkerDone( final I2CDataSet aAnalysisResult )
  {
    this.dialog.createReport( aAnalysisResult );
  }

}

/* EOF */
