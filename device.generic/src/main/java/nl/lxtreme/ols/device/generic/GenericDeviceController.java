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
package nl.lxtreme.ols.device.generic;


import java.awt.*;
import java.io.*;

import nl.lxtreme.ols.api.devices.*;


/**
 * @author jawi
 */
public class GenericDeviceController implements DeviceController
{
  // CONSTANTS

  private static final String NAME = "Generic I/O";

  // VARIABLES

  private GenericDeviceConfigDialog configDialog = null;
  private boolean setup = false;

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#cancel()
   */
  @Override
  public void cancel() throws IllegalStateException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#captureData(nl.lxtreme.ols.api.devices.CaptureCallback)
   */
  @Override
  public void captureData( final CaptureCallback aCallback ) throws IOException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#getName()
   */
  @Override
  public String getName()
  {
    return NAME;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#isCapturing()
   */
  @Override
  public boolean isCapturing()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#isSetup()
   */
  @Override
  public boolean isSetup()
  {
    return this.setup;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.DeviceController#setupCapture(java.awt.Window)
   */
  @Override
  public boolean setupCapture( final Window aParent )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.configDialog != null ) && ( this.configDialog.getOwner() != aParent ) )
    {
      this.configDialog.dispose();
      this.configDialog = null;
    }
    // if no valid dialog exists, create one
    if ( this.configDialog == null )
    {
      this.configDialog = new GenericDeviceConfigDialog( aParent );
    }

    return ( this.setup = this.configDialog.showDialog() );
  }

}
