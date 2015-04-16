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
package nl.lxtreme.ols.device.generic;


import java.awt.*;
import java.io.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.devices.*;


/**
 * Provides a device that can read from any file-based source and use this as
 * capture device.
 */
public class GenericDevice implements Device
{
  // CONSTANTS

  private static final String NAME = "Generic I/O";

  // VARIABLES

  private GenericDeviceConfigDialog deviceConfig = null;
  private boolean setup = false;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    // No-op...
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionTask createAcquisitionTask( final AcquisitionProgressListener aProgressListener )
      throws IOException
  {
    String dataFormat = this.deviceConfig.getDataFormat();
    if ( GenericDeviceConfigDialog.DATA_FORMATS[0].equals( dataFormat ) )
    {
      return new RawDataAcquisitionTask( this.deviceConfig, aProgressListener );
    }

    return new OlsDataAcquisitionTask( this.deviceConfig, aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CancelTask createCancelTask() throws IOException
  {
    // Nothing special is needed...
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#getName()
   */
  @Override
  public String getName()
  {
    return NAME;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#isSetup()
   */
  @Override
  public boolean isSetup()
  {
    return this.setup;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#setupCapture(java.awt.Window)
   */
  @Override
  public boolean setupCapture( final Window aParent )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.deviceConfig != null ) && ( this.deviceConfig.getOwner() != aParent ) )
    {
      this.deviceConfig.dispose();
      this.deviceConfig = null;
    }
    // if no valid dialog exists, create one
    if ( this.deviceConfig == null )
    {
      this.deviceConfig = new GenericDeviceConfigDialog( aParent );
    }

    return ( this.setup = this.deviceConfig.showDialog() );
  }
}
