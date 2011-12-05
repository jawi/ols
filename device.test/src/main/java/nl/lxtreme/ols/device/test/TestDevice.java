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
package nl.lxtreme.ols.device.test;


import java.awt.*;
import java.io.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.devices.*;


/**
 * The device controller for the testing device.
 */
public class TestDevice implements Device
{
  // VARIABLES

  private TestDeviceDialog configDialog;
  private boolean setup = false;

  // CONSTRUCTORS

  /**
   * 
   */
  public TestDevice()
  {
    super();
  }

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
    return new TestAcquisitionTask( this.configDialog, aProgressListener );
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
    return "Test Device";
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
   * @see nl.lxtreme.ols.api.devices.Device#setupCapture()
   */
  @Override
  public boolean setupCapture( final Window aOwner )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.configDialog != null ) && ( this.configDialog.getOwner() != aOwner ) )
    {
      this.configDialog.dispose();
      this.configDialog = null;
    }
    // if no valid dialog exists, create one
    if ( this.configDialog == null )
    {
      this.configDialog = new TestDeviceDialog( aOwner );
    }

    return ( this.setup = this.configDialog.showDialog() );
  }
}

/* EOF */
