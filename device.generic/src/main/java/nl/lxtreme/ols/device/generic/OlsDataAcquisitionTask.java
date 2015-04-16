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


import java.io.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a generic acquisition task that can read OLS data file-format from
 * any file-based source.
 */
public final class OlsDataAcquisitionTask implements AcquisitionTask
{
  // VARIABLES

  private final AcquisitionProgressListener progressListener;
  private final GenericDeviceConfigDialog deviceConfig;

  // CONSTRUCTORS

  /**
   * Creates a new GenericDevice instance.
   *
   * @param aContext
   *          the bundle context to use;
   * @param aDeviceConfig
   *          the device configuration to use.
   */
  public OlsDataAcquisitionTask( final GenericDeviceConfigDialog aDeviceConfig,
      final AcquisitionProgressListener aProgressListener )
  {
    this.deviceConfig = aDeviceConfig;
    this.progressListener = aProgressListener;

  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult call() throws IOException
  {
    Reader reader = null;

    try
    {
      this.progressListener.acquisitionInProgress( 0 );

      reader = new FileReader( this.deviceConfig.getDevicePath() );

      AcquisitionResult result = OlsDataHelper.read( reader );

      this.progressListener.acquisitionInProgress( 100 );

      return result;
    }
    catch ( IOException exception )
    {
      // Rethrow the caught exception...
      throw exception;
    }
    finally
    {
      HostUtils.closeResource( reader );
    }
  }
}
