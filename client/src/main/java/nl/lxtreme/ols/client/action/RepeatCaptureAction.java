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
package nl.lxtreme.ols.client.action;


import java.io.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.client.*;


/**
 * Provides a "repeat capture" action which simply repeats the capture with the
 * current settings.
 */
public class RepeatCaptureAction extends CaptureAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RepeatCapture";

  // CONSTRUCTORS

  /**
   * Creates a new RepeatCaptureAction instance.
   * 
   * @param aHost
   *          the host this action belongs to;
   * @param aProject
   *          the project to use.
   */
  public RepeatCaptureAction( final Host aHost, final Project aProject )
  {
    super( ID, ICON_RECAPTURE_DATA, "Repeat capture", "Repeat the capture with current device settings.", aHost,
        aProject );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.client.action.CaptureAction#doCaptureData(nl.lxtreme.ols.api.devices.DeviceController)
   */
  @Override
  protected void doCaptureData( final DeviceController aController, final CaptureCallback aCallback )
  throws IOException
  {
    aController.captureData( aCallback );
  }
}

/* EOF */
