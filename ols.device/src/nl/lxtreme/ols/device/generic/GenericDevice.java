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
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.task.execution.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.Component;


/**
 * Provides a device that can read from any file-based source and use this as
 * capture device.
 */
public class GenericDevice implements Device
{
  // CONSTANTS

  private static final String NAME = "Generic I/O";

  // VARIABLES

  private GenericConfig lastConfig = null;
  private GenericDeviceConfigDialog configDialog = null;

  private volatile TaskExecutionService taskExecutionService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Future<AcquisitionData> acquireData( DeviceConfiguration aConfig, AcquisitionProgressListener aProgressListener )
  {
    if ( aConfig == null || !( aConfig instanceof GenericConfig ) )
    {
      throw new IllegalArgumentException( "Invalid device configuration!" );
    }

    String devicePath = ( ( GenericConfig )aConfig ).getDevicePath();
    int channelCount = ( ( GenericConfig )aConfig ).getChannelCount();
    int sampleCount = ( ( GenericConfig )aConfig ).getSampleCount();
    int sampleRate = ( ( GenericConfig )aConfig ).getSampleRate();
    int sampleWidth = ( ( GenericConfig )aConfig ).getSampleWidth();

    return this.taskExecutionService.execute( new GenericDeviceAcquisitionTask( devicePath, channelCount, sampleRate,
        sampleCount, sampleWidth, aProgressListener ), Collections.singletonMap( "type", "acquisition" ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return NAME;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSetup()
  {
    return this.lastConfig != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceConfiguration setupDevice()
  {
    final Window currentWindow = SwingComponentUtils.getCurrentWindow();

    disposeConfigDialog();

    this.configDialog = new GenericDeviceConfigDialog( currentWindow );

    GenericConfig config = this.lastConfig;
    if ( this.configDialog.showDialog() )
    {
      config = this.lastConfig = this.configDialog.getConfig();
    }
    return config;
  }

  /**
   * Called when this class is unregistered as OSGi service.
   */
  protected void destroy( final Component aComponent )
  {
    disposeConfigDialog();
  }

  /**
   * Disposes the current configuration dialog, if one is still visible on
   * screen. If no configuration dialog is visible, this method does nothing.
   */
  private void disposeConfigDialog()
  {
    SwingComponentUtils.dispose( this.configDialog );
    this.configDialog = null;
  }
}
