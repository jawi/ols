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


import static nl.lxtreme.ols.device.generic.GenericConstants.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import org.apache.felix.dm.Component;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.task.execution.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a device that can read from any file-based source and use this as
 * capture device.
 */
public class GenericDevice implements Device
{
  // CONSTANTS

  private static final String NAME = "Generic I/O";

  // VARIABLES

  private Map<String, ? extends Serializable> lastConfig = null;
  private GenericDeviceConfigDialog configDialog = null;

  private volatile TaskExecutionService taskExecutionService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Future<AcquisitionData> acquireData( Map<String, ? extends Serializable> aConfig,
      AcquisitionProgressListener aProgressListener )
  {
    if ( ( aConfig == null ) || !isValid( aConfig ) )
    {
      throw new IllegalArgumentException( "Invalid device configuration!" );
    }

    String devicePath = ( String )aConfig.get( KEY_DEVICE_PATH );
    int channelCount = ( ( Number )aConfig.get( KEY_CHANNEL_COUNT ) ).intValue();
    int sampleRate = ( ( Number )aConfig.get( KEY_SAMPLE_RATE ) ).intValue();
    int sampleCount = ( ( Number )aConfig.get( KEY_SAMPLE_COUNT ) ).intValue();
    int sampleWidth = ( ( Number )aConfig.get( KEY_SAMPLE_WIDTH ) ).intValue();

    return this.taskExecutionService.execute( new GenericDeviceAcquisitionTask( devicePath, channelCount, sampleRate,
        sampleCount, sampleWidth, aProgressListener ) );
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
  public Map<String, ? extends Serializable> setupDevice()
  {
    final Window currentWindow = SwingComponentUtils.getCurrentWindow();

    disposeConfigDialog();

    this.configDialog = new GenericDeviceConfigDialog( currentWindow );

    Map<String, ? extends Serializable> config = this.lastConfig;

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

  private boolean isPositiveNumber( Object value )
  {
    int num;
    if ( value == null || !( value instanceof Number ) )
    {
      return false;
    }
    num = ( ( Number )value ).intValue();
    if ( num < 1 )
    {
      return false;
    }
    return true;
  }

  /**
   * Validates the given device configuration.
   * 
   * @param aConfig
   *          the configuration to validate, cannot be <code>null</code>.
   * @return <code>true</code> if the given configuration was valid,
   *         <code>false</code> otherwise.
   */
  private boolean isValid( Map<String, ? extends Serializable> aConfig )
  {
    Object value = aConfig.get( KEY_CHANNEL_COUNT );
    if ( value == null || !( value instanceof Number ) )
    {
      return false;
    }
    int num = ( ( Number )value ).intValue();
    if ( num < 1 || num > OlsConstants.MAX_CHANNELS )
    {
      return false;
    }

    if ( !isPositiveNumber( aConfig.get( KEY_SAMPLE_COUNT ) ) )
    {
      return false;
    }

    if ( !isPositiveNumber( aConfig.get( KEY_SAMPLE_RATE ) ) )
    {
      return false;
    }

    if ( !isPositiveNumber( aConfig.get( KEY_SAMPLE_WIDTH ) ) )
    {
      return false;
    }

    value = aConfig.get( KEY_DEVICE_PATH );
    if ( value == null || !( value instanceof String ) )
    {
      return false;
    }

    return new File( ( String )value ).exists();
  }
}
