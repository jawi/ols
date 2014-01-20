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
package nl.lxtreme.ols.device.demo;


import static nl.lxtreme.ols.device.demo.DemoConstants.*;

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
 * The device controller for the testing device.
 */
public class DemoDevice implements Device
{
  // CONSTANTS

  static final Map<String, IDataGenerator> GENERATORS = new LinkedHashMap<String, IDataGenerator>();

  static
  {
    final Class<?>[] generators = { SawtoothDataGenerator.class, ZeroDataGenerator.class, SineDataGenerator.class,
        OddEvenGenerator.class, RandomDataGenerator.class, I2CGenerator.class, OneWireGenerator.class,
        ManchesterEncoder.class, ClockedCounterGenerator.class, StateDataGenerator.class, BurstGenerator.class,
        UnityGenerator.class };

    for ( Class<?> generator : generators )
    {
      try
      {
        IDataGenerator inst = ( IDataGenerator )generator.newInstance();
        GENERATORS.put( inst.getName(), inst );
      }
      catch ( Exception exception )
      {
        exception.printStackTrace();
      }
    }
  }

  // VARIABLES

  private Map<String, ? extends Serializable> lastConfig = null;
  private DemoDeviceDialog configDialog = null;

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

    int channelCount = ( ( Number )aConfig.get( KEY_CHANNEL_COUNT ) ).intValue();
    int sampleCount = ( ( Number )aConfig.get( KEY_SAMPLE_COUNT ) ).intValue();
    String generatorName = ( String )aConfig.get( KEY_GENERATOR_NAME );

    IDataGenerator generator = GENERATORS.get( generatorName );
    if ( generator == null )
    {
      throw new IllegalArgumentException( "Invalid device configuration!" );
    }

    return this.taskExecutionService.execute( new DemoAcquisitionTask( channelCount, sampleCount, generator,
        aProgressListener ), Collections.singletonMap( "type", "acquisition" ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Demo Device";
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

    this.configDialog = new DemoDeviceDialog( currentWindow );

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

    value = aConfig.get( KEY_SAMPLE_COUNT );
    if ( value == null || !( value instanceof Number ) )
    {
      return false;
    }
    num = ( ( Number )value ).intValue();
    if ( num < 1 )
    {
      return false;
    }

    value = aConfig.get( KEY_GENERATOR_NAME );
    if ( value == null || !GENERATORS.containsKey( value ) )
    {
      return false;
    }

    return true;
  }
}

/* EOF */
