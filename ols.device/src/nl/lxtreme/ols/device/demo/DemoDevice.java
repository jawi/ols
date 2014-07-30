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


import java.awt.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.device.demo.generator.*;
import nl.lxtreme.ols.task.execution.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.Component;


/**
 * The device controller for the testing device.
 */
public class DemoDevice implements Device
{
  // CONSTANTS

  static final Map<String, DataGenerator> GENERATORS = new LinkedHashMap<String, DataGenerator>();

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
        DataGenerator inst = ( DataGenerator )generator.newInstance();
        GENERATORS.put( inst.getName(), inst );
      }
      catch ( Exception exception )
      {
        exception.printStackTrace();
      }
    }
  }

  // VARIABLES

  private DemoConfig lastConfig = null;
  private DemoDeviceDialog configDialog = null;

  private volatile TaskExecutionService taskExecutionService;

  // METHODS

  static DataGenerator getGeneratorByName( String aName )
  {
    if ( aName == null )
    {
      throw new IllegalArgumentException( "Name cannot be null!" );
    }
    DataGenerator result = GENERATORS.get( aName );
    if ( result == null )
    {
      throw new IllegalArgumentException( "No such generator: " + aName );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Future<AcquisitionData> acquireData( DeviceConfiguration aConfig, AcquisitionProgressListener aProgressListener )
  {
    if ( aConfig == null || !( aConfig instanceof DemoConfig ) )
    {
      throw new IllegalArgumentException( "Invalid device configuration!" );
    }

    int channelCount = ( ( DemoConfig )aConfig ).getChannelCount();
    int sampleCount = ( ( DemoConfig )aConfig ).getSampleCount();
    DataGenerator generator = ( ( DemoConfig )aConfig ).getGenerator();

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
  public DeviceConfiguration setupDevice()
  {
    final Window currentWindow = SwingComponentUtils.getCurrentWindow();

    disposeConfigDialog();

    this.configDialog = new DemoDeviceDialog( currentWindow );

    DemoConfig config = this.lastConfig;
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

/* EOF */
