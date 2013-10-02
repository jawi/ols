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
package nl.lxtreme.ols.device.demo;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;


/**
 * Denotes an acquisition task for a testing device that outputs static
 * (generated) data.
 */
public class DemoAcquisitionTask implements AcquisitionTask
{
  // VARIABLES

  private final DemoDeviceDialog configDialog;
  private final AcquisitionProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new TestDevice instance.
   * 
   * @param aConfigDialog
   * @param aProgressListener
   */
  public DemoAcquisitionTask( final DemoDeviceDialog aConfigDialog, final AcquisitionProgressListener aProgressListener )
  {
    this.configDialog = aConfigDialog;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData call() throws Exception
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    
    final int dataLength = this.configDialog.getDataLength();
    final int channels = this.configDialog.getChannels();
    final IDataGenerator generator = this.configDialog.getDataGenerator();

    generator.generate( channels, dataLength, builder, this.progressListener );

    return builder.build();
  }
}
