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
import nl.lxtreme.ols.task.execution.*;


/**
 * Denotes an acquisition task for a testing device that outputs static
 * (generated) data.
 */
public class DemoAcquisitionTask implements Task<AcquisitionData>
{
  // VARIABLES

  private final int channelCount;
  private final int sampleCount;
  private final DataGenerator generator;
  private final AcquisitionProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DemoAcquisitionTask} instance.
   * 
   * @param aChannelCount
   * @param aSampleCount
   * @param aGenerator
   * @param aProgressListener
   */
  public DemoAcquisitionTask( int aChannelCount, int aSampleCount, DataGenerator aGenerator,
      AcquisitionProgressListener aProgressListener )
  {
    this.channelCount = aChannelCount;
    this.sampleCount = aSampleCount;
    this.generator = aGenerator;
    this.progressListener = aProgressListener;
  }

  // METHODS

  @Override
  public AcquisitionData call() throws Exception
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    this.generator.generate( this.channelCount, this.sampleCount, builder, this.progressListener );

    return builder.build();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Demo Acquisition Task";
  }
}
