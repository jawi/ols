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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.demo;


import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a common interface for all custom data generators.
 */
public interface DataGenerator
{
  // CONSTANTS
  
  int SR_1MHZ = 1000000;
  int SR_4MHZ = 4 * SR_1MHZ;
  int SR_10MHZ = 10 * SR_1MHZ;
  int SR_1GHZ = 1000 * SR_1MHZ;
  
  // METHODS

  /**
   * @return a display name for this data generator, never <code>null</code>.
   */
  String getName();

  /**
   * Generates the data and adds it to the given {@link AcquisitionDataBuilder}.
   * 
   * @param aChannelCount
   *          the number of channels to generate data for;
   * @param aSampleCount
   *          the number of samples to generate;
   * @param aBuilder
   *          the builder to add the generated data to, cannot be
   *          <code>null</code>.
   * @param aProgressListener
   *          the progress listener to report the progress to, cannot be
   *          <code>null</code>.
   */
  void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener );

}
