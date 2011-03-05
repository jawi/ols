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
package nl.lxtreme.ols.device.generic;


import java.io.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class GenericDevice extends SwingWorker<AcquisitionResult, Sample>
{
  // CONSTANTS

  public static final String PROP_CAPTURE_PROGRESS = "progress";
  public static final String PROP_CAPTURE_STATE = "state";

  private static final Logger LOG = Logger.getLogger( GenericDevice.class.getName() );

  // VARIABLES

  private final GenericDeviceConfigDialog deviceConfig;

  private InputStream inputStream;

  private volatile boolean running;

  // CONSTRUCTORS

  /**
   * @param aDeviceConfig
   */
  public GenericDevice( final GenericDeviceConfigDialog aDeviceConfig )
  {
    this.deviceConfig = aDeviceConfig;
  }

  // METHODS

  /**
   * Returns whether or not this device is capturing samples.
   * 
   * @return <code>true</code> if this device is capturing, <code>false</code>
   *         otherwise.
   */
  public boolean isRunning()
  {
    return this.running;
  }

  /**
   * Stops the capture (if possible).
   */
  public void stop()
  {
    if ( this.running )
    {
      this.running = false;
    }
  }

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected AcquisitionResult doInBackground() throws Exception
  {
    this.running = true;

    final int width = this.deviceConfig.getSampleWidth();
    final int depth = this.deviceConfig.getSampleDepth();
    final int rate = this.deviceConfig.getSampleRate();
    final int channels = this.deviceConfig.getChannelCount();

    final int count = depth * width;

    final int[] values = new int[count];
    final long[] timestamps = new long[count];

    try
    {
      this.inputStream = new FileInputStream( this.deviceConfig.getDevicePath() );

      int idx = 0;
      while ( idx < count )
      {
        final int sample = readSample( width );

        LOG.log( Level.FINE, "Read: 0x{0}", Integer.toHexString( sample ) );

        values[idx] = sample;
        timestamps[idx] = idx;

        idx++;
      }

      return new CapturedData( values, timestamps, Ols.NOT_AVAILABLE, rate, channels,
          channels, idx );
    }
    finally
    {
      HostUtils.closeResource( this.inputStream );

      this.running = false;
      this.inputStream = null;
    }
  }

  /**
   * Reads <code>channels</code> / 8 bytes from stream and compiles them into a
   * single integer.
   * 
   * @param aChannelCount
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readSample( final int aSampleWidth ) throws IOException, InterruptedException
  {
    int v, value = 0;

    for ( int i = 0; i < aSampleWidth; i++ )
    {
      v = this.inputStream.read();

      // Any timeouts/interrupts occurred?
      if ( v < 0 )
      {
        throw new InterruptedException( "Data readout interrupted: EOF." );
      }
      else if ( Thread.interrupted() )
      {
        throw new InterruptedException( "Data readout interrupted." );
      }

      value |= v << ( 8 * i );
    }

    return value;
  }

}
