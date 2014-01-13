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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.pulsecount;


import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a {@link SwingWorker} to measure the frequency, dutycycle and such
 * asynchronously from the UI.
 */
final class SignalMeasurerWorker extends SwingWorker<PulseCountInfo, Boolean>
{
  // VARIABLES

  private final PulseCountView pulseCountView;
  private final AcquisitionData data;
  private final int index;
  private final long startTimestamp;
  private final long endTimestamp;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalMeasurerWorker} instance.
   * 
   * @param aPulseCountView
   *          the main view to update;
   * @param aData
   *          the acquired data to measure;
   * @param aChannel
   *          the channel to measure;
   * @param aCursorA
   *          the cursor denoting the start of measurement, can be
   *          <code>null</code>;
   * @param aCursorB
   *          the cursor denoting the end of measurement, can be
   *          <code>null</code>.
   */
  public SignalMeasurerWorker( PulseCountView aPulseCountView, AcquisitionData aData, Channel aChannel,
      Cursor aCursorA, Cursor aCursorB )
  {
    this.pulseCountView = aPulseCountView;
    this.data = aData;
    this.index = aChannel.getIndex();
    this.startTimestamp = aCursorA != null ? aCursorA.getTimestamp() : -1L;
    this.endTimestamp = aCursorB != null ? aCursorB.getTimestamp() : -1L;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected PulseCountInfo doInBackground() throws Exception
  {
    long[] timestamps = this.data.getTimestamps();

    long start = this.startTimestamp;
    if ( start < 0L )
    {
      start = timestamps[0];
    }
    long end = this.endTimestamp;
    if ( end < 0L )
    {
      end = timestamps[timestamps.length - 1];
    }

    return new SignalMeasurer( this.data, this.index, start, end ).run();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void done()
  {
    try
    {
      this.pulseCountView.updatePulseCountInformation( get() );
    }
    catch ( InterruptedException exception )
    {
      // Ignore, we're already terminating...
    }
    catch ( ExecutionException exception )
    {
      exception.printStackTrace();
    }
  }
}
