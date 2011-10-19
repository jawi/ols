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
package nl.lxtreme.ols.api.devices;


import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a callback for receiving captured data and keeping notified of the
 * capture progress.
 */
public interface CaptureCallback extends ProgressCallback
{
  // METHODS

  /**
   * Called when the capture was aborted.
   * 
   * @param aReason
   *          a reason why this run might be aborted, can be any kind of string
   *          message, such as an exception message, user message, etc. Can be
   *          <code>null</code>.
   */
  public void captureAborted( final String aReason );

  /**
   * Called upon completion of the capture process.
   * 
   * @param aData
   *          the captured data, never <code>null</code>.
   */
  public void captureComplete( final AcquisitionResult aData );

  /**
   * Called when the capture is just started.
   * <p>
   * This method will be called before any other methods of this interface.
   * </p>
   * 
   * @param aSampleRate
   *          the sample rate of the capture, in Hertz (Hz);
   * @param aChannelCount
   *          the number of channels in a single sample (1..32);
   * @param aChannelMask
   *          the bitmask used to mask out the relevant channels.
   */
  public void captureStarted( final int aSampleRate, final int aChannelCount, final int aChannelMask );

  /**
   * Called when a number of samples are captured.
   * <p>
   * There is no guarantee this method will be called at all! However, if
   * called, it will always be in order.
   * </p>
   * 
   * @param aSamples
   *          the captured samples, never <code>null</code>.
   */
  public void samplesCaptured( final List<Sample> aSamples );
}

/* EOF */
