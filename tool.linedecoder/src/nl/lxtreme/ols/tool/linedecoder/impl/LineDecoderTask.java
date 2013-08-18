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
package nl.lxtreme.ols.tool.linedecoder.impl;


import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.linedecoder.*;


/**
 * Represents the work-horse of the line decoding routine.
 */
public class LineDecoderTask implements ToolTask<AcquisitionResult>
{
  // VARIABLES

  private volatile LineDecoder decoder;
  private volatile int[] lines;
  private volatile boolean inverted;
  private volatile boolean recoverClock;
  private volatile int clockSpeed;

  private final ToolContext context;
  private final AnnotationListener annotationListener;
  private final ToolProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new LineDecoderTask instance.
   * 
   * @param aContext
   *          the tool context to use, cannot be <code>null</code>;
   * @param aProgressListener
   *          the progress listener to use for reporting the progress, cannot be
   *          <code>null</code>;
   * @param aAnnotationListener
   *          the annotation listener to use for reporting annotations, cannot
   *          be <code>null</code>.
   */
  public LineDecoderTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.annotationListener = aAnnotationListener;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult call() throws Exception
  {
    final LineDecoderToolContextImpl decoderContext = new LineDecoderToolContextImpl( this.context, this.lines,
        this.inverted, this.recoverClock, this.clockSpeed );

    return this.decoder.decode( decoderContext, this.annotationListener, this.progressListener );
  }

  /**
   * @param aChannels
   */
  public void setChannels( final int[] aChannels )
  {
    this.lines = Arrays.copyOf( aChannels, aChannels.length );
  }

  /**
   * @param aSpeed
   */
  public void setClockSpeed( final int aSpeed )
  {
    this.clockSpeed = aSpeed;
  }

  /**
   * Sets whether or not the line decoder is expected to invert its data.
   * 
   * @param aInverted
   *          <code>true</code> if the data is inverted, <code>false</code>
   *          otherwise.
   */
  public void setInverted( final boolean aInverted )
  {
    this.inverted = aInverted;
  }

  /**
   * Sets the line decoder to use.
   * 
   * @param aDecoder
   *          the decoder to use, cannot be <code>null</code>.
   */
  public void setLineDecoder( final LineDecoder aDecoder )
  {
    this.decoder = aDecoder;
  }

  /**
   * Sets recoverClock to the given value.
   * 
   * @param aRecoverClock
   *          the recoverClock to set.
   */
  public void setRecoverClock( final boolean aRecoverClock )
  {
    this.recoverClock = aRecoverClock;
  }
}
