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
package nl.lxtreme.ols.tool.can;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * The actual CAN-bus decoder.
 */
public class CanBusDecoderTask implements ToolTask<CanBusDataSet>
{
  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final ToolAnnotationHelper annHelper;

  private int startOfDecode;
  private int endOfDecode;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CanBusDecoderTask} instance.
   * 
   * @param aContext
   *          the tool context to use;
   * @param aProgressListener
   *          the progress listener to report progress to.
   */
  public CanBusDecoderTask( ToolContext aContext, ToolProgressListener aProgressListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annHelper = new ToolAnnotationHelper( aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return CanBusDecoder.NAME;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CanBusDataSet call() throws Exception
  {
    final AcquisitionData data = this.context.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    // TODO do the actual decoding here!

    return new CanBusDataSet( this.startOfDecode, this.endOfDecode, data );
  }

  /**
   * Sets the decoding area.
   * 
   * @param aStartOfDecode
   *          a start sample index, >= 0;
   * @param aEndOfDecode
   *          a ending sample index, >= 0.
   */
  public void setDecodingArea( final int aStartOfDecode, final int aEndOfDecode )
  {
    this.startOfDecode = aStartOfDecode;
    this.endOfDecode = aEndOfDecode;
  }

  // TODO add additional setters for configuring this task properly, such as which channels to use, etc.

}
