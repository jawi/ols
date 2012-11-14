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
package nl.lxtreme.ols.client.tool;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Provides a default tool context implementation.
 */
final class ToolContextImpl implements ToolContext
{
  // VARIABLES

  private final Session session;
  private final ToolProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolContextImpl} instance.
   * 
   * @param aSession
   *          the current session;
   * @param aProgressListener
   *          the progress listener to use for listening to a tools progress.
   */
  public ToolContextImpl( final Session aSession, final ToolProgressListener aProgressListener )
  {
    this.session = aSession;
    this.progressListener = aProgressListener;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addAnnotation( final Annotation aAnnotation )
  {
    AnnotationData data = this.session.getAnnotationData();
    data.add( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations( final int... aChannelIdxs )
  {
    AnnotationData data = this.session.getAnnotationData();
    for ( int channelIdx : aChannelIdxs )
    {
      data.clear( channelIdx );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData getAcquisitionData()
  {
    return this.session.getAcquisitionData();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getChannelCount()
  {
    return getAcquisitionData().getChannelCount();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getEnabledChannels()
  {
    AcquisitionData acquisitionData = getAcquisitionData();
    if ( acquisitionData == null )
    {
      return 0;
    }
    return acquisitionData.getEnabledChannels();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getEndSampleIndex()
  {
    int endOfDecode = -1;
    int dataLength = 0;

    final AcquisitionData data = this.session.getAcquisitionData();

    if ( data != null )
    {
      if ( data.isCursorsVisible() )
      {
        Cursor cursor = data.getCursor( 1 );
        if ( cursor.isDefined() )
        {
          endOfDecode = data.getSampleIndex( cursor.getTimestamp() ) + 1;
        }
      }

      dataLength = data.getValues().length;
    }

    if ( ( endOfDecode < 0 ) || ( endOfDecode >= dataLength ) )
    {
      endOfDecode = dataLength - 1;
    }

    return endOfDecode;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLength()
  {
    return Math.max( 0, getEndSampleIndex() - getStartSampleIndex() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolProgressListener getProgressListener()
  {
    return this.progressListener;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Session getSession()
  {
    return this.session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getStartSampleIndex()
  {
    int startOfDecode = -1;

    final AcquisitionData data = this.session.getAcquisitionData();

    if ( data != null )
    {
      if ( data.isCursorsVisible() )
      {
        Cursor cursor = data.getCursor( 0 );
        if ( cursor.isDefined() )
        {
          startOfDecode = data.getSampleIndex( cursor.getTimestamp() ) - 1;
        }
      }
    }

    startOfDecode = Math.max( 0, startOfDecode );

    return startOfDecode;
  }
}
