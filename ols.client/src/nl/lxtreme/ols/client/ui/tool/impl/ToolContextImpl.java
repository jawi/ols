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
package nl.lxtreme.ols.client.ui.tool.impl;


import nl.lxtreme.ols.common.*;
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
  private final Configuration configuration;
  private final ToolProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolContextImpl} instance.
   * 
   * @param aSession
   *          the current session;
   * @param aDecodeContext
   *          the decoding context to use;
   * @param aProgressListener
   *          the progress listener to use for listening to a tools progress.
   */
  public ToolContextImpl( final Session aSession, final Configuration aConfiguration,
      final ToolProgressListener aProgressListener )
  {
    this.session = aSession;
    this.configuration = aConfiguration;
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
    AcquisitionData acquisitionData = getAcquisitionData();
    if ( acquisitionData == null )
    {
      return 0;
    }
    return acquisitionData.getChannelCount();
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
    final AcquisitionData data = this.session.getAcquisitionData();

    int endOfDecode = -1;
    int dataLength = 0;

    if ( data != null )
    {
      dataLength = data.getValues().length - 1;
      endOfDecode = dataLength;

      if ( !isDecodeAll() )
      {
        endOfDecode = data.getSampleIndex( getMarkerB() );
      }
    }

    return Math.max( 0, Math.min( endOfDecode, dataLength ) );
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
    final AcquisitionData data = this.session.getAcquisitionData();

    int startOfDecode = -1;
    int dataLength = 0;

    if ( data != null )
    {
      dataLength = data.getValues().length - 1;
      startOfDecode = 0;

      if ( !isDecodeAll() )
      {
        startOfDecode = data.getSampleIndex( getMarkerA() );
      }
    }

    return Math.max( 0, Math.min( startOfDecode, dataLength ) );
  }

  /**
   * @return
   */
  private boolean isDecodeAll()
  {
    Object value = this.configuration.asMap().get( Constants.PROPERTY_DECODE_ENTIRE_TIMELINE );
    return Boolean.TRUE.equals( value );
  }

  /**
   * @return the timestamp of the first marker, >= 0 or -1.
   */
  private long getMarkerA()
  {
    Object value = this.configuration.asMap().get( Constants.PROPERTY_DECODE_MARKER_A );
    if ( !( value instanceof Number ) )
    {
      return -1;
    }
    return ( ( Number )value ).longValue();
  }

  /**
   * @return the timestamp of the second marker, >= 0 or -1.
   */
  private long getMarkerB()
  {
    Object value = this.configuration.asMap().get( Constants.PROPERTY_DECODE_MARKER_B );
    if ( !( value instanceof Number ) )
    {
      return -1;
    }
    return ( ( Number )value ).longValue();
  }
}
