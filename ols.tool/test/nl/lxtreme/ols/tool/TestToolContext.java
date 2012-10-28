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
package nl.lxtreme.ols.tool;


import org.mockito.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Provides an implementation of {@link ToolContext} for testing purposes.
 */
public class TestToolContext implements ToolContext
{
  // VARIABLES

  private final Session session;
  private final int startSampleIdx;
  private final int endSampleIdx;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TestToolContext} instance.
   * 
   * @param aData
   * @param aListener
   */
  public TestToolContext( final AcquisitionData aData, final AnnotationListener aListener )
  {
    this( aData, Math.max( 0, aData.getSampleIndex( aData.getTriggerPosition() ) - 1 ), aData.getValues().length - 1,
        aListener );
  }

  /**
   * Creates a new {@link TestToolContext} instance.
   * 
   * @param aData
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   * @param aListener
   */
  public TestToolContext( final AcquisitionData aData, final int aStartSampleIdx, final int aEndSampleIdx,
      final AnnotationListener aListener )
  {
    this.startSampleIdx = Math.max( 0, aStartSampleIdx );
    this.endSampleIdx = Math.min( aEndSampleIdx, aData.getValues().length - 1 );
    this.progressListener = Mockito.mock( ToolProgressListener.class );
    this.annotationListener = aListener;

    this.session = new Session()
    {
      private volatile AcquisitionData data = aData;

      @Override
      public void setAcquisitionData( final AcquisitionData aData ) throws IllegalArgumentException
      {
        this.data = aData;
      }

      @Override
      public void reset()
      {
        this.data = null;
      }

      @Override
      public boolean hasData()
      {
        return this.data != null;
      }

      @Override
      public AcquisitionData getAcquisitionData()
      {
        return this.data;
      }
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addAnnotation( final Annotation aAnnotation )
  {
    this.annotationListener.onAnnotation( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations( final int... aChannelIdxs )
  {
    for ( int channelIdx : aChannelIdxs )
    {
      this.annotationListener.clearAnnotations( channelIdx );
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
    return getAcquisitionData().getEnabledChannels();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getEndSampleIndex()
  {
    return this.endSampleIdx;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLength()
  {
    return 0;
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
    return this.startSampleIdx;
  }
}
