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
package nl.lxtreme.ols.client.session;


import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;

import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides a default implementation of {@link Session} that posts events using
 * {@link EventAdmin} in case of changes in the acquisition data.
 */
public class SessionImpl implements Session
{
  // VARIABLES

  private final AnnotationData annotationData;

  // Injected by Felix DM...
  private volatile EventAdmin eventAdmin;
  private volatile LogService logService;

  private volatile AcquisitionData data;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SessionImpl} instance.
   */
  public SessionImpl()
  {
    this.data = null;
    this.annotationData = new AnnotationDataImpl();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData getAcquisitionData()
  {
    return this.data;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AnnotationData getAnnotationData()
  {
    return this.annotationData;
  }

  /**
   * Returns the composition.
   * 
   * @return an array of objects forming the composition.
   */
  public Object[] getComposition()
  {
    return new Object[] { this, this.annotationData };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasData()
  {
    return this.data != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void reset()
  {
    this.data = null;
    this.annotationData.clearAll();

    this.logService.log( LogService.LOG_INFO, "Resetting session ..." );

    this.eventAdmin.postEvent( createEvent( null ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setAcquisitionData( final AcquisitionData aData ) throws IllegalArgumentException
  {
    if ( aData == null )
    {
      throw new IllegalArgumentException( "Data cannot be null!" );
    }
    this.data = aData;

    this.logService.log( LogService.LOG_INFO, "Setting acquisition data ..." );

    this.eventAdmin.postEvent( createEvent( aData ) );
  }

  /**
   * Creates a event for posting to the {@link EventAdmin}.
   * 
   * @param aData
   *          the acquisition data for which to create an event.
   * @return an {@link Event} instance, never <code>null</code>.
   */
  private Event createEvent( final AcquisitionData aData )
  {
    Map<String, Object> props = new HashMap<String, Object>();
    props.put( Session.KEY_ACQUISITION_DATA, aData );

    return new Event( Session.TOPIC_ACQUISITION_DATA_CHANGED, props );
  }
}
