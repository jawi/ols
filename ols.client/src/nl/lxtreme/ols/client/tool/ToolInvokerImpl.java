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


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.common.Configuration;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.event.Event;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Default implementation for {@link ToolInvoker}.
 */
public class ToolInvokerImpl implements ToolInvoker
{
  // INNER TYPES

  /**
   * Provides a thread-safe version of {@link Configuration}.
   */
  static class MutableConfiguration implements Configuration
  {
    // VARIABLES

    private final AtomicReference<Map<Object, Object>> mapRef;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MutableConfiguration} instance.
     */
    public MutableConfiguration()
    {
      this.mapRef = new AtomicReference<Map<Object, Object>>( new HashMap<Object, Object>() );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<Object, Object> asMap()
    {
      return this.mapRef.get();
    }

    /**
     * Sets the configuration of this object to the given value.
     * 
     * @param aValue
     *          the new configuration to set, cannot be <code>null</code>.
     */
    @SuppressWarnings( "rawtypes" )
    public void set( final Dictionary aValue )
    {
      Map<Object, Object> value = asMap( aValue );
      Map<Object, Object> old;
      do
      {
        old = this.mapRef.get();
      }
      while ( !this.mapRef.compareAndSet( old, value ) );
    }

    /**
     * Converts a given {@link Dictionary} to a {@link Map}.
     * 
     * @param aValue
     *          the dictionary to convert, can be <code>null</code>.
     * @return a map representation of the given {@link Dictionary}, or an empty
     *         map is the given value was <code>null</code>.
     */
    @SuppressWarnings( "rawtypes" )
    private Map<Object, Object> asMap( final Dictionary aValue )
    {
      HashMap<Object, Object> result = new HashMap<Object, Object>();
      if ( aValue != null )
      {
        Enumeration keys = aValue.keys();
        while ( keys.hasMoreElements() )
        {
          Object key = keys.nextElement();
          result.put( key, aValue.get( key ) );
        }
      }
      return result;
    }
  }

  // VARIABLES

  private final Tool delegate;
  private final MutableConfiguration configuration;

  // Injected by Felix DM...
  private volatile BundleContext bundleContext;
  private volatile MetaTypeService metaTypeService;
  private volatile EventAdmin eventAdmin;
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService log;
  private volatile Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolInvokerImpl} instance.
   */
  public ToolInvokerImpl( final Tool aTool )
  {
    this.delegate = aTool;
    this.configuration = new MutableConfiguration();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean configure( final Window aParent )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo == null ) || ( metaTypeInfo.getPids().length != 1 ) )
    {
      // Not metatyped; assume it has no configuration to be performed...
      this.log.log( LogService.LOG_INFO,
          "No metatype information to base tool configuration on; assuming no configuration is needed..." );
      return true;
    }

    String pid = metaTypeInfo.getPids()[0];
    ObjectClassDefinition ocd = metaTypeInfo.getObjectClassDefinition( pid, aParent.getLocale().toString() );

    AcquisitionDataInfo dataInfo = new AcquisitionDataInfo( this.session );

    ToolConfigurationEditor editor = new ToolConfigurationEditor( aParent, ocd, dataInfo, this.configuration.asMap() );

    final boolean result = editor.showDialog();
    if ( result )
    {
      try
      {
        // Post back the configuration to ConfigAdmin...
        updateConfiguration( pid, editor.getProperties() );
      }
      catch ( IOException exception )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to update configuration!", exception );
        JErrorDialog.showDialog( aParent, "Failed to update configuration!", exception );
      }
    }

    return result;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolCategory getCategory()
  {
    return this.delegate.getCategory();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return this.delegate.getName();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke() throws ToolException
  {
    this.log.log( LogService.LOG_DEBUG, "Invoking tool: " + getName() );

    final SwingWorker<Void, Integer> worker = new SwingWorker<Void, Integer>()
    {
      // VARIABLES

      private final Tool delegate = ToolInvokerImpl.this.delegate;
      private final Session session = ToolInvokerImpl.this.session;
      private final EventAdmin eventAdmin = ToolInvokerImpl.this.eventAdmin;
      private final Configuration configuration = ToolInvokerImpl.this.configuration;
      private final Long startTime = Long.valueOf( System.currentTimeMillis() );

      // METHODS

      @Override
      protected Void doInBackground() throws Exception
      {
        final ToolContext context = new ToolContextImpl( this.session, new ToolProgressListener()
        {
          @Override
          public void setProgress( final int aPercentage )
          {
            publish( Integer.valueOf( aPercentage ) );
          }
        } );

        this.eventAdmin.postEvent( createEvent( TOOL_STATUS_STARTED, null ) );

        this.delegate.invoke( context, this.configuration );

        return null;
      }

      /**
       * {@inheritDoc}
       */
      @Override
      protected void process( final List<Integer> aChunks )
      {
        if ( !aChunks.isEmpty() )
        {
          Integer lastValue = aChunks.get( aChunks.size() - 1 );

          this.eventAdmin.postEvent( createEvent( lastValue ) );
        }
      }

      @Override
      protected void done()
      {
        String state;
        Throwable ex = null;
        try
        {
          // This should return immediately...
          get();

          state = TOOL_STATUS_SUCCESS;
        }
        catch ( InterruptedException exception )
        {
          state = TOOL_STATUS_CANCELLED;
        }
        catch ( ExecutionException exception )
        {
          state = TOOL_STATUS_FAILED;
          ex = exception.getCause();
        }
        this.eventAdmin.postEvent( createEvent( state, ex ) );
      }

      private Event createEvent( final String aState, final Throwable aException )
      {
        Map<Object, Object> props = new HashMap<Object, Object>();
        props.put( KEY_TOOL_NAME, this.delegate.getName() );
        props.put( KEY_TOOL_START_TIME, this.startTime );
        props.put( KEY_TOOL_EXCEPTION, aException );
        props.put( KEY_TOOL_STATE, aState );
        return new Event( TOPIC_TOOL_STATUS, props );
      }

      private Event createEvent( final Integer aPercentage )
      {
        Map<Object, Object> props = new HashMap<Object, Object>();
        props.put( KEY_TOOL_NAME, this.delegate.getName() );
        props.put( KEY_TOOL_START_TIME, this.startTime );
        props.put( KEY_TOOL_PROGRESS, aPercentage );
        return new Event( TOPIC_TOOL_PROGRESS, props );
      }
    };
    worker.execute();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    this.log.log( LogService.LOG_DEBUG, "Tool configuration updated for: " + getName() );
    this.configuration.set( aProperties );
  }

  /**
   * Called by Felix DM upon initialization of this component.
   */
  final void init( final Component aComponent )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo != null ) && ( metaTypeInfo.getPids().length == 1 ) )
    {
      Dictionary<Object, Object> dict = new Hashtable<Object, Object>();
      dict.put( Constants.SERVICE_PID, metaTypeInfo.getPids()[0] );

      aComponent.setServiceProperties( dict );
    }
  }

  /**
   * @return a {@link MetaTypeInformation} instance, never <code>null</code>.
   */
  private MetaTypeInformation getMetaTypeInfo()
  {
    return this.metaTypeService.getMetaTypeInformation( this.bundleContext.getBundle() );
  }

  /**
   * @param aOwner
   * @param aPid
   * @param aProperties
   */
  private void updateConfiguration( final String aPid, final Dictionary<Object, Object> aProperties )
      throws IOException
  {
    org.osgi.service.cm.Configuration config = this.configAdmin.getConfiguration( aPid );
    config.update( aProperties );

    this.configuration.set( aProperties );
  }
}
