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


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.editor.*;
import nl.lxtreme.ols.client.ui.tool.*;
import nl.lxtreme.ols.client.ui.util.*;
import nl.lxtreme.ols.common.Configuration;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.event.Event;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Default implementation for {@link ToolInvoker}.
 */
public class ToolInvokerImpl extends DelegateServiceWrapper<Tool> implements ToolInvoker
{
  // VARIABLES

  private volatile ToolConfigurationEditor configEditor;
  // Injected by Felix DM...
  private volatile DependencyManager dependencyManager;
  private volatile EventAdmin eventAdmin;
  private volatile LogService log;
  private volatile Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolInvokerImpl} instance.
   */
  public ToolInvokerImpl( final Tool aTool )
  {
    super( aTool );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void configure( final Window aParent, final ConfigurationListener aListener )
  {
    ObjectClassDefinition ocd = getOCD( aParent.getLocale() );
    if ( ocd == null )
    {
      // Not metatyped; assume it has no configuration to be performed...
      this.log.log( LogService.LOG_INFO, "No metatype information to base tool configuration on for " + getName()
          + "; assuming no configuration is needed..." );
      return;
    }

    this.configEditor = ToolConfigurationEditor.create( aParent, ocd, getConfiguration().asMap(),
        new AcquisitionDataInfo( this.session ) );
    this.configEditor.addDialogStateListener( new DialogStateListener()
    {
      private final ToolConfigurationEditor configEditor = ToolInvokerImpl.this.configEditor;
      private final DependencyManager dependencyManager = ToolInvokerImpl.this.dependencyManager;
      private final LogService log = ToolInvokerImpl.this.log;

      @Override
      public void onStateChanged( final DialogStatus aState )
      {
        if ( ( DialogStatus.OK == aState ) && ( this.configEditor != null ) && this.configEditor.areSettingsValid() )
        {
          String pid = this.configEditor.getPid();

          // Register a configuration listener that notifies the original
          // callback when the configuration is actually valid...
          Component comp = this.dependencyManager.createComponent()
              .setInterface( ConfigurationListener.class.getName(), null ) //
              .setImplementation( new ConfigurationListenerWrapper( aListener, pid ) );
          this.dependencyManager.add( comp );

          try
          {
            // Post back the configuration to ConfigAdmin...
            updateConfiguration( pid, this.configEditor.getProperties() );
          }
          catch ( IOException exception )
          {
            this.log.log( LogService.LOG_WARNING, "Failed to update configuration!", exception );
            JErrorDialog.showDialog( null, "Failed to update configuration!", exception );
          }
        }

        // Clear our the reference to let it be GC'd...
        ToolInvokerImpl.this.configEditor = null;
      }
    } );

    getWindowManager().show( this.configEditor ); // Blocks...
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolCategory getCategory()
  {
    return getDelegate().getCategory();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return getDelegate().getName();
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

      private final Tool delegate = getDelegate();
      private final Configuration configuration = getConfiguration();
      private final Session session = ToolInvokerImpl.this.session;
      private final EventAdmin eventAdmin = ToolInvokerImpl.this.eventAdmin;
      private final Long startTime = Long.valueOf( System.currentTimeMillis() );

      // METHODS

      @Override
      protected Void doInBackground() throws Exception
      {
        final ToolContext context = new ToolContextImpl( this.session, this.configuration, new ToolProgressListener()
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
        props.put( KEY_TOOL_NAME, getToolName() );
        props.put( KEY_TOOL_START_TIME, this.startTime );
        props.put( KEY_TOOL_EXCEPTION, aException );
        props.put( KEY_TOOL_STATE, aState );
        return new Event( TOPIC_TOOL_STATUS, props );
      }

      private Event createEvent( final Integer aPercentage )
      {
        Map<Object, Object> props = new HashMap<Object, Object>();
        props.put( KEY_TOOL_NAME, getToolName() );
        props.put( KEY_TOOL_START_TIME, this.startTime );
        props.put( KEY_TOOL_PROGRESS, aPercentage );
        return new Event( TOPIC_TOOL_PROGRESS, props );
      }

      /**
       * @return a tool name, never <code>null</code>.
       */
      private String getToolName()
      {
        String result = this.delegate.getName();
        if ( result.endsWith( "..." ) )
        {
          result = result.substring( 0, result.length() - 3 );
        }
        return result.trim();
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
    getConfiguration().set( aProperties );
  }

  /**
   * @return the window manager, never <code>null</code>.
   */
  private WindowManager getWindowManager()
  {
    return Client.getInstance().getWindowManager();
  }
}
