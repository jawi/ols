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
package nl.lxtreme.ols.client.ui.util;


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.ConfigurationEditor.ConfigurationChangeListener;
import nl.lxtreme.ols.common.Configuration;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides a base class for service wrappers.
 */
public abstract class DelegateServiceWrapper<SERVICE>
{
  // INNER TYPES

  /**
   * Wraps a given {@link ConfigurationListener} to be called only for a
   * specified PID. When the event is received, the component is unregistered.
   */
  protected static final class ConfigurationListenerWrapper implements ConfigurationListener
  {
    // VARIABLES

    private final ConfigurationListener delegate;
    private final String pid;
    // Injected by Felix DM...
    private volatile DependencyManager dependencyManager;
    private volatile Component component;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ConfigurationListenerWrapper} instance.
     */
    public ConfigurationListenerWrapper( final ConfigurationListener aDelegate, final String aPID )
    {
      this.delegate = aDelegate;
      this.pid = aPID;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void configurationEvent( final ConfigurationEvent aEvent )
    {
      if ( this.pid.equals( aEvent.getPid() ) )
      {
        this.delegate.configurationEvent( aEvent );

        // As we've got what we wanted, remove the component from our service
        // registry...
        this.dependencyManager.remove( this.component );
      }
    }
  }

  /**
   * Provides a thread-safe version of {@link Configuration}.
   */
  protected static final class MutableConfiguration implements Configuration
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
     * @return <code>true</code> if this configuration is empty,
     *         <code>false</code> otherwise.
     */
    public boolean isEmpty()
    {
      return this.mapRef.get().isEmpty();
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

  private final SERVICE delegate;
  private final MutableConfiguration configuration;

  private Class<? extends ConfigurationEditor> customConfigEditorClass;
  private ConfigurationEditor configEditor;

  // Injected by Felix DM...
  private volatile BundleContext bundleContext;
  private volatile MetaTypeService metaTypeService;
  private volatile ConfigurationAdmin configAdmin;
  private volatile DependencyManager dependencyManager;
  protected volatile LogService log;

  // CONSTRUCTOR

  /**
   * Creates a new DelegateServiceWrapper instance.
   */
  public DelegateServiceWrapper( final SERVICE aDelegate )
  {
    this.delegate = aDelegate;
    this.configuration = new MutableConfiguration();
  }

  // METHODS

  /**
   * @param aParent
   * @param aListener
   */
  public void configure( final Window aParent, final ConfigurationListener aListener )
  {
    this.configEditor = createEditor( aParent );
    if ( this.configEditor == null )
    {
      this.log.log( LogService.LOG_WARNING, "Failed to configure " + getName()
          + "; could not find its metatype information, nor a specified configuration editor!" );
      return;
    }

    this.configEditor.addConfigurationChangeListener( new ConfigurationChangeListener()
    {
      private final DependencyManager dependencyManager = DelegateServiceWrapper.this.dependencyManager;
      private final LogService log = DelegateServiceWrapper.this.log;

      @Override
      public void onConfigurationAcknowledged( final String aPID, final Map<Object, Object> aConfiguration )
      {
        // Register a configuration listener that notifies the original
        // callback when the configuration is actually valid...
        Component comp = this.dependencyManager.createComponent()
            .setInterface( ConfigurationListener.class.getName(), null ) //
            .setImplementation( new ConfigurationListenerWrapper( aListener, aPID ) );
        this.dependencyManager.add( comp );

        try
        {
          // Post back the configuration to ConfigAdmin...
          updateConfiguration( aPID, aConfiguration );
        }
        catch ( IOException exception )
        {
          this.log.log( LogService.LOG_WARNING, "Failed to update configuration!", exception );
          JErrorDialog.showDialog( null, "Failed to update configuration!", exception );
        }

        clearEditorReference();
      }

      @Override
      public void onConfigurationDiscarded()
      {
        clearEditorReference();
      }
    } );

    showEditor();
  }

  /**
   * @return
   */
  public abstract String getName();

  /**
   * {@inheritDoc}
   */
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    this.log.log( LogService.LOG_DEBUG, "Configuration updated for: " + getName() );
    getConfiguration().set( aProperties );
  }

  /**
   * Clears the editor reference after its being used.
   */
  final void clearEditorReference()
  {
    if ( this.configEditor != null )
    {
      ReflectionUtil.callMethod( this.configEditor, "destroy" );

      // Clear our the reference to let it be GC'd...
      this.configEditor = null;
    }
  }

  /**
   * Factory method for creating a new {@link ConfigurationEditor} instance.
   * 
   * @param aParent
   *          the parent window to use for displaying dialogs.
   * @return a {@link ConfigurationEditor} instance, can be <code>null</code> in
   *         case it could not be created.
   */
  protected final ConfigurationEditor createEditor( final Window aParent )
  {
    final ObjectClassDefinition ocd = getOCD( aParent.getLocale() );

    ConfigurationEditor result = null;

    // A custom editor has priority over the metatype definition...
    if ( this.customConfigEditorClass != null )
    {
      Class<?>[][] signatures;
      Object[][] parameters;
      Map<Object, Object> config = getConfiguration().asMap();

      if ( ocd != null )
      {
        signatures = new Class<?>[][] { { Window.class, ObjectClassDefinition.class, Map.class },
            { Window.class, Map.class }, {} };
        parameters = new Object[][] { { aParent, ocd, config }, { aParent, config }, {} };
      }
      else
      {
        signatures = new Class<?>[][] { { Window.class, Map.class }, {} };
        parameters = new Object[][] { { aParent, config }, {} };
      }

      try
      {
        result = ReflectionUtil.newInstance( this.customConfigEditorClass, signatures, parameters );

        // Allow the editor to have an initialize() method to be called...
        ReflectionUtil.callMethod( result, "initialize" );
      }
      catch ( Exception exception )
      {
        this.log.log( LogService.LOG_INFO, "Failed to create custom configuration editor for " + getName()
            + "; trying metatype editor!", exception );
      }
    }

    // Fall back to the generic metatype editor...
    if ( result == null )
    {
      if ( ocd == null )
      {
        // Not metatyped; assume it has no configuration to be performed...
        this.log.log( LogService.LOG_DEBUG, "Cannot configure " + getName() + ", as it has no metatype information!" );
      }
      else
      {
        result = createEditor( aParent, ocd );
      }
    }

    return result;
  }

  /**
   * Factory method for creating a new {@link ConfigurationEditor} instance
   * based on a given {@link ObjectClassDefinition}.
   * 
   * @param aParent
   *          the parent window to use for displaying dialogs, can be
   *          <code>null</code>;
   * @param aOCD
   *          the object class definition to use for the editor, cannot be
   *          <code>null</code>.
   * @return a {@link ConfigurationEditor} instance, never <code>null</code>.
   */
  protected abstract ConfigurationEditor createEditor( final Window aParent, ObjectClassDefinition aOCD );

  /**
   * Returns the current value of configuration.
   * 
   * @return the configuration
   */
  protected final MutableConfiguration getConfiguration()
  {
    return this.configuration;
  }

  /**
   * Returns the current value of delegate.
   * 
   * @return the delegate service, never <code>null</code>.
   */
  protected final SERVICE getDelegate()
  {
    return this.delegate;
  }

  /**
   * @return a {@link MetaTypeInformation} instance, can be <code>null</code> if
   *         the bundle has no metatype information.
   */
  protected final MetaTypeInformation getMetaTypeInfo()
  {
    return this.metaTypeService.getMetaTypeInformation( this.bundleContext.getBundle() );
  }

  /**
   * @param aLocale
   *          the locale to use for the {@link ObjectClassDefinition}.
   * @return a {@link ObjectClassDefinition} instance, can be <code>null</code>.
   */
  protected final ObjectClassDefinition getOCD( final Locale aLocale )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo == null ) || ( metaTypeInfo.getPids().length != 1 ) )
    {
      // Not metatyped; assume it has no configuration to be performed...
      return null;
    }

    String pid = metaTypeInfo.getPids()[0];

    return metaTypeInfo.getObjectClassDefinition( pid, aLocale.toString() );
  }

  /**
   * @return the window manager, never <code>null</code>.
   */
  protected final WindowManager getWindowManager()
  {
    return Client.getInstance().getWindowManager();
  }

  /**
   * Called by Felix DM upon initialization of this component.
   */
  @SuppressWarnings( "unchecked" )
  protected final void init( final Component aComponent )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo != null ) && ( metaTypeInfo.getPids().length == 1 ) )
    {
      Dictionary<Object, Object> dict = aComponent.getServiceProperties();
      dict.put( Constants.SERVICE_PID, metaTypeInfo.getPids()[0] );

      aComponent.setServiceProperties( dict );
    }

    findCustomConfigEditorClass();
  }

  /**
   * Shows the current configuration editor, if it is a Swing-component.
   */
  protected void showEditor()
  {
    if ( this.configEditor instanceof Window )
    {
      getWindowManager().show( ( Window )this.configEditor );
    }
  }

  /**
   * @param aPid
   * @param aProperties
   */
  protected final void updateConfiguration( final String aPid, final Map<Object, Object> aProperties )
      throws IOException
  {
    Properties props = new Properties();
    props.putAll( aProperties );

    org.osgi.service.cm.Configuration config = this.configAdmin.getConfiguration( aPid );
    config.update( props );

    getConfiguration().set( props );
  }

  /**
   * Tries to determine whether the bundle
   */
  @SuppressWarnings( { "rawtypes", "unchecked" } )
  private void findCustomConfigEditorClass()
  {
    // Use the bundle of the *delegate*, so it can tell us what configuration
    // editor we should use...
    Bundle bundle = FrameworkUtil.getBundle( this.delegate.getClass() );
    Dictionary headers = bundle.getHeaders();

    Object customConfigurationEditorValue = headers.get( ConfigurationEditor.BUNDLE_HEADER_KEY );
    try
    {
      if ( customConfigurationEditorValue != null )
      {
        Class clazz = bundle.loadClass( customConfigurationEditorValue.toString() );
        if ( ConfigurationEditor.class.isAssignableFrom( clazz ) )
        {
          this.customConfigEditorClass = clazz;
        }
        else
        {
          this.log.log( LogService.LOG_INFO, getName() + " specified a custom configuration "
              + " editor that is not an instance of " + ConfigurationEditor.class.getName() );
        }
      }
    }
    catch ( ClassNotFoundException exception )
    {
      // Log only; don't do anything about this...
      this.log.log( LogService.LOG_INFO, "Failed to load custom configuration editor ("
          + customConfigurationEditorValue + ")", exception );
      exception.printStackTrace();
    }
  }
}
