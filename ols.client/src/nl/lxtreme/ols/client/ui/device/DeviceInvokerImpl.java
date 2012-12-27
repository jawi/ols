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
package nl.lxtreme.ols.client.ui.device;


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.common.Configuration;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * 
 */
public class DeviceInvokerImpl implements DeviceInvoker
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

  private final Device delegate;
  private final MutableConfiguration configuration;

  // Injected by Felix DM...
  private volatile BundleContext bundleContext;
  private volatile MetaTypeService metaTypeService;
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService log;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceInvokerImpl instance.
   */
  public DeviceInvokerImpl( final Device aDevice )
  {
    this.delegate = aDevice;
    this.configuration = new MutableConfiguration();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData acquireData( final DeviceProgressListener aProgressListener ) throws IOException,
      InterruptedException
  {
    return this.delegate.acquireData( this.configuration, aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    this.delegate.cancelAcquisition();
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
          "No metatype information to base device configuration on for " + this.delegate.getName()
              + "; assuming no configuration is needed..." );
      return true;
    }

    String pid = metaTypeInfo.getPids()[0];
    ObjectClassDefinition ocd = metaTypeInfo.getObjectClassDefinition( pid, aParent.getLocale().toString() );

    DeviceConfigurationEditor editor = DeviceConfigurationEditor.create( aParent, ocd, this.configuration.asMap() );

    getWindowManager().show( editor ); // Blocks...

    if ( editor.areSettingsValid() )
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

    return editor.areSettingsValid() && ( editor.getDialogStatus() == DialogStatus.OK );
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
  public boolean isSetup()
  {
    return !this.configuration.isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    this.log.log( LogService.LOG_DEBUG, "Device configuration updated for: " + getName() );
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
      dict.put( org.osgi.framework.Constants.SERVICE_PID, metaTypeInfo.getPids()[0] );

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
   * @return the window manager, never <code>null</code>.
   */
  private WindowManager getWindowManager()
  {
    return Client.getInstance().getWindowManager();
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
