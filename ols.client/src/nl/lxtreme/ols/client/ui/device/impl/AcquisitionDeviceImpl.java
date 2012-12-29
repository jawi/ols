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
package nl.lxtreme.ols.client.ui.device.impl;


import java.awt.*;
import java.io.*;
import java.util.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.client.ui.editor.*;
import nl.lxtreme.ols.client.ui.util.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides an implementation of {@link AcquisitionDevice}.
 */
public class AcquisitionDeviceImpl extends DelegateServiceWrapper<Device> implements AcquisitionDevice
{
  // VARIABLES

  private volatile DeviceConfigurationEditor configEditor;
  // Injected by Felix DM...
  private volatile DependencyManager dependencyManager;
  private volatile LogService log;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionDeviceImpl} instance.
   */
  public AcquisitionDeviceImpl( final Device aDevice )
  {
    super( aDevice );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData acquireData( final DeviceProgressListener aProgressListener ) throws IOException,
      InterruptedException
  {
    return getDelegate().acquireData( getConfiguration(), aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    getDelegate().cancelAcquisition();
  }

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
      this.log.log( LogService.LOG_INFO, "No metatype information to base device configuration on for " + getName()
          + "; assuming no configuration is needed..." );
      return;
    }

    this.configEditor = DeviceConfigurationEditor.create( aParent, ocd, getConfiguration().asMap() );
    this.configEditor.addDialogStateListener( new DialogStateListener()
    {
      private final DeviceConfigurationEditor configEditor = AcquisitionDeviceImpl.this.configEditor;
      private final DependencyManager dependencyManager = AcquisitionDeviceImpl.this.dependencyManager;
      private final LogService log = AcquisitionDeviceImpl.this.log;

      @Override
      public void onStateChanged( final DialogStatus aState )
      {
        if ( ( aState == DialogStatus.OK ) && ( this.configEditor != null ) && this.configEditor.areSettingsValid() )
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
        AcquisitionDeviceImpl.this.configEditor = null;
      }
    } );

    getWindowManager().show( this.configEditor );
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
  public boolean isSetup()
  {
    return !getConfiguration().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    this.log.log( LogService.LOG_DEBUG, "Device configuration updated for: " + getName() );
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
