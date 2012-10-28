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
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.common.Configuration;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Default implementation for {@link ToolInvoker}.
 */
public class ToolInvokerImpl implements ToolInvoker
{
  // INNER TYPES

  /**
   * Provides a generic editor for a tool configuration.
   */
  final class GenericToolConfigurationEditor extends JDialog implements StatusAwareCloseableDialog
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final ToolConfigPanel configPanel;
    private final String pid;
    private DialogStatus status;

    // CONSTRUCTORS

    /**
     * Creates a new {@link GenericToolConfigurationEditor} instance.
     * 
     * @param aContext
     */
    public GenericToolConfigurationEditor( final Window aParent, final String aPID, final ObjectClassDefinition aOCD,
        final ToolContext aContext, final Map<Object, Object> aSettings )
    {
      super( aParent, ModalityType.APPLICATION_MODAL );

      setTitle( aOCD.getName() );

      // Avoid the dialog to be resized automatically...
      getRootPane().putClientProperty( "unmanaged", Boolean.TRUE );

      this.pid = aPID;
      this.configPanel = new ToolConfigPanel( aOCD, aContext, aSettings );

      final JButton closeButton = StandardActionFactory.createCloseButton();
      final JButton okButton = StandardActionFactory.createOkButton();

      final JComponent buttonBar = SwingComponentUtils.createButtonPane( okButton, closeButton );

      SwingComponentUtils.setupDialogContentPane( this, this.configPanel, buttonBar, closeButton );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void close()
    {
      if ( this.status == DialogStatus.OK )
      {
        // Update the configuration...
        updateConfiguration( this, this.pid, this.configPanel.getProperties() );
      }

      setVisible( false );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean setDialogStatus( final DialogStatus aStatus )
    {
      if ( ( aStatus == DialogStatus.OK ) && !this.configPanel.areSettingsValid() )
      {
        return false;
      }
      this.status = aStatus;
      return true;
    }

    /**
     * @return
     */
    public boolean showDialog()
    {
      setVisible( true );
      return this.status == DialogStatus.OK;
    }
  }

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
  private volatile MetaTypeService metaTypeService;
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService logService;

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
  public boolean configure( final Window aParent, final ToolContext aContext )
  {
    MetaTypeInformation metaTypeInformation = getMetaTypeInfo();
    if ( ( metaTypeInformation == null ) || ( metaTypeInformation.getPids().length != 1 ) )
    {
      return true;
    }

    String pid = metaTypeInformation.getPids()[0];

    ObjectClassDefinition ocd = metaTypeInformation.getObjectClassDefinition( pid, aParent.getLocale().toString() );

    GenericToolConfigurationEditor editor = new GenericToolConfigurationEditor( aParent, pid, ocd, aContext,
        this.configuration.asMap() );

    return editor.showDialog();
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
  public void invoke( final ToolContext aContext ) throws ToolException
  {
    this.logService.log( LogService.LOG_DEBUG, "Invoking tool: " + getName() );
    this.delegate.invoke( aContext, this.configuration );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    this.logService.log( LogService.LOG_DEBUG, "Tool configuration updated for: " + getName() );
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
   * @param aOwner
   * @param aPid
   * @param aProperties
   */
  final void updateConfiguration( final Window aOwner, final String aPid, final Dictionary<Object, Object> aProperties )
  {
    try
    {
      org.osgi.service.cm.Configuration config = this.configAdmin.getConfiguration( aPid );
      config.update( aProperties );

      this.configuration.set( aProperties );
    }
    catch ( IOException exception )
    {
      this.logService.log( LogService.LOG_WARNING, "Failed to retrieve configuration!", exception );
      JErrorDialog.showDialog( aOwner, "Failed to retrieve configuration!", exception );
    }
  }

  /**
   * @return a {@link MetaTypeInformation} instance, never <code>null</code>.
   */
  private MetaTypeInformation getMetaTypeInfo()
  {
    Bundle bundle = FrameworkUtil.getBundle( this.delegate.getClass() );
    return this.metaTypeService.getMetaTypeInformation( bundle );
  }
}
