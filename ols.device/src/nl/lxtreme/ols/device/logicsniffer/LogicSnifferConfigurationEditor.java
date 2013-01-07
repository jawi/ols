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
package nl.lxtreme.ols.device.logicsniffer;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;

import javax.microedition.io.*;
import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.device.logicsniffer.profile.*;
import nl.lxtreme.ols.device.logicsniffer.ui.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.io.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides a custom configuration editor for the LogicSniffer device.
 */
public class LogicSnifferConfigurationEditor extends JDialog implements ConfigurationEditor, DeviceMetadataProvider,
    DeviceProfileProvider
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final EventListenerList eventListeners;

  private JTabbedPane contentPane;

  private DependencyManager dm;
  private volatile DeviceProfileManager deviceProfileManager;
  private volatile ConnectorService connectorService;
  private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link LogicSnifferConfigurationEditor} instance.
   */
  public LogicSnifferConfigurationEditor( final Window aParent, final ObjectClassDefinition aOCD,
      final Map<Object, Object> aInitialValues )
  {
    super( aParent, "OLS Capture settings", ModalityType.DOCUMENT_MODAL );

    this.eventListeners = new EventListenerList();

    initDialog( aOCD, aInitialValues );
    buildDialog( aOCD, aInitialValues );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addConfigurationChangeListener( final ConfigurationChangeListener aListener )
  {
    this.eventListeners.add( ConfigurationChangeListener.class, aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceProfile getDefaultProfile()
  {
    if ( this.deviceProfileManager == null )
    {
      return null;
    }
    return this.deviceProfileManager.getDefaultProfile();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<DeviceProfile> getDeviceProfiles()
  {
    List<DeviceProfile> result = Collections.emptyList();
    if ( this.deviceProfileManager != null )
    {
      result = this.deviceProfileManager.getDeviceProfiles();
    }
    Collections.sort( result );
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceMetadata readMetadata( final String aConnectionURI ) throws IOException
  {
    StreamConnection conn = null;

    try
    {
      conn = ( StreamConnection )this.connectorService
          .open( aConnectionURI, ConnectorService.READ_WRITE, true /* timeouts */);

      LogicSnifferMetadata metadata = new LogicSnifferDetector( this.logService ).detect( conn );
      // Wire up the corresponding device profile...
      if ( ( metadata != null ) && ( this.deviceProfileManager != null ) )
      {
        metadata.setDeviceProfile( this.deviceProfileManager.findProfile( metadata.getName() ) );
      }

      return metadata;
    }
    finally
    {
      if ( conn != null )
      {
        conn.close();
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeConfigurationChangeListener( final ConfigurationChangeListener aListener )
  {
    this.eventListeners.remove( ConfigurationChangeListener.class, aListener );
  }

  /**
   * Called automatically when this editor is no longer used.
   */
  @SuppressWarnings( "unchecked" )
  final void destroy()
  {
    if ( this.dm != null )
    {
      List<Component> components = this.dm.getComponents();
      for ( Component c : components )
      {
        this.dm.remove( c );
      }
      this.dm = null;
    }

    // Release all native resources...
    dispose();
  }

  /**
   * Called automatically when this editor is about to be used.
   */
  final void initialize()
  {
    Bundle bundle = FrameworkUtil.getBundle( getClass() );
    if ( bundle != null )
    {
      this.deviceProfileManager = new DeviceProfileManager();

      Dictionary<String, String> props = new Hashtable<String, String>();
      props.put( Constants.SERVICE_PID, DeviceProfileManager.SERVICE_PID );

      this.dm = new DependencyManager( bundle.getBundleContext() );
      this.dm.add( //
          this.dm.createComponent() //
              .setInterface( ManagedServiceFactory.class.getName(), props ) //
              .setImplementation( this.deviceProfileManager ) //
          );
      this.dm.add( //
          this.dm.createComponent() //
              .setImplementation( this ) //
              .add( this.dm.createServiceDependency() //
                  .setService( ConnectorService.class ) //
                  .setInstanceBound( true ) //
                  .setRequired( true ) //
              ) //
              .add( this.dm.createServiceDependency() //
                  .setService( LogService.class ) //
                  .setInstanceBound( true ) //
                  .setRequired( false ) //
              ) );
    }

    updateFields();
  }

  /**
   * Builds this dialog.
   */
  private void buildDialog( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    JButton captureButton = new JButton( "Capture" );
    JButton cancelButton = StandardActionFactory.createCancelButton();
    JComponent buttonPane = createButtonPane( captureButton, cancelButton );

    setupDialogContentPane( this, this.contentPane, buttonPane, captureButton );
  }

  /**
   * @return a panel instance that centers the given component.
   */
  private JPanel centerPanel( final JComponent aComponent )
  {
    final JPanel result = new JPanel( new GridBagLayout() );
    result.add( aComponent, new GridBagConstraints( 0, 0, 1, 1, 0.1, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    result.add( new JLabel(), new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER,
        GridBagConstraints.BOTH, new Insets( 0, 0, 0, 0 ), 0, 0 ) );

    return result;
  }

  /**
   * @return a panel with the controls for the acquisition settings, never
   *         <code>null</code>.
   */
  private AcquisitionSettingsPanel createAcquisitionSettingsPane( final ObjectClassDefinition aOCD,
      final Map<Object, Object> aInitialValues )
  {
    return new AcquisitionSettingsPanel( aOCD, aInitialValues );
  }

  /**
   * @return a panel with the controls for the connection settings, never
   *         <code>null</code>.
   */
  private ConnectionSettingsPanel createConnectionSettingsPane( final ObjectClassDefinition aOCD,
      final Map<Object, Object> aInitialValues )
  {
    return new ConnectionSettingsPanel( aOCD, aInitialValues, this, this );
  }

  /**
   * @return a panel with the controls for the trigger settings, never
   *         <code>null</code>.
   */
  private TriggerSettingsPanel createTriggerPane( final ObjectClassDefinition aOCD,
      final Map<Object, Object> aInitialValues )
  {
    return new TriggerSettingsPanel( aOCD, aInitialValues );
  }

  /**
   * Initializes all components of this dialog.
   */
  private void initDialog( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    ConnectionSettingsPanel connSettings = createConnectionSettingsPane( aOCD, aInitialValues );
    AcquisitionSettingsPanel acquisitionSettings = createAcquisitionSettingsPane( aOCD, aInitialValues );
    TriggerSettingsPanel triggerSettings = createTriggerPane( aOCD, aInitialValues );

    // Wire all change listeners...
    addPropertyChangeListener( "fields", connSettings );
    connSettings.addDeviceProfileChangeListener( acquisitionSettings );
    connSettings.addDeviceProfileChangeListener( triggerSettings );

    this.contentPane = new JTabbedPane( SwingConstants.TOP, JTabbedPane.SCROLL_TAB_LAYOUT );
    this.contentPane.addTab( "Connection", centerPanel( connSettings ) );
    this.contentPane.addTab( "Acquisition", centerPanel( acquisitionSettings ) );
    this.contentPane.addTab( "Triggers", centerPanel( triggerSettings ) );
  }

  /**
   * Fires an update event to all interested listeners.
   */
  private void updateFields()
  {
    firePropertyChange( "fields", -1L, System.currentTimeMillis() );
  }
}
