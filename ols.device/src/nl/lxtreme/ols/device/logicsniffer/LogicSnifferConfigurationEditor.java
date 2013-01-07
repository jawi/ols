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
import java.awt.event.*;
import java.beans.*;
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
import nl.lxtreme.ols.util.swing.editor.util.*;

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
public class LogicSnifferConfigurationEditor extends JDialog implements PropertyChangeListener, ConfigurationEditor,
    DeviceMetadataProvider, DeviceProfileProvider
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ObjectClassDefinition ocd;
  private final Map<Object, Object> initialValues;
  private final EventListenerList eventListeners;

  private ConnectionSettingsPanel connectionSettings;
  private AcquisitionSettingsPanel acquisitionSettings;
  private TriggerSettingsPanel triggerSettings;
  private JTabbedPane contentPane;
  private JButton captureButton;
  private JButton cancelButton;

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

    this.ocd = aOCD;
    this.initialValues = new HashMap<Object, Object>( aInitialValues );

    this.eventListeners = new EventListenerList();
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
   * @return <code>true</code> if the settings are valid, <code>false</code>
   *         otherwise.
   */
  public boolean areSettingsValid()
  {
    if ( !this.connectionSettings.areSettingsValid() )
    {
      return false;
    }
    if ( !this.acquisitionSettings.areSettingsValid() )
    {
      return false;
    }
    if ( !this.triggerSettings.areSettingsValid() )
    {
      return false;
    }
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceProfile findProfile( final String aIdentifier )
  {
    if ( this.deviceProfileManager == null )
    {
      return null;
    }
    return this.deviceProfileManager.findProfile( aIdentifier );
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
   * Returns the current value of PID.
   * 
   * @return the PID, cannot be <code>null</code>.
   */
  public String getPid()
  {
    return this.ocd.getID();
  }

  /**
   * @return the configuration properties, never <code>null</code>.
   */
  public Map<Object, Object> getProperties()
  {
    Map<Object, Object> result = new HashMap<Object, Object>();

    this.connectionSettings.getConfiguration( result );
    this.acquisitionSettings.getConfiguration( result );
    this.triggerSettings.getConfiguration( result );

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    if ( GenericComponentChangeAdapter.PROPERTY_NAME.equals( aEvent.getPropertyName() ) )
    {
      this.cancelButton.setEnabled( true );
      this.captureButton.setEnabled( areSettingsValid() );
    }
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
   * Fires an event to all {@link ConfigurationChangeListener}s that the
   * configuration is acknowledged.
   */
  final void fireConfigurationAcknowledgedEvent()
  {
    ConfigurationChangeListener[] listeners = this.eventListeners.getListeners( ConfigurationChangeListener.class );
    for ( ConfigurationChangeListener listener : listeners )
    {
      listener.onConfigurationAcknowledged( getPid(), getProperties() );
    }
  }

  /**
   * Fires an event to all {@link ConfigurationChangeListener}s that the
   * configuration is to be discarded.
   */
  final void fireConfigurationDiscardedEvent()
  {
    ConfigurationChangeListener[] listeners = this.eventListeners.getListeners( ConfigurationChangeListener.class );
    for ( ConfigurationChangeListener listener : listeners )
    {
      listener.onConfigurationDiscarded();
    }
  }

  /**
   * Called automatically when this editor is about to be used.
   */
  final void initialize()
  {
    this.deviceProfileManager = new DeviceProfileManager();

    Bundle bundle = FrameworkUtil.getBundle( getClass() );
    if ( bundle != null )
    {
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

    initDialog( this.ocd, this.initialValues );
    buildDialog();

    updateFields();
  }

  /**
   * Builds this dialog.
   */
  private void buildDialog()
  {
    this.captureButton = new JButton( "Capture" );
    this.captureButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        fireConfigurationAcknowledgedEvent();
      }
    } );
    this.cancelButton = StandardActionFactory.createCancelButton();
    this.cancelButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        fireConfigurationDiscardedEvent();
      }
    } );
    JComponent buttonPane = createButtonPane( this.captureButton, this.cancelButton );

    setupDialogContentPane( this, this.contentPane, buttonPane, this.captureButton );
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
    this.connectionSettings = createConnectionSettingsPane( aOCD, aInitialValues );
    this.connectionSettings.addPropertyChangeListener( this );

    this.acquisitionSettings = createAcquisitionSettingsPane( aOCD, aInitialValues );
    this.acquisitionSettings.addPropertyChangeListener( this );

    this.triggerSettings = createTriggerPane( aOCD, aInitialValues );
    this.triggerSettings.addPropertyChangeListener( this );

    // Wire all change listeners...
    addPropertyChangeListener( "fields", this.connectionSettings );
    this.connectionSettings.addDeviceProfileChangeListener( this.acquisitionSettings );
    this.connectionSettings.addDeviceProfileChangeListener( this.triggerSettings );

    this.contentPane = new JTabbedPane( SwingConstants.TOP, JTabbedPane.SCROLL_TAB_LAYOUT );
    this.contentPane.addTab( "Connection", centerPanel( this.connectionSettings ) );
    this.contentPane.addTab( "Acquisition", centerPanel( this.acquisitionSettings ) );
    this.contentPane.addTab( "Triggers", centerPanel( this.triggerSettings ) );
  }

  /**
   * Fires an update event to all interested listeners.
   */
  private void updateFields()
  {
    firePropertyChange( "fields", -1L, System.currentTimeMillis() );
  }
}
