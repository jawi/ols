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
package nl.lxtreme.ols.device.logicsniffer.ui;


import static nl.lxtreme.ols.device.logicsniffer.ui.OcdHelper.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static org.sump.device.logicsniffer.ConfigDialogHelper.*;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.basic.*;

import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.device.logicsniffer.*;
import nl.lxtreme.ols.device.logicsniffer.profile.*;
import nl.lxtreme.ols.device.logicsniffer.profile.DeviceProfile.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.editor.*;

import org.osgi.service.metatype.*;

import purejavacomm.*;


/**
 * Provides a custom panel for editing the connection settings.
 */
public class ConnectionSettingsPanel extends JPanel implements PropertyChangeListener
{
  // INNER TYPES

  /**
   * Renders a device interface.
   */
  static final class DeviceInterfaceComboBoxRenderer extends EnumItemRenderer<DeviceInterface>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDisplayValue( final DeviceInterface aValue )
    {
      switch ( aValue )
      {
        case NETWORK:
          return "Network";
        case SERIAL:
          return "Serial port";
        case USB:
          return "USB";
      }
      return super.getDisplayValue( aValue );
    }
  }

  /**
   * Provides a combobox model for device profile types.
   */
  final class DeviceProfileTypeComboBoxModel extends AbstractListModel implements ComboBoxModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final DeviceProfileProvider provider;
    private volatile Object selected = null;

    /**
     * Creates a new {@link DeviceProfileTypeComboBoxModel} instance.
     */
    public DeviceProfileTypeComboBoxModel( final DeviceProfileProvider aProvider )
    {
      this.provider = aProvider;
    }

    // METHODS

    /**
     * @see javax.swing.ListModel#getElementAt(int)
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      final List<DeviceProfile> profiles = this.provider.getDeviceProfiles();
      if ( ( profiles == null ) || profiles.isEmpty() )
      {
        return null;
      }
      return profiles.get( aIndex );
    }

    /**
     * @see javax.swing.ComboBoxModel#getSelectedItem()
     */
    @Override
    public Object getSelectedItem()
    {
      if ( this.selected == null )
      {
        this.selected = this.provider.getDefaultProfile();
      }
      return this.selected;
    }

    /**
     * @see javax.swing.ListModel#getSize()
     */
    @Override
    public int getSize()
    {
      List<DeviceProfile> profiles = this.provider.getDeviceProfiles();
      return ( profiles == null ) ? 0 : profiles.size();
    }

    /**
     * @see javax.swing.ComboBoxModel#setSelectedItem(java.lang.Object)
     */
    @Override
    public void setSelectedItem( final Object aItem )
    {
      this.selected = aItem;
    }
  }

  /**
   * Provides a combobox renderer for device profiles.
   */
  static final class DeviceProfileTypeComboBoxRenderer extends BasicComboBoxRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
        final boolean aIsSelected, final boolean aCellHasFocus )
    {
      Object value = aValue;
      if ( value instanceof DeviceProfile )
      {
        final DeviceProfile profile = ( DeviceProfile )value;
        value = profile.getDescription();
        if ( ( value == null ) || ( String.valueOf( value ).isEmpty() ) )
        {
          value = profile.getType();
        }
      }
      return super.getListCellRendererComponent( aList, value, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  /**
   * Provides an action to invoke the detect device type.
   */
  final class ReadDeviceMetadataAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ReadDeviceMetadataAction} instance.
     */
    public ReadDeviceMetadataAction()
    {
      super( "Read device metadata" );
      putValue( Action.LONG_DESCRIPTION, "Returns the results of the 'metadata' command." );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      setEnabled( false );

      try
      {
        obtainDeviceMetadata();
      }
      finally
      {
        setEnabled( true );
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String DETAILS_TMPL = "<html><style>body { font-family: sans-serif; font-size: 9pt; margin-left: 16px; } th { text-align: right; }</style><body><table>"
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "</table></body></html>";

  private static final String ERROR_TMPL = "<html><style>body { font-family: sans-serif; font-size: 9pt; margin-left: 8px; } th { text-align: right; }</style><body><table>"
      + "<tr><th>Detection failed!</th></tr><tr><td>%s</td></tr><tr><td>&#160;</td></tr><tr><td>&#160;</td></tr></table></body></html>";

  // VARIABLES

  private JComboBox deviceInterface;
  private JTextField remoteHost;
  private JTextField remotePort;
  private JComboBox port;
  private JComboBox portSpeed;

  private JButton readDeviceMetadata;
  private JComboBox deviceType;
  private JLabel deviceTypeDetails;

  private final EventListenerList eventListeners;
  private final List<JComponent> components;
  private final DeviceMetadataProvider metadataProvider;
  private final DeviceProfileProvider profileProvider;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ConnectionSettingsPanel} instance.
   */
  public ConnectionSettingsPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues,
      final DeviceMetadataProvider aMetadataProvider, final DeviceProfileProvider aProfileProvider )
  {
    super( new SpringLayout() );

    this.eventListeners = new EventListenerList();
    this.metadataProvider = aMetadataProvider;
    this.profileProvider = aProfileProvider;
    this.components = new ArrayList<JComponent>();

    initPanel( aOCD, aInitialValues );
    buildPanel();
  }

  // METHODS

  /**
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addDeviceProfileChangeListener( final DeviceProfileChangedListener aListener )
  {
    this.eventListeners.add( DeviceProfileChangedListener.class, aListener );
  }

  /**
   * @param aConfiguration
   *          the configuration map to fill with the configuration settings of
   *          this panel, never <code>null</code>.
   * @return the given configuration, never <code>null</code>.
   */
  public Map<Object, Object> getConfiguration( final Map<Object, Object> aConfiguration )
  {
    aConfiguration.putAll( new EditorUtils().getComponentValues( this.components ) );
    return aConfiguration;
  }

  /**
   * Returns the connection URI based on the components of this panel.
   * 
   * @return a connection URI, never <code>null</code>.
   */
  public String getConnectionURI()
  {
    String result;

    if ( isNetworkConnection() )
    {
      final String address = this.remoteHost.getText();
      final Integer port = getNumericValue( this.remotePort );

      result = String.format( "socket://%s:%d", address, port );
    }
    else if ( isSerialConnection() )
    {
      final String portName = getComboBoxText( this.port );
      final Integer baudrate = getNumericValue( this.portSpeed );

      result = String.format( "comm:%s;baudrate=%d;bitsperchar=8;parity=none;stopbits=1;flowcontrol=off", portName,
          baudrate );
    }
    else
    {
      throw new IllegalStateException( "Unknown/unsupported device interface: "
          + this.deviceInterface.getSelectedItem() );
    }

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    if ( "fields".equals( aEvent.getPropertyName() ) )
    {
      fireDeviceProfileChangedEvent();
    }
  }

  /**
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeDeviceProfileChangeListener( final DeviceProfileChangedListener aListener )
  {
    this.eventListeners.remove( DeviceProfileChangedListener.class, aListener );
  }

  /**
   * Fires a device profile changed event to all interested listeners.
   */
  final void fireDeviceProfileChangedEvent()
  {
    DeviceProfile selectedDeviceProfile = getSelectedDeviceProfile();

    DeviceProfileChangedListener[] listeners = this.eventListeners.getListeners( DeviceProfileChangedListener.class );
    for ( DeviceProfileChangedListener listener : listeners )
    {
      listener.deviceProfileChanged( selectedDeviceProfile );
    }
  }

  /**
   * Tries to determine the device metadata.
   */
  final void obtainDeviceMetadata()
  {
    SwingWorker<DeviceMetadata, Void> worker = new SwingWorker<DeviceMetadata, Void>()
    {
      private final DeviceMetadataProvider metadataProvider = ConnectionSettingsPanel.this.metadataProvider;
      private final JButton readDeviceMetadata = ConnectionSettingsPanel.this.readDeviceMetadata;

      @Override
      protected DeviceMetadata doInBackground() throws Exception
      {
        this.readDeviceMetadata.setEnabled( false );

        return this.metadataProvider.readMetadata( getConnectionURI() );
      }

      @Override
      protected void done()
      {
        String text;

        try
        {
          text = getMetadataDetailsAsText( get() );
        }
        catch ( InterruptedException exception )
        {
          text = getErrorAsText( exception );
        }
        catch ( ExecutionException exception )
        {
          text = getErrorAsText( exception.getCause() );
        }

        setDeviceMetadataText( text );

        this.readDeviceMetadata.setEnabled( true );
      }
    };

    worker.execute();
  }

  /**
   * @param aText
   */
  final void setDeviceMetadataText( final String aText )
  {
    this.deviceTypeDetails.setText( aText );
  }

  /**
   * Enabled/disables the read metadata button according to the current
   * settings.
   */
  final void enableReadMetadataState()
  {
    boolean enabled;
    if ( isNetworkConnection() )
    {
      String addr = this.remoteHost.getText();
      String port = this.remotePort.getText();
      enabled = ( addr != null ) && !"".equals( addr.trim() ) && ( port != null ) && !"".equals( port.trim() );
    }
    else
    {
      Object value = this.port.getSelectedItem();
      enabled = ( value != null ) && !"".equals( String.valueOf( value ).trim() );
    }
    this.readDeviceMetadata.setEnabled( enabled );
  }

  /**
   * Builds this dialog.
   */
  private void buildPanel()
  {
    SpringLayoutUtils.addSeparator( this, "General" );

    add( createRightAlignedLabel( "Connection type" ) );
    add( this.deviceInterface );

    SpringLayoutUtils.addSeparator( this, "" );

    add( createRightAlignedLabel( "Remote host address" ) );
    add( this.remoteHost );

    add( createRightAlignedLabel( "Remote port" ) );
    add( this.remotePort );

    SpringLayoutUtils.addSeparator( this, "" );

    add( createRightAlignedLabel( "Serial port" ) );
    add( this.port );

    add( createRightAlignedLabel( "Port speed" ) );
    add( this.portSpeed );

    SpringLayoutUtils.addSeparator( this, "Device" );

    add( SwingComponentUtils.createRightAlignedLabel( "Type" ) );
    add( this.deviceType );

    add( new JLabel( "" ) );
    add( this.readDeviceMetadata );

    add( new JLabel( "" ) );
    add( this.deviceTypeDetails );

    SpringLayoutUtils.makeEditorGrid( this, 10, 10 );
  }

  /**
   * @param aException
   * @return
   */
  private String getErrorAsText( final Throwable aException )
  {
    return String.format( ERROR_TMPL, aException.getMessage() );
  }

  /**
   * @param aMetadata
   * @return
   */
  private String getMetadataDetailsAsText( final DeviceMetadata aMetadata )
  {
    String header1 = "Device type", text1 = "-";
    String header2 = "   Firmware", text2 = "-";
    String header3 = "   Protocol", text3 = "-";
    String header4 = "  Ancillary", text4 = "-";

    if ( aMetadata != null )
    {
      Object value;
      if ( ( value = aMetadata.getName() ) != null )
      {
        text1 = String.valueOf( value );
      }
      if ( ( value = aMetadata.getFpgaVersion() ) != null )
      {
        text2 = String.valueOf( value );
      }
      if ( ( value = aMetadata.getProtocolVersion() ) != null )
      {
        text3 = String.valueOf( value );
      }
      if ( ( value = aMetadata.getAncillaryVersion() ) != null )
      {
        text4 = String.valueOf( value );
      }
    }

    return String.format( DETAILS_TMPL, header1, text1, header2, text2, header3, text3, header4, text4 );
  }

  /**
   * @return the current selected device profile, never <code>null</code>.
   */
  private DeviceProfile getSelectedDeviceProfile()
  {
    return ( DeviceProfile )this.deviceType.getSelectedItem();
  }

  /**
   * @param aConfig
   */
  private void initPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    EditorUtils editorUtils = new EditorUtils();

    ActionListener listener = new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        enableReadMetadataState();
      }
    };

    this.deviceInterface = createEditor( aOCD, "deviceInterface", aInitialValues );
    this.remoteHost = createEditor( aOCD, "remoteHost", aInitialValues );
    this.remoteHost.addActionListener( listener );
    this.remotePort = createEditor( aOCD, "remotePort", aInitialValues );
    this.remotePort.addActionListener( listener );

    AttributeDefinition ad = OcdHelper.getAttributeDefinition( aOCD, "localPort" );
    this.port = new JLazyComboBox( new JLazyComboBox.ItemProvider()
    {
      @Override
      @SuppressWarnings( "unchecked" )
      public Object[] getItems()
      {
        final Enumeration<CommPortIdentifier> portIdentifiers = CommPortIdentifier.getPortIdentifiers();
        final List<String> portList = new ArrayList<String>();

        while ( portIdentifiers.hasMoreElements() )
        {
          CommPortIdentifier portId = portIdentifiers.nextElement();
          if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
          {
            portList.add( portId.getName() );
          }
        }

        return portList.toArray( new String[portList.size()] );
      }
    } );
    // allow people to put their own port name into it...
    this.port.setSelectedItem( editorUtils.getDefaultValue( ad, aInitialValues.get( "localPort" ) ) );
    this.port.putClientProperty( EditorUtils.PROPERTY_ATTRIBUTE, ad );
    this.port.setEditable( true );
    this.port.addActionListener( listener );

    this.portSpeed = createEditor( aOCD, "portSpeed", aInitialValues );
    this.portSpeed.setEditable( true );
    this.portSpeed.addActionListener( listener );

    this.readDeviceMetadata = new JButton( new ReadDeviceMetadataAction() );
    this.deviceTypeDetails = new JLabel( getMetadataDetailsAsText( null ) );

    ad = OcdHelper.getAttributeDefinition( aOCD, "deviceProfile" );
    this.deviceType = new JComboBox( new DeviceProfileTypeComboBoxModel( this.profileProvider ) );
    this.deviceType.setRenderer( new DeviceProfileTypeComboBoxRenderer() );
    this.deviceType.setSelectedItem( editorUtils.getDefaultValue( ad, aInitialValues.get( "deviceProfile" ) ) );
    this.deviceType.putClientProperty( EditorUtils.PROPERTY_ATTRIBUTE, ad );
    this.deviceType.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        fireDeviceProfileChangedEvent();
      }
    } );

    // Make sure the initial values reflect to the component states...
    enableReadMetadataState();

    this.components.addAll( Arrays.<JComponent> asList( this.deviceInterface, this.remoteHost, this.remotePort,
        this.port, this.portSpeed, this.deviceType ) );

    editorUtils.applyComponentProperties( this.components );
  }

  /**
   * @return <code>true</code> if a network connection is selected,
   *         <code>false</code> otherwise.
   */
  private boolean isNetworkConnection()
  {
    return "NETWORK".equals( this.deviceInterface.getSelectedItem() );
  }

  /**
   * @return <code>true</code> if a serial connection is selected,
   *         <code>false</code> otherwise.
   */
  private boolean isSerialConnection()
  {
    return "SERIAL".equals( this.deviceInterface.getSelectedItem() );
  }
}
