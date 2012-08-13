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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package org.sump.device.logicsniffer;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import org.sump.device.logicsniffer.LogicSnifferConfigDialog.DeviceProfileTypeComboBoxRenderer;
import org.sump.device.logicsniffer.profile.*;


/**
 * 
 */
public abstract class LogicSnifferDeviceProfilePanel implements Configurable
{
  // INNER TYPES

  /**
   * Provides an action to toggle between auto-detection of the device type and
   * manual override.
   */
  final class AutoDetectDeviceTypeAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new LogicSnifferDeviceProfilePanel.AutoDetectDeviceTypeAction
     * instance.
     */
    public AutoDetectDeviceTypeAction()
    {
      super( "Auto detect device type?" );
      putValue( Action.LONG_DESCRIPTION, "When checked the device type will be autodetected." );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final JCheckBox source = ( JCheckBox )aEvent.getSource();
      // Update the component states...
      setAutoDetectDeviceType( source.isSelected() );
    }
  }

  /**
   * Provides an action to invoke the detect device type.
   */
  final class DetectDeviceTypeAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new LogicSnifferDeviceProfilePanel.DetectDeviceTypeAction
     * instance.
     */
    public DetectDeviceTypeAction()
    {
      super( "Detect ..." );
      putValue( Action.LONG_DESCRIPTION,
          "Tries to detect the current device type using the values entered in this dialog." );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final Component component = ( Component )aEvent.getSource();

      component.setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );
      setEnabled( false );

      try
      {
        detectDeviceType();
      }
      finally
      {
        component.setCursor( Cursor.getDefaultCursor() );
        setEnabled( true );
      }
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

    private final DeviceProfileManager deviceProfileManager;
    private volatile Object selected = null;

    /**
     * Creates a new {@link DeviceProfileTypeComboBoxModel} instance.
     */
    public DeviceProfileTypeComboBoxModel( final DeviceProfileManager aDeviceProfileManager )
    {
      this.deviceProfileManager = aDeviceProfileManager;
    }

    // METHODS

    /**
     * Finds the element with a given type.
     * 
     * @param aType
     *          the identifying type of the element to find.
     * @return the element with the given identifying type, or <code>null</code>
     *         if no such type could be found.
     */
    public Object findElementByType( final String aType )
    {
      return this.deviceProfileManager.getProfile( aType );
    }

    /**
     * @see javax.swing.ListModel#getElementAt(int)
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      final List<DeviceProfile> profiles = this.deviceProfileManager.getProfiles();
      if ( profiles.isEmpty() )
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
      return this.selected;
    }

    /**
     * @see javax.swing.ListModel#getSize()
     */
    @Override
    public int getSize()
    {
      return this.deviceProfileManager.getSize();
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

  // VARIABLES

  private JButton detectDeviceButton;
  private JComboBox deviceTypeSelect;
  private JCheckBox autoDetectDeviceType;
  private JEditorPane deviceTypeDetails;

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( LogicSnifferDeviceProfilePanel.class.getName() );

  private static final String DETAILS_TMPL = "<html><style>body { font-family: sans-serif; font-size: 9px; margin-left: 8px; } th { text-align: right; }</style><body><table>"
      + "<tr><th colspan=2>%s</th></tr>"
      + "<tr><th>%s</th><td>%s</td></tr>"
      + "<tr><th>%s</th><td>%s</td></tr>"
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "</table></body></html>";

  private static final String ERROR_TMPL = "<html><style>body { font-family: sans-serif; font-size: 9px; margin-left: 8px; } th { text-align: right; }</style><body><table>"
      + "<tr><th>Detection failed!</th></tr><tr><td>%s</td></tr></table></body></html>";

  // VARIABLES

  private final LogicSnifferDevice device;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDeviceProfilePanel instance.
   */
  public LogicSnifferDeviceProfilePanel( final LogicSnifferDevice aDevice )
  {
    super();

    this.device = aDevice;

    initPanel();
  }

  // METHODS

  /**
   * Builds this panel.
   */
  public void buildPanel( final Container aContainer )
  {
    aContainer.add( createRightAlignedLabel( "Device type" ) );
    aContainer.add( this.autoDetectDeviceType );

    aContainer.add( new JLabel( "" ) );
    aContainer.add( this.detectDeviceButton );

    aContainer.add( new JLabel( "" ) );
    aContainer.add( this.deviceTypeSelect );

    aContainer.add( new JLabel( "" ) );
    aContainer.add( this.deviceTypeDetails );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void readPreferences( final UserSettings aSettings )
  {
    final String preferredDeviceType = aSettings.get( "deviceType", null );
    if ( ( preferredDeviceType != null ) && !"null".equals( preferredDeviceType ) )
    {
      final Object element = ( ( DeviceProfileTypeComboBoxModel )this.deviceTypeSelect.getModel() )
          .findElementByType( preferredDeviceType );
      if ( element != null )
      {
        this.deviceTypeSelect.setSelectedItem( element );
      }
    }
  }

  /**
   * @param aAutoDetect
   */
  public void setAutoDetectDeviceType( final boolean aAutoDetect )
  {
    this.detectDeviceButton.setEnabled( aAutoDetect );
    this.deviceTypeSelect.setEnabled( !aAutoDetect );

    // Empty the details as to indicate this switch...
    this.deviceTypeDetails.setText( getEmtpyMetadataDetails() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void writePreferences( final UserSettings aSettings )
  {
    aSettings.put( "deviceType", String.valueOf( this.deviceTypeSelect.getSelectedItem() ) );
  }

  /**
   * Detects the device type.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  final void detectDeviceType()
  {
    // Make sure we don't allow this method to be called concurrently!
    this.detectDeviceButton.setEnabled( false );

    LogicSnifferDetectionTask detectTask = new LogicSnifferDetectionTask( this.device, getConnectionURI() );

    try
    {
      LogicSnifferMetadata metadata = null;
      String details = getEmtpyMetadataDetails();

      try
      {
        metadata = detectTask.call();
      }
      catch ( IOException exception )
      {
        LOG.log( Level.INFO, "Failed to detect device!", exception );
        details = getErrorMetadataDetails( exception );
      }

      if ( metadata != null )
      {
        final DeviceProfile deviceProfile = metadata.getDeviceProfile();
        if ( deviceProfile != null )
        {
          // Update the selection of the combobox directly; causing everything
          // to be synchronized nicely...
          this.deviceTypeSelect.setSelectedItem( deviceProfile );
          // Ensure it is updated immediately...
          this.deviceTypeSelect.repaint();
        }

        details = getMetadataDetailsAsText( metadata );
      }

      this.deviceTypeDetails.setText( details );
    }
    finally
    {
      // Restore the state of this button...
      this.detectDeviceButton.setEnabled( true );
    }
  }

  /**
   * @return
   */
  protected abstract String getConnectionURI();

  /**
   * @param aProfile
   */
  protected abstract void updateDeviceProfile( final DeviceProfile aProfile );

  /**
   * @param aMetadata
   * @return
   */
  private String getEmtpyMetadataDetails()
  {
    return String.format( DETAILS_TMPL, "&#160;", "&#160;", "&#160;", "&#160;", "&#160;", "&#160;", "&#160;" );
  }

  /**
   * @param aMetadata
   * @return
   */
  private String getErrorMetadataDetails( final IOException exception )
  {
    return String.format( ERROR_TMPL, exception.getMessage() );
  }

  /**
   * @param aMetadata
   * @return
   */
  private String getMetadataDetailsAsText( final LogicSnifferMetadata aMetadata )
  {
    String header1 = "&#160;", text1 = "";
    String header2 = "&#160;", text2 = "";
    String header3 = "&#160;", text3 = "";

    Object version;
    if ( ( version = aMetadata.getFpgaVersion() ) != null )
    {
      header1 = "Firmware version";
      text1 = String.valueOf( version );
    }
    if ( ( version = aMetadata.getProtocolVersion() ) != null )
    {
      header2 = "Protocol version";
      text2 = String.valueOf( version );
    }
    if ( ( version = aMetadata.getAncillaryVersion() ) != null )
    {
      header3 = "Ancillary version";
      text3 = String.valueOf( version );
    }

    return String.format( DETAILS_TMPL, "Firmware details", header1, text1, header2, text2, header3, text3 );
  }

  /**
   * Initializes the components of this panel.
   */
  private void initPanel()
  {
    this.detectDeviceButton = new JButton( new DetectDeviceTypeAction() );

    this.autoDetectDeviceType = new JCheckBox( new AutoDetectDeviceTypeAction() );

    this.deviceTypeDetails = new JEditorPane( "text/html", getEmtpyMetadataDetails() );
    this.deviceTypeDetails.setEditable( false );
    this.deviceTypeDetails.setEnabled( false );
    this.deviceTypeDetails.setOpaque( false );

    // NOTE: create this component as last component, as it will fire an event
    // that uses all other components!!!
    this.deviceTypeSelect = new JComboBox( new DeviceProfileTypeComboBoxModel( this.device.getDeviceProfileManager() ) );

    // Don't auto-detect at first...
    setAutoDetectDeviceType( false );

    this.deviceTypeSelect.setRenderer( new DeviceProfileTypeComboBoxRenderer() );
    this.deviceTypeSelect.addItemListener( new ItemListener()
    {
      /**
       * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
       */
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        final DeviceProfile profile = ( DeviceProfile )combobox.getSelectedItem();
        if ( profile != null )
        {
          updateDeviceProfile( profile );
        }
      }
    } );
    // By default, select the "OLS" device, if available...
    this.deviceTypeSelect.setSelectedItem( this.device.getDefaultProfile() );
  }

}
