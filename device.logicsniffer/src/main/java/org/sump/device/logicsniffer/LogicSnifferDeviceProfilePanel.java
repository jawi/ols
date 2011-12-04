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

import javax.microedition.io.*;
import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.component.*;

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
      catch ( IOException exception )
      {
        JErrorDialog.showDialog( getCurrentWindow(), "Device detect failed!", exception );
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

    private volatile Object selected = null;

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
      return getDeviceProfileManager().getProfile( aType );
    }

    /**
     * @see javax.swing.ListModel#getElementAt(int)
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      return getDeviceProfileManager().getProfiles().get( aIndex );
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
      return getDeviceProfileManager().getSize();
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

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDeviceProfilePanel instance.
   */
  public LogicSnifferDeviceProfilePanel()
  {
    super();

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
  final void detectDeviceType() throws IOException
  {
    // Make sure we don't allow this method to be called concurrently!
    this.detectDeviceButton.setEnabled( false );

    LogicSnifferDetectionTask detectTask = null;

    try
    {
      detectTask = new LogicSnifferDetectionTask( getDeviceProfileManager(), getConnection() );

      final LogicSnifferMetadata metadata = detectTask.call();

      if ( metadata != null )
      {
        final DeviceProfile deviceProfile = metadata.getDeviceProfile();
        if ( deviceProfile != null )
        {
          // Update the selection of the combobox directly; causing everything
          // to
          // be synchronized nicely...
          this.deviceTypeSelect.setSelectedItem( deviceProfile );
          // Ensure it is updated immediately...
          this.deviceTypeSelect.repaint();
        }

        String details = getMetadataDetailsAsText( metadata );
        this.deviceTypeDetails.setText( details );
      }
    }
    finally
    {
      HostUtils.closeResource( detectTask );
      // Restore the state of this button...
      this.detectDeviceButton.setEnabled( true );
    }
  }

  /**
   * @return
   */
  protected abstract StreamConnection getConnection() throws IOException;

  /**
   * @return
   */
  protected abstract DeviceProfileManager getDeviceProfileManager();

  /**
   * @param aProfile
   */
  protected abstract void updateDeviceProfile( DeviceProfile aProfile );

  /**
   * @param aMetadata
   * @return
   */
  private String getEmtpyMetadataDetails()
  {
    StringBuilder details = new StringBuilder();
    details
        .append( "<html><style>body { font-family: sans-serif; font-size: 9px; margin-left: 8px; }</style><body><table>" );

    details.append( "<tr><th>&#160;</th><td></td></tr>" );
    details.append( "<tr><th>&#160;</th><td></td></tr>" );
    details.append( "<tr><th>&#160;</th><td></td></tr>" );
    details.append( "</table></body></html>" );
    return details.toString();
  }

  /**
   * @param aMetadata
   * @return
   */
  private String getMetadataDetailsAsText( final LogicSnifferMetadata aMetadata )
  {
    StringBuilder details = new StringBuilder();
    details
        .append( "<html><style>body { font-family: sans-serif; font-size: 9px; margin-left: 8px; }</style><body><table>" );

    Object version = aMetadata.getFpgaVersion();
    if ( version != null )
    {
      details.append( "<tr><th>Firmware version</th><td>" ).append( version ).append( "</td></tr>" );
    }
    else
    {
      details.append( "<tr><th>&#160;</th><td></td></tr>" );
    }
    version = aMetadata.getProtocolVersion();
    if ( version != null )
    {
      details.append( "<tr><th>Protocol version</th><td>" ).append( version ).append( "</td></tr>" );
    }
    else
    {
      details.append( "<tr><th>&#160;</th><td></td></tr>" );
    }
    version = aMetadata.getAncillaryVersion();
    if ( version != null )
    {
      details.append( "<tr><th>Ancillary version</th><td>" ).append( version ).append( "</td></tr>" );
    }
    else
    {
      details.append( "<tr><th>&#160;</th><td></td></tr>" );
    }
    details.append( "</table></body></html>" );
    return details.toString();
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
    this.deviceTypeSelect = new JComboBox( new DeviceProfileTypeComboBoxModel() );

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
    this.deviceTypeSelect.setSelectedItem( getDeviceProfileManager().getDefaultProfile() );
  }

}
