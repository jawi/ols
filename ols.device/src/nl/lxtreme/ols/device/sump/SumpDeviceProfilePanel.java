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
package nl.lxtreme.ols.device.sump;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.List;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.plaf.basic.*;

import org.osgi.framework.*;

import nl.lxtreme.ols.device.sump.profile.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.Configurable;


/**
 * 
 */
public abstract class SumpDeviceProfilePanel implements Configurable
{
  // INNER TYPES

  /**
   * Provides a combobox model for device profile types.
   */
  final class DeviceProfileTypeComboBoxModel extends AbstractListModel implements ComboBoxModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final List<DeviceProfile> profiles;
    private volatile Object selected = null;

    /**
     * Creates a new {@link DeviceProfileTypeComboBoxModel} instance.
     */
    public DeviceProfileTypeComboBoxModel( List<DeviceProfile> aProfiles )
    {
      this.profiles = aProfiles;
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
      for ( DeviceProfile profile : this.profiles )
      {
        if ( aType.equals( profile.getType() ) )
        {
          return profile;
        }
      }
      return null;
    }

    /**
     * @see javax.swing.ListModel#getElementAt(int)
     */
    @Override
    public Object getElementAt( final int aIndex )
    {
      if ( this.profiles.isEmpty() )
      {
        return null;
      }
      return this.profiles.get( aIndex );
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
      return this.profiles.size();
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
  final class ShowDeviceMetadataAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ShowDeviceMetadataAction} instance.
     */
    public ShowDeviceMetadataAction()
    {
      super( "Show device metadata" );
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
      catch ( InvalidSyntaxException exception )
      {
        LOG.log( Level.WARNING, "Failed to show device metadata due to an invalid filter!", exception );
      }
      finally
      {
        setEnabled( true );
      }
    }
  }

  // VARIABLES

  private JButton showMetadataButton;
  private JComboBox deviceTypeSelect;
  private JEditorPane deviceTypeDetails;

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SumpDeviceProfilePanel.class.getName() );

  private static final String DETAILS_TMPL = "<html><style>body { font-family: sans-serif; font-size: 8px; margin-left: 8px; } th { text-align: right; }</style><body><table>"
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "<tr><th>%s</th><td>%s</td></tr>" //
      + "</table></body></html>";

  private static final String ERROR_TMPL = "<html><style>body { font-family: sans-serif; font-size: 8px; margin-left: 8px; } th { text-align: right; }</style><body><table>"
      + "<tr><th>Detection failed!</th></tr><tr><td>%s</td></tr><tr><td>&#160;</td></tr><tr><td>&#160;</td></tr></table></body></html>";

  // VARIABLES

  private final SumpDevice device;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDeviceProfilePanel instance.
   */
  public SumpDeviceProfilePanel( final SumpDevice aDevice )
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
    aContainer.add( SwingComponentUtils.createRightAlignedLabel( "Device type" ) );
    aContainer.add( this.deviceTypeSelect );

    aContainer.add( new JLabel( "" ) );
    aContainer.add( this.getShowMetadataButton() );

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
   * {@inheritDoc}
   */
  @Override
  public final void writePreferences( final UserSettings aSettings )
  {
    DeviceProfile selectedProfile = ( DeviceProfile )this.deviceTypeSelect.getSelectedItem();
    aSettings.put( "deviceType", selectedProfile.getType() );
  }

  /**
   * Detects the device type.
   * 
   * @throws InvalidSyntaxException
   *           in case of an invalid filter.
   */
  final void obtainDeviceMetadata() throws InvalidSyntaxException
  {
    SwingComponentUtils.getCurrentWindow().setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );

    SumpDetectionTask detectTask = new SumpDetectionTask();

    try
    {
      DeviceMetadata metadata = null;
      String details = getEmtpyMetadataDetails();

      try
      {
        metadata = detectTask.detect( this.device.createStreamConnection( getConnectionURI() ) );
      }
      catch ( IOException exception )
      {
        LOG.log( Level.INFO, "Failed to detect device!", exception );
        details = getErrorMetadataDetails( exception );
      }

      if ( metadata != null )
      {
        final DeviceProfile deviceProfile = getDeviceProfile( metadata );
        if ( deviceProfile == null )
        {
          LOG.info( "No device profile obtained from metadata!" );
        }
        // Update the selection of the combobox directly; causing everything
        // to be synchronized nicely...
        this.deviceTypeSelect.setSelectedItem( deviceProfile );
        // Ensure it is updated immediately...
        this.deviceTypeSelect.repaint();

        details = getMetadataDetailsAsText( metadata );
      }

      this.deviceTypeDetails.setText( details );
    }
    finally
    {
      SwingComponentUtils.getCurrentWindow().setCursor( null );
    }
  }

  /**
   * Determines the device profile for the current attached device. The device
   * profile provides us with detailed information about the capabilities of a
   * certain SUMP-compatible device.
   * 
   * @param aName
   *          the name of the device, can be <code>null</code>.
   * @return a device profile, or <code>null</code> if no such profile could be
   *         determined.
   * @throws InvalidSyntaxException
   *           in case the given metadata contains an invalid filter clause.
   */
  private DeviceProfile getDeviceProfile( DeviceMetadata aMetadata ) throws InvalidSyntaxException
  {
    DeviceProfile profile = null;

    if ( aMetadata != null )
    {
      profile = this.device.findDeviceProfile( aMetadata );

      if ( profile != null )
      {
        LOG.log( Level.INFO, "Using device profile: {0}", profile.getDescription() );
      }
      else
      {
        LOG.log( Level.SEVERE, "No device profile found matching: {0}", aMetadata.getName() );
      }
    }
    else
    {
      LOG.log( Level.SEVERE, "No metadata provided! Cannot determine device profile..." );
    }

    return profile;
  }

  /**
   * @return
   */
  protected abstract String getConnectionURI();

  /**
   * @param aProfile
   *          the device profile to update to, can be <code>null</code>.
   */
  protected abstract void updateDeviceProfile( final DeviceProfile aProfile );

  /**
   * @param aMetadata
   * @return
   */
  private String getEmtpyMetadataDetails()
  {
    return getMetadataDetailsAsText( new DeviceMetadata() );
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
  private String getMetadataDetailsAsText( final DeviceMetadata aMetadata )
  {
    String header1 = "Device type", text1 = "-";
    String header2 = "   Firmware", text2 = "-";
    String header3 = "   Protocol", text3 = "-";
    String header4 = "  Ancillary", text4 = "-";

    Object version;
    if ( ( version = aMetadata.getName() ) != null )
    {
      text1 = String.valueOf( version );
    }
    if ( ( version = aMetadata.getFpgaVersion() ) != null )
    {
      text2 = String.valueOf( version );
    }
    if ( ( version = aMetadata.getProtocolVersion() ) != null )
    {
      text3 = String.valueOf( version );
    }
    if ( ( version = aMetadata.getAncillaryVersion() ) != null )
    {
      text4 = String.valueOf( version );
    }

    return String.format( DETAILS_TMPL, header1, text1, header2, text2, header3, text3, header4, text4 );
  }

  /**
   * Initializes the components of this panel.
   */
  private void initPanel()
  {
    this.setShowMetadataButton( new JButton( new ShowDeviceMetadataAction() ) );

    this.deviceTypeDetails = new JEditorPane( "text/html", getEmtpyMetadataDetails() );
    this.deviceTypeDetails.setEditable( false );
    this.deviceTypeDetails.setEnabled( true );
    this.deviceTypeDetails.setOpaque( false );

    // NOTE: create this component as last component, as it will fire an event
    // that uses all other components!!!
    this.deviceTypeSelect = new JComboBox( new DeviceProfileTypeComboBoxModel( this.device.getDeviceProfiles() ) );

    this.deviceTypeSelect.setRenderer( new DeviceProfileTypeComboBoxRenderer() );
    this.deviceTypeSelect.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        final JComboBox combobox = ( JComboBox )aEvent.getSource();
        final DeviceProfile profile = ( DeviceProfile )combobox.getSelectedItem();
        updateDeviceProfile( profile );
      }
    } );

    DeviceProfile defaultProfile = this.device.getDefaultProfile();
    if ( defaultProfile != null )
    {
      // By default, select the "OLS" device, if available...
      this.deviceTypeSelect.setSelectedItem( defaultProfile );
    }
  }

  /**
   * Returns the current value of showMetadataButton.
   * @return the showMetadataButton
   */
  public JButton getShowMetadataButton()
  {
    return showMetadataButton;
  }

  /**
   * Sets showMetadataButton to the given value.
   * @param aShowMetadataButton the showMetadataButton to set.
   */
  public void setShowMetadataButton( JButton aShowMetadataButton )
  {
    this.showMetadataButton = aShowMetadataButton;
  }
}
