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
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.client.*;


/**
 * Provides a "capture" action in which the current device controller is asked
 * to start a data capture.
 */
public class CaptureAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "Capture";

  // VARIABLES

  private final Host host;
  private final Project project;

  // CONSTRUCTORS

  /**
   * Creates a new CaptureAction instance.
   * 
   * @param aHost
   *          the host this action belongs to;
   * @param aPropertyManager
   *          the property manager to use.
   */
  public CaptureAction( final Host aHost, final Project aProject )
  {
    this( ID, ICON_CAPTURE_DATA, "Capture", "Starts capturing data from the logic analyser", aHost, aProject );
  }

  /**
   * Creates a new CaptureAction instance.
   * 
   * @param aID
   *          the ID of this action;
   * @param aIconName
   *          the name of the icon to use for this action;
   * @param aName
   *          the name of this action;
   * @param aDescription
   *          the description (tooltip) to use for this action;
   * @param aHost
   *          the host this action belongs to;
   * @param aPropertyManager
   *          the property manager to use.
   */
  protected CaptureAction( final String aID, final String aIconName, final String aName, final String aDescription,
      final Host aHost, final Project aProject )
  {
    super( aID, aIconName, aName, aDescription );
    this.host = aHost;
    this.project = aProject;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    final DeviceController devCtrl = this.host.getCurrentDeviceController();
    if ( devCtrl == null )
    {
      JOptionPane.showMessageDialog( ( Component )aEvent.getSource(), "No capturing device found!", "Capture error",
          JOptionPane.ERROR_MESSAGE );
      return;
    }

    try
    {
      captureData( devCtrl );
    }
    catch ( IOException exception )
    {
      this.host.captureAborted( "I/O problem: " + exception.getMessage() );
      exception.printStackTrace();
    }
  }

  /**
   * Does the actual capturing of the data from the given device controller.
   * 
   * @param aController
   *          the device controller to use for capturing the data, cannot be
   *          <code>null</code>;
   * @param aCallback
   *          the callback to use for the capturing process, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void doCaptureData( final DeviceController aController, final CaptureCallback aCallback )
  throws IOException
  {
    if ( aController.setupCapture() )
    {
      aController.captureData( aCallback );
    }
  }

  /**
   * Captures the data from the given device controller.
   * 
   * @param aController
   *          the device controller to use for capturing the data, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  private void captureData( final DeviceController aController ) throws IOException
  {
    // Read back the properties of the device...
    this.project.addConfigurable( aController );

    doCaptureData( aController, this.host );
  }
}

/* EOF */
