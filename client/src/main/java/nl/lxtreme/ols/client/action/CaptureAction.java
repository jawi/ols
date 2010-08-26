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

import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.client.Host.MainFrame;
import nl.lxtreme.ols.util.swing.*;


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

  private final MainFrame mainFrame;

  // CONSTRUCTORS

  /**
   * Creates a new CaptureAction instance.
   * 
   * @param aFrame
   *          the frame this action belongs to.
   */
  public CaptureAction( final MainFrame aFrame )
  {
    this( ID, ICON_CAPTURE_DATA, "Capture", "Starts capturing data from the logic analyser", aFrame );
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
   * @param aFrame
   *          the frame this action belongs to.
   */
  protected CaptureAction( final String aID, final String aIconName, final String aName, final String aDescription,
      final MainFrame aFrame )
  {
    super( aID, aIconName, aName, aDescription );
    this.mainFrame = aFrame;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    final DeviceController devCtrl = this.mainFrame.getCurrentDeviceController();
    if ( devCtrl == null )
    {
      JOptionPane.showMessageDialog( ( Component )aEvent.getSource(), "No capturing device found!", "Capture error",
          JOptionPane.ERROR_MESSAGE );
      return;
    }

    try
    {
      captureData( owner, devCtrl );
    }
    catch ( IOException exception )
    {
      this.mainFrame.captureAborted( "I/O problem: " + exception.getMessage() );
      exception.printStackTrace();
    }
  }

  /**
   * Does the actual capturing of the data from the given device controller.
   * 
   * @param aOwner
   *          the owning window;
   * @param aController
   *          the device controller to use for capturing the data, cannot be
   *          <code>null</code>;
   * @param aCallback
   *          the callback to use for the capturing process, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  protected void doCaptureData( final Window aOwner, final DeviceController aController, final CaptureCallback aCallback )
      throws IOException
  {
    if ( aController.setupCapture( aOwner ) )
    {
      aController.captureData( aCallback );
    }
  }

  /**
   * Captures the data from the given device controller.
   * 
   * @param aOwner
   *          the owning window;
   * @param aController
   *          the device controller to use for capturing the data, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  private void captureData( final Window aOwner, final DeviceController aController ) throws IOException
  {
    doCaptureData( aOwner, aController, this.mainFrame );
  }
}

/* EOF */
