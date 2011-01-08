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
package nl.lxtreme.ols.device.generic;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;

import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
public class GenericDeviceConfigDialog extends JDialog implements Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  /** the path to the device; /dev/ttyS0 or similar. */
  private String devicePath;
  /** the sample rate in Hertz (Hz). */
  private int sampleRate;
  /** the number of samples to take. */
  private int sampleDepth;
  /** the width (in bytes) of each sample. */
  private int sampleWidth;
  /** the number of channels in each sample. */
  private int channelCount;

  private boolean setupConfirmed;

  // CONSTRUCTORS

  /**
   * Creates a new GenericDeviceConfigDialog instance.
   * 
   * @param aParent
   *          the parent window of this dialog, can be <code>null</code>.
   */
  public GenericDeviceConfigDialog( final Window aParent )
  {
    super( aParent, "Generic capture settings", ModalityType.DOCUMENT_MODAL );

    initDialog();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  /**
   * @return the channelCount
   */
  public int getChannelCount()
  {
    return this.channelCount;
  }

  /**
   * @return the devicePath
   */
  public String getDevicePath()
  {
    return this.devicePath;
  }

  /**
   * @return the sampleDepth
   */
  public int getSampleDepth()
  {
    return this.sampleDepth;
  }

  /**
   * @return the sampleRate
   */
  public int getSampleRate()
  {
    return this.sampleRate;
  }

  /**
   * @return the sampleWidth
   */
  public int getSampleWidth()
  {
    return this.sampleWidth;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void readPreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
  }

  /**
   * Shows this dialog on screen.
   * 
   * @return <code>true</code> if this dialog is confirmed, <code>false</code>
   *         if it was cancelled.
   */
  public boolean showDialog()
  {
    setVisible( true );
    return this.setupConfirmed;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  @Override
  public void writePreferences( final Preferences aPreferences )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @return
   */
  private JComponent createContents()
  {
    return new JPanel();
  }

  /**
   * @return
   */
  private JComponent createDialogButtonPanel()
  {
    final JButton closeButton = StandardActionFactory.createCloseButton();

    final JButton okButton = new JButton( "Ok" );
    okButton.setPreferredSize( closeButton.getPreferredSize() );
    okButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        GenericDeviceConfigDialog.this.setupConfirmed = true;
        close();
      }
    } );

    return SwingComponentUtils.createButtonPane( new JButton[] { okButton, closeButton } );
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JComponent contents = createContents();
    final JComponent buttonPane = createDialogButtonPanel();

    SwingComponentUtils.setupDialogContentPane( this, contents, buttonPane );
  }
}
