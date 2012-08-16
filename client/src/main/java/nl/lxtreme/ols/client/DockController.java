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
package nl.lxtreme.ols.client;


import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;

import org.noos.xing.mydoggy.*;
import org.noos.xing.mydoggy.plaf.*;


/**
 * Provides a simple controller for the dock windows.
 */
public class DockController implements IMeasurementListener
{
  // CONSTANTS

  public static final String TW_ACQUISITION = AcquisitionDetailsView.ID;
  public static final String TW_MEASURE = MeasurementView.ID;
  public static final String TW_CURSORS = CursorDetailsView.ID;

  public static final String GROUP_DEFAULT = "Default";

  // VARIABLES

  private final File dataStorage;
  private final AtomicReference<MyDoggyToolWindowManager> windowManagerRef;

  private volatile boolean wasHidden = false;

  private AcquisitionDetailsView captureDetails;
  private CursorDetailsView cursorDetails;
  private MeasurementView measurementDetails;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DockController} instance.
   */
  public DockController( final File aDataStorage, final SignalDiagramController aSignalDiagramController )
  {
    this.dataStorage = aDataStorage;

    this.windowManagerRef = new AtomicReference<MyDoggyToolWindowManager>();

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        DockController.this.captureDetails = AcquisitionDetailsView.create( aSignalDiagramController );
        DockController.this.cursorDetails = CursorDetailsView.create( aSignalDiagramController );
        DockController.this.measurementDetails = MeasurementView.create( aSignalDiagramController );

        final MyDoggyToolWindowManager wm = new MyDoggyToolWindowManager( Locale.getDefault(),
            MyDoggyToolWindowManager.class.getClassLoader() );
        wm.setDockableMainContentMode( false );

        // First one wins...
        DockController.this.windowManagerRef.compareAndSet( null, wm );
      }
    } );
  }

  // METHODS

  /**
   * @param aWindow
   */
  private static void tweakToolWindow( final ToolWindow aWindow )
  {
    RepresentativeAnchorDescriptor<?> anchorDesc = aWindow.getRepresentativeAnchorDescriptor();
    anchorDesc.setTitle( aWindow.getTitle() );
    anchorDesc.setPreviewEnabled( false );
    if ( aWindow.getIcon() != null )
    {
      anchorDesc.setIcon( aWindow.getIcon() );
    }

    final ToolWindowType[] types = ToolWindowType.values();
    for ( ToolWindowType type : types )
    {
      ToolWindowTypeDescriptor desc = aWindow.getTypeDescriptor( type );
      desc.setAnimating( false );
      desc.setAutoHide( false );
      desc.setEnabled( true );
      desc.setHideRepresentativeButtonOnVisible( false );
      desc.setIdVisibleOnTitleBar( false );
      desc.setTitleBarButtonsVisible( false );
      desc.setTitleBarVisible( false );
    }

    DockedTypeDescriptor desc = ( DockedTypeDescriptor )aWindow.getTypeDescriptor( ToolWindowType.DOCKED );
    desc.setPopupMenuEnabled( false );

    aWindow.setAvailable( true );
    aWindow.setHideOnZeroTabs( false );
    aWindow.setVisible( UIManager.getBoolean( UIManagerKeys.SHOW_TOOL_WINDOWS_DEFAULT ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void disableMeasurementMode()
  {
    ToolWindow toolWindow = getManager().getToolWindow( TW_MEASURE );
    toolWindow.setVisible( this.wasHidden );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void enableMeasurementMode()
  {
    ToolWindow toolWindow = getManager().getToolWindow( TW_MEASURE );
    this.wasHidden = toolWindow.isVisible();

    toolWindow.setVisible( true );
  }

  /**
   * @return
   */
  public Component get()
  {
    return getManager();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleMeasureEvent( final MeasurementInfo aEvent )
  {
    // nop
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isListening()
  {
    return true;
  }

  /**
   * @param aToolWindow
   * @param aGroupName
   */
  public void registerToolWindow( final IToolWindow aToolWindow, final String aGroupName )
  {
    MyDoggyToolWindowManager wm = getManager();

    ToolWindow tw = wm.registerToolWindow( aToolWindow.getId(), aToolWindow.getName(), aToolWindow.getIcon(),
        ( Component )aToolWindow, ToolWindowAnchor.RIGHT );

    final ToolWindowGroup group = wm.getToolWindowGroup( aGroupName );
    group.setImplicit( false );
    group.addToolWindow( tw );

    tweakToolWindow( tw );
  }

  /**
   * @param aComponent
   *          the main content to set, should not be <code>null</code>.
   */
  public void setMainContent( final Component aComponent )
  {
    getManager().setMainContent( aComponent );
  }

  /**
   * @return the current tool window manager, cannot be <code>null</code>.
   */
  final MyDoggyToolWindowManager getManager()
  {
    return this.windowManagerRef.get();
  }

  /**
   * Starts this dock controller.
   */
  void start()
  {
    registerToolWindow( this.cursorDetails, DockController.GROUP_DEFAULT );
    registerToolWindow( this.captureDetails, DockController.GROUP_DEFAULT );
    registerToolWindow( this.measurementDetails, DockController.GROUP_DEFAULT );

    File dataFile = new File( this.dataStorage, "dock.settings" );
    if ( ( this.dataStorage == null ) || !dataFile.exists() )
    {
      // Don't do anything when there's no file...
      return;
    }

    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( dataFile );
      getManager().getPersistenceDelegate().apply( fis );
    }
    catch ( FileNotFoundException exception )
    {
      // Ignore; we shouldn't be here anyways due to dataFile.exists...
    }
    finally
    {
      HostUtils.closeResource( fis );
    }
  }

  /**
   * Closes this dock controller.
   */
  void stop()
  {
    if ( this.dataStorage == null )
    {
      // Don't do anything when there's no file...
      return;
    }

    File dataFile = new File( this.dataStorage, "dock.settings" );
    FileOutputStream fos = null;

    try
    {
      fos = new FileOutputStream( dataFile );
      getManager().getPersistenceDelegate().save( fos );
    }
    catch ( FileNotFoundException exception )
    {
      // Ignore; we shouldn't be here anyways due to dataFile.exists...
    }
    finally
    {
      HostUtils.closeResource( fos );
    }
  }
}
