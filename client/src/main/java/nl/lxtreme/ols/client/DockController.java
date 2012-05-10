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
import java.util.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.swing.*;

import org.noos.xing.mydoggy.*;
import org.noos.xing.mydoggy.plaf.*;
import org.noos.xing.mydoggy.plaf.ui.content.*;


/**
 * 
 */
public class DockController
{
  // INNER TYPES

  // CONSTANTS

  public static final String TW_ACQUISITION = AcquisitionDetailsView.ID;
  public static final String TW_MEASURE = MeasurementView.ID;
  public static final String TW_CURSORS = CursorDetailsView.ID;

  public static final String GROUP_SIGNAL = "Signal";
  public static final String GROUP_MEASURE = "Measure";

  // VARIABLES

  private final MyDoggyToolWindowManager windowManager;

  /**
   * Creates a new {@link DockController} instance.
   */
  public DockController()
  {
    final MyDoggyToolWindowManager wm = new MyDoggyToolWindowManager( Locale.getDefault(),
        MyDoggyToolWindowManager.class.getClassLoader() );

    final ContentManager contentManager = wm.getContentManager();
    contentManager.setContentManagerUI( new MyDoggyMultiSplitContentManagerUI() );

    this.windowManager = wm;
  }

  // METHODS

  /**
   * @param aWindow
   */
  private static void tweakToolWindow( final ToolWindow aWindow, final int aDockLength )
  {
    // RepresentativeAnchorDescriptor<?> anchorDesc =
    // aWindow.getRepresentativeAnchorDescriptor();
    // anchorDesc.setPreviewEnabled( false );
    //
    // final ToolWindowType[] types = ToolWindowType.values();
    // for ( ToolWindowType type : types )
    // {
    // ToolWindowTypeDescriptor desc = aWindow.getTypeDescriptor( type );
    // desc.setHideRepresentativeButtonOnVisible( true );
    // desc.setIdVisibleOnTitleBar( false );
    // }
    //
    // DockedTypeDescriptor desc = ( DockedTypeDescriptor
    // )aWindow.getTypeDescriptor( ToolWindowType.DOCKED );
    // desc.setDockLength( aDockLength );
    // desc.setHideRepresentativeButtonOnVisible( true );
    // desc.setPopupMenuEnabled( false );

    aWindow.setAvailable( true );
    aWindow.setHideOnZeroTabs( true );
  }

  /**
   * @return
   */
  public Component get()
  {
    return this.windowManager;
  }

  /**
   * @param aToolWindow
   * @param aGroupName
   */
  public void registerToolWindow( final IToolWindow aToolWindow, final String aGroupName )
  {
    ToolWindow tw = this.windowManager.registerToolWindow( aToolWindow.getId(), aToolWindow.getName(),
        aToolWindow.getIcon(), ( Component )aToolWindow, ToolWindowAnchor.RIGHT );

    final ToolWindowGroup group = this.windowManager.getToolWindowGroup( aGroupName );
    group.setImplicit( true );
    group.addToolWindow( tw );

    // Given string is based on some experiments with the best "default"
    // length...
    final int dockLength = SwingComponentUtils.getStringWidth( "XXXXXXXXXXXXXXXXXXXXXXXXXXXX" );

    tweakToolWindow( tw, dockLength );
  }

  /**
   * @param aComponent
   */
  public void setMainContent( final Component aComponent )
  {
    this.windowManager.setMainContent( aComponent );
  }

}
