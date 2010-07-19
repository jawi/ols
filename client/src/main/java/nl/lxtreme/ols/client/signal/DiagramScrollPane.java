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
package nl.lxtreme.ols.client.signal;


import java.awt.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.*;


/**
 * 
 */
public abstract class DiagramScrollPane extends JScrollPane implements ActionProvider
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram     diagram;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramScrollPane( final Project aProject )
  {
    super();

    // We want to be painted fully (non transparent)...
    setOpaque( true );

    this.diagram = new Diagram( this );
    setViewportView( this.diagram );

    // Presumably the fastest way of rendering...
    getViewport().setScrollMode( JViewport.BLIT_SCROLL_MODE );

    if ( aProject != null )
    {
      aProject.addConfigurable( this.diagram );
    }
  }

  // METHODS

  /**
   * Returns the current captured data.
   * 
   * @return a captured data object, can be <code>null</code>.
   */
  public CapturedData getCapturedData()
  {
    return this.diagram.getCapturedData();
  }

  /**
   * @param aCursorNo
   */
  public void gotoCursorPosition( final int aCursorNo )
  {
    if ( this.diagram.getCursorMode() )
    {
      gotoPosition( this.diagram.getCapturedData().getCursorPosition( aCursorNo ) );
    }
  }

  /**
   * 
   */
  public void gotoTriggerPosition()
  {
    // do this only with data and trigger available
    if ( this.diagram.hasCapturedData() && this.diagram.getCapturedData().hasTriggerData() )
    {
      gotoPosition( this.diagram.getCapturedData().triggerPosition );
    }
  }

  /**
   * @param aFile
   * @throws IOException
   */
  public void loadData( final File aFile ) throws IOException
  {
    this.diagram.setCapturedData( new CapturedData( aFile ) );
    this.diagram.zoomFit();
  }

  /**
   * @param aFile
   * @throws IOException
   */
  public void saveData( final File aFile ) throws IOException
  {
    this.diagram.getCapturedData().writeToFile( aFile );
  }

  /**
   * @param aCapturedData
   */
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.diagram.setCapturedData( aCapturedData );
  }

  /**
   * @param aState
   */
  public void setCursorMode( final boolean aState )
  {
    this.diagram.setCursorMode( aState );
  }

  /**
   * @param aI
   */
  public void setCursorPosition( final int aCursorIdx )
  {
    this.diagram.setCursorPosition( aCursorIdx );
  }

  /**
   * @param aOwner
   */
  public void showLabelsDialog( final Window aOwner )
  {
    this.diagram.showLabelsDialog( aOwner );
  }

  /**
   * @param aOwner
   */
  public void showSettingsDialog( final Window aOwner )
  {
    this.diagram.showSettingsDialog( aOwner );
  }

  /**
   * 
   */
  public void zoomDefault()
  {
    this.diagram.zoomDefault();
  }

  /**
   * 
   */
  public void zoomIn()
  {
    final JViewport vp = getViewport();
    final Point viewPosition = vp.getViewPosition();
    viewPosition.x = ( int )( viewPosition.x * 2.0 );

    this.diagram.zoomIn();

    vp.setViewPosition( viewPosition );
  }

  /**
   * 
   */
  public void zoomOut()
  {
    final JViewport vp = getViewport();
    final Point viewPosition = vp.getViewPosition();
    viewPosition.x = ( int )( viewPosition.x / 2.0 );

    this.diagram.zoomOut();

    vp.setViewPosition( viewPosition );
  }

  /**
   * 
   */
  public void zoomToFit()
  {
    this.diagram.zoomFit();
  }

  /**
   * set Diagram viewport position
   * 
   * @param aSamplePos
   *          sample position
   */
  private void gotoPosition( final long aSamplePos )
  {
    final Dimension dim = this.diagram.getPreferredSize();
    final JViewport vp = getViewport();

    // do nothing if the zoom factor is nearly the viewport size
    if ( dim.width < vp.getWidth() * 2 )
    {
      return;
    }

    final int pos = Math.max( 0, this.diagram.getTargetPosition( dim.width, aSamplePos ) - 20 );
    vp.setViewPosition( new Point( pos, 0 ) );
  }

}

/* EOF */
