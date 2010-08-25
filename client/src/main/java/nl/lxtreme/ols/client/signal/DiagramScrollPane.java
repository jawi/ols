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
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;


/**
 * 
 */
public class DiagramScrollPane extends JScrollPane
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram diagram;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramScrollPane( final Project aProject, final ActionProvider aProvider )
  {
    super();

    setCorner( UPPER_LEFT_CORNER, new JLabel( " " ) );

    this.diagram = new Diagram( aProvider );
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
   * @return
   */
  public AnnotatedData getAnnotatedData()
  {
    return this.diagram.getAnnotatedData();
  }

  /**
   * @param aCursorNo
   */
  public void gotoCursorPosition( final int aCursorNo )
  {
    if ( this.diagram.getCursorMode() )
    {
      gotoPosition( getAnnotatedData().getCursorPosition( aCursorNo ) );
    }
  }

  /**
   * 
   */
  public void gotoTriggerPosition()
  {
    final long triggerPosition = getAnnotatedData().getTriggerPosition();
    if ( triggerPosition != CapturedData.NOT_AVAILABLE )
    {
      gotoPosition( triggerPosition );
    }
  }

  /**
   * @param aFile
   * @throws IOException
   */
  public void loadData( final File aFile ) throws IOException
  {
    final FileReader reader = new FileReader( aFile );

    final AnnotatedData data = getAnnotatedData();
    try
    {
      data.read( reader );
    }
    finally
    {
      reader.close();

      this.diagram.zoomFit();
    }
  }

  /**
   * @param aFile
   * @throws IOException
   */
  public void saveData( final File aFile ) throws IOException
  {
    final FileWriter writer = new FileWriter( aFile );
    try
    {
      getAnnotatedData().write( writer );
    }
    finally
    {
      writer.flush();
      writer.close();
    }
  }

  /**
   * @param aCapturedData
   */
  public void setCapturedData( final CapturedData aData )
  {
    this.diagram.getAnnotatedData().setCapturedData( aData );
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
