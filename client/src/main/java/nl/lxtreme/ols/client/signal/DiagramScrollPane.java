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
import java.awt.event.*;
import java.io.*;
import java.net.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.icons.*;


/**
 * Provides a scrollpane for the diagram.
 */
public class DiagramScrollPane extends JScrollPane
{
  // INNER TYPES

  /**
   */
  static final class ScrollPaneContextAction extends AbstractAction implements PopupMenuListener
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final JPopupMenu popup;
    private final ActionProvider actionProvider;

    // CONSTRUCTORS

    /**
     * Creates a new ScrollPaneContextAction instance.
     */
    public ScrollPaneContextAction( final ActionProvider aActionProvider )
    {
      super( "" );

      final URL url = IconLocator.class.getResource( IconLocator.ICON_DIAGRAM_SETTINGS );
      putValue( Action.LARGE_ICON_KEY, new ImageIcon( url ) );

      this.actionProvider = aActionProvider;

      this.popup = new JPopupMenu();
      this.popup.addPopupMenuListener( this );
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final JButton source = ( JButton )aEvent.getSource();
      final int x = -1;
      final int y = source.getHeight();

      this.popup.show( source, x, y );
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuCanceled(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuCanceled( final PopupMenuEvent aEvent )
    {
      // NO-op
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeInvisible( final PopupMenuEvent aEvent )
    {
      // NO-op
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeVisible( final PopupMenuEvent aEvent )
    {
      final MenuElement[] menuitems = this.popup.getSubElements();
      if ( ( menuitems == null ) || ( menuitems.length == 0 ) )
      {
        // First time we're going to show this menu, dynamically build it...
        this.popup.add( new JMenuItem( this.actionProvider.getAction( ShowDiagramSettingsAction.ID ) ) );
        this.popup.add( new JMenuItem( this.actionProvider.getAction( ShowDiagramLabelsAction.ID ) ) );
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram diagram;
  private final JButton contextButton;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramScrollPane( final ActionProvider aProvider )
  {
    super();

    this.contextButton = new JButton( new ScrollPaneContextAction( aProvider ) );
    this.contextButton.setBorder( BorderFactory.createLineBorder( Color.GRAY ) );
    this.contextButton.setBackground( Color.WHITE );
    this.contextButton.setVisible( false );

    this.diagram = new Diagram( aProvider );
    setViewportView( this.diagram );

    setCorner( UPPER_LEFT_CORNER, this.contextButton );

    // Presumably the fastest way of rendering...
    getViewport().setScrollMode( JViewport.BLIT_SCROLL_MODE );
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
    this.contextButton.setVisible( aData != null );
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

    final int pos = Math.max( 0, this.diagram.getTargetPosition( aSamplePos ) - 20 );
    vp.setViewPosition( new Point( pos, 0 ) );
  }

}

/* EOF */
