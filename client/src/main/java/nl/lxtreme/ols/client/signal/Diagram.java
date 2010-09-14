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

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.signal.laf.*;
import nl.lxtreme.ols.util.*;


/**
 * This component displays a diagram which is obtained from a
 * {@link CapturedDataImpl} object. The settings for the diagram are obtained
 * from the embedded {@link DiagramSettingsDialog} and
 * {@link DiagramLabelsDialog} objects. Look there for an overview of ways to
 * display data.
 * <p>
 * Component size changes with the size of the diagram. Therefore it should only
 * be used from within a JScrollPane.
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public final class Diagram extends JComponent implements Scrollable, DiagramCursorChangeListener
{
  // INNER TYPES

  private static final long serialVersionUID = 1L;

  private static final double CURSOR_HOVER = 5.0;

  private static final boolean DEBUG = Boolean.parseBoolean( System
      .getProperty( "nl.lxtreme.ols.client.debug", "false" ) );

  /** The maximum scale that is possible in this diagram. */
  public static final double MAX_SCALE = 15;

  // VARIABLES

  private final DiagramSettings diagramSettings;
  private final DiagramTimeLine timeLine;
  private final DiagramRowLabels rowLabels;
  private double scale;
  private final ClientController controller;
  private final DataContainer dataContainer;

  // CONSTRUCTORS

  /**
   * Create a new empty diagram to be placed in a container.
   */
  public Diagram( final ClientController aController )
  {
    super();

    this.controller = aController;
    this.dataContainer = aController.getDataContainer();

    this.diagramSettings = new MutableDiagramSettings();

    this.rowLabels = new DiagramRowLabels( this );
    this.timeLine = new DiagramTimeLine( this );

    setMinimumSize( new Dimension( 25, 1 ) );

    aController.addCursorChangeListener( this );
    aController.addCursorChangeListener( this.timeLine );
  }

  /**
   * Convert x position to sample index.
   * 
   * @param aXpos
   *          horizontal position (in pixels).
   * @return sample index
   */
  static final long xToIndex( final CapturedData aData, final Point aPoint, final double aScale )
  {
    long index = ( long )Math.max( 0.0, ( aPoint.getX() / aScale ) );

    if ( ( aData != null ) && ( index >= aData.getAbsoluteLength() ) )
    {
      index = aData.getAbsoluteLength() - 1;
    }

    return index;
  }

  /**
   * Calls the <code>configureEnclosingScrollPane</code> method.
   * 
   * @see #configureEnclosingScrollPane
   */
  @Override
  public void addNotify()
  {
    super.addNotify();
    configureEnclosingScrollPane();

    // Make sure to properly initialize our (custom) UI...
    updateUI();
  }

  /**
   * Convert x position to sample index.
   * 
   * @param aXpos
   *          horizontal position in pixels
   * @return sample index
   */
  public long convertPointToSampleIndex( final Point aPoint )
  {
    return Diagram.xToIndex( this.dataContainer, aPoint, this.scale );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramCursorChangeListener#cursorChanged(int,
   *      int)
   */
  @Override
  public void cursorChanged( final int aCursorIdx, final int aMousePos )
  {
    repaint();
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramCursorChangeListener#cursorRemoved(int)
   */
  @Override
  public void cursorRemoved( final int aCursorIdx )
  {
    repaint();
  }

  /**
   * Moves the cursor with a given index to a given mouse X position.
   * 
   * @param aCursorIdx
   *          the cursor index to move, should be >= 0;
   * @param aMousePosition
   *          the point containing the new X position of the cursor.
   */
  public final void dragCursor( final int aCursorIdx, final Point aMousePosition )
  {
    if ( !this.dataContainer.isCursorsEnabled() )
    {
      return;
    }

    this.controller.setCursorPosition( aCursorIdx, aMousePosition );
  }

  /**
   * @param aChannelIdx
   * @param aMouseXpos
   * @return
   */
  public final ChannelAnnotation getAnnotationHover( final int aChannelIdx, final Point aMousePosition )
  {
    final int sampleIdx = this.dataContainer.getSampleIndex( convertPointToSampleIndex( aMousePosition ) );
    return this.dataContainer.getChannelAnnotation( aChannelIdx, sampleIdx );
  }

  /**
   * Determines whether the given mouse X position is actually in the vicinity
   * of a cursor.
   * 
   * @param aMouseXpos
   *          the mouse X position to test.
   * @return the index of the cursor the given mouse position is near, or -1 if
   *         there's no cursor set or nowhere near a cursor.
   */
  public final int getCursorHover( final Point aMousePosition )
  {
    final long idx = convertPointToSampleIndex( aMousePosition );
    final double threshold = CURSOR_HOVER / this.scale;
    for ( int i = 0; i < DataContainer.MAX_CURSORS; i++ )
    {
      final long cursorPosition = this.dataContainer.getCursorPosition( i );
      if ( cursorPosition < 0 )
      {
        continue;
      }
      if ( Math.abs( idx - cursorPosition ) < threshold )
      {
        return i;
      }
    }
    return -1;
  }

  /**
   * @return the dataContainer
   */
  public final DataContainer getDataContainer()
  {
    return this.dataContainer;
  }

  /**
   * Returns the diagram settings.
   * 
   * @return the diagram settings, never <code>null</code>.
   */
  public DiagramSettings getDiagramSettings()
  {
    return this.diagramSettings;
  }

  /**
   * @see javax.swing.Scrollable#getPreferredScrollableViewportSize()
   */
  @Override
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  /**
   * @return the scale
   */
  public final double getScale()
  {
    return this.scale;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      return aVisibleRect.width - DiagramTimeLineUI.TIMELINE_INCREMENT;
    }
    else
    {
      return aVisibleRect.height - getDiagramSettings().getChannelHeight();
    }
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportHeight()
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportWidth()
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    int currentPosition = 0;
    final int maxUnitIncrement;
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      currentPosition = aVisibleRect.x;
      maxUnitIncrement = DiagramTimeLineUI.TIMELINE_INCREMENT;
    }
    else
    {
      currentPosition = aVisibleRect.y;
      maxUnitIncrement = getDiagramSettings().getChannelHeight();
    }

    // Return the number of pixels between currentPosition
    // and the nearest tick mark in the indicated direction.
    if ( aDirection < 0 )
    {
      final int newPosition = currentPosition - ( currentPosition / maxUnitIncrement ) * maxUnitIncrement;
      return ( newPosition == 0 ) ? maxUnitIncrement : newPosition;
    }
    else
    {
      return ( ( currentPosition / maxUnitIncrement ) + 1 ) * maxUnitIncrement - currentPosition;
    }
  }

  /**
   * calulate the position within a window (pane) based on current page and zoom
   * settings
   * 
   * @param width
   *          window width
   * @param aPosition
   *          sample position
   * @return current position within window
   */
  public int getTargetPosition( final long aPosition )
  {
    return ( int )( Math.max( 0.0, aPosition ) * this.scale );
  }

  /**
   * Sets this Diagram's viewport position
   * 
   * @param aSamplePos
   *          sample position
   */
  public void gotoPosition( final long aSamplePos )
  {
    final JViewport vp = getViewPort();
    if ( vp == null )
    {
      return;
    }

    final int width = vp.getWidth();

    // do nothing if the zoom factor is nearly the viewport size
    final Dimension dim = getPreferredSize();
    if ( dim.width < width * 2 )
    {
      return;
    }

    final int pos = Math.max( 0, getTargetPosition( aSamplePos ) - ( width / 2 ) );
    vp.setViewPosition( new Point( pos, 0 ) );
  }

  /**
   * Calls the <code>unconfigureEnclosingScrollPane</code> method.
   * 
   * @see #unconfigureEnclosingScrollPane
   */
  @Override
  public void removeNotify()
  {
    unconfigureEnclosingScrollPane();
    super.removeNotify();
  }

  /**
   * @see java.awt.Component#repaint()
   */
  @Override
  public void repaint()
  {
    super.repaint();
    this.rowLabels.repaint();
    this.timeLine.repaint();
  }

  /**
   * Calculates the preferred width and height of this component.
   * 
   * @return a preferred size, never <code>null</code>.
   */
  public void updatePreferredSize()
  {
    final DiagramUI diagramUI = ( DiagramUI )this.ui;
    final Dimension newDiagramSize = diagramUI.calculateNewDimension( this );
    if ( newDiagramSize.equals( getPreferredSize() ) )
    {
      // No actual resizing necessary; only repaint us and our
      // headers/borders...
      this.rowLabels.repaint();
      this.timeLine.repaint();
      repaint();
    }
    else
    {
      // Resize this component and its headers/borders...
      this.rowLabels.setPreferredSize( newDiagramSize );
      this.timeLine.setPreferredSize( newDiagramSize );
      setPreferredSize( newDiagramSize );
    }
  }

  /**
   * Update status information. Notifies {@link StatusChangeListener}.
   * 
   * @param aDragging
   *          <code>true</code> indicates that dragging information should be
   *          added
   */
  public final void updateTooltipText( final Point aMousePosition, final ChannelAnnotation aAnnotation )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    final StringBuffer sb = new StringBuffer( " " );

    final int row = aMousePosition.y / getDiagramSettings().getChannelHeight();
    if ( row <= this.dataContainer.getChannels() + ( this.dataContainer.getChannels() / 9 ) )
    {
      if ( row % 9 == 8 )
      {
        sb.append( "Byte " ).append( ( row / 9 ) );
      }
      else
      {
        sb.append( "Channel " ).append( ( row - ( row / 9 ) ) );
      }
      sb.append( " | " );
    }

    if ( aAnnotation != null )
    {
      sb.append( aAnnotation.getData() );
    }
    else
    {
      final long idxMouseX = convertPointToSampleIndex( aMousePosition );
      final long triggerPosition = this.dataContainer.getTriggerPosition();

      final long absMouseX = idxMouseX - triggerPosition;
      if ( DEBUG || !this.dataContainer.hasTimingData() )
      {
        sb.append( "Sample " ).append( absMouseX );
      }
      else
      {
        final int sampleRate = this.dataContainer.getSampleRate();
        sb.append( "Time " ).append( DisplayUtils.displayScaledTime( absMouseX, sampleRate ) );
      }
    }

    final String status = sb.toString();
    // System.out.println( "STATUS = " + status );
    setToolTipText( status );
  }

  /**
   * Overridden in order to set a custom UI, which not only paints this diagram,
   * but also can be used to manage the various settings, such as colors,
   * height, and so on.
   * 
   * @see javax.swing.JComponent#updateUI()
   */
  @Override
  public void updateUI()
  {
    setUI( new DiagramUI( this.controller ) );
  }

  /**
   * Reverts back to the standard zoom level.
   */
  public void zoomDefault()
  {
    setScale( MAX_SCALE );

    updatePreferredSize();
    revalidateAll();
  }

  /**
   * Zooms in by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  public void zoomIn()
  {
    zoomIn( null );
  }

  /**
   * Zooms in by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  public final void zoomIn( final Point aPoint )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    double newScale = this.scale;
    if ( newScale < MAX_SCALE )
    {
      setScale( newScale * 2.0 );

      updatePreferredSize();
      updatePreferredLocation( aPoint, 2.0 );

      revalidateAll();
    }
  }

  /**
   * Zooms out by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  public void zoomOut()
  {
    zoomOut( null );
  }

  /**
   * Zooms out by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  public final void zoomOut( final Point aPoint )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    final double fitScaleFactor = getZoomToFitScale();

    double newScale = this.scale;
    if ( newScale > fitScaleFactor )
    {
      setScale( newScale * 0.5 );

      updatePreferredSize();
      updatePreferredLocation( aPoint, 0.5 );

      revalidateAll();
    }
  }

  /**
   * Zooms to fitting the view on Display.
   */
  public void zoomToFit()
  {
    // avoid null pointer exception when no data available
    if ( this.dataContainer == null )
    {
      return;
    }

    final double fitScaleFactor = getZoomToFitScale();

    setScale( fitScaleFactor );

    updatePreferredSize();
    revalidateAll();
  }

  /**
   * If this component is the <code>viewportView</code> of an enclosing
   * <code>JScrollPane</code> (the usual situation), configure this
   * <code>ScrollPane</code> by, amongst other things, installing the diagram's
   * <code>timeline</code> as the <code>columnHeaderView</code> of the scroll
   * pane.
   * 
   * @see #addNotify
   */
  private void configureEnclosingScrollPane()
  {
    final Container p = getParent();
    if ( p instanceof JViewport )
    {
      final Container gp = p.getParent();
      if ( gp instanceof JScrollPane )
      {
        final JScrollPane scrollPane = ( JScrollPane )gp;
        // Make certain we are the viewPort's view and not, for
        // example, the rowHeaderView of the scrollPane -
        // an implementor of fixed columns might do this.
        final JViewport viewport = scrollPane.getViewport();
        if ( ( viewport == null ) || ( viewport.getView() != this ) )
        {
          return;
        }
        scrollPane.setColumnHeaderView( this.timeLine );
        scrollPane.setRowHeaderView( this.rowLabels );
      }
    }
  }

  /**
   * Returns the viewport if this diagram is enclosed in a JScrollPane.
   * 
   * @return the viewport of the enclosing JScrollPane, or <code>null</code> if
   *         this diagram is not enclosed in a JScrollPane.
   */
  private JViewport getViewPort()
  {
    final JScrollPane scrollPane = ( JScrollPane )SwingUtilities.getAncestorOfClass( JScrollPane.class, this );
    if ( scrollPane == null )
    {
      return null;
    }
    return scrollPane.getViewport();
  }

  /**
   * Calculates the scale that should be set to make this diagram fit entirely
   * in the current view.
   * 
   * @return a zoom-to-fit scale, > 0.
   */
  private double getZoomToFitScale()
  {
    int visibleWidth;

    final JViewport viewport = getViewPort();
    if ( viewport != null )
    {
      visibleWidth = viewport.getWidth() - 5;
    }
    else
    {
      visibleWidth = SwingUtilities.getWindowAncestor( this ).getWidth() - this.rowLabels.getWidth() - 20;
    }

    final int width = Math.max( 1, visibleWidth );
    final double fitScaleFactor = width / ( double )this.dataContainer.getAbsoluteLength();
    return fitScaleFactor;
  }

  /**
   * Revalidates this diagram and all of its headers/borders.
   */
  private void revalidateAll()
  {
    this.timeLine.revalidate();
    this.rowLabels.revalidate();
    revalidate();
  }

  /**
   * Sets the scale to the given value.
   * 
   * @param aScale
   *          the scale to set, cannot be <code>null</code>.
   */
  private void setScale( final double aScale )
  {
    this.scale = aScale;
  }

  /**
   * Reverses the effect of <code>configureEnclosingScrollPane</code> by
   * replacing the <code>columnHeaderView</code> of the enclosing scroll pane
   * with <code>null</code>.
   * 
   * @see #removeNotify
   * @see #configureEnclosingScrollPane
   */
  private void unconfigureEnclosingScrollPane()
  {
    final Container p = getParent();
    if ( p instanceof JViewport )
    {
      final Container gp = p.getParent();
      if ( gp instanceof JScrollPane )
      {
        final JScrollPane scrollPane = ( JScrollPane )gp;
        // Make certain we are the viewPort's view and not, for
        // example, the rowHeaderView of the scrollPane -
        // an implementor of fixed columns might do this.
        final JViewport viewport = scrollPane.getViewport();
        if ( ( viewport == null ) || ( viewport.getView() != this ) )
        {
          return;
        }
        scrollPane.setColumnHeaderView( null );
        scrollPane.setRowHeaderView( null );
      }
    }
  }

  /**
   * Tries to update the preferred location (= the current location, or the
   * given location).
   * 
   * @param aPoint
   *          the point to keep in the center, can be <code>null</code> to keep
   *          the current center position as-is;
   * @param aScaleFactor
   *          the scale multiplication factor, e.g. 2.0 for zooming in, 0.5 for
   *          zooming out.
   */
  private void updatePreferredLocation( final Point aPoint, final double aScaleFactor )
  {
    final JViewport vp = getViewPort();
    if ( vp != null )
    {
      final int vpWidth = vp.getWidth();
      final int zoomDir = aScaleFactor >= 1.0 ? -1 : aScaleFactor == 0.0 ? 0 : +1;

      /*
       * Idea taken from:
       * <http://stackoverflow.com/questions/115103/how-do-you-implement
       * -position-sensitive-zooming-inside-a-jscrollpane>
       */

      final Point location = getLocation();

      int locX = 0;
      if ( aPoint != null )
      {
        int offX = ( int )( aPoint.x * aScaleFactor ) - aPoint.x;
        // we only zoom in width, never in height...
        locX = location.x - offX;
      }
      else
      {
        // we only zoom in width, never in height...
        // XXX works in general OK for zooming in, for zooming out it has some
        // issues...
        locX = ( int )( ( location.x * aScaleFactor ) + zoomDir * ( vpWidth / 2.0 ) );
      }

      setLocation( locX, location.y );
    }
  }
}
