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
import java.util.*;
import java.util.logging.*;

import javax.swing.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
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
public final class Diagram extends JComponent implements Scrollable, DiagramSettings, DiagramCursorChangeListener
{
  // INNER TYPES

  /**
   * Denotes a polyline for drawing byte values.
   */
  static final class ByteValuePolyline
  {
    public final int[] x;
    public final int[] y1;
    public final int[] y2;
    public final int n;

    public ByteValuePolyline( final int aN )
    {
      this.n = aN;
      this.x = new int[aN];
      this.y1 = new int[aN];
      this.y2 = new int[aN];
    }
  }

  /**
   * 
   */
  final class MouseListener extends MouseAdapter
  {
    // VARIABLES

    private int currentCursor;
    private int startDragXpos;

    // METHODS

    /**
     * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      if ( aEvent.getClickCount() == 2 )
      {
        final Point point = aEvent.getPoint();
        if ( aEvent.isAltDown() || aEvent.isShiftDown() )
        {
          // Zoom out...
          zoomOut( point );
        }
        else
        {
          // Zoom in...
          zoomIn( point );
        }
      }
    }

    /**
     * Handles mouse dragged events and produces status change "events"
     * accordingly.
     */
    @Override
    public void mouseDragged( final MouseEvent aEvent )
    {
      final Point mousePosition = aEvent.getPoint();
      if ( this.currentCursor >= 0 )
      {
        dragCursor( this.currentCursor, mousePosition );
      }

      updateTooltipText( mousePosition, true /* aDragging */, this.startDragXpos, null );
    }

    /**
     * Handles mouse moved events and produces status change "events"
     * accordingly.
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final Point mousePosition = aEvent.getPoint();

      final int channel = ( mousePosition.y / getChannelHeight() );
      final ChannelAnnotation annotation = getAnnotationHover( channel, mousePosition );

      final int cursorIdx = getCursorHover( mousePosition );
      if ( cursorIdx >= 0 )
      {
        this.currentCursor = cursorIdx;
        this.startDragXpos = mousePosition.x;
        Diagram.this.setCursor( Diagram.this.cursorDrag );
      }
      else
      {
        this.currentCursor = -1;
        this.startDragXpos = -1;
        Diagram.this.setCursor( Diagram.this.cursorDefault );
      }

      updateTooltipText( mousePosition, false /* aDragging */, this.startDragXpos, annotation );
    }

    /**
     * Handles mouse button events for context menu
     */
    @Override
    public void mousePressed( final MouseEvent aEvent )
    {
      evaluatePopupEvent( aEvent );
    }

    /**
     * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseReleased( final MouseEvent aEvent )
    {
      evaluatePopupEvent( aEvent );
    }

    /**
     * @param aEvent
     */
    private void evaluatePopupEvent( final MouseEvent aEvent )
    {
      if ( aEvent.isPopupTrigger() )
      {
        showContextMenu( aEvent.getPoint() );
      }
    }
  }

  /**
   * Denotes a polyline for drawing signals.
   */
  static final class SignalPolyline
  {
    public final int[] x;
    public final int[] y;
    public final int n;

    public SignalPolyline( final int aN )
    {
      this.n = aN;
      this.x = new int[aN];
      this.y = new int[aN];
    }
  }

  // CONSTANTS

  public static final String CONTEXTMENU_LOCATION_KEY = "OLS.ContextMenu.location";

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( Diagram.class.getName() );

  static final double MAX_SCALE = 15;
  private static final double CURSOR_HOVER = 5.0;
  private static final int PADDING_Y = 2;

  private static final Stroke SOLID_THICK = new BasicStroke( 1.5f );

  private static final boolean DEBUG = Boolean.parseBoolean( System
      .getProperty( "nl.lxtreme.ols.client.debug", "false" ) );

  // VARIABLES

  private final DiagramTimeLine timeLine;
  private final DiagramRowLabels rowLabels;
  private double scale;
  private final Cursor cursorDefault;
  private final Cursor cursorDrag;
  private final JPopupMenu contextMenu;
  private final ClientController controller;
  /**
   * Display settings for each group. Can be any combinations (OR-ed) of the
   * defined MODE_* values.
   */
  private final int[] groupSettings;
  private final Color signalColor;
  private final Color triggerColor;
  private final Color gridColor;
  private final Color textColor;
  private final Color timeColor;
  private final Color groupBackgroundColor;
  private final Color backgroundColor;
  private final Color labelColor;
  private final Color[] cursorColors;
  private final Color[] channelColors;
  private final int channelHeight;
  private final int scopeHeight;

  private final DataContainer dataContainer;

  /**
   * Create a new empty diagram to be placed in a container.
   */
  public Diagram( final ClientController aController )
  {
    super();

    this.controller = aController;
    this.dataContainer = aController.getDataContainer();

    // this.backgroundColor = new Color( 0x10, 0x10, 0x10 );
    // this.signalColor = new Color( 0x30, 0x4b, 0x75 );
    // this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    // this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    // this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    // this.textColor = Color.WHITE;
    // this.timeColor = Color.WHITE;
    // this.labelColor = new Color( 0x82, 0x87, 0x8f );
    this.backgroundColor = Color.WHITE;
    this.signalColor = new Color( 0x30, 0x4b, 0x75 );
    this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    this.textColor = new Color( 0x25, 0x25, 0x25 );
    this.timeColor = new Color( 0x25, 0x25, 0x25 );
    this.labelColor = new Color( 0x82, 0x87, 0x8f );

    this.cursorColors = makeColorPalette( DataContainer.MAX_CURSORS, DataContainer.MAX_CURSORS );
    this.channelColors = makeMonochromaticColorPalette( DataContainer.MAX_CHANNELS );

    this.channelHeight = 30;
    this.scopeHeight = 133;

    this.groupSettings = new int[4];
    for ( int i = 0; i < this.groupSettings.length; i++ )
    {
      this.groupSettings[i] = DISPLAY_CHANNELS | DISPLAY_BYTE;
    }

    this.contextMenu = new JPopupMenu();
    for ( int i = 0; i < 10; i++ )
    {
      final Action setCursorAction = aController.getAction( SetCursorAction.getCursorId( i ) );
      this.contextMenu.add( new JCheckBoxMenuItem( setCursorAction ) );
    }

    this.cursorDefault = getCursor();
    this.cursorDrag = new Cursor( Cursor.MOVE_CURSOR );

    this.rowLabels = new DiagramRowLabels( this.dataContainer );
    this.rowLabels.setDiagramSettings( this );

    this.timeLine = new DiagramTimeLine( this.dataContainer );
    this.timeLine.setDiagramSettings( this );

    setMinimumSize( new Dimension( 25, 1 ) );
    setBackground( getBackgroundColor() );

    aController.addCursorChangeListener( this );
    aController.addCursorChangeListener( this.timeLine );

    final MouseListener mouseListener = new MouseListener();

    addMouseListener( mouseListener );
    addMouseMotionListener( mouseListener );
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getBackgroundColor()
   */
  public final Color getBackgroundColor()
  {
    return this.backgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getChannelHeight()
   */
  @Override
  public final int getChannelHeight()
  {
    return this.channelHeight;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getCursorColor(int)
   */
  @Override
  public final Color getCursorColor( final int aCursorIdx )
  {
    return this.cursorColors[aCursorIdx];
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGridColor()
   */
  public final Color getGridColor()
  {
    return this.gridColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getGroupBackgroundColor()
   */
  public final Color getGroupBackgroundColor()
  {
    return this.groupBackgroundColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getLabelColor()
   */
  public final Color getLabelColor()
  {
    return this.labelColor;
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getScopeHeight()
   */
  @Override
  public final int getScopeHeight()
  {
    return this.scopeHeight;
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
      return aVisibleRect.width - DiagramTimeLine.TIMELINE_INCREMENT;
    }
    else
    {
      return aVisibleRect.height - getChannelHeight();
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
      maxUnitIncrement = DiagramTimeLine.TIMELINE_INCREMENT;
    }
    else
    {
      currentPosition = aVisibleRect.y;
      maxUnitIncrement = getChannelHeight();
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getSignalColor(int)
   */
  public final Color getSignalColor( final int aChannelIdx )
  {
    return this.channelColors[aChannelIdx];
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getSignalHeight()
   */
  @Override
  public final int getSignalHeight()
  {
    return getChannelHeight() - 4;
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTextColor()
   */
  public final Color getTextColor()
  {
    return this.textColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTimeColor()
   */
  public final Color getTimeColor()
  {
    return this.timeColor;
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#getTriggerColor()
   */
  public final Color getTriggerColor()
  {
    return this.triggerColor;
  }

  /**
   * Returns the current scale.
   * 
   * @return the scale as double value.
   */
  public double getZoomScale()
  {
    return this.scale;
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowByte(int)
   */
  public final boolean isShowByte( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DISPLAY_BYTE ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowChannels(int)
   */
  public final boolean isShowChannels( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DISPLAY_CHANNELS ) > 0 );
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#isShowScope(int)
   */
  public final boolean isShowScope( final int aGroup )
  {
    return ( ( this.groupSettings[aGroup] & DISPLAY_SCOPE ) > 0 );
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
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowByte(int, boolean)
   */
  @Override
  public final void setShowByte( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DISPLAY_BYTE;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DISPLAY_BYTE;
    }
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowChannels(int,
   *      boolean)
   */
  @Override
  public final void setShowChannels( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DISPLAY_CHANNELS;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DISPLAY_CHANNELS;
    }
  }

  /**
   * @see nl.lxtreme.ols.client.signal.DiagramSettings#setShowScope(int,
   *      boolean)
   */
  @Override
  public final void setShowScope( final int aGroup, final boolean aShow )
  {
    if ( aShow )
    {
      this.groupSettings[aGroup] |= DISPLAY_SCOPE;
    }
    else
    {
      this.groupSettings[aGroup] &= ~DISPLAY_SCOPE;
    }
  }

  /**
   * Calculates the preferred width and height of this component.
   * 
   * @return a preferred size, never <code>null</code>.
   */
  public void updatePreferredSize( final Point aPoint, final double aScaleFactor )
  {
    final Dimension newDiagramSize = calculateNewDimension();

    setPreferredSize( newDiagramSize );
    this.timeLine.setPreferredSize( newDiagramSize );
    this.rowLabels.setPreferredSize( newDiagramSize );

    updatePreferredLocation( aPoint, aScaleFactor );

    this.timeLine.revalidate();
    this.rowLabels.revalidate();
    revalidate();
  }

  /**
   * Reverts back to the standard zoom level.
   */
  public void zoomDefault()
  {
    setScale( MAX_SCALE );
    updatePreferredSize( null, 0.0 );
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
    updatePreferredSize( null, 0.0 );
  }

  /**
   * Moves the cursor with a given index to a given mouse X position.
   * 
   * @param aCursorIdx
   *          the cursor index to move, should be >= 0;
   * @param aMousePosition
   *          the point containing the new X position of the cursor.
   */
  final void dragCursor( final int aCursorIdx, final Point aMousePosition )
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
  final ChannelAnnotation getAnnotationHover( final int aChannelIdx, final Point aMousePosition )
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
  final int getCursorHover( final Point aMousePosition )
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
   * @param aEvent
   */
  final void showContextMenu( final Point aPosition )
  {
    if ( this.dataContainer.hasCapturedData() )
    {
      this.contextMenu.putClientProperty( CONTEXTMENU_LOCATION_KEY, aPosition );
      this.contextMenu.show( this, aPosition.x, aPosition.y );
    }
  }

  /**
   * Update status information. Notifies {@link StatusChangeListener}.
   * 
   * @param aDragging
   *          <code>true</code> indicates that dragging information should be
   *          added
   */
  final void updateTooltipText( final Point aMousePosition, final boolean aDragging, final int aStartDragXpos,
      final ChannelAnnotation aAnnotation )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    final StringBuffer sb = new StringBuffer( " " );

    final int row = aMousePosition.y / getChannelHeight();
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
      final int sampleRate = this.dataContainer.getSampleRate();
      final long triggerPosition = this.dataContainer.getTriggerTimePosition();

      // if ( this.dataContainer.isCursorsEnabled() )
      // {
      // // print cursor data to status line
      // final long absCursorPosA = this.dataContainer.getCursorPosition( 0 ) -
      // triggerPosition;
      // final long absCursorPosB = this.dataContainer.getCursorPosition( 1 ) -
      // triggerPosition;
      // final long relCursorPos = this.dataContainer.getCursorPosition( 0 ) -
      // this.dataContainer.getCursorPosition( 1 );
      //
      // if ( !this.dataContainer.hasTimingData() )
      // {
      // sb.append( "Sample@1=" ).append( absCursorPosA ).append( " | " );
      // sb.append( "Sample@2=" ).append( absCursorPosB ).append( " | " );
      // sb.append( "Distance(1,2)=" ).append( relCursorPos );
      // }
      // else
      // {
      // sb.append( "Time@1=" ).append( DisplayUtils.displayScaledTime(
      // absCursorPosA, sampleRate ) ).append( " | " );
      // sb.append( "Time@2=" ).append( DisplayUtils.displayScaledTime(
      // absCursorPosB, sampleRate ) );
      // sb.append( " (duration " ).append( DisplayUtils.displayScaledTime(
      // Math.abs( relCursorPos ), sampleRate ) );
      // if ( relCursorPos != 0 )
      // {
      // sb.append( ", " ).append( "frequency " );
      // final double frequency = Math.abs( ( double )sampleRate / ( double
      // )relCursorPos );
      // sb.append( DisplayUtils.displayFrequency( frequency ) );
      // }
      // sb.append( ")" );
      // }
      // }
      // else
      {
        // print origin status when no cursors used
        final long idxMouseDragX = convertPointToSampleIndex( new Point( aStartDragXpos, 0 ) );
        final long idxMouseX = convertPointToSampleIndex( aMousePosition );

        if ( aDragging && ( idxMouseDragX != idxMouseX ) )
        {
          final long relDrag = idxMouseDragX - idxMouseX;
          final long absMouseDragX = idxMouseDragX - triggerPosition;

          if ( !this.dataContainer.hasTimingData() )
          {
            sb.append( "Sample " ).append( absMouseDragX );
            sb.append( " (distance " ).append( relDrag ).append( ")" );
          }
          else
          {
            final double frequency = Math.abs( ( double )sampleRate / ( double )relDrag );

            sb.append( "Time " ).append( DisplayUtils.displayScaledTime( absMouseDragX, sampleRate ) );
            sb.append( " (duration " ).append( DisplayUtils.displayScaledTime( relDrag, sampleRate ) ).append( ", " );
            sb.append( "Frequency " ).append( DisplayUtils.displayFrequency( frequency ) ).append( ")" );
          }
        }
        else
        {
          final long absMouseX = idxMouseX - triggerPosition;
          if ( !this.dataContainer.hasTimingData() )
          {
            sb.append( "Sample " ).append( absMouseX );
          }
          else
          {
            sb.append( "Time " ).append( DisplayUtils.displayScaledTime( absMouseX, sampleRate ) );
          }
        }
      }
    }

    final String status = sb.toString();
    // System.out.println( "STATUS = " + status );
    setToolTipText( status );
  }

  /**
   * Zooms in by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  final void zoomIn( final Point aPoint )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    double newScale = this.scale;
    if ( newScale < MAX_SCALE )
    {
      setScale( newScale * 2.0 );
      updatePreferredSize( aPoint, 2.0 );
    }
  }

  /**
   * Zooms out by factor 2 and resizes the component accordingly.
   * 
   * @return <code>true</code> if the zoom action succeeded, <code>false</code>
   *         otherwise.
   */
  final void zoomOut( final Point aPoint )
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
      updatePreferredSize( aPoint, 0.5 );
    }
  }

  /**
   * Paints the diagram to the extend necessary.
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    if ( !this.dataContainer.hasCapturedData() )
    {
      return;
    }

    final long start = System.currentTimeMillis();
    final Graphics2D g2d = ( Graphics2D )aGraphics;

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = aGraphics.getClipBounds();

    final int cx = clipArea.x;
    final int cy = clipArea.y;
    final int cw = cx + clipArea.width;
    final int ch = cy + clipArea.height;

    // find index of first & last row that needs drawing
    final long firstRow = convertPointToSampleIndex( new Point( cx, 0 ) );
    final long lastRow = convertPointToSampleIndex( new Point( cw, 0 ) ) + 1;

    // paint portion of background that needs drawing
    g2d.setColor( getBackgroundColor() );
    g2d.fillRect( cx, cy, cw, ch );

    // draw trigger if existing and visible
    final long triggerPosition = this.dataContainer.getTriggerTimePosition();
    if ( ( triggerPosition >= firstRow ) && ( triggerPosition <= lastRow ) )
    {
      g2d.setColor( getTriggerColor() );
      g2d.fillRect( ( int )( triggerPosition * this.scale ) - 1, cy, ( int )( this.scale + 2 ), ch );
    }

    // draw all signal groups...
    drawSignals( g2d, clipArea, firstRow, lastRow );

    // draw cursors if enabled...
    drawCursors( g2d, firstRow, lastRow );

    if ( DEBUG )
    {
      final long end = System.currentTimeMillis();
      LOG.log( Level.INFO, "Render time = {0}ms.", ( end - start ) );
    }
  }

  /**
   * @return
   */
  private Dimension calculateNewDimension()
  {
    final int channels = this.dataContainer.getChannels();
    final int enabledChannels = this.dataContainer.getEnabledChannels();

    final int channelHeight = getChannelHeight();
    final int scopeHeight = getScopeHeight();

    int height = 0;
    for ( int group = 0; ( group < channels / 8 ) && ( group < 4 ); group++ )
    {
      if ( ( ( enabledChannels >> ( 8 * group ) ) & 0xff ) != 0 )
      {
        if ( isShowChannels( group ) )
        {
          height += channelHeight * 8;
        }
        if ( isShowScope( group ) )
        {
          height += scopeHeight;
        }
        if ( isShowByte( group ) )
        {
          height += channelHeight;
        }
      }
    }

    final int width = ( int )( this.scale * this.dataContainer.getAbsoluteLength() );

    final Dimension newDiagramSize = new Dimension( width, height );
    return newDiagramSize;
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
   * @param g2d
   * @param aFirstRow
   * @param aLastRow
   */
  private void drawCursors( final Graphics2D g2d, final long aFirstRow, final long aLastRow )
  {
    if ( this.dataContainer.isCursorsEnabled() )
    {
      for ( int i = 0, size = DataContainer.MAX_CURSORS; i < size; i++ )
      {
        final long cursorPosition = this.dataContainer.getCursorPosition( i );
        if ( ( cursorPosition >= aFirstRow ) && ( cursorPosition <= aLastRow ) )
        {
          final int cursorPos = ( int )( cursorPosition * this.scale );

          g2d.setColor( getCursorColor( i ) );
          g2d.drawLine( cursorPos, 0, cursorPos, 36 * getChannelHeight() );
        }
      }
    }
  }

  /**
   * @param g
   * @param clipArea
   * @param y
   */
  private void drawGridLine( final Graphics g, final Rectangle clipArea, final int y )
  {
    g.setColor( getGridColor() );
    g.drawLine( clipArea.x, y, clipArea.x + clipArea.width, y );
  }

  /**
   * Draws all signals, byte values and scopes.
   * 
   * @param aGraphics
   *          the canvas to paint on;
   * @param aClipArea
   *          the clip area to paint;
   * @param aFromIndex
   *          the first sample to paint;
   * @param aToIndex
   *          the last sample to paint.
   */
  private void drawSignals( final Graphics2D aGraphics, final Rectangle aClipArea, final long aFromIndex,
      final long aToIndex )
  {
    final int channels = this.dataContainer.getChannels();
    final int enabled = this.dataContainer.getEnabledChannels();
    final long[] timestamps = this.dataContainer.getTimestamps();
    final int[] values = this.dataContainer.getValues();

    final int channelHeight = getChannelHeight();
    final int signalHeight = getSignalHeight();
    final int scopeHeight = getScopeHeight();

    final double scopeScaleFactor = ( 256.0 / ( scopeHeight - 2 * PADDING_Y ) );

    final int center = ( int )( this.scale / 2.0 );

    final FontMetrics fm = aGraphics.getFontMetrics();
    final int labelYpos = ( int )( channelHeight - ( fm.getHeight() / 2.0 ) + 1 );

    final int n = 2 * timestamps.length;

    // Search the first sample index the is right before the to-be-displayed
    // from index...
    int dataStartIndex = 0;
    do
    {
      if ( timestamps[dataStartIndex] >= aFromIndex )
      {
        // Found it; use this as starting time-index...
        dataStartIndex = Math.max( 0, dataStartIndex - 1 );
        break;
      }
      dataStartIndex++;
    }
    while ( dataStartIndex < timestamps.length );

    int bofs = 0;

    for ( int block = 0; ( block < channels / 8 ) && ( block < 4 ); block++ )
    {
      final boolean blockEnabled = ( ( enabled >> ( 8 * block ) ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( isShowChannels( block ) )
      {
        final SignalPolyline polyline = new SignalPolyline( n );

        // draw actual data
        for ( int bit = 0; bit < 8; bit++ )
        {
          final int channelIdx = 8 * block + bit;
          long currentSample = aFromIndex - 1;
          int pIdx = 0;

          int dataIndex = dataStartIndex;
          while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
          {
            final long nextSample;

            final int currentValue = ( values[dataIndex] >> channelIdx ) & 0x01;
            if ( dataIndex >= values.length - 1 )
            {
              nextSample = aToIndex + 1;
            }
            else
            {
              nextSample = timestamps[dataIndex + 1];
            }

            // Calculate display coordinates...
            final int x1 = ( int )( ( this.scale * currentSample ) - center );
            final int x2 = ( int )( ( this.scale * ( nextSample - 1 ) ) + center );
            final int y1 = bofs + channelHeight * bit + signalHeight * ( 1 - currentValue ) + 1;

            polyline.x[pIdx] = x1;
            polyline.y[pIdx] = y1;
            polyline.x[pIdx + 1] = x2;
            polyline.y[pIdx + 1] = y1;
            pIdx += 2;

            // Update loop administration...
            dataIndex++;
            currentSample = nextSample;
          }

          aGraphics.setColor( getSignalColor( channelIdx ) );
          aGraphics.drawPolyline( polyline.x, polyline.y, pIdx );

          // XXX XXX
          final Color oldColor = aGraphics.getColor();
          final Paint oldPaint = aGraphics.getPaint();
          final Stroke oldStroke = aGraphics.getStroke();
          final Composite oldComposite = aGraphics.getComposite();
          final Object oldHint = aGraphics.getRenderingHint( RenderingHints.KEY_ANTIALIASING );
          aGraphics.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

          final Iterator<ChannelAnnotation> annotations = this.dataContainer.getChannelAnnotations( channelIdx,
              dataStartIndex, timestamps.length );
          while ( annotations.hasNext() )
          {
            final ChannelAnnotation annotation = annotations.next();

            final long startIdx = timestamps[annotation.getStartIndex()];
            final long endIdx = timestamps[annotation.getEndIndex()];
            final String data = annotation.getData() != null ? String.valueOf( annotation.getData() ) : "";

            final int x1 = ( int )( this.scale * startIdx );
            final int x2 = ( int )( this.scale * endIdx );
            final int y1 = bofs + channelHeight * channelIdx;

            final int textXoffset = ( int )( ( x2 - x1 - fm.stringWidth( data ) ) / 2.0 );

            final Color newColor = getSignalColor( channelIdx );
            final Color newBrighterColor = newColor.brighter();
            final Color newDarkerColor = newColor.darker().darker();
            final GradientPaint newPaint = new GradientPaint( x1, y1 - 5, newBrighterColor, x1, y1
                + ( signalHeight / 2 ), newDarkerColor );

            final int annotationWidth = ( x2 - x1 );
            final int annotationHeight = ( signalHeight - 6 );
            final int arc = ( int )( annotationHeight / 2.0 );

            aGraphics.setComposite( AlphaComposite.SrcOver.derive( 0.75f ) );
            aGraphics.setStroke( SOLID_THICK );

            aGraphics.setPaint( newPaint );
            aGraphics.fillRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            aGraphics.setColor( newBrighterColor );
            aGraphics.drawRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            if ( textXoffset > 0 )
            {
              aGraphics.setColor( Color.WHITE );
              aGraphics.setComposite( oldComposite );

              aGraphics.drawString( data, x1 + textXoffset, y1 + labelYpos - 4 );
            }
          }

          aGraphics.setRenderingHint( RenderingHints.KEY_ANTIALIASING, oldHint );
          aGraphics.setPaint( oldPaint );
          aGraphics.setComposite( oldComposite );
          aGraphics.setStroke( oldStroke );
          aGraphics.setColor( oldColor );
          // XXX XXX

          drawGridLine( aGraphics, aClipArea, channelHeight * bit + bofs + ( channelHeight - 1 ) );
        }
        bofs += ( channelHeight * 8 );
      }

      if ( isShowScope( block ) )
      {
        final SignalPolyline scopePolyline = new SignalPolyline( n );

        int pIdx = 0;

        int val = bofs;
        int dataIndex = dataStartIndex;

        while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
        {
          val = ( int )( ( 0xff - ( ( values[dataIndex] >> ( 8 * block ) ) & 0xff ) ) / scopeScaleFactor );

          scopePolyline.x[pIdx] = ( int )( timestamps[dataIndex] * this.scale );
          scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
          pIdx++;

          dataIndex++;
        }

        scopePolyline.x[pIdx] = ( int )( aToIndex * this.scale );
        scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
        pIdx++;

        // draw actual data
        aGraphics.setColor( this.signalColor ); // XXX
        aGraphics.drawPolyline( scopePolyline.x, scopePolyline.y, pIdx );
        bofs += scopeHeight;
        // draw bottom grid line
        drawGridLine( aGraphics, aClipArea, bofs );
      }

      if ( isShowByte( block ) )
      {
        final ByteValuePolyline bytePolyline = new ByteValuePolyline( n );

        long currentSample = aFromIndex - 1;
        int pIdx = 0;

        int dataIndex = dataStartIndex;

        while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
        {
          final long nextSample;

          final int currentValue = ( values[dataIndex] >> ( 8 * block ) ) & 0xff;
          if ( dataIndex >= values.length - 1 )
          {
            nextSample = aToIndex + 1;
          }
          else
          {
            nextSample = timestamps[dataIndex + 1];
          }

          // Calculate display coordinates...
          final int x1 = ( int )( this.scale * currentSample - center );
          final int x2 = ( int )( this.scale * ( nextSample - 1 ) + center );

          final int bit = ( dataIndex % 2 );
          final int y1 = bofs + signalHeight * ( 1 - bit );
          final int y2 = bofs + signalHeight * bit;

          bytePolyline.x[pIdx] = x1;
          bytePolyline.y1[pIdx] = y1;
          bytePolyline.y2[pIdx] = y2;
          bytePolyline.x[pIdx + 1] = x2;
          bytePolyline.y1[pIdx + 1] = y1;
          bytePolyline.y2[pIdx + 1] = y2;
          pIdx += 2;

          // if steady long enough, add hex value
          final String byteValue = String.format( "%02X", currentValue );
          final int labelWidth = fm.stringWidth( byteValue );

          if ( ( x2 - x1 ) > labelWidth )
          {
            final int labelXpos = ( int )( ( x1 + x2 - labelWidth ) / 2.0 );

            aGraphics.drawString( byteValue, labelXpos, bofs + labelYpos );
          }

          // Update loop administration...
          dataIndex++;
          currentSample = nextSample;
        }

        aGraphics.setColor( this.signalColor ); // XXX
        aGraphics.drawPolyline( bytePolyline.x, bytePolyline.y1, pIdx );
        aGraphics.drawPolyline( bytePolyline.x, bytePolyline.y2, pIdx );
        bofs += channelHeight;
        // draw bottom grid line
        drawGridLine( aGraphics, aClipArea, bofs );
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
   * @param aI
   * @param aFreq1
   * @param aFreq2
   * @param aFreq3
   * @param aPhase1
   * @param aPhase2
   * @param aPhase3
   * @return
   */
  private Color makeColorGradient( final int aI, final double aFreq1, final double aFreq2, final double aFreq3,
      final double aPhase1, final double aPhase2, final double aPhase3 )
  {
    final int width = 127;
    final int center = 128;
    final int red = ( int )( Math.sin( aFreq1 * aI + aPhase1 ) * width + center );
    final int grn = ( int )( Math.sin( aFreq2 * aI + aPhase2 ) * width + center );
    final int blu = ( int )( Math.sin( aFreq3 * aI + aPhase3 ) * width + center );
    return new Color( red, grn, blu );
  }

  /**
   * @return
   */
  private Color[] makeColorPalette( final int aLength, final int aSteps )
  {
    final Color[] result = new Color[aLength];
    final double freq = 2 * Math.PI / aSteps;
    for ( int i = 0; i < result.length; i++ )
    {
      result[i] = makeColorGradient( i, freq, freq, freq, 0.0, 2.0, 4.0 );
    }
    return result;
  }

  /**
   * @return
   */
  private Color[] makeMonochromaticColorPalette( final int aLength )
  {
    final Color[] result = new Color[aLength];
    Arrays.fill( result, this.signalColor );
    return result;
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
    this.timeLine.setScale( aScale );
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
