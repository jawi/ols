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
import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.util.*;


/**
 * This component displays a diagram which is obtained from a {@link CapturedData} object.
 * The settings for the diagram are obtained from the embedded {@link DiagramSettingsDialog} and
 * {@link DiagramLabelsDialog} objects.
 * Look there for an overview of ways to display data.
 * <p>
 * Component size changes with the size of the diagram. Therefore it should only be used from within a JScrollPane.
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public final class Diagram extends JComponent implements Configurable, Scrollable
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
    public final int   n;

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
     * Handles mouse dragged events and produces status change "events" accordingly.
     */
    @Override
    public void mouseDragged( final MouseEvent aEvent )
    {
      final int mouseXpos = aEvent.getX();
      final int mouseYpos = aEvent.getY();

      if ( this.currentCursor >= 0 )
      {
        dragCursor( this.currentCursor, mouseXpos );
      }

      updateStatus( mouseXpos, mouseYpos, true /* aDragging */, this.startDragXpos );
    }

    /**
     * Handles mouse moved events and produces status change "events" accordingly.
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final int mouseXpos = aEvent.getX();
      final int mouseYpos = aEvent.getY();

      final int cursorIdx = getCursorHover( mouseXpos );
      if ( cursorIdx >= 0 )
      {
        this.currentCursor = cursorIdx;
        this.startDragXpos = mouseXpos;
        Diagram.this.setCursor( Diagram.this.cursorDrag );
      }
      else
      {
        this.currentCursor = -1;
        this.startDragXpos = -1;
        Diagram.this.setCursor( Diagram.this.cursorDefault );
      }

      updateStatus( mouseXpos, mouseYpos, false /* aDragging */, this.startDragXpos );
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
    public final int   n;

    public SignalPolyline( final int aN )
    {
      this.n = aN;
      this.x = new int[aN];
      this.y = new int[aN];
    }
  }

  // CONSTANTS

  private static final long           serialVersionUID = 1L;

  private static final Logger         LOG              = Logger.getLogger( Diagram.class.getName() );

  static final double                 MAX_SCALE        = 10;
  static final double                 CURSOR_HOVER     = 5.0;
  static final int                    PADDING_Y        = 2;

  // VARIABLES

  private volatile CapturedData       capturedData;

  private final DiagramTimeLine       timeLine;
  private final DiagramRowLabels      rowLabels;
  private final DiagramSettingsDialog settings;
  private final DiagramLabelsDialog   labels;
  private final ActionProvider        actionProvider;
  private double                      scale;
  private int                         timeDivider;
  private int                         pageLen;
  private final Cursor                cursorDefault;
  private final Cursor                cursorDrag;
  private final EventListenerList     evenListeners;
  private final JPopupMenu            contextMenu;

  private int                         newCursorPosition;

  /**
   * Create a new empty diagram to be placed in a container.
   */
  public Diagram( final ActionProvider aActionProvider )
  {
    super();

    this.actionProvider = aActionProvider;

    this.contextMenu = new JPopupMenu();
    for ( int i = 0; i < 10; i++ )
    {
      this.contextMenu.add( new JMenuItem( new SetCursorAction( this, i ) ) );
    }

    this.capturedData = null;
    this.settings = new DiagramSettingsDialog();
    this.labels = new DiagramLabelsDialog();

    this.cursorDefault = getCursor();
    this.cursorDrag = new Cursor( Cursor.MOVE_CURSOR );
    this.evenListeners = new EventListenerList();

    this.timeDivider = 1;
    this.pageLen = 0;

    this.rowLabels = new DiagramRowLabels();
    this.rowLabels.setDiagramSettings( this.settings );

    this.timeLine = new DiagramTimeLine();
    this.timeLine.setDiagramSettings( this.settings );

    setMinimumSize( new Dimension( 25, 1 ) );
    setBackground( this.settings.getBackgroundColor() );

    addCursorChangeListener( this.timeLine );

    final MouseListener mouseListener = new MouseListener();

    addMouseListener( mouseListener );
    addMouseMotionListener( mouseListener );
  }

  /**
   * @param aListener
   */
  public void addCursorChangeListener( final DiagramCursorChangeListener aListener )
  {
    this.evenListeners.add( DiagramCursorChangeListener.class, aListener );
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
   * Returns the captured data object currently displayed in the diagram.
   * 
   * @return diagram's current captured data
   */
  public CapturedData getCapturedData()
  {
    return ( this.capturedData );
  }

  /**
   * get current cursor mode
   */
  public boolean getCursorMode()
  {
    if ( hasCapturedData() )
    {
      return this.capturedData.cursorEnabled;
    }
    else
    {
      return false;
    }
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
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(java.awt.Rectangle, int, int)
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
      return aVisibleRect.height - this.settings.getChannelHeight();
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
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(java.awt.Rectangle, int, int)
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
      maxUnitIncrement = this.settings.getChannelHeight();
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
   * Returns the current diagram settings.
   * 
   * @return the diagram settings, never <code>null</code>.
   */
  public final DiagramSettingsDialog getSettings()
  {
    return this.settings;
  }

  /**
   * calulate the position within a window (pane) based on current page and zoom settings
   * 
   * @param width
   *          window width
   * @param pos
   *          sample position
   * @return current position within window
   */
  public int getTargetPosition( final int width, long pos )
  {
    if ( pos < 0 )
    {
      pos = 0;
    }
    return ( int )( ( double )pos * ( double )width * this.scale / this.pageLen );
  }

  /**
   * Returns wheter or not the diagram has any data.
   * 
   * @return <code>true</code> if captured data exists, <code>false</code> otherwise
   */
  public boolean hasCapturedData()
  {
    return ( this.capturedData != null );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.util.Properties)
   */
  public void readProperties( final Properties properties )
  {
    this.settings.readProperties( properties );
    this.labels.readProperties( properties );
    resize();
  }

  /**
   * @param aListener
   */
  public void removeCursorChangeListener( final DiagramCursorChangeListener aListener )
  {
    this.evenListeners.remove( DiagramCursorChangeListener.class, aListener );
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
   * Sets the captured data object to use for drawing the diagram.
   * 
   * @param capturedData
   *          captured data to base diagram on
   */
  public void setCapturedData( final CapturedData capturedData )
  {
    this.capturedData = capturedData;
    // Update row & labels...
    this.timeLine.setCapturedData( capturedData );
    this.rowLabels.setCapturedData( capturedData );

    // reset zoom, etc.
    setScale( MAX_SCALE );
    this.timeDivider = 1;
    this.pageLen = 0;

    // show data
    zoomDefault();
  }

  /**
   * Enable/Disable diagram cursors
   */
  public void setCursorMode( final boolean aEnabled )
  {
    if ( hasCapturedData() )
    {
      // Update the cursor actions accordingly...
      getAction( GotoCursor1Action.ID ).setEnabled( aEnabled );
      getAction( GotoCursor2Action.ID ).setEnabled( aEnabled );

      // Update the cursor state of the contained data...
      this.capturedData.cursorEnabled = aEnabled;
    }
  }

  /**
   * @param aCursorIdx
   */
  public void setCursorPosition( final int aCursorIdx )
  {
    internalSetCursorPosition( aCursorIdx, this.newCursorPosition );
  }

  /**
   * Sets the scale to the given value.
   * 
   * @param aScale
   *          the scale to set, cannot be <code>null</code>.
   */
  public void setScale( final double aScale )
  {
    this.scale = aScale;
    this.timeLine.setScale( aScale );
  }

  /**
   * Display the diagram labels dialog.
   * Will block until the dialog is closed again.
   */
  public void showLabelsDialog( final Window frame )
  {
    if ( this.labels.showDialog( frame ) == DiagramLabelsDialog.OK )
    {
      this.rowLabels.setDiagramLabels( this.labels.getDiagramLabels() );

      resize();
    }
  }

  /**
   * Display the diagram settings dialog.
   * Will block until the dialog is closed again.
   */
  public void showSettingsDialog( final Window frame )
  {
    if ( this.settings.showDialog( frame ) == DiagramSettingsDialog.OK )
    {
      this.rowLabels.setDiagramSettings( this.settings );
      this.timeLine.setDiagramSettings( this.settings );

      resize();
    }
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.util.Properties)
   */
  public void writeProperties( final Properties properties )
  {
    this.settings.writeProperties( properties );
    this.labels.writeProperties( properties );
  }

  /**
   * Reverts back to the standard zoom level.
   */
  public void zoomDefault()
  {
    setScale( MAX_SCALE );
    calculatePages();
    resize();
  }

  /**
   * Zooms to fitting the view on Display.
   */
  public void zoomFit()
  {
    final Window owner = SwingUtilities.getWindowAncestor( this );
    final int width = Math.max( 1, ( owner.getWidth() - ( this.rowLabels.getWidth() + 20 ) ) );

    // avoid null pointer exception when no data available
    if ( this.capturedData == null )
    {
      return;
    }

    if ( this.capturedData.absoluteLength > 0 )
    {
      setScale( ( double )width / ( double )this.capturedData.absoluteLength );
    }
    else
    {
      setScale( MAX_SCALE );
    }

    calculatePages();
    resize();
  }

  /**
   * Zooms in by factor 2 and resizes the component accordingly.
   */
  public void zoomIn()
  {
    double newScale = this.scale;
    if ( newScale < MAX_SCALE )
    {
      newScale = Math.min( MAX_SCALE, newScale * 2.0 );
      setScale( newScale );
      calculatePages();
      resize();
    }
  }

  /**
   * Zooms out by factor 2 and resizes the component accordingly.
   */
  public void zoomOut()
  {
    final double newScale = this.scale / 2.0;
    setScale( newScale );
    calculatePages();
    resize();
  }

  /**
   * Update status information.
   * Notifies {@link StatusChangeListener}.
   * 
   * @param aDragging
   *          <code>true</code> indicates that dragging information should be added
   */
  final void updateStatus( final int aMouseXpos, final int aMouseYpos, final boolean aDragging, final int aStartDragXpos )
  {
    if ( this.capturedData == null )
    {
      return;
    }

    final StringBuffer sb = new StringBuffer( " " );

    final int row = aMouseYpos / this.settings.getChannelHeight();
    if ( row <= this.capturedData.channels + ( this.capturedData.channels / 9 ) )
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

    final int captureRate = this.capturedData.rate;
    final long triggerPosition = this.capturedData.triggerPosition;

    if ( this.capturedData.cursorEnabled )
    {
      // print cursor data to status line
      final long absCursorPosA = this.capturedData.getCursorPosition( 1 ) - triggerPosition;
      final long absCursorPosB = this.capturedData.getCursorPosition( 2 ) - triggerPosition;
      final long relCursorPos = this.capturedData.getCursorPosition( 1 ) - this.capturedData.getCursorPosition( 2 );

      if ( !this.capturedData.hasTimingData() )
      {
        sb.append( "Sample@A=" ).append( absCursorPosA ).append( " | " );
        sb.append( "Sample@B=" ).append( absCursorPosB ).append( " | " );
        sb.append( "Distance(A,B)=" ).append( relCursorPos );
      }
      else
      {
        sb.append( "Time@A=" ).append( DisplayUtils.displayScaledTime( absCursorPosA, captureRate ) ).append( " | " );
        sb.append( "Time@B=" ).append( DisplayUtils.displayScaledTime( absCursorPosB, captureRate ) );
        sb.append( " (duration " ).append( DisplayUtils.displayScaledTime( Math.abs( relCursorPos ), captureRate ) );
        if ( relCursorPos != 0 )
        {
          sb.append( ", " ).append( "frequency " );
          final double frequency = Math.abs( ( double )captureRate / ( double )relCursorPos );
          sb.append( DisplayUtils.displayFrequency( frequency ) );
        }
        sb.append( ")" );
      }
    }
    else
    {
      // print origin status when no cursors used
      final long idxMouseDragX = xToIndex( aStartDragXpos );
      final long idxMouseX = xToIndex( aMouseXpos );

      if ( aDragging && ( idxMouseDragX != idxMouseX ) )
      {
        final long relDrag = idxMouseDragX - idxMouseX;
        final long absMouseDragX = idxMouseDragX - triggerPosition;

        if ( !this.capturedData.hasTimingData() )
        {
          sb.append( "Sample " ).append( absMouseDragX );
          sb.append( " (distance " ).append( relDrag ).append( ")" );
        }
        else
        {
          final double frequency = Math.abs( ( double )captureRate / ( double )relDrag );

          sb.append( "Time " ).append( DisplayUtils.displayScaledTime( absMouseDragX, captureRate ) );
          sb.append( " (duration " ).append( DisplayUtils.displayScaledTime( relDrag, captureRate ) ).append( ", " );
          sb.append( "Frequency " ).append( DisplayUtils.displayFrequency( frequency ) ).append( ")" );
        }
      }
      else
      {
        final long absMouseX = idxMouseX - triggerPosition;
        if ( !this.capturedData.hasTimingData() )
        {
          sb.append( "Sample " ).append( absMouseX );
        }
        else
        {
          sb.append( "Time " ).append( DisplayUtils.displayScaledTime( absMouseX, captureRate ) );
        }
      }
    }

    final String status = sb.toString();
    // System.out.println( "STATUS = " + status );
    setToolTipText( status );
  }

  /**
   * Moves the cursor with a given index to a given mouse X position.
   * 
   * @param aCursorIdx
   *          the cursor index to move, should be >= 0;
   * @param aMouseXpos
   *          the new X position of the cursor.
   */
  protected final void dragCursor( final int aCursorIdx, final int aMouseXpos )
  {
    if ( ( this.capturedData == null ) || !this.capturedData.cursorEnabled )
    {
      return;
    }

    internalSetCursorPosition( aCursorIdx, aMouseXpos );
  }

  /**
   * @param aID
   * @return
   */
  protected final Action getAction( final String aID )
  {
    return this.actionProvider.getAction( aID );
  }

  /**
   * Determines whether the given mouse X position is actually in the vicinity of a cursor.
   * 
   * @param aMouseXpos
   *          the mouse X position to test.
   * @return the index of the cursor the given mouse position is near, or -1 if there's no cursor set or nowhere near a
   *         cursor.
   */
  protected int getCursorHover( final int aMouseXpos )
  {
    final long idx = xToIndex( aMouseXpos );
    final double threshold = CURSOR_HOVER / this.scale;
    for ( int i = 0; i < this.capturedData.cursorPositions.length; i++ )
    {
      final long cursorPosition = this.capturedData.getCursorPosition( i );
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
   * Paints the diagram to the extend necessary.
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    if ( this.capturedData == null )
    {
      return;
    }

    final long start = System.currentTimeMillis();

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = aGraphics.getClipBounds();

    final int cx = clipArea.x;
    final int cy = clipArea.y;
    final int cw = cx + clipArea.width;
    final int ch = cy + clipArea.height;

    // find index of first & last row that needs drawing
    final long firstRow = xToIndex( cx );
    final long lastRow = xToIndex( cw ) + 1;
    final long visibleSamples = lastRow - firstRow;

    // calculate time divider for samplecount > 2^31-1
    this.timeDivider = 1;
    while ( ( visibleSamples / this.timeDivider ) >= Integer.MAX_VALUE )
    {
      this.timeDivider++;
    }

    // paint portion of background that needs drawing
    aGraphics.setColor( this.settings.getBackgroundColor() );
    aGraphics.fillRect( cx, cy, cw, ch );

    // draw trigger if existing and visible
    final long triggerPosition = getTriggerPosition();
    if ( ( triggerPosition >= firstRow ) && ( triggerPosition <= lastRow ) )
    {
      aGraphics.setColor( this.settings.getTriggerColor() );
      aGraphics.fillRect( ( int )( triggerPosition * this.scale ) - 1, cy, ( int )( this.scale + 2 ), ch );
    }

    // draw all signal groups...
    drawSignals( aGraphics, clipArea, firstRow, lastRow );

    // draw cursors if enabled
    if ( getCursorMode() )
    {
      for ( int i = 0, size = this.capturedData.cursorPositions.length; i < size; i++ )
      {
        final long cursorPosition = this.capturedData.cursorPositions[i];
        if ( ( cursorPosition >= firstRow ) && ( cursorPosition <= lastRow ) )
        {
          final int cursorPos = ( int )( cursorPosition * this.scale );

          aGraphics.setColor( this.settings.getCursorColor( i ) );
          aGraphics.drawLine( cursorPos, 0, cursorPos, 36 * this.settings.getChannelHeight() );
        }
      }
    }

    final long end = System.currentTimeMillis();
    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "Render time = " + ( end - start ) + " ms." );
    }
  }

  /**
   * @param aEvent
   */
  protected final void showContextMenu( final Point aPosition )
  {
    if ( this.capturedData != null )
    {
      // Implicitly enable cursor mode, the user already had made its
      // intensions clear that he want to have this by opening up the
      // context menu anyway...
      setCursorMode( true );

      this.newCursorPosition = aPosition.x;

      this.contextMenu.show( this, aPosition.x, aPosition.y );
    }
  }

  /**
   * @param pos
   * @return
   */
  private int calcTmpPos( final long pos )
  {
    long lval = ( long )( pos * this.timeDivider * this.scale );
    if ( lval >= Integer.MAX_VALUE )
    {
      lval = Integer.MAX_VALUE - 100;
    }
    if ( lval < 0 )
    {
      lval = 0;
    }
    return ( int )lval;
  }

  /**
   * calculate number and size of pages for various zoom levels.
   */
  private void calculatePages()
  {
    if ( this.capturedData != null )
    {
      final double maxAvailableWidth = Integer.MAX_VALUE - 100;
      final double currentScaledSize = ( long )( this.scale * this.capturedData.absoluteLength );
      if ( currentScaledSize > maxAvailableWidth )
      {
        final int maxPages = ( int )Math.ceil( currentScaledSize / maxAvailableWidth );
        this.pageLen = ( int )( this.scale * this.capturedData.absoluteLength / maxPages );
      }
      else
      {
        this.pageLen = ( int )( this.scale * this.capturedData.absoluteLength );
      }
    }
    else
    {
      this.pageLen = 0;
    }
    // System.out.println( "pageLen=" + this.pageLen );
  }

  /**
   * If this component is the <code>viewportView</code> of an enclosing <code>JScrollPane</code> (the usual situation),
   * configure this <code>ScrollPane</code> by, amongst other things,
   * installing the diagram's <code>timeline</code> as the <code>columnHeaderView</code> of the scroll pane.
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
   * @param g
   * @param clipArea
   * @param y
   */
  private void drawGridLine( final Graphics g, final Rectangle clipArea, final int y )
  {
    g.setColor( this.settings.getGridColor() );
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
  private void drawSignals( final Graphics aGraphics, final Rectangle aClipArea, long aFromIndex, long aToIndex )
  {
    final int channels = this.capturedData.channels;
    final int enabled = this.capturedData.enabledChannels;
    final long[] time = this.capturedData.timestamps;
    final int[] data = this.capturedData.values;

    final int channelHeight = this.settings.getChannelHeight();
    final int signalHeight = this.settings.getSignalHeight();
    final int scopeHeight = this.settings.getScopeHeight();
    final double scopeScaleFactor = ( 256.0 / ( scopeHeight - 2 * PADDING_Y ) );
    final double correctedScale = this.scale * this.timeDivider;
    final int center = ( int )( correctedScale / 2.0 );

    final FontMetrics fm = aGraphics.getFontMetrics();
    final int labelYpos = ( int )( channelHeight - ( fm.getHeight() / 2.0 ) + 1 );

    final int n = 2 * time.length;
    final ByteValuePolyline bytePolyline = new ByteValuePolyline( n );
    final SignalPolyline scopePolyline = new SignalPolyline( n );
    final SignalPolyline polyline = new SignalPolyline( n );

    // Search the first sample index the is right before the to-be-displayed from index...
    int dataStartIndex = 0;
    do
    {
      if ( time[dataStartIndex] >= aFromIndex )
      {
        // Found it; use this as starting time-index...
        dataStartIndex = Math.max( 0, dataStartIndex - 1 );
        break;
      }
      dataStartIndex++;
    }
    while ( dataStartIndex < time.length );

    aFromIndex /= this.timeDivider;
    aToIndex /= this.timeDivider;

    int bofs = 0;
    // drawGridLine( aGraphics, aClipArea, bofs++ );

    for ( int block = 0; ( block < channels / 8 ) && ( block < 4 ); block++ )
    {
      final boolean blockEnabled = ( ( enabled >> ( 8 * block ) ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( this.settings.isShowChannels( block ) )
      {
        // draw actual data
        for ( int bit = 0; bit < 8; bit++ )
        {
          long currentSample = aFromIndex - 1;
          int pIdx = 0;

          int dataIndex = dataStartIndex;
          while ( ( dataIndex < data.length ) && ( time[dataIndex] <= aToIndex ) )
          {
            final long nextSample;

            final int currentValue = ( data[dataIndex] >> 8 * block + bit ) & 0x01;
            if ( dataIndex >= data.length - 1 )
            {
              nextSample = aToIndex + 1;
            }
            else
            {
              nextSample = time[dataIndex + 1] / this.timeDivider;
            }

            // Calculate display coordinates...
            final int x1 = ( int )( ( correctedScale * currentSample ) - center );
            final int x2 = ( int )( ( correctedScale * ( nextSample - 1 ) ) + center );
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

          aGraphics.setColor( this.settings.getSignalColor() );
          aGraphics.drawPolyline( polyline.x, polyline.y, pIdx );
          drawGridLine( aGraphics, aClipArea, channelHeight * bit + bofs + ( channelHeight - 1 ) );
        }
        bofs += ( channelHeight * 8 );
      }

      if ( this.settings.isShowScope( block ) )
      {
        int pIdx = 0;

        int val = bofs;
        long pos = aFromIndex - 1;
        int dataIndex = dataStartIndex;

        while ( ( dataIndex < data.length ) && ( time[dataIndex] <= aToIndex ) )
        {
          pos = time[dataIndex] / this.timeDivider;
          val = ( int )( ( 0xff - ( ( data[dataIndex] >> ( 8 * block ) ) & 0xff ) ) / scopeScaleFactor );

          scopePolyline.x[pIdx] = calcTmpPos( pos );
          scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
          pIdx++;

          dataIndex++;
        }

        scopePolyline.x[pIdx] = calcTmpPos( aToIndex );
        scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
        pIdx++;

        // draw actual data
        aGraphics.setColor( this.settings.getSignalColor() );
        aGraphics.drawPolyline( scopePolyline.x, scopePolyline.y, pIdx );
        bofs += scopeHeight;
        // draw bottom grid line
        drawGridLine( aGraphics, aClipArea, bofs );
      }

      if ( this.settings.isShowByte( block ) )
      {
        long currentSample = aFromIndex - 1;
        int pIdx = 0;

        aGraphics.setColor( this.settings.getSignalColor() );

        int dataIndex = dataStartIndex;

        while ( ( dataIndex < data.length ) && ( time[dataIndex] <= aToIndex ) )
        {
          final long nextSample;

          final int currentValue = ( data[dataIndex] >> ( 8 * block ) ) & 0xff;
          if ( dataIndex >= data.length - 1 )
          {
            nextSample = aToIndex + 1;
          }
          else
          {
            nextSample = time[dataIndex + 1] / this.timeDivider;
          }

          // Calculate display coordinates...
          final int x1 = ( int )( correctedScale * currentSample - center );
          final int x2 = ( int )( correctedScale * ( nextSample - 1 ) + center );

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

        aGraphics.setColor( this.settings.getSignalColor() );
        aGraphics.drawPolyline( bytePolyline.x, bytePolyline.y1, pIdx );
        aGraphics.drawPolyline( bytePolyline.x, bytePolyline.y2, pIdx );
        bofs += channelHeight;
        // draw bottom grid line
        drawGridLine( aGraphics, aClipArea, bofs );
      }
    }
  }

  /**
   * @param aCursorIdx
   * @param aMouseXpos
   */
  private void fireCursorChangedEvent( final int aCursorIdx, final int aMouseXpos )
  {
    final DiagramCursorChangeListener[] listeners = this.evenListeners.getListeners( DiagramCursorChangeListener.class );
    for ( final DiagramCursorChangeListener listener : listeners )
    {
      listener.cursorChanged( aCursorIdx, aMouseXpos );
    }
  }

  /**
   * Returns the current trigger position, if available/applicable.
   * 
   * @return the current trigger position (index value), or -1L in case no captured data is available or no trigger is
   *         set.
   */
  private long getTriggerPosition()
  {
    if ( this.capturedData == null )
    {
      return -1L;
    }
    return this.capturedData.hasTriggerData() ? this.capturedData.triggerPosition : -1L;
  }

  /**
   * Sets the actual cursor position.
   * 
   * @param aCursorIdx the index of the cursor to set;
   * @param aMouseXpos the new X-position (of the mouse) representing the new cursor position.
   */
  private void internalSetCursorPosition( final int aCursorIdx, final int aMouseXpos )
  {
    final long index = xToIndex( aMouseXpos );

    // notify cursor change listeners
    if ( ( index > 0 ) && ( index < ( this.capturedData.absoluteLength - 1 ) ) )
    {
      this.capturedData.setCursorPosition( aCursorIdx, index );

      fireCursorChangedEvent( aCursorIdx, aMouseXpos );

      repaint();
    }
  }

  /**
   * Resizes the diagram as required by available data and scaling factor.
   */
  private void resize()
  {
    if ( this.capturedData == null )
    {
      return;
    }

    final int channelHeight = this.settings.getChannelHeight();
    final int scopeHeight = this.settings.getScopeHeight();

    int height = 0;
    for ( int group = 0; ( group < this.capturedData.channels / 8 ) && ( group < 4 ); group++ )
    {
      if ( ( ( this.capturedData.enabledChannels >> ( 8 * group ) ) & 0xff ) != 0 )
      {
        if ( this.settings.isShowChannels( group ) )
        {
          height += channelHeight * 8;
        }
        if ( this.settings.isShowScope( group ) )
        {
          height += scopeHeight;
        }
        if ( this.settings.isShowByte( group ) )
        {
          height += channelHeight;
        }
      }
    }

    final int width = ( int )( this.scale * this.capturedData.absoluteLength );

    final Dimension newDiagramSize = new Dimension( width, height );
    if ( !getPreferredSize().equals( newDiagramSize ) )
    {
      this.timeLine.setPreferredSize( newDiagramSize );
      this.timeLine.revalidate();

      this.rowLabels.setPreferredSize( newDiagramSize );
      this.rowLabels.revalidate();

      setPreferredSize( newDiagramSize );
      revalidate();
    }

    repaint();
  }

  /**
   * Reverses the effect of <code>configureEnclosingScrollPane</code> by replacing the <code>columnHeaderView</code> of
   * the enclosing
   * scroll pane with <code>null</code>.
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
   * Convert x position to sample index.
   * 
   * @param x
   *          horizontal position in pixels
   * @return sample index
   */
  private long xToIndex( final int x )
  {
    long index = ( long )( x / this.scale );
    if ( index < 0 )
    {
      index = 0;
    }
    if ( index >= this.capturedData.absoluteLength )
    {
      index = this.capturedData.absoluteLength - 1;
    }
    return ( index );
  }
}
