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
public final class Diagram extends JComponent implements Configurable, Scrollable, DiagramSettings
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
     * Handles mouse dragged events and produces status change "events"
     * accordingly.
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

      updateStatus( mouseXpos, mouseYpos, true /* aDragging */, this.startDragXpos, null );
    }

    /**
     * Handles mouse moved events and produces status change "events"
     * accordingly.
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final int mouseXpos = aEvent.getX();
      final int mouseYpos = aEvent.getY();

      final int channel = ( mouseYpos / getChannelHeight() );
      final ChannelAnnotation annotation = getAnnotationHover( channel, mouseXpos );

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

      updateStatus( mouseXpos, mouseYpos, false /* aDragging */, this.startDragXpos, annotation );
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

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( Diagram.class.getName() );

  static final double MAX_SCALE = 15;
  static final double CURSOR_HOVER = 5.0;
  static final int PADDING_Y = 2;

  private static final Stroke SOLID_NORMAL = new BasicStroke( 0.0f );
  private static final Stroke SOLID_THICK = new BasicStroke( 1.5f );

  // VARIABLES

  private final AnnotatedData data;

  private final DiagramTimeLine timeLine;
  private final DiagramRowLabels rowLabels;
  private final ActionProvider actionProvider;
  private double scale;
  private final Cursor cursorDefault;
  private final Cursor cursorDrag;
  private final EventListenerList evenListeners;
  private final JPopupMenu contextMenu;
  private int newCursorPosition;

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

  private static final boolean DEBUG = Boolean.parseBoolean( System
      .getProperty( "nl.lxtreme.ols.client.debug", "false" ) );

  /**
   * Create a new empty diagram to be placed in a container.
   */
  public Diagram( final ActionProvider aActionProvider )
  {
    super();

    // Based on color scheme "sleepyhollow";
    // <http://www.colorschemer.com/schemes/viewscheme.php?id=8379>

    // Not used: new Color( 0x40, 0x2c, 0x29 )
    this.backgroundColor = new Color( 0x10, 0x10, 0x10 );
    this.signalColor = new Color( 0x30, 0x4b, 0x75 );
    this.triggerColor = new Color( 0x82, 0x87, 0x8f );
    this.groupBackgroundColor = new Color( 0x82, 0x87, 0x8f );
    this.gridColor = new Color( 0xc9, 0xc9, 0xc9 );
    this.textColor = Color.WHITE;
    this.timeColor = Color.WHITE;
    this.labelColor = new Color( 0x82, 0x87, 0x8f );

    this.cursorColors = makeColorPalette( AnnotatedData.MAX_CURSORS, AnnotatedData.MAX_CURSORS );
    this.channelColors = makeColorPalette( AnnotatedData.MAX_CHANNELS, 8 );

    this.channelHeight = 30;
    this.scopeHeight = 133;

    this.groupSettings = new int[4];
    for ( int i = 0; i < this.groupSettings.length; i++ )
    {
      this.groupSettings[i] = DISPLAY_CHANNELS | DISPLAY_BYTE;
    }

    this.actionProvider = aActionProvider;

    this.contextMenu = new JPopupMenu();
    for ( int i = 0; i < 10; i++ )
    {
      this.contextMenu.add( new JCheckBoxMenuItem( new SetCursorAction( this, i ) ) );
    }

    this.data = new AnnotatedData();

    this.cursorDefault = getCursor();
    this.cursorDrag = new Cursor( Cursor.MOVE_CURSOR );
    this.evenListeners = new EventListenerList();

    this.rowLabels = new DiagramRowLabels( this.data );
    this.rowLabels.setDiagramSettings( this );

    this.timeLine = new DiagramTimeLine( this.data );
    this.timeLine.setDiagramSettings( this );

    setMinimumSize( new Dimension( 25, 1 ) );
    setBackground( getBackgroundColor() );

    addCursorChangeListener( this.timeLine );

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
  static final long xToIndex( final AnnotatedData aData, final int aXpos, final double aScale )
  {
    long index = Math.max( 0, ( long )( aXpos / aScale ) );

    if ( aData.hasCapturedData() && ( index >= aData.getAbsoluteLength() ) )
    {
      index = aData.getAbsoluteLength() - 1;
    }

    return index;
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
   * @return the diagram's current (annotated) captured data, can be
   *         <code>null</code>.
   */
  public final AnnotatedData getAnnotatedData()
  {
    return this.data;
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
   * get current cursor mode
   */
  public boolean getCursorMode()
  {
    if ( hasCapturedData() )
    {
      return this.data.isCursorsEnabled();
    }
    else
    {
      return false;
    }
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
   * Returns the current diagram settings.
   * 
   * @return the diagram settings, never <code>null</code>.
   */
  public final DiagramSettings getSettings()
  {
    return this;
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
   * Returns wheter or not the diagram has any data.
   * 
   * @return <code>true</code> if captured data exists, <code>false</code>
   *         otherwise
   */
  public boolean hasCapturedData()
  {
    return ( this.data != null ) && this.data.hasCapturedData();
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
   * @see nl.lxtreme.ols.api.Configurable#readProperties(String,
   *      java.util.Properties)
   */
  @Override
  public void readProperties( final String aNamespace, final Properties properties )
  {
    // NO-op
  }

  /**
   * @param aListener
   */
  public void removeCursorChangeListener( final DiagramCursorChangeListener aListener )
  {
    this.evenListeners.remove( DiagramCursorChangeListener.class, aListener );
  }

  /**
   * Removes the cursor at the given position.
   * 
   * @param aCursorIdx
   *          the cursor index to remove.
   */
  public void removeCursorPosition( final int aCursorIdx )
  {
    internalSetCursorPosition( aCursorIdx, -1 );
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
      this.data.setCursorEnabled( aEnabled );
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
   * Display the diagram labels dialog. Will block until the dialog is closed
   * again.
   */
  public void showLabelsDialog( final Window aParent )
  {
    DiagramLabelsDialog dialog = new DiagramLabelsDialog( aParent, getAnnotatedData() );
    if ( dialog.showDialog() )
    {
      this.rowLabels.repaint();
      resize();
    }

    dialog.dispose();
    dialog = null;
  }

  /**
   * Display the diagram settings dialog. Will block until the dialog is closed
   * again.
   */
  public void showSettingsDialog( final Window aParent )
  {
    DiagramSettingsDialog dialog = new DiagramSettingsDialog( aParent, getSettings() );
    if ( dialog.showDialog() )
    {
      resize();
    }

    dialog.dispose();
    dialog = null;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(String,
   *      java.util.Properties)
   */
  @Override
  public void writeProperties( final String aNamespace, final Properties properties )
  {
    // NO-op
  }

  /**
   * Reverts back to the standard zoom level.
   */
  public void zoomDefault()
  {
    setScale( MAX_SCALE );
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
    if ( !hasCapturedData() )
    {
      return;
    }

    final long absoluteLength = this.data.getAbsoluteLength();
    if ( absoluteLength != CapturedData.NOT_AVAILABLE )
    {
      setScale( width / ( double )absoluteLength );
    }
    else
    {
      setScale( MAX_SCALE );
    }

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
    resize();
  }

  /**
   * Update status information. Notifies {@link StatusChangeListener}.
   * 
   * @param aDragging
   *          <code>true</code> indicates that dragging information should be
   *          added
   */
  final void updateStatus( final int aMouseXpos, final int aMouseYpos, final boolean aDragging,
      final int aStartDragXpos, final ChannelAnnotation aAnnotation )
  {
    if ( !hasCapturedData() )
    {
      return;
    }

    final StringBuffer sb = new StringBuffer( " " );

    final int row = aMouseYpos / getChannelHeight();
    if ( row <= this.data.getChannels() + ( this.data.getChannels() / 9 ) )
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
      final int sampleRate = this.data.getSampleRate();
      final long triggerPosition = this.data.getTriggerPosition();

      if ( this.data.isCursorsEnabled() )
      {
        // print cursor data to status line
        final long absCursorPosA = this.data.getCursorPosition( 1 ) - triggerPosition;
        final long absCursorPosB = this.data.getCursorPosition( 2 ) - triggerPosition;
        final long relCursorPos = this.data.getCursorPosition( 1 ) - this.data.getCursorPosition( 2 );

        if ( !this.data.hasTimingData() )
        {
          sb.append( "Sample@A=" ).append( absCursorPosA ).append( " | " );
          sb.append( "Sample@B=" ).append( absCursorPosB ).append( " | " );
          sb.append( "Distance(A,B)=" ).append( relCursorPos );
        }
        else
        {
          sb.append( "Time@A=" ).append( DisplayUtils.displayScaledTime( absCursorPosA, sampleRate ) ).append( " | " );
          sb.append( "Time@B=" ).append( DisplayUtils.displayScaledTime( absCursorPosB, sampleRate ) );
          sb.append( " (duration " ).append( DisplayUtils.displayScaledTime( Math.abs( relCursorPos ), sampleRate ) );
          if ( relCursorPos != 0 )
          {
            sb.append( ", " ).append( "frequency " );
            final double frequency = Math.abs( ( double )sampleRate / ( double )relCursorPos );
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

          if ( !this.data.hasTimingData() )
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
          if ( !this.data.hasTimingData() )
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
   * Moves the cursor with a given index to a given mouse X position.
   * 
   * @param aCursorIdx
   *          the cursor index to move, should be >= 0;
   * @param aMouseXpos
   *          the new X position of the cursor.
   */
  protected final void dragCursor( final int aCursorIdx, final int aMouseXpos )
  {
    if ( !hasCapturedData() || !this.data.isCursorsEnabled() )
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
   * @param aChannelIdx
   * @param aMouseXpos
   * @return
   */
  protected ChannelAnnotation getAnnotationHover( final int aChannelIdx, final int aMouseXpos )
  {
    final long idx = xToIndex( aMouseXpos );
    return this.data.getChannelAnnotation( aChannelIdx, idx );
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
  protected int getCursorHover( final int aMouseXpos )
  {
    final long idx = xToIndex( aMouseXpos );
    final double threshold = CURSOR_HOVER / this.scale;
    for ( int i = 0; i < AnnotatedData.MAX_CURSORS; i++ )
    {
      final long cursorPosition = this.data.getCursorPosition( i );
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
    if ( !hasCapturedData() )
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
    final long firstRow = xToIndex( cx );
    final long lastRow = xToIndex( cw ) + 1;

    // paint portion of background that needs drawing
    g2d.setColor( getBackgroundColor() );
    g2d.fillRect( cx, cy, cw, ch );

    // draw trigger if existing and visible
    final long triggerPosition = this.data.getTriggerPosition();
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
   * @param aEvent
   */
  protected final void showContextMenu( final Point aPosition )
  {
    if ( hasCapturedData() )
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
    if ( getCursorMode() )
    {
      for ( int i = 0, size = AnnotatedData.MAX_CURSORS; i < size; i++ )
      {
        final long cursorPosition = this.data.getCursorPosition( i );
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
    final int channels = this.data.getChannels();
    final int enabled = this.data.getEnabledChannels();
    final long[] timestamps = this.data.getTimestamps();
    final int[] values = this.data.getValues();

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
          final Composite oldComposite = aGraphics.getComposite();

          final Iterator<ChannelAnnotation> annotations = this.data.getChannelAnnotations( channelIdx, aFromIndex,
              aToIndex );
          while ( annotations.hasNext() )
          {
            final ChannelAnnotation annotation = annotations.next();

            long startIdx = annotation.getStartIndex();
            long endIdx = annotation.getEndIndex();
            final String data = annotation.getData() != null ? String.valueOf( annotation.getData() ) : "";

            final int x1 = ( int )( this.scale * startIdx );
            final int x2 = ( int )( this.scale * endIdx );
            final int y1 = bofs + channelHeight * channelIdx;

            final int textXoffset = ( int )( ( x2 - x1 - fm.stringWidth( data ) ) / 2.0 );

            final Color newColor = getSignalColor( channelIdx );

            aGraphics.setPaint( new GradientPaint( x1, y1 - 5, newColor.brighter(), x2, y1 + signalHeight, newColor
                .darker() ) );
            aGraphics.setComposite( AlphaComposite.SrcOver.derive( 0.75f ) );

            aGraphics.fillRoundRect( x1, y1 + 4, ( x2 - x1 ), ( signalHeight - 6 ), signalHeight / 2, signalHeight / 2 );

            aGraphics.setColor( newColor.brighter() );
            aGraphics.drawRoundRect( x1, y1 + 4, ( x2 - x1 ), ( signalHeight - 6 ), signalHeight / 2, signalHeight / 2 );

            if ( textXoffset > 0 )
            {
              aGraphics.setColor( Color.WHITE );
              aGraphics.setComposite( oldComposite );
              aGraphics.drawString( data, x1 + textXoffset, y1 + labelYpos - 4 );
            }
          }

          aGraphics.setPaint( oldPaint );
          aGraphics.setComposite( oldComposite );
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
   * @param aCursorIdx
   * @param aMouseXpos
   */
  private void fireCursorChangedEvent( final int aCursorIdx, final int aMouseXpos )
  {
    final DiagramCursorChangeListener[] listeners = this.evenListeners.getListeners( DiagramCursorChangeListener.class );
    for ( final DiagramCursorChangeListener listener : listeners )
    {
      if ( aMouseXpos >= 0 )
      {
        listener.cursorChanged( aCursorIdx, aMouseXpos );
      }
      else
      {
        listener.cursorRemoved( aCursorIdx );
      }
    }
  }

  /**
   * Sets the actual cursor position.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set;
   * @param aMouseXpos
   *          the new X-position (of the mouse) representing the new cursor
   *          position, if < 0 then the cursor will be removed.
   */
  private void internalSetCursorPosition( final int aCursorIdx, final int aMouseXpos )
  {
    if ( aMouseXpos < 0 )
    {
      // Remove the cursor...
      this.data.setCursorPosition( aCursorIdx, Long.MIN_VALUE );

      fireCursorChangedEvent( aCursorIdx, aMouseXpos );
    }
    else
    {
      final long index = xToIndex( aMouseXpos );

      // notify cursor change listeners
      if ( ( index > 0 ) && ( index < ( this.data.getAbsoluteLength() - 1 ) ) )
      {
        this.data.setCursorPosition( aCursorIdx, index );

        fireCursorChangedEvent( aCursorIdx, aMouseXpos );
      }
    }

    repaint();
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
   * Resizes the diagram as required by available data and scaling factor.
   */
  private void resize()
  {
    if ( !hasCapturedData() )
    {
      return;
    }

    final int channels = this.data.getChannels();
    final int enabledChannels = this.data.getEnabledChannels();

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

    final int width = ( int )( this.scale * this.data.getAbsoluteLength() );

    final Dimension newDiagramSize = new Dimension( width, height );

    this.timeLine.setPreferredSize( newDiagramSize );
    this.timeLine.revalidate();

    this.rowLabels.setPreferredSize( newDiagramSize );
    this.rowLabels.revalidate();

    setPreferredSize( newDiagramSize );
    revalidate();

    repaint();
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
   * Convert x position to sample index.
   * 
   * @param aXpos
   *          horizontal position in pixels
   * @return sample index
   */
  private long xToIndex( final int aXpos )
  {
    return Diagram.xToIndex( this.data, aXpos, this.scale );
  }
}
