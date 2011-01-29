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
package nl.lxtreme.ols.client.diagram.laf;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.settings.*;
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class DiagramUI extends ComponentUI
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
   * Provides a context menu popup listener.
   */
  static final class ContextMenuListener implements PopupMenuListener
  {
    // METHODS

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuCanceled(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuCanceled( final PopupMenuEvent aEvent )
    {
      final JPopupMenu menu = ( JPopupMenu )aEvent.getSource();
      menu.putClientProperty( CONTEXTMENU_LOCATION_KEY, null );
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeInvisible( final PopupMenuEvent aEvent )
    {
      final JPopupMenu menu = ( JPopupMenu )aEvent.getSource();
      final Diagram diagram = ( Diagram )menu.getInvoker();

      final Point location = menu.getLocationOnScreen();
      // we *must* convert the mouse location from the screen to the diagram
      // coordinate space...
      SwingUtilities.convertPointFromScreen( location, diagram );

      menu.putClientProperty( CONTEXTMENU_LOCATION_KEY, location );
    }

    /**
     * @see javax.swing.event.PopupMenuListener#popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent)
     */
    @Override
    public void popupMenuWillBecomeVisible( final PopupMenuEvent aEvent )
    {
    }
  }

  /**
   * 
   */
  final class MouseListener extends MouseAdapter
  {
    // VARIABLES

    private int currentCursor;

    // METHODS

    /**
     * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      final Diagram diagram = ( Diagram )aEvent.getSource();
      if ( isZoomEvent( aEvent ) )
      {
        final Point point = aEvent.getPoint();
        if ( aEvent.isAltDown() || aEvent.isShiftDown() )
        {
          // Zoom out...
          diagram.zoomOut( point );
        }
        else
        {
          // Zoom in...
          diagram.zoomIn( point );
        }
      }
      else
      {
        // Not handled by us; let other components handle it...
        final JScrollPane scrollpane = ( JScrollPane )SwingUtilities.getAncestorOfClass( JScrollPane.class, diagram );
        if ( scrollpane != null )
        {
          scrollpane.dispatchEvent( aEvent );
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
      final Diagram diagram = ( Diagram )aEvent.getSource();
      final Point mousePosition = aEvent.getPoint();

      if ( this.currentCursor >= 0 )
      {
        diagram.dragCursor( this.currentCursor, mousePosition );
      }

      final int channelIdx = diagram.convertPointToChannelIndex( mousePosition );
      updateTooltipText( diagram, mousePosition, channelIdx, null );
    }

    /**
     * Handles mouse moved events and produces status change "events"
     * accordingly.
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final Diagram diagram = ( Diagram )aEvent.getSource();
      final Point mousePosition = aEvent.getPoint();

      // Determine whether we're hovering over an annotation...
      ChannelAnnotation annotation = null;

      final int channelIdx = diagram.convertPointToChannelIndex( mousePosition );
      if ( channelIdx >= 0 )
      {
        annotation = diagram.getAnnotationHover( channelIdx, mousePosition );
      }

      // Determine whether we're hovering over a cursor...
      this.currentCursor = -1;
      Cursor mouseCursor = DiagramUI.this.cursorDefault;

      final int cursorIdx = diagram.getCursorHover( mousePosition );
      if ( cursorIdx >= 0 )
      {
        this.currentCursor = cursorIdx;
        mouseCursor = DiagramUI.this.cursorDrag;
      }

      // Update the graphical state of the diagram...
      diagram.setCursor( mouseCursor );
      updateTooltipText( diagram, mousePosition, channelIdx, annotation );
    }

    /**
     * @see java.awt.event.MouseAdapter#mouseWheelMoved(java.awt.event.MouseWheelEvent)
     */
    @Override
    public void mouseWheelMoved( final MouseWheelEvent aEvent )
    {
      final Diagram diagram = ( Diagram )aEvent.getSource();
      final Point point = aEvent.getPoint();

      final int notches = aEvent.getWheelRotation();
      // Is CTRL also down?
      final boolean isCtrlDown = aEvent.isControlDown();
      // On Mac OSX the mouse wheel events are also generated for all kinds of
      // other scrolling events which only confuses the user...
      final boolean isZoomEvent = !HostUtils.isMacOSX() && isCtrlDown;

      if ( isZoomEvent && ( notches < 0 ) )
      {
        diagram.zoomIn( point );
      }
      else if ( isZoomEvent && ( notches > 0 ) )
      {
        diagram.zoomOut( point );
      }
      else
      {
        // Not handled by us; let other components handle it...
        final JScrollPane scrollpane = ( JScrollPane )SwingUtilities.getAncestorOfClass( JScrollPane.class, diagram );
        if ( scrollpane != null )
        {
          scrollpane.dispatchEvent( aEvent );
        }
      }
    }

    /**
     * Returns whether the given mouse event is a "zoom in/out" event.
     * 
     * @param aEvent
     *          the mouse event to check, cannot be <code>null</code>.
     * @return <code>true</code> if the given mouse event is a zoom event,
     *         <code>false</code> otherwise.
     */
    private boolean isZoomEvent( final MouseEvent aEvent )
    {
      return aEvent.getClickCount() == 2;
    }

    /**
     * Update status information. Notifies {@link StatusChangeListener}.
     * 
     * @param aDragging
     *          <code>true</code> indicates that dragging information should be
     *          added
     */
    private void updateTooltipText( final Diagram aDiagram, final Point aMousePosition, final int aChannelIndex,
        final ChannelAnnotation aAnnotation )
    {
      final DataContainer dataContainer = aDiagram.getDataContainer();
      if ( !dataContainer.hasCapturedData() )
      {
        return;
      }

      final StringBuffer sb = new StringBuffer();
      if ( aChannelIndex < 0 )
      {
        sb.append( "Scope/byte " ).append( Math.abs( aChannelIndex + 1 ) );
      }
      else
      {
        sb.append( "Channel " ).append( aChannelIndex );
      }
      sb.append( " | " );

      final long triggerPosition = dataContainer.getTriggerPosition();
      final int sampleRate = dataContainer.getSampleRate();

      if ( aAnnotation != null )
      {
        final double start = dataContainer.calculateTime( aAnnotation.getStartIndex() );
        final double end = dataContainer.calculateTime( aAnnotation.getEndIndex() );

        sb.append( aAnnotation.getData() );
        sb.append( " | Time: " );
        sb.append( DisplayUtils.displayTime( start ) );
        sb.append( ".." );
        sb.append( DisplayUtils.displayTime( end ) );
      }
      else
      {
        final long idxMouseX = aDiagram.convertPointToSampleIndex( aMousePosition );

        if ( !dataContainer.hasTimingData() )
        {
          sb.append( "Sample: " ).append( idxMouseX );
        }
        else
        {
          long absMouseX = idxMouseX;
          if ( dataContainer.hasTriggerData() )
          {
            absMouseX -= triggerPosition;
          }
          sb.append( "Time: " ).append( DisplayUtils.displayScaledTime( absMouseX, sampleRate ) );
        }
      }

      aDiagram.setToolTipText( sb.toString() );
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

  public static final String CONTEXTMENU_LOCATION_KEY = "OLS.ContextMenu.location";

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( DiagramUI.class.getName() );

  private static final int PADDING_Y = 2;

  private static final Stroke SOLID_THICK = new BasicStroke( 1.5f );

  // VARIABLES

  private final JPopupMenu contextMenu;
  private Font labelFont;
  private final Cursor cursorDefault;
  private final Cursor cursorDrag;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramUI( final ActionProvider aActionProvider )
  {
    this.contextMenu = new JPopupMenu();
    this.contextMenu.addPopupMenuListener( new ContextMenuListener() );
    for ( int i = 0; i < 10; i++ )
    {
      final Action setCursorAction = aActionProvider.getAction( SetCursorAction.getCursorId( i ) );
      this.contextMenu.add( new JCheckBoxMenuItem( setCursorAction ) );
    }
    this.contextMenu.addSeparator();
    this.contextMenu.add( aActionProvider.getAction( ClearCursors.ID ) );

    this.cursorDefault = Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );
    this.cursorDrag = Cursor.getPredefinedCursor( Cursor.MOVE_CURSOR );

  }

  // METHODS

  /**
   * @return
   */
  public Dimension calculateNewDimension( final Diagram aDiagram )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();

    final int enabledChannels = dataContainer.getEnabledChannels();

    final DiagramSettings settings = aDiagram.getDiagramSettings();

    final int channelHeight = settings.getChannelHeight();
    final int scopeHeight = settings.getScopeHeight();

    int height = 0;

    final int blockCnt = dataContainer.getBlockCount();
    for ( int block = 0; block < blockCnt; block++ )
    {
      if ( ( ( enabledChannels >> ( CapturedData.CHANNELS_PER_BLOCK * block ) ) & 0xff ) != 0 )
      {
        if ( settings.isShowChannels( block ) )
        {
          height += channelHeight * dataContainer.getChannelsForBlock( block );
        }
        if ( settings.isShowScope( block ) )
        {
          height += scopeHeight;
        }
        if ( settings.isShowByte( block ) )
        {
          height += channelHeight;
        }
      }
    }

    final int width = ( int )( aDiagram.getScale() * dataContainer.getAbsoluteLength() );

    final Dimension newDiagramSize = new Dimension( width, height );
    return newDiagramSize;
  }

  // METHODS

  /**
   * @see javax.swing.plaf.ComponentUI#getPreferredSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getPreferredSize( final JComponent aComponent )
  {
    final Diagram diagram = ( Diagram )aComponent;
    return calculateNewDimension( diagram );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#installUI(javax.swing.JComponent)
   */
  @Override
  public void installUI( final JComponent aComponent )
  {
    final Diagram diagram = ( Diagram )aComponent;
    diagram.setComponentPopupMenu( this.contextMenu );

    this.labelFont = LafHelper.getDefaultFont();

    final MouseListener mouseListener = new MouseListener();

    diagram.addMouseListener( mouseListener );
    diagram.addMouseMotionListener( mouseListener );
    diagram.addMouseWheelListener( mouseListener );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#paint(java.awt.Graphics,
   *      javax.swing.JComponent)
   */
  @Override
  public void paint( final Graphics aCanvas, final JComponent aComponent )
  {
    final Diagram diagram = ( Diagram )aComponent;

    final DataContainer dataContainer = diagram.getDataContainer();
    if ( !dataContainer.hasCapturedData() )
    {
      return;
    }

    final long start = System.currentTimeMillis();
    LOG.log( Level.FINE, "Start diagram rendering = {0}ms.", Long.valueOf( start ) );
    final Graphics2D canvas = ( Graphics2D )aCanvas;

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = canvas.getClipBounds();

    final int cx = clipArea.x;
    final int cw = cx + clipArea.width;

    // find index of first & last row that needs drawing
    final long firstRow = diagram.convertPointToSampleIndex( new Point( cx, 0 ) );
    final long lastRow = diagram.convertPointToSampleIndex( new Point( cw, 0 ) ) + 1;

    canvas.setFont( this.labelFont );

    // draw all signal groups...
    paintSignals( canvas, diagram, clipArea, firstRow, lastRow );

    // draw cursors if enabled...
    paintCursors( canvas, diagram, clipArea, firstRow, lastRow );

    final long end = System.currentTimeMillis();
    LOG.log( Level.FINE, "Render time = {0}ms.", Long.valueOf( end - start ) );
  }

  /**
   * @param aEvent
   */
  final void showContextMenu( final Diagram aDiagram, final Point aPosition )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();
    if ( dataContainer.hasCapturedData() )
    {
      this.contextMenu.putClientProperty( CONTEXTMENU_LOCATION_KEY, aPosition );
      this.contextMenu.show( aDiagram, aPosition.x, aPosition.y );
    }
  }

  /**
   * @param aChannelIdx
   * @param aSettings
   * @return
   */
  private Color getSignalColor( final int aChannelIdx, final DiagramSettings aSettings )
  {
    Color result = aSettings.getSignalColor();
    if ( ColorTarget.SIGNALS.equals( aSettings.getColorTarget() ) )
    {
      result = aSettings.getChannelColor( aChannelIdx );
    }
    return result;
  }

  /**
   * @see LafHelper#paintChannelBackground(Graphics2D, DiagramSettings,
   *      Rectangle, int, int)
   */
  private void paintChannelBackground( final Graphics2D aCanvas, final DiagramSettings aSettings,
      final Rectangle aClipArea, final int aChannelIdx, final int aYoffset )
  {
    LafHelper.paintChannelBackground( aCanvas, aSettings, aClipArea, aChannelIdx, aYoffset );
  }

  /**
   * @param aCanvas
   * @param aFirstRow
   * @param aLastRow
   */
  private void paintCursors( final Graphics2D aCanvas, final Diagram aDiagram, final Rectangle aClipArea,
      final long aFirstRow, final long aLastRow )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();
    if ( dataContainer.isCursorsEnabled() )
    {
      final DiagramSettings settings = aDiagram.getDiagramSettings();

      final int y1 = aClipArea.y;
      final int y2 = y1 + aClipArea.height;

      for ( int i = 0, size = CapturedData.MAX_CURSORS; i < size; i++ )
      {
        final Long cursorPosition = dataContainer.getCursorPosition( i );
        if ( cursorPosition == null )
        {
          continue;
        }

        if ( ( cursorPosition.longValue() >= aFirstRow ) && ( cursorPosition.longValue() <= aLastRow ) )
        {
          final int cursorPos = ( int )( cursorPosition.longValue() * aDiagram.getScale() );

          aCanvas.setColor( settings.getCursorColor( i ) );
          aCanvas.drawLine( cursorPos, y1, cursorPos, y2 );
        }
      }
    }
  }

  /**
   * @param aCanvas
   * @param aClipArea
   * @param aYoffset
   */
  private void paintGridLine( final Graphics2D aCanvas, final Diagram aDiagram, final Rectangle aClipArea,
      final int aYoffset )
  {
    aCanvas.setColor( aDiagram.getDiagramSettings().getGridColor() );
    aCanvas.drawLine( aClipArea.x, aYoffset, aClipArea.x + aClipArea.width, aYoffset );
  }

  /**
   * Draws all signals, byte values and scopes.
   * 
   * @param aCanvas
   *          the canvas to paint on;
   * @param aClipArea
   *          the clip area to paint;
   * @param aFromIndex
   *          the first sample to paint;
   * @param aToIndex
   *          the last sample to paint.
   */
  private void paintSignals( final Graphics2D aCanvas, final Diagram aDiagram, final Rectangle aClipArea,
      final long aFromIndex, final long aToIndex )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();
    final DiagramSettings settings = aDiagram.getDiagramSettings();

    final int enabled = dataContainer.getEnabledChannels();
    final long[] timestamps = dataContainer.getTimestamps();
    final int[] values = dataContainer.getValues();

    final int channelHeight = settings.getChannelHeight();
    final int signalHeight = settings.getSignalHeight();
    final int scopeHeight = settings.getScopeHeight();

    // Where is the signal to be drawn?
    final int signalOffset;
    if ( SignalAlignment.BOTTOM.equals( settings.getSignalAlignment() ) )
    {
      signalOffset = ( channelHeight - signalHeight ) - 2;
    }
    else if ( SignalAlignment.CENTER.equals( settings.getSignalAlignment() ) )
    {
      signalOffset = ( channelHeight - signalHeight ) / 2;
    }
    else
    {
      signalOffset = 1;
    }

    final double scopeScaleFactor = ( 256.0 / ( scopeHeight - 2 * PADDING_Y ) );

    final double scale = aDiagram.getScale();

    final double edgeSlopeFactor = ( settings.getEdgeSlope() == EdgeSlope.PERPENDICULAR ) ? 0.5 : 0.3;
    final double edgeX = ( scale * edgeSlopeFactor );

    final FontMetrics fm = aCanvas.getFontMetrics();
    final int fontHeight = fm.getHeight();
    final int n = 2 * timestamps.length;

    // Search the first sample index the is right before the to-be-displayed
    // from index...
    int dataStartIndex = 0;
    do
    {
      if ( timestamps[dataStartIndex] >= aFromIndex )
      {
        // Found it; use this as starting time-index...
        break;
      }
      dataStartIndex++;
    }
    while ( dataStartIndex < timestamps.length );
    // Make sure everything is inside the expected ranges...
    dataStartIndex = Math.min( timestamps.length - 1, Math.max( 0, dataStartIndex - 1 ) );

    // Search the last sample index the is right before the to-be-displayed
    // to index...
    int dataEndIndex = dataStartIndex;
    do
    {
      if ( timestamps[dataEndIndex] >= aToIndex )
      {
        // Found it; use this as starting time-index...
        break;
      }
      dataEndIndex++;
    }
    while ( dataEndIndex < timestamps.length );
    // Make sure everything is inside the expected ranges...
    dataEndIndex = Math.min( timestamps.length - 1, Math.max( 0, dataEndIndex - 1 ) );

    final long triggerPosition = dataContainer.getTriggerPosition();

    int yofs = 0;

    final int blockCnt = dataContainer.getBlockCount();
    for ( int block = 0; block < blockCnt; block++ )
    {
      final int channelsOffset = CapturedData.CHANNELS_PER_BLOCK * block;
      final boolean blockEnabled = ( ( enabled >> channelsOffset ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( settings.isShowChannels( block ) )
      {
        final SignalPolyline polyline = new SignalPolyline( n );

        // draw actual data
        final int channelsPerBlock = dataContainer.getChannelsForBlock( block );
        for ( int bit = 0; bit < channelsPerBlock; bit++ )
        {
          final int channelIdx = channelsOffset + bit;

          final Color signalColor = getSignalColor( channelIdx, settings );
          final Color newBrighterColor = signalColor.brighter();
          final Color newDarkerColor = signalColor.darker().darker().darker();

          final int py1 = channelHeight * bit + yofs;
          // Paint the (optional) channel background...
          paintChannelBackground( aCanvas, settings, aClipArea, channelIdx, py1 );

          // draw trigger if existing and visible
          if ( ( triggerPosition >= aFromIndex ) && ( triggerPosition <= aToIndex ) )
          {
            final Composite oldComposite = aCanvas.getComposite();

            aCanvas.setColor( settings.getTriggerColor() );
            aCanvas.setComposite( AlphaComposite.SrcOver.derive( 0.35f ) );
            aCanvas.fillRect( ( int )( triggerPosition * scale ) + 1, py1, Math.max( 1, ( int )scale - 1 ),
                channelHeight );
            aCanvas.setComposite( oldComposite );
          }

          int dataIndex = dataStartIndex;
          long currentSample = timestamps[dataIndex];
          int pIdx = 0;

          while ( dataIndex <= dataEndIndex )
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
            int x1 = ( int )( ( scale * currentSample ) - edgeX );
            int x2 = ( int )( ( scale * ( nextSample - 1 ) ) + edgeX );
            final int y1 = py1 + ( signalHeight * ( 1 - currentValue ) ) + signalOffset;

            polyline.x[pIdx] = x1;
            polyline.y[pIdx] = y1;
            polyline.x[pIdx + 1] = x2;
            polyline.y[pIdx + 1] = y1;

            pIdx += 2;

            // Update loop administration...
            dataIndex++;
            currentSample = nextSample;
          }

          aCanvas.setColor( signalColor );
          aCanvas.drawPolyline( polyline.x, polyline.y, pIdx );

          // Create a new canvas instance so we can paint our annotations
          // without having to restore anything...
          final Graphics2D newCanvas = ( Graphics2D )aCanvas.create();
          newCanvas.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

          final Iterator<ChannelAnnotation> annotations = dataContainer.getChannelAnnotations( channelIdx,
              dataStartIndex, timestamps.length );
          while ( annotations.hasNext() )
          {
            final ChannelAnnotation annotation = annotations.next();

            final long startIdx = timestamps[annotation.getStartIndex()];
            final long endIdx = timestamps[annotation.getEndIndex()];

            final String data = annotation.getData() != null ? String.valueOf( annotation.getData() ) : "";

            final int x1 = ( int )( scale * startIdx );
            final int x2 = ( int )( scale * endIdx );
            final int y1 = yofs + channelHeight * channelIdx;

            final int textWidth = fm.stringWidth( data );

            final GradientPaint annotationPaint = new GradientPaint( x1, y1 - 5, newBrighterColor, x1, y1
                + ( signalHeight / 2 ), newDarkerColor );

            final int annotationWidth = ( x2 - x1 ) + 2;
            final int annotationHeight = ( fontHeight + 0 );
            final int arc = ( int )( annotationHeight / 2.0 );
            final int textXoffset = ( int )( ( annotationWidth - textWidth ) / 2.0 );

            final Composite oldComposite = newCanvas.getComposite();

            newCanvas.setComposite( AlphaComposite.SrcOver.derive( 0.8f ) );
            newCanvas.setStroke( SOLID_THICK );

            newCanvas.setPaint( annotationPaint );
            newCanvas.fillRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            newCanvas.setColor( newBrighterColor );
            newCanvas.drawRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            newCanvas.setComposite( oldComposite );

            if ( textXoffset > 0 )
            {
              newCanvas.setColor( Color.WHITE );

              newCanvas.drawString( data, x1 + textXoffset, y1 + fontHeight );
            }
          }

          paintGridLine( aCanvas, aDiagram, aClipArea, channelHeight * bit + yofs + ( channelHeight - 1 ) );
        }
        yofs += ( channelHeight * CapturedData.CHANNELS_PER_BLOCK );
      }

      if ( settings.isShowScope( block ) )
      {
        final SignalPolyline scopePolyline = new SignalPolyline( n );

        int pIdx = 0;

        int val = yofs;
        int dataIndex = dataStartIndex;

        while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
        {
          val = ( int )( ( 0xff - ( ( values[dataIndex] >> channelsOffset ) & 0xff ) ) / scopeScaleFactor );

          scopePolyline.x[pIdx] = ( int )( timestamps[dataIndex] * scale );
          scopePolyline.y[pIdx] = yofs + val + PADDING_Y;
          pIdx++;

          dataIndex++;
        }

        scopePolyline.x[pIdx] = ( int )( aToIndex * scale );
        scopePolyline.y[pIdx] = yofs + val + PADDING_Y;
        pIdx++;

        // draw background...
        aCanvas.setColor( settings.getBackgroundColor() );
        aCanvas.fillRect( aClipArea.x, yofs, aClipArea.width, scopeHeight );

        // draw actual data
        aCanvas.setColor( settings.getScopeColor() );
        aCanvas.drawPolyline( scopePolyline.x, scopePolyline.y, pIdx );

        yofs += scopeHeight;
        // draw bottom grid line
        paintGridLine( aCanvas, aDiagram, aClipArea, yofs - 1 );
      }

      if ( settings.isShowByte( block ) )
      {
        final ByteValuePolyline bytePolyline = new ByteValuePolyline( n );

        long currentSample = aFromIndex - 1;
        int pIdx = 0;

        int dataIndex = dataStartIndex;

        // draw background...
        aCanvas.setColor( settings.getGroupBackgroundColor() );
        aCanvas.fillRect( aClipArea.x, yofs, aClipArea.width, channelHeight );

        while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
        {
          final long nextSample;

          final int currentValue = ( values[dataIndex] >> channelsOffset ) & 0xff;
          if ( dataIndex >= values.length - 1 )
          {
            nextSample = aToIndex + 1;
          }
          else
          {
            nextSample = timestamps[dataIndex + 1];
          }

          // Calculate display coordinates...
          final int x1 = ( int )( scale * currentSample - edgeX );
          final int x2 = ( int )( scale * ( nextSample - 1 ) + edgeX );

          final int bit = ( dataIndex % 2 );
          final int y1 = yofs + signalHeight * ( 1 - bit ) + signalOffset;
          final int y2 = yofs + signalHeight * bit + signalOffset;

          bytePolyline.x[pIdx] = x1;
          bytePolyline.y1[pIdx] = y1;
          bytePolyline.y2[pIdx] = y2;
          bytePolyline.x[pIdx + 1] = x2;
          bytePolyline.y1[pIdx + 1] = y1;
          bytePolyline.y2[pIdx + 1] = y2;
          pIdx += 2;

          // if steady long enough, add hex value
          final String byteValue = String.format( "%02X", Integer.valueOf( currentValue ) );
          final int labelWidth = fm.stringWidth( byteValue );

          if ( ( x2 - x1 ) > labelWidth )
          {
            final int labelXpos = ( int )( ( x1 + x2 - labelWidth ) / 2.0 );

            aCanvas.setColor( settings.getTextColor() );
            aCanvas.drawString( byteValue, labelXpos, yofs + ( fontHeight + signalOffset ) );
          }

          // Update loop administration...
          dataIndex++;
          currentSample = nextSample;
        }

        aCanvas.setColor( settings.getGroupByteColor() );
        aCanvas.drawPolyline( bytePolyline.x, bytePolyline.y1, pIdx );
        aCanvas.drawPolyline( bytePolyline.x, bytePolyline.y2, pIdx );

        yofs += channelHeight;
        // draw bottom grid line
        paintGridLine( aCanvas, aDiagram, aClipArea, yofs - 1 );
      }
    }
  }
}
