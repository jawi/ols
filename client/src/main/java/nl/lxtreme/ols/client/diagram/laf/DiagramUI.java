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

      diagram.updateTooltipText( mousePosition, null );
    }

    /**
     * Handles mouse moved events and produces status change "events"
     * accordingly.
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final Diagram diagram = ( Diagram )aEvent.getSource();
      final DiagramSettings settings = diagram.getDiagramSettings();

      final Point mousePosition = aEvent.getPoint();

      final int channel = ( mousePosition.y / settings.getChannelHeight() );

      final ChannelAnnotation annotation = diagram.getAnnotationHover( channel, mousePosition );
      final int cursorIdx = diagram.getCursorHover( mousePosition );

      if ( cursorIdx >= 0 )
      {
        this.currentCursor = cursorIdx;
        diagram.setCursor( DiagramUI.this.cursorDrag );
      }
      else
      {
        this.currentCursor = -1;
        diagram.setCursor( DiagramUI.this.cursorDefault );
      }

      diagram.updateTooltipText( mousePosition, annotation );
    }

    /**
     * @see java.awt.event.MouseAdapter#mouseWheelMoved(java.awt.event.MouseWheelEvent)
     */
    @Override
    public void mouseWheelMoved( final MouseWheelEvent aEvent )
    {
      if ( HostUtils.isMacOSX() )
      {
        // On Mac OSX the mouse wheel events are also generated for all kinds of
        // other scrolling events which only confuses the user...
        return;
      }

      final Diagram diagram = ( Diagram )aEvent.getSource();
      final Point point = aEvent.getPoint();

      final int notches = aEvent.getWheelRotation();
      if ( notches < 0 )
      {
        diagram.zoomIn( point );
      }
      else if ( notches > 0 )
      {
        diagram.zoomOut( point );
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

  private static final boolean DEBUG = Boolean.parseBoolean( System
      .getProperty( "nl.lxtreme.ols.client.debug", "false" ) );

  private static final Logger LOG = Logger.getLogger( DiagramUI.class.getName() );

  private static final int PADDING_Y = 2;

  private static final Stroke SOLID_THICK = new BasicStroke( 1.5f );

  // VARIABLES

  private final JPopupMenu contextMenu;

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

    final int channels = dataContainer.getChannels();
    final int enabledChannels = dataContainer.getEnabledChannels();

    final DiagramSettings settings = aDiagram.getDiagramSettings();

    final int channelHeight = settings.getChannelHeight();
    final int scopeHeight = settings.getScopeHeight();

    int height = 0;
    for ( int group = 0; ( group < channels / 8 ) && ( group < 4 ); group++ )
    {
      if ( ( ( enabledChannels >> ( 8 * group ) ) & 0xff ) != 0 )
      {
        if ( settings.isShowChannels( group ) )
        {
          height += channelHeight * 8;
        }
        if ( settings.isShowScope( group ) )
        {
          height += scopeHeight;
        }
        if ( settings.isShowByte( group ) )
        {
          height += channelHeight;
        }
      }
    }

    final int width = ( int )( aDiagram.getScale() * dataContainer.getAbsoluteLength() );

    final Dimension newDiagramSize = new Dimension( width, height );
    return newDiagramSize;
  }

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
  public void paint( final Graphics aGraphics, final JComponent aComponent )
  {
    final Diagram diagram = ( Diagram )aComponent;
    final DiagramSettings settings = diagram.getDiagramSettings();

    final DataContainer dataContainer = diagram.getDataContainer();
    if ( !dataContainer.hasCapturedData() )
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
    final long firstRow = diagram.convertPointToSampleIndex( new Point( cx, 0 ) );
    final long lastRow = diagram.convertPointToSampleIndex( new Point( cw, 0 ) ) + 1;

    // paint portion of background that needs drawing
    g2d.setColor( settings.getBackgroundColor() );
    g2d.fillRect( cx, cy, cw, ch );

    // draw trigger if existing and visible
    final long triggerPosition = dataContainer.getTriggerPosition();
    if ( ( triggerPosition >= firstRow ) && ( triggerPosition <= lastRow ) )
    {
      final double scale = diagram.getScale();

      g2d.setColor( settings.getTriggerColor() );
      g2d.fillRect( ( int )( triggerPosition * scale ) - 1, cy, ( int )( scale + 2 ), ch );
    }

    // draw all signal groups...
    drawSignals( g2d, diagram, clipArea, firstRow, lastRow );

    // draw cursors if enabled...
    drawCursors( g2d, diagram, firstRow, lastRow );

    if ( DEBUG )
    {
      final long end = System.currentTimeMillis();
      LOG.log( Level.INFO, "Render time = {0}ms.", ( end - start ) );
    }
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
   * @param aCanvas
   * @param aFirstRow
   * @param aLastRow
   */
  private void drawCursors( final Graphics2D aCanvas, final Diagram aDiagram, final long aFirstRow, final long aLastRow )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();
    if ( dataContainer.isCursorsEnabled() )
    {
      final DiagramSettings settings = aDiagram.getDiagramSettings();

      for ( int i = 0, size = DataContainer.MAX_CURSORS; i < size; i++ )
      {
        final long cursorPosition = dataContainer.getCursorPosition( i );
        if ( ( cursorPosition >= aFirstRow ) && ( cursorPosition <= aLastRow ) )
        {
          final int cursorPos = ( int )( cursorPosition * aDiagram.getScale() );

          aCanvas.setColor( settings.getCursorColor( i ) );
          aCanvas.drawLine( cursorPos, 0, cursorPos, 36 * settings.getChannelHeight() );
        }
      }
    }
  }

  /**
   * @param aCanvas
   * @param aClipArea
   * @param aYoffset
   */
  private void drawGridLine( final Graphics2D aCanvas, final Diagram aDiagram, final Rectangle aClipArea,
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
  private void drawSignals( final Graphics2D aCanvas, final Diagram aDiagram, final Rectangle aClipArea,
      final long aFromIndex, final long aToIndex )
  {
    final DataContainer dataContainer = aDiagram.getDataContainer();
    final DiagramSettings settings = aDiagram.getDiagramSettings();

    final int channels = dataContainer.getChannels();
    final int enabled = dataContainer.getEnabledChannels();
    final long[] timestamps = dataContainer.getTimestamps();
    final int[] values = dataContainer.getValues();

    final int channelHeight = settings.getChannelHeight();
    final int signalHeight = settings.getSignalHeight();
    final int scopeHeight = settings.getScopeHeight();

    final double scopeScaleFactor = ( 256.0 / ( scopeHeight - 2 * PADDING_Y ) );

    final double scale = aDiagram.getScale();
    final int edgeX = ( int )( scale * 0.5 );

    final FontMetrics fm = aCanvas.getFontMetrics();
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

      if ( settings.isShowChannels( block ) )
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
            final int x1 = ( int )( ( scale * currentSample ) - edgeX );
            final int x2 = ( int )( ( scale * ( nextSample - 1 ) ) + edgeX );
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

          aCanvas.setColor( settings.getChannelColor( channelIdx ) );
          aCanvas.drawPolyline( polyline.x, polyline.y, pIdx );

          // XXX
          final Color oldColor = aCanvas.getColor();
          final Paint oldPaint = aCanvas.getPaint();
          final Stroke oldStroke = aCanvas.getStroke();
          final Composite oldComposite = aCanvas.getComposite();
          final Object oldHint = aCanvas.getRenderingHint( RenderingHints.KEY_ANTIALIASING );
          aCanvas.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

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
            final int y1 = bofs + channelHeight * channelIdx;

            final int textXoffset = ( int )( ( x2 - x1 - fm.stringWidth( data ) ) / 2.0 );

            final Color newColor = settings.getChannelColor( channelIdx );
            final Color newBrighterColor = newColor.brighter();
            final Color newDarkerColor = newColor.darker().darker();
            final GradientPaint newPaint = new GradientPaint( x1, y1 - 5, newBrighterColor, x1, y1
                + ( signalHeight / 2 ), newDarkerColor );

            final int annotationWidth = ( x2 - x1 );
            final int annotationHeight = ( signalHeight - 6 );
            final int arc = ( int )( annotationHeight / 2.0 );

            aCanvas.setComposite( AlphaComposite.SrcOver.derive( 0.75f ) );
            aCanvas.setStroke( SOLID_THICK );

            aCanvas.setPaint( newPaint );
            aCanvas.fillRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            aCanvas.setColor( newBrighterColor );
            aCanvas.drawRoundRect( x1, y1 + 4, annotationWidth, annotationHeight, arc, arc );

            if ( textXoffset > 0 )
            {
              aCanvas.setColor( Color.WHITE );
              aCanvas.setComposite( oldComposite );

              aCanvas.drawString( data, x1 + textXoffset, y1 + labelYpos - 4 );
            }
          }

          aCanvas.setRenderingHint( RenderingHints.KEY_ANTIALIASING, oldHint );
          aCanvas.setPaint( oldPaint );
          aCanvas.setComposite( oldComposite );
          aCanvas.setStroke( oldStroke );
          aCanvas.setColor( oldColor );
          // XXX

          drawGridLine( aCanvas, aDiagram, aClipArea, channelHeight * bit + bofs + ( channelHeight - 1 ) );
        }
        bofs += ( channelHeight * 8 );
      }

      if ( settings.isShowScope( block ) )
      {
        final SignalPolyline scopePolyline = new SignalPolyline( n );

        int pIdx = 0;

        int val = bofs;
        int dataIndex = dataStartIndex;

        while ( ( dataIndex < values.length ) && ( timestamps[dataIndex] <= aToIndex ) )
        {
          val = ( int )( ( 0xff - ( ( values[dataIndex] >> ( 8 * block ) ) & 0xff ) ) / scopeScaleFactor );

          scopePolyline.x[pIdx] = ( int )( timestamps[dataIndex] * scale );
          scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
          pIdx++;

          dataIndex++;
        }

        scopePolyline.x[pIdx] = ( int )( aToIndex * scale );
        scopePolyline.y[pIdx] = bofs + val + PADDING_Y;
        pIdx++;

        // draw actual data
        aCanvas.setColor( settings.getScopeColor() );
        aCanvas.drawPolyline( scopePolyline.x, scopePolyline.y, pIdx );
        bofs += scopeHeight;
        // draw bottom grid line
        drawGridLine( aCanvas, aDiagram, aClipArea, bofs - 1 );
      }

      if ( settings.isShowByte( block ) )
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
          final int x1 = ( int )( scale * currentSample - edgeX );
          final int x2 = ( int )( scale * ( nextSample - 1 ) + edgeX );

          final int bit = ( dataIndex % 2 );
          final int y1 = bofs + signalHeight * ( 1 - bit ) + 1;
          final int y2 = bofs + signalHeight * bit + 1;

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

            aCanvas.drawString( byteValue, labelXpos, bofs + labelYpos );
          }

          // Update loop administration...
          dataIndex++;
          currentSample = nextSample;
        }

        aCanvas.setColor( settings.getGroupByteColor() );
        aCanvas.drawPolyline( bytePolyline.x, bytePolyline.y1, pIdx );
        aCanvas.drawPolyline( bytePolyline.x, bytePolyline.y2, pIdx );
        bofs += channelHeight;
        // draw bottom grid line
        drawGridLine( aCanvas, aDiagram, aClipArea, bofs - 1 );
      }
    }
  }
}
