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
package nl.lxtreme.ols.client.signaldisplay.util;


import java.awt.*;
import java.util.*;
import java.util.List;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.client.action.SmartJumpAction.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 *
 */
public final class SmartJumpHelper
{
  // INNER TYPES

  /**
   *
   */
  private static class CursorComparator implements Comparator<Cursor>
  {
    @Override
    public int compare( final Cursor aCursor1, final Cursor aCursor2 )
    {
      return ( int )( aCursor1.getTimestamp() - aCursor2.getTimestamp() );
    }
  }

  // VARIABLES

  private final SignalDiagramController controller;
  private final JumpDirection direction;
  private final JumpType type;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SmartJumpHelper} instance.
   */
  public SmartJumpHelper( final SignalDiagramController aController, final JumpDirection aDirection,
      final JumpType aType )
  {
    this.controller = aController;
    this.direction = aDirection;
    this.type = aType;
  }

  // METHODS

  /**
   * @param aChannelIndex
   * @return
   */
  public long getSmartJumpPosition( final int aChannelIndex )
  {
    Rectangle viewSize = this.controller.getViewComponent().getVisibleRect();

    SignalElement signalElement = getSignalDiagramModel().getSignalElementManager().getDigitalSignalByChannelIndex(
        aChannelIndex );
    if ( signalElement != null )
    {
      Point refPoint = new Point( ( int )Math.round( viewSize.getCenterX() ), signalElement.getYposition() );

      long refTimestamp = locationToTimestamp( refPoint );

      switch ( this.type )
      {
        case ANNOTATION:
          return getAnnotationJumpPosition( signalElement, refTimestamp );

        case CURSOR:
          return getCursorJumpPosition( refTimestamp );

        case SIGNAL_EDGE:
          return getSignalEdgeJumpPosition( aChannelIndex, refTimestamp );
      }
    }

    return -1L;
  }

  /**
   * @param signalElement
   * @param refTimestamp
   * @return
   */
  private long getAnnotationJumpPosition( final SignalElement signalElement, final long refTimestamp )
  {
    AnnotationsHelper helper = new AnnotationsHelper( signalElement );

    DataAnnotation<?> annotation = null;
    if ( this.direction.isLeft() )
    {
      annotation = helper.getAnnotationBefore( refTimestamp );
    }
    else
    {
      annotation = helper.getAnnotationAfter( refTimestamp );
    }

    if ( annotation != null )
    {
      long start = annotation.getStartTimestamp();
      long end = annotation.getEndTimestamp();

      return start + ( ( end - start ) / 2L );
    }

    return -1L;
  }

  /**
   * @param refTimestamp
   * @return
   */
  private long getCursorJumpPosition( final long refTimestamp )
  {
    List<Cursor> cursors = new ArrayList<Cursor>( Arrays.asList( getSignalDiagramModel().getDefinedCursors() ) );
    Collections.sort( cursors, new CursorComparator() );

    Cursor foundCursor = null;
    for ( Cursor cursor : cursors )
    {
      long timestamp = cursor.getTimestamp();
      if ( this.direction.isLeft() )
      {
        if ( timestamp < refTimestamp )
        {
          foundCursor = cursor;
        }
        else
        {
          break;
        }
      }
      else
      {
        if ( timestamp > refTimestamp )
        {
          foundCursor = cursor;
          break;
        }
      }
    }

    if ( foundCursor != null )
    {
      return foundCursor.getTimestamp();
    }

    return -1L;
  }

  /**
   * @return
   */
  private SignalDiagramModel getSignalDiagramModel()
  {
    return this.controller.getViewModel();
  }

  /**
   * @param aChannelIndex
   * @param refTimestamp
   * @return
   */
  private long getSignalEdgeJumpPosition( final int aChannelIndex, final long refTimestamp )
  {
    SignalDiagramModel model = getSignalDiagramModel();
    if ( this.direction.isLeft() )
    {
      return model.findEdgeBefore( aChannelIndex, refTimestamp );
    }
    return model.findEdgeAfter( aChannelIndex, refTimestamp );
  }

  /**
   * @param aPoint
   * @return
   */
  private long locationToTimestamp( final Point aPoint )
  {
    return getSignalDiagramModel().locationToTimestamp( aPoint );
  }
}
