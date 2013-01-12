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
package nl.lxtreme.ols.client.ui.action;


import java.awt.*;

import nl.lxtreme.ols.client.ui.action.SmartJumpAction.JumpDirection;
import nl.lxtreme.ols.client.ui.action.SmartJumpAction.JumpType;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.signalelement.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;


/**
 * 
 */
final class SmartJumpHelper
{
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
  public Long getSmartJumpPosition( final SignalElement aSignalElement, final Rectangle viewSize )
  {
    if ( aSignalElement != null )
    {
      Point refPoint = new Point( ( int )Math.round( viewSize.getCenterX() ), aSignalElement.getYposition() );

      long refTimestamp = locationToTimestamp( refPoint );

      switch ( this.type )
      {
        case ANNOTATION:
          return getAnnotationJumpPosition( aSignalElement, refTimestamp );

        case CURSOR:
          return getCursorJumpPosition( refTimestamp );

        case SIGNAL_EDGE:
          return getSignalEdgeJumpPosition( aSignalElement, refTimestamp );
      }
    }

    return null;
  }

  /**
   * @param aSignalElement
   * @param aReferenceTime
   * @return
   */
  private Long getAnnotationJumpPosition( final SignalElement aSignalElement, final long aReferenceTime )
  {
    int channelIdx = aSignalElement.getChannel().getIndex();
    AnnotationHelper helper = this.controller.getAnnotationsHelper();

    DataAnnotation annotation = null;
    if ( this.direction.isLeft() )
    {
      annotation = helper.getAnnotationBefore( channelIdx, aReferenceTime );
    }
    else
    {
      annotation = helper.getAnnotationAfter( channelIdx, aReferenceTime );
    }

    if ( annotation != null )
    {
      long start = annotation.getStartTimestamp();
      long end = annotation.getEndTimestamp();

      return Long.valueOf( start + ( ( end - start ) / 2L ) );
    }

    return null;
  }

  /**
   * @param refTimestamp
   * @return
   */
  private Long getCursorJumpPosition( final long refTimestamp )
  {
    final AcquisitionData data = getSignalDiagramModel().getAcquisitionData();

    long result = this.direction.isLeft() ? Long.MIN_VALUE : Long.MAX_VALUE;
    for ( Cursor cursor : data.getCursors() )
    {
      if ( !cursor.isDefined() )
      {
        continue;
      }

      long timestamp = cursor.getTimestamp();
      if ( this.direction.isLeft() )
      {
        if ( timestamp < refTimestamp )
        {
          result = Math.max( result, timestamp );
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
          result = Math.min( result, timestamp );
        }
      }
    }

    if ( ( result > Long.MIN_VALUE ) && ( result < Long.MAX_VALUE ) )
    {
      return Long.valueOf( result );
    }

    return null;
  }

  /**
   * @return
   */
  private SignalDiagramModel getSignalDiagramModel()
  {
    return this.controller.getSignalDiagramModel();
  }

  /**
   * @param aChannelIndex
   * @param refTimestamp
   * @return
   */
  private Long getSignalEdgeJumpPosition( final SignalElement aSignalElement, final long refTimestamp )
  {
    SignalDiagramModel model = getSignalDiagramModel();

    long timestamp;
    if ( this.direction.isLeft() )
    {
      timestamp = model.findEdgeBefore( aSignalElement, refTimestamp );
    }
    else
    {
      timestamp = model.findEdgeAfter( aSignalElement, refTimestamp );
    }

    return Long.valueOf( timestamp );
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
