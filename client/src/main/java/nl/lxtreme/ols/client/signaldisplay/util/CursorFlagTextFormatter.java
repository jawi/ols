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


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Provides a text formatter for cursor flags.
 */
public final class CursorFlagTextFormatter
{
  // INNER TYPES

  /**
   * Denotes how to represent a cursor label. Used for automatic placement of
   * cursor labels.
   */
  public static enum LabelStyle
  {
    INDEX_ONLY, TIME_ONLY, LABEL_ONLY, INDEX_LABEL, LABEL_TIME;
  }

  // VARIABLES

  private final SignalDiagramModel model;

  // CONSTRUCTORS

  /**
   * Creates a new CursorFlagRenderer instance.
   */
  public CursorFlagTextFormatter( final SignalDiagramModel aModel )
  {
    this.model = aModel;
  }

  // METHODS

  /**
   * Returns the cursor flag text for the cursor with the given index.
   * 
   * @param aModel
   *          the signal diagram model;
   * @param aCursorTimestamp
   *          the timestamp of the cursor;
   * @param aStyle
   *          the style of the cursor flag text, cannot be <code>null</code>.
   * @return a cursor flag text, or an empty string if the cursor with the given
   *         index is undefined.
   */
  public static String getCursorFlagText( final SignalDiagramModel aModel, final Cursor aCursor, final LabelStyle aStyle )
  {
    return new CursorFlagTextFormatter( aModel ).getCursorFlagText( aCursor, aStyle );
  }

  /**
   * Returns the cursor flag text for the cursor with the given index.
   * 
   * @param aCursor
   *          the cursor to render, cannot be <code>null</code>;
   * @param aStyle
   *          the style of the cursor flag text, cannot be <code>null</code>.
   * @return a cursor flag text, or an empty string if the cursor with the given
   *         index is undefined.
   */
  public String getCursorFlagText( final Cursor aCursor, final LabelStyle aStyle )
  {
    if ( !aCursor.isDefined() )
    {
      return "";
    }

    final double timestamp = this.model.getTimestamp( aCursor.getTimestamp() );

    final String timestampText;
    if ( this.model.hasTimingData() )
    {
      timestampText = Unit.Time.toUnit( timestamp ).formatHumanReadable( timestamp );
    }
    else
    {
      timestampText = "#" + ( int )timestamp;
    }

    Integer index = Integer.valueOf( aCursor.getIndex() + 1 );

    String label = aCursor.getLabel();
    if ( !aCursor.hasLabel() )
    {
      label = index.toString();
    }

    switch ( aStyle )
    {
      case LABEL_TIME:
        return String.format( "%s: %s", label, timestampText );
      case INDEX_LABEL:
        return String.format( "%d: %s", index, label );
      case TIME_ONLY:
        return timestampText;
      case LABEL_ONLY:
        return label;
      case INDEX_ONLY:
      default:
        return String.format( "%d", index );
    }
  }
}
