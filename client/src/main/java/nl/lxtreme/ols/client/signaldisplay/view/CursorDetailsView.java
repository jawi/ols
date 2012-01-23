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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.view;


import static nl.lxtreme.ols.util.DisplayUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.cursor.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * 
 */
public class CursorDetailsView extends AbstractViewLayer implements ICursorChangeListener, HyperlinkListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final JEditorPane cursorInfoField;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDetailsView instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  private CursorDetailsView( final SignalDiagramController aController )
  {
    super( aController );

    this.cursorInfoField = new JEditorPane( "text/html", asText() );

    initComponent();
  }

  // METHODS

  /**
   * Factory method to create a new {@link CursorDetailsView} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link CursorDetailsView} instance, never <code>null</code>.
   */
  public static CursorDetailsView create( final SignalDiagramController aController )
  {
    final CursorDetailsView result = new CursorDetailsView( aController );

    aController.addCursorChangeListener( result );

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorAdded( final Cursor aCursor )
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorChanged( final String aPropertyName, final Cursor aOldCursor, final Cursor aNewCursor )
  {
    if ( !ICursorChangeListener.PROPERTY_COLOR.equals( aPropertyName ) )
    {
      updateViewText();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorRemoved( final Cursor aOldCursor )
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsInvisible()
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsVisible()
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void hyperlinkUpdate( final HyperlinkEvent aEvent )
  {
    if ( HyperlinkEvent.EventType.ACTIVATED.equals( aEvent.getEventType() ) )
    {
      String desc = aEvent.getDescription();
      if ( desc.startsWith( "#" ) )
      {
        desc = desc.substring( 1 );
      }

      try
      {
        long value = Long.parseLong( desc );
        getController().getSignalDiagram().scrollToTimestamp( 0, value );
      }
      catch ( NumberFormatException exception )
      {
        // Ignore...
      }
    }
  }

  /**
   * @param aEvent
   * @return
   */
  private String asText()
  {
    final SignalDiagramController ctrl = getController();

    final SignalDiagramModel model = ctrl.getSignalDiagramModel();
    if ( ( model == null ) || !model.isCursorMode() )
    {
      return "";
    }

    final Font labelFont = UIManager.getFont( LafDefaults.SWING_LABEL_FONT );

    final StringBuilder sb = new StringBuilder( "<html><head><style>td, th {" );
    sb.append( toCssString( labelFont ) ).append( "} th { font-weight: bold; }</style></head><body><table>" );
    for ( int c = 0; c < CursorImpl.MAX_CURSORS; c++ )
    {
      final Cursor cursor = model.getCursor( c );
      if ( !cursor.isDefined() )
      {
        continue;
      }

      String label = cursor.getLabel();
      if ( !cursor.hasLabel() )
      {
        label = "";
      }

      sb.append( "<tr><th align='right'>" );
      sb.append( c + 1 ).append( ":" ).append( "</th>" );
      sb.append( "<td>" ).append( label ).append( "</td>" );
      sb.append( "<td align='right'><a href='#" ).append( cursor.getTimestamp() ).append( "'>" );
      sb.append( displayTime( cursor.getTimestamp() / ( double )model.getSampleRate() ) );
      sb.append( "</a></td></tr>" );
    }
    sb.append( "</table></body></html>" );
    return sb.toString();
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );

    setLayout( new BorderLayout() );

    add( this.cursorInfoField, BorderLayout.NORTH );

    this.cursorInfoField.setEditable( false );
    this.cursorInfoField.setOpaque( false );
    this.cursorInfoField.addHyperlinkListener( this );
  }

  /**
   * Updates the view text and schedules this component for a repaint.
   */
  private void updateViewText()
  {
    this.cursorInfoField.setText( asText() );
    repaint( 50L );
  }
}
