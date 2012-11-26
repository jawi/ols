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
package nl.lxtreme.ols.client.signaldisplay.view.toolwindow;


import static nl.lxtreme.ols.client.signaldisplay.view.CursorFlagTextFormatter.*;
import static nl.lxtreme.ols.util.swing.SpringLayoutUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.marker.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.client.signaldisplay.view.CursorFlagTextFormatter.LabelStyle;
import nl.lxtreme.ols.client.signaldisplay.view.toolwindow.ClickableLink.LinkListener;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a dockable tool window that shows the details of the defined
 * cursors.
 */
public class CursorDetailsView extends AbstractViewLayer implements IToolWindow, IMarkerChangeListener,
    IDataModelChangeListener, LinkListener
{
  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Cursor";

  private static final long serialVersionUID = 1L;

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
    aController.addDataModelChangeListener( result );

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerAdded( final Marker aCursor )
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerChanged( final String aPropertyName, final Marker aNewCursor )
  {
    if ( !IMarkerChangeListener.PROPERTY_COLOR.equals( aPropertyName ) )
    {
      updateViewText();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerMoved( final long aOldTimestamp, final long aNewTimestamp )
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerRemoved( final Marker aOldCursor )
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
  public void dataModelChanged( final AcquisitionData aData )
  {
    updateViewText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon getIcon()
  {
    return null; // XXX
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return ID;
  }

  /**
   * @param aLinkId
   */
  @Override
  public void linkActivated( final Object aLinkId )
  {
    if ( aLinkId instanceof Long )
    {
      long timestamp = ( ( Long )aLinkId ).longValue();
      getController().getSignalDiagram().scrollToTimestamp( timestamp );
    }
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Marker details" );

    updateViewText();
  }

  /**
   * Updates the view text and schedules this component for a repaint.
   */
  private void updateViewText()
  {
    final SignalDiagramController ctrl = getController();

    final SignalDiagramModel model = ctrl.getSignalDiagramModel();

    final boolean cursorsEnabled;
    final Marker[] markers;
    if ( model != null )
    {
      markers = getDefinedMarkers();
      cursorsEnabled = model.isCursorMode();
    }
    else
    {
      markers = new Marker[0];
      cursorsEnabled = false;
    }

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JPanel panel = new JPanel( new SpringLayout() );

        addSeparator( panel, "Markers" );

        for ( Marker marker : markers )
        {
          if ( !marker.isDefined() )
          {
            continue;
          }

          String label = "" + ( marker.getIndex() + 1 );
          if ( marker.hasLabel() )
          {
            label = label.concat( ", " ).concat( marker.getLabel() );
          }

          panel.add( createRightAlignedLabel( label.concat( ":" ) ) );

          String linkText = getCursorFlagText( model, marker, LabelStyle.TIME_ONLY );

          ClickableLink link = new ClickableLink( linkText, Long.valueOf( marker.getTimestamp() ) );
          link.setLinkListener( CursorDetailsView.this );
          link.setEnabled( cursorsEnabled );
          link.setForeground( Color.BLUE );

          panel.add( link );
        }

        if ( markers.length == 0 )
        {
          panel.add( createRightAlignedLabel( "No" ) );
          panel.add( new JLabel( "markers defined." ) );
        }

        makeEditorGrid( panel, 10, 10 );

        removeAll();
        add( panel, BorderLayout.NORTH );

        validate();
        repaint();
      }
    } );
  }
}
