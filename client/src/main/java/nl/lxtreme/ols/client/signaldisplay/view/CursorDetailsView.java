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


import static nl.lxtreme.ols.util.swing.SpringLayoutUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static nl.lxtreme.ols.client.signaldisplay.util.CursorFlagTextFormatter.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.util.ClickableLink.LinkListener;
import nl.lxtreme.ols.client.signaldisplay.util.CursorFlagTextFormatter.LabelStyle;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a dockable tool window that shows the details of the defined
 * cursors.
 */
public class CursorDetailsView extends AbstractViewLayer implements IToolWindow, ICursorChangeListener,
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
  public void dataModelChanged( final DataSet aDataSet )
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
      getController().scrollToTimestamp( timestamp );
    }
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Cursor details" );

    updateViewText();
  }

  /**
   * Updates the view text and schedules this component for a repaint.
   */
  private void updateViewText()
  {
    final SignalDiagramController ctrl = getController();

    final SignalDiagramModel model = ctrl.getViewModel();
    final boolean cursorsEnabled;

    final Cursor[] cursors;
    if ( model != null )
    {
      cursors = model.getDefinedCursors();
      cursorsEnabled = model.isCursorMode();
    }
    else
    {
      cursors = new Cursor[0];
      cursorsEnabled = false;
    }

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        final JPanel panel = new JPanel( new SpringLayout() );

        addSeparator( panel, "Cursors" );

        for ( Cursor cursor : cursors )
        {
          String label = "" + ( cursor.getIndex() + 1 );
          if ( cursor.hasLabel() )
          {
            label = label.concat( ", " ).concat( cursor.getLabel() );
          }

          panel.add( createRightAlignedLabel( label.concat( ":" ) ) );

          String linkText = getCursorFlagText( model, cursor, LabelStyle.TIME_ONLY );

          ClickableLink link = new ClickableLink( linkText, Long.valueOf( cursor.getTimestamp() ) );
          link.setLinkListener( CursorDetailsView.this );
          link.setEnabled( cursorsEnabled );
          link.setForeground( Color.BLUE );

          panel.add( link );
        }

        if ( cursors.length == 0 )
        {
          panel.add( createRightAlignedLabel( "No" ) );
          panel.add( new JLabel( "cursors defined." ) );
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
