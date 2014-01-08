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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.cursors;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor.LabelStyle;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.component.ClickableLink.LinkListener;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the defined cursors.
 */
public class CursorDetailsView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "CursorDetails";

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorDetailsView} instance.
   */
  public CursorDetailsView()
  {
    super( ID );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected void build()
  {
    updateViewText( null, false );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Cursor details" );

    aFrame.setInitIndex( 0 );

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    if ( aController != null )
    {
      Cursor[] cursors = aData.getCursors();

      updateViewText( aController, aData.areCursorsVisible(), cursors );
    }
    else
    {
      updateViewText( null, false );
    }
  }

  private void updateViewText( final ViewController aController, boolean aCursorsVisible, Cursor... aCursors )
  {
    JPanel panel = new JPanel( new SpringLayout() );

    for ( Cursor cursor : aCursors )
    {
      if ( !cursor.isDefined() )
      {
        continue;
      }

      String label = cursor.getLabel( LabelStyle.INDEX_LABEL );
      String linkText = cursor.getLabel( LabelStyle.TIME_ONLY );

      ClickableLink link = new ClickableLink( linkText, Long.valueOf( cursor.getTimestamp() ) );
      link.setLinkListener( new LinkListener()
      {
        @Override
        public void linkActivated( Object aLinkId )
        {
          long timestamp = ( ( Long )aLinkId ).longValue();
          aController.scrollToTimestamp( timestamp );
        }
      } );
      link.setEnabled( aCursorsVisible );
      link.setForeground( Color.BLUE );

      panel.add( createRightAlignedLabel( label.concat( ":" ) ) );
      panel.add( link );
    }

    if ( panel.getComponentCount() == 0 )
    {
      panel.add( createRightAlignedLabel( "No" ) );
      panel.add( new JLabel( "cursors defined." ) );
    }

    makeEditorGrid( panel );

    removeAll();
    add( panel, BorderLayout.NORTH );

    validate();
    repaint();
  }
}
