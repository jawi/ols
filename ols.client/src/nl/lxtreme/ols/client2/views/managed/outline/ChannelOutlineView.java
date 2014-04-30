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
package nl.lxtreme.ols.client2.views.managed.outline;


import java.awt.*;

import javax.swing.*;
import javax.swing.tree.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;

import com.jidesoft.docking.*;


/**
 * Provides a view showing a channel(group) outline.
 */
public class ChannelOutlineView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "ChannelOutline";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JTree tree;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelOutlineView} instance.
   */
  public ChannelOutlineView()
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
    add( new JScrollPane( this.tree ), BorderLayout.CENTER );

    updateView();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Channel outline" );

    this.tree = new JTree();
    this.tree.setRootVisible( false );
    this.tree.setShowsRootHandles( true );
    this.tree.setEditable( true );
    // Enable DnD support...
    this.tree.setDragEnabled( true );
    this.tree.setDropMode( DropMode.INSERT );
    this.tree.setTransferHandler( new JLxTreeTransferHandler() );

    ElementTreeRenderer cellRenderer = new ElementTreeRenderer();

    this.tree.setCellRenderer( cellRenderer );
    this.tree.setCellEditor( new ElementCellEditor( this.tree, cellRenderer ) );

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
    if ( aData != null )
    {
      updateView( aData.getChannelGroups() );
    }
    else
    {
      updateView();
    }
  }

  /**
   * @return a tree structure based on the initial element model.
   */
  private DefaultMutableTreeNode createTree( ChannelGroup[] aChannelGroups )
  {
    ElementTreeNode rootNode = new ElementTreeNode();

    for ( ChannelGroup group : aChannelGroups )
    {
      ElementTreeNode groupNode = new ElementTreeNode( group );
      rootNode.add( groupNode );

      for ( Channel channel : group.getChannels() )
      {
        groupNode.add( new ElementTreeNode( channel ) );
      }
    }

    return rootNode;
  }

  private void updateView( ChannelGroup... aChannelGroups )
  {
    this.tree.setModel( new DefaultTreeModel( createTree( aChannelGroups ) ) );
    // Initially expand all rows...
    for ( int i = 0; i < this.tree.getRowCount(); i++ )
    {
      this.tree.expandRow( i );
    }

    this.tree.repaint();
  }
}
