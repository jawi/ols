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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing;


import java.awt.datatransfer.*;
import java.util.*;

import javax.swing.*;
import javax.swing.tree.*;


/**
 * Provides a custom {@link TransferHandler} for use in a {@link JTree}.
 * <p>
 * Based on: &lt;http://www.coderanch.com/t/346509/GUI/java/JTree-drag-drop-tree-Java&gt;.
 * </p>
 */
public class JLxTreeTransferHandler extends TransferHandler
{
  // INNER TYPES

  /**
   * 
   */
  public class NodesTransferable implements Transferable
  {
    private final DefaultMutableTreeNode[] nodes;

    public NodesTransferable( DefaultMutableTreeNode[] nodes )
    {
      this.nodes = nodes;
    }

    public Object getTransferData( DataFlavor flavor ) throws UnsupportedFlavorException
    {
      if ( !isDataFlavorSupported( flavor ) )
      {
        throw new UnsupportedFlavorException( flavor );
      }
      return this.nodes;
    }

    public DataFlavor[] getTransferDataFlavors()
    {
      return flavors;
    }

    public boolean isDataFlavorSupported( DataFlavor flavor )
    {
      return nodesFlavor.equals( flavor );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final DataFlavor nodesFlavor;
  private final DataFlavor[] flavors;
  private DefaultMutableTreeNode[] nodesToRemove;

  // CONSTRUCTORS

  /**
   * Creates a new {@link JLxTreeTransferHandler} instance.
   */
  public JLxTreeTransferHandler()
  {
    try
    {
      String mimeType = String.format( "%s;class=\"%s\"", DataFlavor.javaJVMLocalObjectMimeType,
          DefaultMutableTreeNode[].class.getName() );

      this.nodesFlavor = new DataFlavor( mimeType );
      this.flavors = new DataFlavor[] { this.nodesFlavor };
    }
    catch ( ClassNotFoundException e )
    {
      throw new RuntimeException( "ClassNotFound: " + e.getMessage() );
    }
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canImport( TransferSupport aSupport )
  {
    if ( !aSupport.isDrop() )
    {
      return false;
    }

    aSupport.setShowDropLocation( true );

    if ( !aSupport.isDataFlavorSupported( this.nodesFlavor ) )
    {
      return false;
    }

    JTree.DropLocation dropLocation = ( JTree.DropLocation )aSupport.getDropLocation();
    JTree tree = ( JTree )aSupport.getComponent();

    // Do not allow a drop on the drag source selections.
    int dropRow = tree.getRowForPath( dropLocation.getPath() );
    int[] selRows = tree.getSelectionRows();
    for ( int i = 0; i < selRows.length; i++ )
    {
      if ( selRows[i] == dropRow )
      {
        return false;
      }
    }

    // Do not allow MOVE-action drops if a non-leaf node is
    // selected unless all of its children are also selected.
    int action = aSupport.getDropAction();
    if ( action == MOVE )
    {
      return haveCompleteNode( tree );
    }

    // Do not allow a non-leaf node to be copied to a level
    // which is less than its source level.
    TreePath dest = dropLocation.getPath();
    DefaultMutableTreeNode target = ( DefaultMutableTreeNode )dest.getLastPathComponent();

    TreePath path = tree.getPathForRow( selRows[0] );
    DefaultMutableTreeNode firstNode = ( DefaultMutableTreeNode )path.getLastPathComponent();

    if ( ( firstNode.getChildCount() > 0 ) && ( target.getLevel() < firstNode.getLevel() ) )
    {
      return false;
    }

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getSourceActions( JComponent aComponent )
  {
    if ( aComponent instanceof JTree )
    {
      return TransferHandler.COPY_OR_MOVE;
    }
    return TransferHandler.NONE;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean importData( TransferSupport aSupport )
  {
    if ( !canImport( aSupport ) )
    {
      return false;
    }

    // Extract transfer data.
    DefaultMutableTreeNode[] nodes = null;
    try
    {
      Transferable t = aSupport.getTransferable();
      nodes = ( DefaultMutableTreeNode[] )t.getTransferData( nodesFlavor );
    }
    catch ( UnsupportedFlavorException ufe )
    {
      System.out.println( "UnsupportedFlavor: " + ufe.getMessage() );
    }
    catch ( java.io.IOException ioe )
    {
      System.out.println( "I/O error: " + ioe.getMessage() );
    }
    // Get drop location info.
    JTree.DropLocation dl = ( JTree.DropLocation )aSupport.getDropLocation();
    int childIndex = dl.getChildIndex();
    TreePath dest = dl.getPath();
    DefaultMutableTreeNode parent = ( DefaultMutableTreeNode )dest.getLastPathComponent();
    JTree tree = ( JTree )aSupport.getComponent();
    DefaultTreeModel model = ( DefaultTreeModel )tree.getModel();
    // Configure for drop mode.
    int index = childIndex; // DropMode.INSERT
    if ( childIndex == -1 )
    { // DropMode.ON
      index = parent.getChildCount();
    }
    // Add data to model.
    for ( int i = 0; i < nodes.length; i++ )
    {
      model.insertNodeInto( nodes[i], parent, index++ );
    }
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Transferable createTransferable( JComponent aComponent )
  {
    if ( !( aComponent instanceof JTree ) )
    {
      return null;
    }

    TreePath[] paths = ( ( JTree )aComponent ).getSelectionPaths();
    if ( paths != null )
    {
      // Make up a node array of copies for transfer and
      // another for/of the nodes that will be removed in
      // exportDone after a successful drop.
      List<DefaultMutableTreeNode> copies = new ArrayList<DefaultMutableTreeNode>();
      List<DefaultMutableTreeNode> toRemove = new ArrayList<DefaultMutableTreeNode>();

      DefaultMutableTreeNode node = ( DefaultMutableTreeNode )paths[0].getLastPathComponent();
      DefaultMutableTreeNode copy = copy( node );

      copies.add( copy );
      toRemove.add( node );

      for ( int i = 1; i < paths.length; i++ )
      {
        DefaultMutableTreeNode next = ( DefaultMutableTreeNode )paths[i].getLastPathComponent();
        // Do not allow higher level nodes to be added to list.
        if ( next.getLevel() < node.getLevel() )
        {
          break;
        }
        else if ( next.getLevel() > node.getLevel() )
        { // child node
          copy.add( copy( next ) );
          // node already contains child
        }
        else
        { // sibling
          copies.add( copy( next ) );
          toRemove.add( next );
        }
      }

      DefaultMutableTreeNode[] nodes = copies.toArray( new DefaultMutableTreeNode[copies.size()] );
      this.nodesToRemove = toRemove.toArray( new DefaultMutableTreeNode[toRemove.size()] );

      return new NodesTransferable( nodes );
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void exportDone( JComponent aComponent, Transferable aTransferable, int aAction )
  {
    if ( ( aAction & MOVE ) == MOVE )
    {
      JTree tree = ( JTree )aComponent;

      DefaultTreeModel model = ( DefaultTreeModel )tree.getModel();
      // Remove nodes saved in nodesToRemove in createTransferable.
      for ( int i = 0; i < this.nodesToRemove.length; i++ )
      {
        model.removeNodeFromParent( this.nodesToRemove[i] );
      }
    }
  }

  /** Defensive copy used in createTransferable. */
  private DefaultMutableTreeNode copy( DefaultMutableTreeNode aNode )
  {
    DefaultMutableTreeNode clone = ( DefaultMutableTreeNode )aNode.clone();
    clone.setUserObject( aNode.getUserObject() );
    return clone;
  }

  private boolean haveCompleteNode( JTree tree )
  {
    int[] selRows = tree.getSelectionRows();
    TreePath path = tree.getPathForRow( selRows[0] );
    DefaultMutableTreeNode first = ( DefaultMutableTreeNode )path.getLastPathComponent();
    int childCount = first.getChildCount();
    // first has children and no children are selected.
    if ( childCount > 0 && selRows.length == 1 )
      return false;
    // first may have children.
    for ( int i = 1; i < selRows.length; i++ )
    {
      path = tree.getPathForRow( selRows[i] );
      DefaultMutableTreeNode next = ( DefaultMutableTreeNode )path.getLastPathComponent();
      if ( first.isNodeChild( next ) )
      {
        // Found a child of first.
        if ( childCount > selRows.length - 1 )
        {
          // Not all children of first are selected.
          return false;
        }
      }
    }
    return true;
  }
}
