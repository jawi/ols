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

  /**
   * Provides a decorated tree cell editor to handle our custom signal elements.
   */
  static class ElementCellEditor extends DefaultTreeCellEditor
  {
    // CONSTRUCTORS

    /**
     * Creates a new {@link ElementCellEditor} instance.
     */
    public ElementCellEditor( JTree aTree, DefaultTreeCellRenderer aRenderer )
    {
      super( aTree, aRenderer );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTreeCellEditorComponent( JTree aTree, Object aValue, boolean aIsSelected, boolean aExpanded,
        boolean aLeaf, int aRow )
    {
      ElementTreeNode treeNode = ( ElementTreeNode )aValue;

      return super.getTreeCellEditorComponent( aTree, treeNode.getText(), aIsSelected, aExpanded, aLeaf, aRow );
    }
  }

  /**
   * Custom tree node for representing our signal elements.
   */
  static class ElementTreeNode extends DefaultMutableTreeNode
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    private static final String ROOT_ID = new String( "<root>" );

    // CONSTRUCTORS

    /**
     * Creates a new {@link ElementTreeNode} instance, used as <b>root</b>-node.
     */
    public ElementTreeNode()
    {
      super( ROOT_ID );
    }

    /**
     * Creates a new {@link ElementTreeNode} instance, used as <b>leaf</b>-node.
     */
    public ElementTreeNode( final Channel aElement )
    {
      super( aElement );
      setAllowsChildren( false );
    }

    /**
     * Creates a new {@link ElementTreeNode} instance, used as
     * <b>group</b>-node.
     */
    public ElementTreeNode( final ChannelGroup aGroup )
    {
      super( aGroup );
      setAllowsChildren( true );
    }

    /**
     * Creates a new {@link ElementTreeNode} instance as copy of the given node.
     */
    public ElementTreeNode( final ElementTreeNode aNode )
    {
      super( aNode.getUserObject() );
      setAllowsChildren( aNode.allowsChildren );
    }

    // METHODS

    /**
     * @return the text of this tree node's user object, can be
     *         <code>null</code>.
     */
    public String getText()
    {
      String result = "";
      if ( this.userObject instanceof ChannelGroup )
      {
        result = ( ( ChannelGroup )this.userObject ).getName();
      }
      else if ( this.userObject instanceof Channel )
      {
        result = ( ( Channel )this.userObject ).getLabel();
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void insert( final MutableTreeNode aNewChild, final int aChildIndex )
    {
      super.insert( aNewChild, aChildIndex );

      Object newChildUserObject = ( ( ElementTreeNode )aNewChild ).getUserObject();

      if ( ( this.userObject instanceof ChannelGroup ) && ( newChildUserObject instanceof Channel ) )
      {
        // Move the actual element as well...
        ChannelGroup group = ( ChannelGroup )this.userObject;
        Channel channel = ( Channel )newChildUserObject;

        if ( channel.getGroup() != group || channel.getIndex() != aChildIndex )
        {
          System.out.println( "TODO" );
        }
      }
    }

    /**
     * @return <code>true</code> if this tree node is visible,
     *         <code>false</code> if not.
     */
    public boolean isVisible()
    {
      boolean result = false;
      if ( this.userObject instanceof ChannelGroup )
      {
        result = true;
        // result = ( ( ChannelGroup )this.userObject ).isVisible();
      }
      else if ( this.userObject instanceof Channel )
      {
        result = ( ( Channel )this.userObject ).isEnabled();
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setParent( final MutableTreeNode aNewParent )
    {
      if ( this.parent == aNewParent )
      {
        // Nothing to do...
        return;
      }

      super.setParent( aNewParent );
    }

    /**
     * Sets the text of this tree node's user object.
     * 
     * @param aText
     *          the text to set, can be <code>null</code>.
     */
    public void setText( String aText )
    {
      if ( this.userObject instanceof ChannelGroup )
      {
        ( ( ChannelGroup )this.userObject ).setName( aText );
      }
      else if ( this.userObject instanceof Channel )
      {
        ( ( Channel )this.userObject ).setLabel( aText );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setUserObject( Object aUserObject )
    {
      if ( ( aUserObject != ROOT_ID ) && ( aUserObject instanceof String ) )
      {
        setText( ( String )aUserObject );
      }
      else if ( aUserObject instanceof Boolean )
      {
        setVisible( ( ( Boolean )aUserObject ).booleanValue() );
      }
    }

    /**
     * @param aVisible
     */
    public void setVisible( boolean aVisible )
    {
      if ( this.userObject instanceof ChannelGroup )
      {
        // ( ( ChannelGroup )this.userObject ).setVisible( aVisible ); TODO
      }
      else if ( this.userObject instanceof Channel )
      {
        ( ( Channel )this.userObject ).setEnabled( aVisible );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return String.format( "ElementTreeNode(%s)@%s", getUserObject(), Integer.toHexString( hashCode() ) );
    }
  }

  /**
   * Provides a custom tree node renderer for our signal elements.
   */
  static class ElementTreeRenderer extends DefaultTreeCellRenderer
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTreeCellRendererComponent( final JTree aTree, final Object aValue, final boolean aSelected,
        final boolean aExpanded, final boolean aLeaf, final int aRow, final boolean aHasFocus )
    {
      Object value = aValue;

      if ( value instanceof DefaultMutableTreeNode )
      {
        value = ( ( DefaultMutableTreeNode )value ).getUserObject();
      }

      if ( value instanceof ChannelGroup )
      {
        ChannelGroup elementGroup = ( ChannelGroup )value;
        value = elementGroup.getName();
      }
      else if ( value instanceof Channel )
      {
        Channel signalElement = ( Channel )value;
        value = signalElement.getLabel();
      }

      JLabel delegate = ( JLabel )super.getTreeCellRendererComponent( aTree, value, aSelected, aExpanded, aLeaf, aRow,
          aHasFocus );
      return delegate;
    }
  }

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
    // Enable DnD support: TODO temporarily disabled as we do not yet support
    // moving channels within channel groups...
    this.tree.setDragEnabled( false );
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
