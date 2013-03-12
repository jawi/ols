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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay.signalelement;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.DialogStatus;
import nl.lxtreme.ols.util.swing.StandardActionFactory.StatusAwareCloseableDialog;


/**
 * Provides a view for the signal element manager.
 */
public class SignalElementManagerView extends JDialog implements StatusAwareCloseableDialog
{
  // INNER TYPES

  /**
   * 
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
     * Creates a new {@link ElementTreeNode} instance, used as
     * <b>group</b>-node.
     */
    public ElementTreeNode( final ElementGroup aGroup )
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

    /**
     * Creates a new {@link ElementTreeNode} instance, used as <b>leaf</b>-node.
     */
    public ElementTreeNode( final SignalElement aElement )
    {
      super( aElement );
      setAllowsChildren( false );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void insert( final MutableTreeNode aNewChild, final int aChildIndex )
    {
      super.insert( aNewChild, aChildIndex );

      Object newChildUserObject = ( ( ElementTreeNode )aNewChild ).getUserObject();

      if ( ( this.userObject instanceof ElementGroup ) && ( newChildUserObject instanceof SignalElement ) )
      {
        // Move the actual element as well...
        ( ( ElementGroup )this.userObject ).moveChannel( ( SignalElement )newChildUserObject, aChildIndex );
      }
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

      if ( this.parent != null )
      {
        System.out.println( this + " parent updated from " + this.parent + " to " + aNewParent );
      }

      super.setParent( aNewParent );
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

  static class ElementTreeRenderer extends DefaultTreeCellRenderer
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTreeCellRendererComponent( final JTree aTree, final Object aValue, final boolean aSel,
        final boolean aExpanded, final boolean aLeaf, final int aRow, final boolean aHasFocus )
    {
      Object value = aValue;
      if ( value instanceof DefaultMutableTreeNode )
      {
        value = ( ( DefaultMutableTreeNode )value ).getUserObject();
      }
      if ( value instanceof ElementGroup )
      {
        value = ( ( ElementGroup )value ).getName();
      }
      if ( value instanceof SignalElement )
      {
        value = ( ( SignalElement )value ).getLabel();
      }
      return super.getTreeCellRendererComponent( aTree, value, aSel, aExpanded, aLeaf, aRow, aHasFocus );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalElementModel model;

  private volatile boolean dialogResult;

  private JButton addGroupButton;

  private JButton cancelButton;
  private JButton okButton;
  private JTree tree;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElementManagerView} instance.
   * 
   * @param aParent
   *          the (optional) parent of this dialog, can be <code>null</code>;
   * @param aElementManager
   *          the signal element manager to use, cannot be <code>null</code>.
   */
  public SignalElementManagerView( final Window aParent, final SignalElementModel aModel )
  {
    super( aParent, "Signal Group Management", ModalityType.APPLICATION_MODAL );

    this.model = aModel;

    initDialog();
    buildDialog();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable#close()
   */
  @Override
  public void close()
  {
    setVisible( false );
    dispose();
  }

  @Override
  public boolean setDialogStatus( final DialogStatus aStatus )
  {
    this.dialogResult = ( aStatus == DialogStatus.OK );
    return true;
  }

  /**
   * @return <code>true</code> if this view was acknowledged positively (ie, Ok
   *         was pressed), <code>false</code> if it was cancelled.
   */
  public boolean showDialog()
  {
    this.dialogResult = false;
    setVisible( true ); // blocks...
    return this.dialogResult;
  }

  /**
   * Adds a new group to our tree and element model.
   */
  final void addNewGroup()
  {
    DefaultTreeModel model = ( DefaultTreeModel )this.tree.getModel();

    int groupCount = this.model.getGroups().size();
    String name = String.format( "Group %d", Integer.valueOf( groupCount + 1 ) );

    // Create model structure...
    ElementGroup newGroup = this.model.addGroup( name );
    newGroup.setVisible( true );

    SignalElement groupSummaryElement = SignalElement.createGroupSummaryElement( newGroup );
    groupSummaryElement.setEnabled( true );
    newGroup.addElement( groupSummaryElement );

    SignalElement analogScopeElement = SignalElement.createAnalogScopeElement( newGroup );
    analogScopeElement.setEnabled( true );
    newGroup.addElement( analogScopeElement );

    // Create tree structure...
    ElementTreeNode rootNode = ( ElementTreeNode )model.getRoot();

    ElementTreeNode groupNode = new ElementTreeNode( newGroup );
    rootNode.add( groupNode );

    groupNode.add( new ElementTreeNode( groupSummaryElement ) );
    groupNode.add( new ElementTreeNode( analogScopeElement ) );

    model.nodesWereInserted( rootNode, new int[] { rootNode.getIndex( groupNode ) } );
    model.nodeStructureChanged( groupNode );
  }

  /**
   * Builds this dialog.
   */
  private void buildDialog()
  {
    JComponent buttonPane = SwingComponentUtils
        .createButtonPane( this.addGroupButton, this.okButton, this.cancelButton );

    JScrollPane scrollPane = new JScrollPane( this.tree );

    SwingComponentUtils.setupWindowContentPane( this, scrollPane, buttonPane, this.cancelButton );
  }

  /**
   * @return a tree structure based on the initial element model.
   */
  private DefaultMutableTreeNode createTree()
  {
    ElementTreeNode rootNode = new ElementTreeNode();

    for ( ElementGroup group : this.model.getGroups() )
    {
      ElementTreeNode groupNode = new ElementTreeNode( group );
      rootNode.add( groupNode );

      for ( SignalElement element : group.getElements() )
      {
        groupNode.add( new ElementTreeNode( element ) );
      }
    }

    return rootNode;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    this.okButton = StandardActionFactory.createOkButton();
    this.cancelButton = StandardActionFactory.createCancelButton();

    this.addGroupButton = new JButton( "+" );
    this.addGroupButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        addNewGroup();
      }
    } );

    this.tree = new JTree( createTree() );
    this.tree.setRootVisible( false );
    this.tree.setShowsRootHandles( true );
    this.tree.setEditable( true );
    this.tree.setCellRenderer( new ElementTreeRenderer() );
    // Enable DnD support...
    this.tree.setDragEnabled( true );
    this.tree.setDropMode( DropMode.INSERT );
    this.tree.setTransferHandler( new JLxTreeTransferHandler() );
  }
}
