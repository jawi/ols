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


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
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
     * @return the text of this tree node's user object, can be
     *         <code>null</code>.
     */
    public String getText()
    {
      String result = "";
      if ( this.userObject instanceof ElementGroup )
      {
        result = ( ( ElementGroup )this.userObject ).getName();
      }
      else if ( this.userObject instanceof SignalElement )
      {
        result = ( ( SignalElement )this.userObject ).getLabel();
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

      if ( ( this.userObject instanceof ElementGroup ) && ( newChildUserObject instanceof SignalElement ) )
      {
        // Move the actual element as well...
        ( ( ElementGroup )this.userObject ).moveChannel( ( SignalElement )newChildUserObject, aChildIndex );
      }
    }

    /**
     * @return <code>true</code> if this tree node is visible,
     *         <code>false</code> if not.
     */
    public boolean isVisible()
    {
      boolean result = false;
      if ( this.userObject instanceof ElementGroup )
      {
        result = ( ( ElementGroup )this.userObject ).isVisible();
      }
      else if ( this.userObject instanceof SignalElement )
      {
        result = ( ( SignalElement )this.userObject ).isEnabled();
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
      if ( this.userObject instanceof ElementGroup )
      {
        ( ( ElementGroup )this.userObject ).setName( aText );
      }
      else if ( this.userObject instanceof SignalElement )
      {
        ( ( SignalElement )this.userObject ).setLabel( aText );
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
      else if ( aUserObject instanceof IUIElement )
      {
        super.setUserObject( aUserObject );
      }
    }

    /**
     * @param aVisible
     */
    public void setVisible( boolean aVisible )
    {
      if ( this.userObject instanceof ElementGroup )
      {
        ( ( ElementGroup )this.userObject ).setVisible( aVisible );
      }
      else if ( this.userObject instanceof SignalElement )
      {
        ( ( SignalElement )this.userObject ).setEnabled( aVisible );
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

      if ( value instanceof ElementGroup )
      {
        ElementGroup elementGroup = ( ElementGroup )value;
        value = elementGroup.getName();
      }
      else if ( value instanceof SignalElement )
      {
        SignalElement signalElement = ( SignalElement )value;
        value = signalElement.getLabel();
      }

      JLabel delegate = ( JLabel )super.getTreeCellRendererComponent( aTree, value, aSelected, aExpanded, aLeaf, aRow,
          aHasFocus );
      return delegate;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalElementManager elementManager;
  private final SignalElementModel model;

  private volatile boolean dialogResult;

  private JButton addGroupButton;
  private JButton removeGroupButton;

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
  public SignalElementManagerView( final Window aParent, final SignalElementManager aElementManager )
  {
    super( aParent, "Signal Group Management", ModalityType.APPLICATION_MODAL );

    this.elementManager = aElementManager;
    this.model = this.elementManager.createSignalElementModelCopy();

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

    if ( this.dialogResult )
    {
      // User confirmed changes, so update the model of the element manager...
      this.elementManager.setSignalElementModel( this.model );
    }

    return this.dialogResult;
  }

  /**
   * Adds a new group to our tree and element model.
   */
  final void addNewGroup()
  {
    DefaultTreeModel model = ( DefaultTreeModel )this.tree.getModel();

    boolean groupSummaryVisibleByDefault = UIManager.getBoolean( GROUP_SUMMARY_VISIBLE_DEFAULT );
    boolean scopeVisibleByDefault = UIManager.getBoolean( ANALOG_SCOPE_VISIBLE_DEFAULT );

    int groupCount = this.model.getGroups().size();
    String name = String.format( "Group %d", Integer.valueOf( groupCount + 1 ) );

    // Create model structure...
    ElementGroup newGroup = this.model.addGroup( name );
    newGroup.setVisible( true );

    SignalElement groupSummaryElement = SignalElement.createGroupSummaryElement( newGroup );
    groupSummaryElement.setEnabled( groupSummaryVisibleByDefault );
    newGroup.addElement( groupSummaryElement );

    SignalElement analogScopeElement = SignalElement.createAnalogScopeElement( newGroup );
    analogScopeElement.setEnabled( scopeVisibleByDefault );
    newGroup.addElement( analogScopeElement );

    // Create tree structure...
    ElementTreeNode rootNode = ( ElementTreeNode )model.getRoot();

    ElementTreeNode groupNode = new ElementTreeNode( newGroup );
    rootNode.add( groupNode );

    groupNode.add( new ElementTreeNode( groupSummaryElement ) );
    groupNode.add( new ElementTreeNode( analogScopeElement ) );

    int index = rootNode.getIndex( groupNode );

    model.nodesWereInserted( rootNode, new int[] { index } );
    model.nodeStructureChanged( groupNode );

    this.tree.expandRow( index );
  }

  /**
   * Removes the current selected group.
   */
  final void removeSelectedGroup()
  {
    TreePath[] selectionPaths = this.tree.getSelectionPaths();
    if ( selectionPaths == null )
    {
      return;
    }

    DefaultTreeModel model = ( DefaultTreeModel )this.tree.getModel();
    ElementTreeNode rootNode = ( ElementTreeNode )model.getRoot();

    for ( TreePath selectionPath : selectionPaths )
    {
      ElementTreeNode treeNode = ( ElementTreeNode )selectionPath.getLastPathComponent();
      if ( treeNode.getUserObject() instanceof ElementGroup )
      {
        ElementGroup elementGroup = ( ElementGroup )treeNode.getUserObject();

        this.model.removeGroup( elementGroup );

        model.nodesWereRemoved( rootNode, new int[] { rootNode.getIndex( treeNode ) }, new Object[] { treeNode } );
      }
    }
  }

  /**
   * Builds this dialog.
   */
  private void buildDialog()
  {
    JComponent buttonPane = createButtonPane( this.addGroupButton, this.removeGroupButton, this.okButton,
        this.cancelButton );

    JScrollPane scrollPane = new JScrollPane( this.tree );

    setupWindowContentPane( this, scrollPane, buttonPane, this.cancelButton );
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

    this.addGroupButton = new JButton( "Add group" );
    this.addGroupButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        addNewGroup();
      }
    } );

    this.removeGroupButton = new JButton( "Remove group" );
    this.removeGroupButton.setEnabled( false );
    this.removeGroupButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( ActionEvent aEvent )
      {
        removeSelectedGroup();
      }
    } );

    this.tree = new JTree( createTree() );
    this.tree.setRootVisible( false );
    this.tree.setShowsRootHandles( true );
    this.tree.setEditable( true );
    // Enable DnD support...
    this.tree.setDragEnabled( true );
    this.tree.setDropMode( DropMode.INSERT );
    this.tree.setTransferHandler( new JLxTreeTransferHandler() );
    this.tree.addTreeSelectionListener( new TreeSelectionListener()
    {
      @Override
      public void valueChanged( TreeSelectionEvent aEvent )
      {
        JTree tree = ( JTree )aEvent.getSource();
        TreePath[] selectionPaths = tree.getSelectionPaths();

        boolean onlyGroupsSelected = ( selectionPaths != null ) && ( selectionPaths.length > 0 );
        if ( selectionPaths != null )
        {
          for ( TreePath selectionPath : selectionPaths )
          {
            ElementTreeNode comp = ( ElementTreeNode )selectionPath.getLastPathComponent();
            onlyGroupsSelected &= ( comp.getUserObject() instanceof ElementGroup );
          }
        }

        // Avoid the last group to be deleted...
        if ( model.getGroups().size() <= 1 )
        {
          onlyGroupsSelected = false;
        }

        removeGroupButton.setEnabled( onlyGroupsSelected );
      }
    } );

    ElementTreeRenderer cellRenderer = new ElementTreeRenderer();

    this.tree.setCellRenderer( cellRenderer );
    this.tree.setCellEditor( new ElementCellEditor( this.tree, cellRenderer ) );

    // Initially expand all rows...
    for ( int i = 0; i < this.tree.getRowCount(); i++ )
    {
      this.tree.expandRow( i );
    }
  }
}
