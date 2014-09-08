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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * @author jawi
 */
public class BundlesDialog extends JDialog implements Closeable
{
  // INNER TYPES

  /**
   * A simple table model for displaying bundle state information.
   */
  final class TableModel extends AbstractTableModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final Bundle[] bundles;

    // CONSTRUCTORS

    /**
     * Creates a new TableModel instance.
     * 
     * @param aBundles
     *          the current active bundles, cannot be <code>null</code>.
     */
    public TableModel( final Bundle[] aBundles )
    {
      this.bundles = aBundles;
    }

    // METHODS

    /**
     * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
     */
    @Override
    public Class<?> getColumnClass( final int aColumnNr )
    {
      return getValueAt( 0, aColumnNr ).getClass();
    }

    /**
     * @see javax.swing.table.TableModel#getColumnCount()
     */
    @Override
    public int getColumnCount()
    {
      return 6;
    }

    /**
     * @see javax.swing.table.AbstractTableModel#getColumnName(int)
     */
    @Override
    public String getColumnName( final int aColumn )
    {
      switch ( aColumn )
      {
        case 0:
          return "";

        case 1:
          return "#";

        case 2:
          return "Symbolic Name";

        case 3:
          return "Version";

        case 4:
          return "State";

        case 5:
          return "Copyright";

        default:
          return null;
      }
    }

    /**
     * @see javax.swing.table.TableModel#getRowCount()
     */
    @Override
    public int getRowCount()
    {
      return this.bundles.length;
    }

    /**
     * @see javax.swing.table.TableModel#getValueAt(int, int)
     */
    @Override
    public Object getValueAt( final int aRowIndex, final int aColumnIndex )
    {
      final Bundle bundle = this.bundles[aRowIndex];
      switch ( aColumnIndex )
      {
        case 0:
          return Boolean.valueOf( bundle.getState() == Bundle.ACTIVE );

        case 1:
          return Long.valueOf( bundle.getBundleId() );

        case 2:
          return bundle.getSymbolicName();

        case 3:
          return bundle.getVersion();

        case 4:
          switch ( bundle.getState() )
          {
            case Bundle.ACTIVE:
              return "Active";
            case Bundle.INSTALLED:
              return "Installed";
            case Bundle.RESOLVED:
              return "Resolved";
            case Bundle.UNINSTALLED:
              return "Uninstalled";
            default:
              return "<???>";
          }
        case 5:
          String copyright = ( String )bundle.getHeaders().get( "Bundle-Copyright" );
          return ( copyright == null ) ? "Unknown" : copyright;

        default:
          return null;
      }
    }

    /**
     * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
     */
    @Override
    public boolean isCellEditable( final int aRow, final int aColumn )
    {
      // Under certain circumstances, the bundles may be enabled/disabled...
      if ( aColumn == 0 )
      {
        return Activator.isDebugMode();
      }
      else
      {
        return false;
      }
    }

    /**
     * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object,
     *      int, int)
     */
    @Override
    public void setValueAt( final Object aValue, final int aRow, final int aColumn )
    {
      final Bundle selectedBundle = this.bundles[aRow];

      if ( selectedBundle.getState() == Bundle.ACTIVE )
      {
        stopBundle( selectedBundle );
      }
      else if ( selectedBundle.getState() == Bundle.RESOLVED )
      {
        startBundle( selectedBundle );
      }

      // Firing this will cause our table to be redrawn...
      fireTableDataChanged();
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Color ALT_COLOR = new Color( 0xCC, 0xCC, 0xCC );
  private static final Color NEW_COLOR = ALT_COLOR.brighter();

  // VARIABLES

  private final BundleContext bundleContext;

  private JTable table;

  // CONSTRUCTORS

  /**
   * Creates a new BundlesDialog instance.
   * 
   * @param aOwner
   *          the owning window, can be <code>null</code>;
   * @param aBundleContext
   *          the bundle context to use for communicating with the OSGi
   *          framework, cannot be <code>null</code>.
   */
  public BundlesDialog( final Window aOwner, final BundleContext aBundleContext )
  {
    super( aOwner, "Running bundles", ModalityType.APPLICATION_MODAL );

    setLocationRelativeTo( aOwner );

    this.bundleContext = aBundleContext;

    initDialog();
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

  /**
   * Display the bundles dialog.
   * 
   * @return always <code>true</code>.
   */
  public boolean showDialog()
  {
    setVisible( true );
    return true;
  }

  /**
   * Starts all bundles.
   * 
   * @param aBundles
   *          the bundles to start, cannot be <code>null</code>.
   */
  final void startAllBundles( final Bundle[] aBundles )
  {
    for ( Bundle bundle : aBundles )
    {
      startBundle( bundle );
    }

    this.table.revalidate();
  }

  /**
   * Starts the given bundle.
   * 
   * @param aBundle
   *          the bundle to start, cannot be <code>null</code>.
   */
  final void startBundle( final Bundle aBundle )
  {
    try
    {
      if ( aBundle.getState() == Bundle.RESOLVED )
      {
        aBundle.start();
      }
    }
    catch ( BundleException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Starting bundle failed!", exception );
    }
  }

  /**
   * Stops all bundles.
   * 
   * @param aBundles
   *          the bundles to stop, cannot be <code>null</code>.
   */
  final void stopAllBundles( final Bundle[] aBundles )
  {
    for ( Bundle bundle : aBundles )
    {
      stopBundle( bundle );
    }

    this.table.revalidate();
  }

  /**
   * Stops the given bundle.
   * 
   * @param aBundle
   *          the bundle to stop, cannot be <code>null</code>.
   */
  final void stopBundle( final Bundle aBundle )
  {
    try
    {
      if ( aBundle.getState() == Bundle.ACTIVE )
      {
        aBundle.stop();
      }
    }
    catch ( BundleException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Stopping bundle failed!", exception );
    }
  }

  /**
   * Creates the bundle view pane.
   * 
   * @return a bundle view pane.
   */
  private JPanel createBundleViewPane()
  {
    final Bundle[] bundles = this.bundleContext.getBundles();

    this.table = new JTable( new TableModel( bundles ) )
    {
      private static final long serialVersionUID = 1L;

      @Override
      public Component prepareRenderer( final TableCellRenderer aRenderer, final int aRow, final int aColumn )
      {
        Component result = super.prepareRenderer( aRenderer, aRow, aColumn );

        if ( !result.getBackground().equals( getSelectionBackground() ) )
        {
          result.setBackground( ( ( aRow % 2 ) != 0 ? ALT_COLOR : NEW_COLOR ) );
        }
        if ( result instanceof JComponent )
        {
          final Object value = getModel().getValueAt( aRow, aColumn );
          ( ( JComponent )result ).setToolTipText( String.valueOf( value ) );
        }
        return result;
      }
    };
    this.table.setColumnSelectionAllowed( false );
    this.table.setRowSelectionAllowed( true );

    this.table.getColumnModel().getColumn( 0 ).setPreferredWidth( 25 );
    this.table.getColumnModel().getColumn( 0 ).setMaxWidth( 25 );
    this.table.getColumnModel().getColumn( 1 ).setPreferredWidth( 30 );
    this.table.getColumnModel().getColumn( 1 ).setMaxWidth( 40 );
    this.table.getColumnModel().getColumn( 2 ).setPreferredWidth( 200 );
    this.table.getColumnModel().getColumn( 3 ).setPreferredWidth( 60 );
    this.table.getColumnModel().getColumn( 3 ).setMaxWidth( 100 );
    this.table.getColumnModel().getColumn( 4 ).setPreferredWidth( 60 );
    this.table.getColumnModel().getColumn( 4 ).setMaxWidth( 100 );

    this.table.getSelectionModel().setSelectionMode( ListSelectionModel.SINGLE_SELECTION );

    final boolean visible = Activator.isDebugMode();

    final JButton selectAll = new JButton( "Select all" );
    selectAll.setVisible( visible );
    selectAll.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        startAllBundles( bundles );
      }
    } );

    final JButton deselectAll = new JButton( "Deselect all" );
    deselectAll.setVisible( visible );
    deselectAll.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        stopAllBundles( bundles );
      }
    } );

    final JButton crashTest = new JButton( "Crash test" );
    crashTest.setVisible( visible );
    crashTest.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        final Runnable thrower = new Runnable()
        {
          public void run()
          {
            throw new RuntimeException( "Test exception; nothing to worry about..." );
          }
        };
        SwingUtilities.invokeLater( thrower );
      }
    } );

    final JPanel selectionPanel = new JPanel();
    selectionPanel.add( selectAll );
    selectionPanel.add( deselectAll );
    selectionPanel.add( crashTest );

    final JPanel view = new JPanel();
    view.setLayout( new BorderLayout() );
    view.add( selectionPanel, BorderLayout.SOUTH );
    view.add( new JScrollPane( this.table ), BorderLayout.CENTER );

    return view;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JPanel editorsPane = createBundleViewPane();
    final JButton cancel = StandardActionFactory.createCloseButton();

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( cancel );

    SwingComponentUtils.setupWindowContentPane( this, editorsPane, buttonPane, cancel );
  }
}
