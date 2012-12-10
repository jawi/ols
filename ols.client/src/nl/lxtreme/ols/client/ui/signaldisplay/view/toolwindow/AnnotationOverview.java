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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.signaldisplay.view.toolwindow;


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.component.JLxTable.TableCellRendererAdapter;


/**
 * Provides a dockable tool-window that presents an overview of the annotations.
 */
public class AnnotationOverview extends AbstractViewLayer implements IToolWindow, IDataModelChangeListener,
    IAnnotationDataChangedListener
{
  // INNER TYPES

  /**
   * Provides a cell renderer adapter for rendering the background using the
   * color provided by data annotations.
   */
  private static class AnnotationCellRenderer implements TableCellRendererAdapter
  {
    // VARIABLES

    private Font font; // cache value

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Component render( final Component aCellComponent, final Object aCellValue, final boolean aIsSelected )
    {
      if ( this.font == null )
      {
        Font f = aCellComponent.getFont();
        this.font = new Font( Font.MONOSPACED, Font.PLAIN, ( int )( 0.95 * f.getSize() ) );
      }
      aCellComponent.setFont( this.font );

      if ( aIsSelected )
      {
        return aCellComponent;
      }

      if ( aCellValue instanceof DataAnnotation )
      {
        Object val = ( ( DataAnnotation )aCellValue ).getProperties().get( DataAnnotation.KEY_COLOR );
        if ( val instanceof String )
        {
          aCellComponent.setBackground( ColorUtils.parseColor( ( String )val ) );
        }
        else if ( val instanceof Color )
        {
          aCellComponent.setBackground( ( Color )val );
        }
      }

      return aCellComponent;
    }
  }

  /**
   * Provides a customer key for use in a {@link Map}.
   */
  private static class AnnotationKey implements Comparable<AnnotationKey>
  {
    // VARIABLES

    final long startTime;
    final long endTime;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationKey} instance.
     */
    public AnnotationKey( final DataAnnotation aAnnotation )
    {
      this( aAnnotation.getStartTimestamp(), aAnnotation.getEndTimestamp() );
    }

    /**
     * Creates a new {@link AnnotationKey} instance.
     */
    AnnotationKey( final long aStartTime, final long aEndTime )
    {
      this.startTime = aStartTime;
      this.endTime = aEndTime;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final AnnotationKey aOtherKey )
    {
      int result = ( int )( this.startTime - aOtherKey.startTime );
      if ( result == 0 )
      {
        result = ( int )( aOtherKey.endTime - this.endTime );
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object obj )
    {
      if ( this == obj )
      {
        return true;
      }
      if ( ( obj == null ) || !( obj instanceof AnnotationKey ) )
      {
        return false;
      }

      final AnnotationKey other = ( AnnotationKey )obj;
      if ( ( this.endTime != other.endTime ) || ( this.startTime != other.startTime ) )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + ( int )( this.endTime ^ ( this.endTime >>> 32 ) );
      result = ( prime * result ) + ( int )( this.startTime ^ ( this.startTime >>> 32 ) );
      return result;
    }
  }

  /**
   * Provides a container for annotations that also acts as a table model.
   */
  private static class AnnotationTableModel extends AbstractTableModel implements TableColumnModel,
      PropertyChangeListener
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    private static final String COL_ID = "id";
    private static final String COL_START_TIME = "startTime";
    private static final String COL_END_TIME = "endTime";

    // VARIABLES

    private final SignalDiagramController controller;
    private final AtomicReference<DataHolder> dataRef;

    private volatile ListSelectionModel listSelectionModel;
    private volatile int columnMargin;
    private volatile int totalColumnWidth;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationTableModel} instance.
     */
    public AnnotationTableModel( final SignalDiagramController aController )
    {
      this.controller = aController;
      this.dataRef = new AtomicReference<DataHolder>( new DataHolder( createDefaultColumns() ) );
      this.listSelectionModel = new DefaultListSelectionModel();
    }

    // METHODS

    /**
     * @return
     */
    private static TableColumn[] createDefaultColumns()
    {
      return createDefaultColumns( new TableColumn[3] );
    }

    /**
     * @return
     */
    private static TableColumn[] createDefaultColumns( final TableColumn[] aColumns )
    {
      assert aColumns.length >= 3 : "Too little columns!";
      aColumns[0] = new TableColumn( 0, 50 );
      aColumns[0].setIdentifier( COL_ID );
      aColumns[0].setHeaderValue( "#" );
      aColumns[1] = new TableColumn( 1, 100 );
      aColumns[1].setIdentifier( COL_START_TIME );
      aColumns[1].setHeaderValue( "Start" );
      aColumns[2] = new TableColumn( 2, 100 );
      aColumns[2].setIdentifier( COL_END_TIME );
      aColumns[2].setHeaderValue( "End" );
      return aColumns;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addColumn( final TableColumn aColumn )
    {
      throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addColumnModelListener( final TableColumnModelListener aListener )
    {
      this.listenerList.add( TableColumnModelListener.class, aListener );
    }

    /**
     * Clears this structure, removing all of its data.
     */
    public void clearStructure( final Integer aChannelIdx )
    {
      if ( aChannelIdx == null )
      {
        setDataHolder( new DataHolder( createDefaultColumns() ) );

        fireTableStructureChanged();
      }
      else
      {
        updateStructure();
      }
    }

    /**
     * Returns the data annotation for the row identified by the given index.
     * 
     * @param aRowIndex
     *          the row index of the row to return the data annotation for.
     * @return a data annotation, can be <code>null</code>.
     */
    public DataAnnotation getAnnotation( final int aRowIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      final int colCount = dataHolder.columns.length;

      for ( int i = Math.min( 3, colCount - 1 ); i < colCount; i++ )
      {
        Object value = dataHolder.data[aRowIndex][i];
        if ( value instanceof DataAnnotation )
        {
          return ( DataAnnotation )value;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TableColumn getColumn( final int aColumnIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      return dataHolder.columns[aColumnIndex];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> getColumnClass( final int aColumnIndex )
    {
      switch ( aColumnIndex )
      {
        case 0:
          return Integer.class;

        case 1:
        case 2:
          return Double.class;

        default:
          return DataAnnotation.class;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnCount()
    {
      final DataHolder dataHolder = getDataHolder();
      return dataHolder.columns.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnIndex( final Object aColumnIdentifier )
    {
      final DataHolder dataHolder = getDataHolder();

      int l = dataHolder.columns.length;
      for ( int i = 0; i < l; i++ )
      {
        TableColumn column = dataHolder.columns[i];
        if ( column.getIdentifier().equals( aColumnIdentifier ) )
        {
          return i;
        }
      }
      throw new IllegalArgumentException( "Identifier not found" );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnIndexAtX( final int aXPosition )
    {
      final DataHolder dataHolder = getDataHolder();

      int l = dataHolder.columns.length;
      int w = aXPosition;
      for ( int i = 0; i < l; i++ )
      {
        TableColumn column = dataHolder.columns[i];

        w = w - column.getWidth();
        if ( w < 0 )
        {
          return i;
        }
      }
      return -1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnMargin()
    {
      return this.columnMargin;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getColumnName( final int aColumn )
    {
      final DataHolder dataHolder = getDataHolder();
      return String.valueOf( dataHolder.columns[aColumn].getHeaderValue() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Enumeration<TableColumn> getColumns()
    {
      final DataHolder dataHolder = getDataHolder();
      return new Enumeration<TableColumn>()
      {
        private final TableColumn[] columns = dataHolder.columns;
        private volatile int idx = 0;

        @Override
        public boolean hasMoreElements()
        {
          return this.idx < this.columns.length;
        }

        @Override
        public TableColumn nextElement()
        {
          if ( this.idx >= this.columns.length )
          {
            throw new NoSuchElementException();
          }
          return this.columns[this.idx++];
        }
      };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getColumnSelectionAllowed()
    {
      return false; // never allowed
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRowCount()
    {
      final DataHolder dataHolder = getDataHolder();
      if ( dataHolder == null )
      {
        return 0;
      }

      return dataHolder.data.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSelectedColumnCount()
    {
      return 0; // always zero selected
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getSelectedColumns()
    {
      return new int[0]; // no selection
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ListSelectionModel getSelectionModel()
    {
      return this.listSelectionModel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTotalColumnWidth()
    {
      if ( this.totalColumnWidth < 0 )
      {
        return calculateTotalColumnWidth();
      }
      return this.totalColumnWidth;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getValueAt( final int aRowIndex, final int aColumnIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      if ( dataHolder == null )
      {
        return null;
      }

      return dataHolder.data[aRowIndex][aColumnIndex];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void moveColumn( final int aColumnIndex, final int aNewIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      final TableColumn[] columns = dataHolder.columns;
      final int columnCount = columns.length;

      if ( ( aColumnIndex < 0 ) || ( aColumnIndex >= columnCount ) || ( aNewIndex < 0 ) || ( aNewIndex >= columnCount ) )
      {
        throw new IllegalArgumentException( "moveColumn() - Index out of range" );
      }

      // If the column has not yet moved far enough to change positions
      // post the event anyway, the "draggedDistance" property of the
      // tableHeader will say how far the column has been dragged.
      // Here we are really trying to get the best out of an
      // API that could do with some rethinking. We preserve backward
      // compatibility by slightly bending the meaning of these methods.
      if ( aColumnIndex == aNewIndex )
      {
        fireColumnMoved( new TableColumnModelEvent( this, aColumnIndex, aNewIndex ) );
        return;
      }

      final TableColumn moved = columns[aColumnIndex];
      // Delete element from array...
      System.arraycopy( columns, aColumnIndex + 1, columns, aColumnIndex, columnCount - 1 - aColumnIndex );
      // Make space for new element...
      System.arraycopy( columns, aNewIndex, columns, aNewIndex + 1, columnCount - 1 - aNewIndex );
      // Set actual (inserted) element...
      columns[aNewIndex] = moved;

      boolean selected = this.listSelectionModel.isSelectedIndex( aColumnIndex );
      this.listSelectionModel.removeIndexInterval( aColumnIndex, aColumnIndex );
      this.listSelectionModel.insertIndexInterval( aNewIndex, 1, true );

      if ( selected )
      {
        this.listSelectionModel.addSelectionInterval( aNewIndex, aNewIndex );
      }
      else
      {
        this.listSelectionModel.removeSelectionInterval( aNewIndex, aNewIndex );
      }

      fireColumnMoved( new TableColumnModelEvent( this, aColumnIndex, aNewIndex ) );
    }

    /**
     * {@inheritDoc}
     */
    public void propertyChange( final PropertyChangeEvent aEvent )
    {
      String name = aEvent.getPropertyName();
      if ( ( name == "width" ) || ( name == "preferredWidth" ) )
      {
        invalidateWidthCache();
        // This is a misnomer, we're using this method
        // simply to cause a relayout.
        fireColumnMarginChanged();
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeColumn( final TableColumn aColumn )
    {
      throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeColumnModelListener( final TableColumnModelListener aListener )
    {
      this.listenerList.remove( TableColumnModelListener.class, aListener );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColumnMargin( final int aNewMargin )
    {
      this.columnMargin = aNewMargin;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColumnSelectionAllowed( final boolean aFlag )
    {
      // Ignore; column selection is not allowed...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectionModel( final ListSelectionModel aNewModel )
    {
      this.listSelectionModel = aNewModel;
    }

    /**
     * Updates the structure of this table, which is a <em>heavy</em> operation
     * and should be called with care!
     */
    public void updateStructure()
    {
      final AnnotationData data = getModel().getAnnotationData();
      final SortedSet<Annotation> annotations = data.getAnnotations();

      SortedMap<AnnotationKey, List<DataAnnotation>> dataAnnotations = new TreeMap<AnnotationKey, List<DataAnnotation>>();
      Map<Integer, String> channelNames = new HashMap<Integer, String>();

      // Step 1: determine dimensions of the table...
      for ( Annotation annotation : annotations )
      {
        final Integer channelIdx = Integer.valueOf( annotation.getChannelIndex() );

        if ( annotation instanceof DataAnnotation )
        {
          if ( !channelNames.containsKey( channelIdx ) )
          {
            channelNames.put( channelIdx, null );
          }

          final DataAnnotation dataAnnotation = ( DataAnnotation )annotation;

          AnnotationKey key = new AnnotationKey( dataAnnotation );
          List<DataAnnotation> values = dataAnnotations.get( key );
          if ( values == null )
          {
            values = new ArrayList<DataAnnotation>();
            dataAnnotations.put( key, values );
          }
          values.add( dataAnnotation );
        }
        else if ( annotation instanceof LabelAnnotation )
        {
          channelNames.put( channelIdx, ( ( LabelAnnotation )annotation ).getData() );
        }
      }

      // Step 2: remove empty columns...
      SortedSet<Integer> seenChannels = new TreeSet<Integer>();
      for ( List<DataAnnotation> entry : dataAnnotations.values() )
      {
        for ( DataAnnotation annotation : entry )
        {
          seenChannels.add( Integer.valueOf( annotation.getChannelIndex() ) );
        }
      }

      // Step 3: normalize column indices...
      SortedMap<Integer, Integer> columnIndices = new TreeMap<Integer, Integer>();
      int columnIdx = 0;
      for ( Integer channelIdx : seenChannels )
      {
        columnIndices.put( channelIdx, Integer.valueOf( columnIdx++ ) );
      }

      // Step 4: create data structure...
      final int columnCount = 3 + columnIndices.size();
      Object[][] newData = new Object[dataAnnotations.size()][columnCount];

      final long triggerPos = getTriggerPosition();
      final double sampleRate = getSampleRate();

      // Step 5: fill data structure...
      int row = 0;
      for ( Map.Entry<AnnotationKey, List<DataAnnotation>> entry : dataAnnotations.entrySet() )
      {
        AnnotationKey key = entry.getKey();

        Object[] columnData = new Object[columnCount];

        columnData[0] = Integer.valueOf( row );
        columnData[1] = Double.valueOf( ( key.startTime - triggerPos ) / sampleRate );
        columnData[2] = Double.valueOf( ( key.endTime - triggerPos ) / sampleRate );
        for ( DataAnnotation annotation : entry.getValue() )
        {
          int idx = columnIndices.get( Integer.valueOf( annotation.getChannelIndex() ) ).intValue();
          columnData[idx + 3] = annotation;
        }
        newData[row++] = columnData;
      }

      // Step 6: create column headers...
      TableColumn[] columns = new TableColumn[columnCount];
      createDefaultColumns( columns );
      columnIdx = 3;
      for ( Integer channelIdx : seenChannels )
      {
        TableColumn column = new TableColumn( columnIdx );
        column.setIdentifier( String.format( "col-%d", Integer.valueOf( columnIdx ) ) );
        column.setHeaderValue( channelNames.get( channelIdx ) );
        column.setWidth( 100 );

        columns[columnIdx] = column;
        columnIdx++;
      }

      // Swap...
      setDataHolder( new DataHolder( newData, columns ) );

      fireTableStructureChanged();
    }

    /**
     * Notifies all listeners that have registered interest for notification on
     * this event type. The event instance is lazily created using the
     * parameters passed into the fire method.
     * 
     * @see EventListenerList
     */
    protected void fireColumnMarginChanged()
    {
      ChangeEvent changeEvent = null;

      TableColumnModelListener[] listeners = this.listenerList.getListeners( TableColumnModelListener.class );
      for ( int i = listeners.length - 1; i >= 0; i-- )
      {
        TableColumnModelListener listener = listeners[i];
        if ( changeEvent == null )
        {
          changeEvent = new ChangeEvent( this );
        }
        listener.columnMarginChanged( changeEvent );
      }
    }

    /**
     * Notifies all listeners that have registered interest for notification on
     * this event type. The event instance is lazily created using the
     * parameters passed into the fire method.
     * 
     * @param aEvent
     *          the event received
     * @see EventListenerList
     */
    protected void fireColumnMoved( final TableColumnModelEvent aEvent )
    {
      TableColumnModelListener[] listeners = this.listenerList.getListeners( TableColumnModelListener.class );
      for ( int i = listeners.length - 1; i >= 0; i-- )
      {
        listeners[i].columnMoved( aEvent );
      }
    }

    /**
     * Calculates the total column width.
     */
    private int calculateTotalColumnWidth()
    {
      DataHolder data = getDataHolder();

      int width = 0;
      for ( TableColumn column : data.columns )
      {
        width += column.getWidth();
      }
      return this.totalColumnWidth = width;
    }

    /**
     * @return
     */
    private DataHolder getDataHolder()
    {
      return this.dataRef.get();
    }

    /**
     * @return
     */
    private SignalDiagramModel getModel()
    {
      return this.controller.getSignalDiagramModel();
    }

    /**
     * @return
     */
    private double getSampleRate()
    {
      return getModel().getSampleRate();
    }

    /**
     * @return
     */
    private long getTriggerPosition()
    {
      final Long result = getModel().getTriggerPosition();
      if ( result == null )
      {
        return 0L;
      }
      return result.longValue();
    }

    /**
     * Invalidates the cached total column-width, causing it to be recalculated
     * on request.
     */
    private void invalidateWidthCache()
    {
      this.totalColumnWidth = -1;
    }

    /**
     * @param aDataHolder
     */
    private void setDataHolder( final DataHolder aDataHolder )
    {
      assert aDataHolder != null : "Data Holder cannot be null!";
      for ( TableColumn column : aDataHolder.columns )
      {
        column.addPropertyChangeListener( this );
      }

      DataHolder old;
      do
      {
        old = this.dataRef.get();
      }
      while ( !this.dataRef.compareAndSet( old, aDataHolder ) );

      if ( old != null )
      {
        for ( TableColumn column : old.columns )
        {
          column.removePropertyChangeListener( this );
        }
      }
    }
  }

  /**
   * Provides a table cell renderer for {@link DataAnnotation}s.
   */
  private static class DataAnnotationCellRenderer extends DefaultTableCellRenderer
  {
    private static final long serialVersionUID = 1L;

    private final AnnotationHelper annotationHelper;

    /**
     * Creates a new {@link DataAnnotationCellRenderer} instance.
     */
    public DataAnnotationCellRenderer()
    {
      final SignalDiagramController controller = Client.getInstance().getSignalDiagramController();

      this.annotationHelper = controller.getAnnotationsHelper();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTableCellRendererComponent( final JTable aTable, final Object aValue,
        final boolean aIsSelected, final boolean aHasFocus, final int aRow, final int aColumn )
    {
      String text = "";
      String tooltip = "";

      final DataAnnotation annotation = ( DataAnnotation )aValue;

      if ( annotation != null )
      {
        text = this.annotationHelper.getText( annotation );
        tooltip = this.annotationHelper.getDescription( annotation );
      }

      JLabel result = ( JLabel )super.getTableCellRendererComponent( aTable, text, aIsSelected, aHasFocus, aRow,
          aColumn );
      if ( !"".equals( tooltip ) )
      {
        result.setToolTipText( tooltip );
      }

      // Make sure the row height is sufficient to fill all text...
      int prefHeight = getPreferredSize().height;
      if ( aTable.getRowHeight( aRow ) < prefHeight )
      {
        aTable.setRowHeight( aRow, prefHeight );
      }

      return result;
    }
  }

  /**
   * Data holder for the {@link AnnotationTableModel}.
   */
  private static class DataHolder
  {
    // VARIABLES

    final Object[][] data;
    final TableColumn[] columns;

    // CONSTRUCTORS

    /**
     * Creates a new {@link DataHolder} instance.
     */
    public DataHolder( final Object[][] aData, final TableColumn[] aColumns )
    {
      this.data = aData;
      this.columns = aColumns;
    }

    /**
     * Creates a new {@link DataHolder} instance.
     */
    public DataHolder( final TableColumn[] aColumns )
    {
      this.data = new Object[0][0];
      this.columns = aColumns;
    }
  }

  /**
   * Proivdes a table cell renderer for relative time.
   */
  private static class TimeCellRenderer extends DefaultTableCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getTableCellRendererComponent( final JTable aTable, final Object aValue,
        final boolean aIsSelected, final boolean aHasFocus, final int aRow, final int aColumn )
    {
      Object value = aValue;
      if ( value instanceof Double )
      {
        value = UnitOfTime.format( ( ( Double )value ).doubleValue() );
      }

      JLabel label = ( JLabel )super.getTableCellRendererComponent( aTable, value, aIsSelected, //
          aHasFocus, aRow, aColumn );
      label.setHorizontalAlignment( SwingConstants.RIGHT );

      return label;
    }
  }

  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Annotations";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final AnnotationTableModel container;

  private JLxTable table;
  private JButton exportButton;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationOverview} instance.
   */
  public AnnotationOverview( final SignalDiagramController aController )
  {
    super( aController );

    this.container = new AnnotationTableModel( aController );
  }

  // METHODS

  /**
   * Factory method to create a new {@link AnnotationOverview} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link AnnotationOverview} instance, never <code>null</code>.
   */
  public static AnnotationOverview create( final SignalDiagramController aController )
  {
    final AnnotationOverview result = new AnnotationOverview( aController );

    aController.addAnnotationDataChangedListener( result );
    aController.addDataModelChangeListener( result );

    result.initComponent();

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void annotationDataChanged( final AnnotationData aData )
  {
    this.container.updateStructure();
    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void annotationDataCleared( final Integer aChannelIdx )
  {
    this.container.clearStructure( aChannelIdx );
    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final AcquisitionData aData )
  {
    this.container.updateStructure();
    updateButtonState();
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
   * Jumps to the given annotation in the current signal diagram.
   * 
   * @param aAnnotation
   *          the annotation to jump to, cannot be <code>null</code>.
   */
  final void jumpTo( final DataAnnotation aAnnotation )
  {
    long start = aAnnotation.getStartTimestamp();
    long end = aAnnotation.getEndTimestamp();

    // determine the middle of the annotation...
    Long ptr = Long.valueOf( start + ( ( end - start ) / 2 ) );

    getController().scrollToTimestamp( ptr );
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Annotations" );

    this.table = new JLxTable( this.container, this.container );
    this.table.setCellRendererAdapter( new AnnotationCellRenderer() );
    this.table.setColumnSelectionAllowed( true );
    this.table.setRowSelectionAllowed( true );
    this.table.setSelectionMode( ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
    this.table.setAutoCreateColumnsFromModel( false );
    this.table.setFillsViewportHeight( true );
    this.table.setIntercellSpacing( new Dimension( 2, 2 ) );
    this.table.setAutoCreateRowSorter( false );
    this.table.setShowGrid( false );
    this.table.setDefaultRenderer( DataAnnotation.class, new DataAnnotationCellRenderer() );
    this.table.setDefaultRenderer( Double.class, new TimeCellRenderer() );
    this.table.addMouseListener( new MouseAdapter()
    {
      @Override
      public void mouseClicked( final MouseEvent aEvent )
      {
        if ( aEvent.getClickCount() == 2 )
        {
          // double clicked...
          JTable source = ( JTable )aEvent.getSource();
          int rowIdx = source.getSelectedRow();

          AnnotationTableModel model = ( AnnotationTableModel )source.getModel();

          DataAnnotation annotation = model.getAnnotation( rowIdx );
          if ( annotation != null )
          {
            jumpTo( annotation );
          }
        }
      }
    } );

    this.exportButton = new JButton( "Export ..." );
    this.exportButton.setEnabled( false );

    JComponent buttonPane = SwingComponentUtils.createButtonPane( this.exportButton );

    add( new JScrollPane( this.table ), BorderLayout.CENTER );
    add( buttonPane, BorderLayout.PAGE_END );
  }

  /**
   * Updates the export button state.
   */
  private void updateButtonState()
  {
    final int rowCount = this.table.getModel().getRowCount();
    this.exportButton.setEnabled( rowCount > 0 );
  }
}
