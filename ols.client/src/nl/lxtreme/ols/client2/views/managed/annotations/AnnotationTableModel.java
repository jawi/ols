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
package nl.lxtreme.ols.client2.views.managed.annotations;


import java.beans.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a container for annotations that also acts as a table model.
 */
class AnnotationTableModel extends AbstractTableModel implements TableColumnModel, PropertyChangeListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String COL_ID = "id";
  private static final String COL_START_TIME = "startTime";
  private static final String COL_END_TIME = "endTime";

  // VARIABLES

  private final AcquisitionData data;
  private final AnnotationData annotations;
  private final AtomicReference<DataHolder> dataRef;

  private volatile ListSelectionModel listSelectionModel;
  private volatile int columnMargin;
  private volatile int totalColumnWidth;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationTableModel} instance.
   */
  public AnnotationTableModel()
  {
    this( null, null );
  }

  /**
   * Creates a new {@link AnnotationTableModel} instance.
   */
  public AnnotationTableModel( AcquisitionData aData, AnnotationData aAnnotations )
  {
    this.data = aData;
    this.annotations = aAnnotations;
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
    aColumns[0].setPreferredWidth( 50 );
    aColumns[1] = new TableColumn( 1, 100 );
    aColumns[1].setIdentifier( COL_START_TIME );
    aColumns[1].setHeaderValue( "Start" );
    aColumns[1].setPreferredWidth( 100 );
    aColumns[2] = new TableColumn( 2, 100 );
    aColumns[2].setIdentifier( COL_END_TIME );
    aColumns[2].setHeaderValue( "End" );
    aColumns[2].setPreferredWidth( 100 );
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
    if ( colCount < 1 )
    {
      return null;
    }

    for ( int i = Math.min( 3, colCount - 1 ); i < colCount; i++ )
    {
      Object[] row = dataHolder.data[aRowIndex];
      if ( row.length <= i )
      {
        continue;
      }

      Object value = row[i];
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
    if ( this.annotations == null )
    {
      return;
    }

    SortedSet<Annotation> annotations = this.annotations.getAnnotations();
    SortedMap<AnnotationKey, List<DataAnnotation>> dataAnnotations = new TreeMap<AnnotationKey, List<DataAnnotation>>();
    Map<Integer, String> channelNames = new HashMap<Integer, String>();

    // Step 1: determine dimensions of the table...
    for ( Annotation annotation : annotations )
    {
      Channel channel = annotation.getChannel();
      if ( channel == null )
      {
        continue;
      }

      Integer channelIdx = Integer.valueOf( channel.getIndex() );
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
      else
      {
        System.out.println( "Ignoring annotation: " + annotation );
      }
    }

    // Step 2: remove empty columns...
    SortedSet<Integer> seenChannels = new TreeSet<Integer>();
    for ( List<DataAnnotation> entry : dataAnnotations.values() )
    {
      for ( DataAnnotation annotation : entry )
      {
        seenChannels.add( Integer.valueOf( annotation.getChannel().getIndex() ) );
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
        int idx = columnIndices.get( Integer.valueOf( annotation.getChannel().getIndex() ) ).intValue();
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
      column.setPreferredWidth( 300 );

      columns[columnIdx] = column;
      columnIdx++;
    }

    // Swap...
    setDataHolder( new DataHolder( newData, columns ) );

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        fireTableStructureChanged();
      }
    } );
  }

  /**
   * Notifies all listeners that have registered interest for notification on
   * this event type. The event instance is lazily created using the parameters
   * passed into the fire method.
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
   * this event type. The event instance is lazily created using the parameters
   * passed into the fire method.
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
  private double getSampleRate()
  {
    return this.data == null ? 0.0 : this.data.getSampleRate();
  }

  /**
   * @return
   */
  private long getTriggerPosition()
  {
    return this.data == null || !this.data.hasTriggerData() ? 0L : this.data.getTriggerPosition();
  }

  /**
   * Invalidates the cached total column-width, causing it to be recalculated on
   * request.
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
