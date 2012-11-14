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
package nl.lxtreme.ols.client.signaldisplay.view.toolwindow;


import java.awt.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a dockable tool-window that presents an overview of the annotations.
 */
public class AnnotationOverview extends AbstractViewLayer implements IToolWindow, IDataModelChangeListener
{
  // INNER TYPES

  /**
   * Provides a container for annotations that also acts as a table model.
   */
  private static class AnnotationContainer extends AbstractTableModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final double sampleRate;
    private final long triggerPos;
    private final AnnotationsHelper annotationsHelper;
    private final AcquisitionData data;
    private final ConcurrentNavigableMap<AnnotationKey, List<DataAnnotation>> map;
    private final List<String> columns;
    private final List<String> defaultColumns;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationContainer} instance.
     */
    public AnnotationContainer( final SignalDiagramController aController, final AcquisitionData aData )
    {
      this.data = aData;
      this.annotationsHelper = aController.getAnnotationsHelper();

      this.map = new ConcurrentSkipListMap<AnnotationKey, List<DataAnnotation>>();
      this.defaultColumns = Arrays.asList( "#", "Start", "Stop" );
      this.columns = new CopyOnWriteArrayList<String>( this.defaultColumns );

      this.sampleRate = aData.getSampleRate();
      this.triggerPos = aData.hasTriggerData() ? aData.getTriggerPosition() : 0L;
    }

    // METHODS

    /**
     * @param aAnnotation
     *          the annotation to add, cannot be <code>null</code>.
     */
    public void add( final Annotation aAnnotation )
    {
      if ( aAnnotation instanceof DataAnnotation )
      {
        DataAnnotation annotation = ( DataAnnotation )aAnnotation;

        AnnotationKey key = new AnnotationKey( annotation.getStartTimestamp(), annotation.getEndTimestamp() );

        List<DataAnnotation> values = this.map.get( key );
        if ( values == null )
        {
          values = new ArrayList<DataAnnotation>();
          this.map.putIfAbsent( key, values );
        }
        values.add( annotation );

        String name = getChannelName( aAnnotation );
        if ( !this.columns.contains( name ) )
        {
          this.columns.add( name );
          fireTableStructureChanged();
        }
        else
        {
          fireTableRowsInserted( this.map.size() - 1, this.map.size() - 1 );
        }
      }
    }

    /**
     * Clears this container.
     */
    public void clear()
    {
      this.map.clear();
      this.columns.retainAll( this.defaultColumns );

      fireTableStructureChanged();
    }

    /**
     * Clears the annotations for the given channel index.
     * 
     * @param aChannelIdx
     *          the channel index to clear the annotations for.
     */
    public void clear( final int aChannelIdx )
    {
      List<Entry<AnnotationKey, List<DataAnnotation>>> entrySet = new ArrayList<Map.Entry<AnnotationKey, List<DataAnnotation>>>(
          this.map.entrySet() );
      for ( Entry<AnnotationKey, List<DataAnnotation>> entry : entrySet )
      {
        List<DataAnnotation> values = new ArrayList<DataAnnotation>( entry.getValue() );
        for ( DataAnnotation annotation : values )
        {
          if ( annotation.getChannelIndex() == aChannelIdx )
          {
            entry.getValue().remove( annotation );
          }
        }

        if ( entry.getValue().isEmpty() )
        {
          this.map.remove( entry.getKey() );
        }
      }
      fireTableStructureChanged();
    }

    public DataAnnotation getAnnotation( final int aRowIndex, final int aColumnIndex )
    {
      List<AnnotationKey> keySet = new ArrayList<AnnotationKey>( this.map.keySet() );
      if ( aRowIndex < keySet.size() )
      {
        List<DataAnnotation> list = this.map.get( keySet.get( aRowIndex ) );
        int defaultColumnCount = this.defaultColumns.size();

        if ( aColumnIndex < defaultColumnCount )
        {
          return null;
        }
        int idx = aColumnIndex - defaultColumnCount;
        if ( idx < list.size() )
        {
          return list.get( idx );
        }
      }
      return null;
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
          return String.class;

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
      return this.columns.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getColumnName( final int aColumn )
    {
      return this.columns.get( aColumn );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRowCount()
    {
      return this.map.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getValueAt( final int aRowIndex, final int aColumnIndex )
    {
      List<AnnotationKey> keySet = new ArrayList<AnnotationKey>( this.map.keySet() );
      if ( aRowIndex < keySet.size() )
      {
        AnnotationKey key = keySet.get( aRowIndex );

        List<DataAnnotation> annotations = this.map.get( key );

        switch ( aColumnIndex )
        {
          case 0:
            return Integer.valueOf( aRowIndex );

          case 1:
            return formatRelativeTime( key.startTime );

          case 2:
            return formatRelativeTime( key.endTime );

          default:
            int idx = aColumnIndex - this.defaultColumns.size();
            if ( idx < annotations.size() )
            {
              return annotations.get( idx );
            }
        }
      }
      return null;
    }

    /**
     * @param aTime
     * @return
     */
    private String formatAbsoluteTime( final long aTime )
    {
      double value = ( aTime / this.sampleRate );
      return UnitOfTime.format( value );
    }

    /**
     * @param aTime
     * @return
     */
    private String formatRelativeTime( final long aTime )
    {
      return formatAbsoluteTime( aTime - this.triggerPos );
    }

    /**
     * @param aAnnotation
     * @return
     */
    private String getChannelName( final Annotation aAnnotation )
    {
      int channelIdx = aAnnotation.getChannelIndex();
      Channel channel = this.data.getChannel( channelIdx );
      if ( channel == null )
      {
        // TODO verify!!!
        System.err.println( "!!! Invalid channel index: " + channelIdx );
        new RuntimeException().printStackTrace();
        return String.format( "ch.%d", channelIdx );
      }
      return channel.getLabel();
    }
  }

  /**
   * Provides a table cell renderer for {@link DataAnnotation}s.
   */
  private static class DataAnnotationCellRenderer extends DefaultTableCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getTableCellRendererComponent( final JTable aTable, final Object aValue,
        final boolean aIsSelected, final boolean aHasFocus, final int aRow, final int aColumn )
    {
      String text = "";
      String tooltip = "";

      final DataAnnotation annotation = ( DataAnnotation )aValue;

      if ( annotation != null )
      {
        final Map<String, Object> properties = annotation.getProperties();
        Object data = annotation.getData();

        text = String.valueOf( data );

        if ( Boolean.TRUE.equals( properties.get( AnnotationHelper.KEY_SYMBOL ) ) )
        {
          // Normal symbol; show the value in pretty form...
          if ( data instanceof Number )
          {
            int value = ( ( Number )data ).intValue();

            if ( ( value >= 32 ) && ( value < 128 ) )
            {
              // plain 7-bit ASCII value
              text = String.format( "%1$d [0x%1$x] (%1$c)", data );
            }
            else
            {
              text = String.format( "%1$d [0x%1$x]", data );
            }
          }
        }

        if ( properties.containsKey( AnnotationHelper.KEY_DESCRIPTION ) )
        {
          tooltip = ( String )properties.get( AnnotationHelper.KEY_DESCRIPTION );
          text = "<html><body><p>" + text + "</p><p>" + tooltip + "</p></body></html>";
        }
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
   * 
   */
  private static class AnnotationJTable extends JTable
  {
    private static final long serialVersionUID = 1L;

    private static final Color ROW_COLOR = Color.WHITE;
    private static final Color ALT_ROW_COLOR = new Color( 0xf2, 0xf2, 0xf4 );

    private Font font;

    /**
     * Creates a new {@link AnnotationJTable} instance.
     */
    public AnnotationJTable( final TableModel aModel )
    {
      super( aModel );

      setCellSelectionEnabled( false );
      setColumnSelectionAllowed( false );
      setRowSelectionAllowed( true );

      setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
      setIntercellSpacing( new Dimension( 0, 0 ) );
      setAutoCreateRowSorter( true );
      setShowGrid( false );

      setDefaultRenderer( DataAnnotation.class, new DataAnnotationCellRenderer() );
    }

    @Override
    public Component prepareRenderer( final TableCellRenderer aRenderer, final int aRow, final int aColumn )
    {
      AnnotationContainer model = ( AnnotationContainer )getModel();

      Object value = getValueAt( aRow, aColumn );

      boolean isSelected = false;
      boolean hasFocus = false;

      // Only indicate the selection and focused cell if not printing
      if ( !isPaintingForPrint() )
      {
        isSelected = isCellSelected( aRow, aColumn );

        boolean rowIsLead = ( this.selectionModel.getLeadSelectionIndex() == aRow );
        boolean colIsLead = ( this.columnModel.getSelectionModel().getLeadSelectionIndex() == aColumn );

        hasFocus = ( rowIsLead && colIsLead ) && isFocusOwner();
      }

      Component comp = aRenderer.getTableCellRendererComponent( this, value, isSelected, hasFocus, aRow, aColumn );

      if ( this.font == null )
      {
        Font f = comp.getFont();
        this.font = new Font( Font.MONOSPACED, Font.PLAIN, ( int )( 0.95 * f.getSize() ) );
      }

      comp.setFont( this.font );

      if ( !isSelected )
      {
        Color bg = ( ( aRow % 2 ) != 0 ) ? ALT_ROW_COLOR : ROW_COLOR;

        DataAnnotation annotation = model.getAnnotation( aRow, aColumn );
        if ( annotation != null )
        {
          Object val = annotation.getProperties().get( AnnotationHelper.KEY_COLOR );
          if ( val instanceof String )
          {
            bg = ColorUtils.parseColor( ( String )val );
          }
          else if ( val instanceof Color )
          {
            bg = ( Color )val;
          }
        }

        comp.setBackground( bg );
      }

      return comp;
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

  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Annotations";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private AnnotationContainer container;

  private volatile JTable table;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationOverview} instance.
   */
  public AnnotationOverview( final SignalDiagramController aController )
  {
    super( aController );
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

    aController.addDataModelChangeListener( result );

    result.initComponent();

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final AcquisitionData aData )
  {
    if ( aData != null )
    {
      this.container = new AnnotationContainer( getController(), aData );
      this.table.setModel( this.container );
    }
    else
    {
      this.container = null;
    }
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
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Annotations" );

    this.table = new AnnotationJTable( this.container );
    this.table.setAutoCreateColumnsFromModel( true );
    this.table.setFillsViewportHeight( true );

    add( new JScrollPane( this.table ), BorderLayout.CENTER );
  }
}
