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
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a dockable tool-window that presents an overview of the annotations.
 */
public class AnnotationOverview extends AbstractViewLayer implements IToolWindow, IDataModelChangeListener,
    IAnnotationDataChangedListener
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

    private final SignalDiagramController controller;
    private final AtomicReference<DataHolder> dataRef;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationContainer} instance.
     */
    public AnnotationContainer( final SignalDiagramController aController )
    {
      this.controller = aController;
      this.dataRef = new AtomicReference<DataHolder>();
    }

    // METHODS

    /**
     * Clears this structure, removing all of its data.
     */
    public void clearStructure( final Integer aChannelIdx )
    {
      if ( aChannelIdx == null )
      {
        this.dataRef.set( null );

        fireTableStructureChanged();
      }
      else
      {
        updateStructure();
      }
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
      final DataHolder dataHolder = this.dataRef.get();
      if ( dataHolder == null )
      {
        return 3;
      }

      return dataHolder.columnNames.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getColumnName( final int aColumn )
    {
      final DataHolder dataHolder = this.dataRef.get();
      if ( dataHolder == null )
      {
        switch ( aColumn )
        {
          case 0:
            return "#";
          case 1:
            return "Start";
          case 2:
            return "End";
          default:
            return null;
        }
      }

      return dataHolder.columnNames[aColumn];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRowCount()
    {
      final DataHolder dataHolder = this.dataRef.get();
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
    public Object getValueAt( final int aRowIndex, final int aColumnIndex )
    {
      final DataHolder dataHolder = this.dataRef.get();
      if ( dataHolder == null )
      {
        return null;
      }

      return dataHolder.data[aRowIndex][aColumnIndex];
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
      int column = 0;
      for ( Integer channelIdx : seenChannels )
      {
        columnIndices.put( channelIdx, Integer.valueOf( column++ ) );
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
      String[] colHeaders = new String[columnCount];
      column = 0;
      colHeaders[column++] = "#";
      colHeaders[column++] = "Start";
      colHeaders[column++] = "End";
      for ( Integer channelIdx : seenChannels )
      {
        colHeaders[column++] = channelNames.get( channelIdx );
      }

      // Swap...
      DataHolder dataHolder = new DataHolder( newData, colHeaders );
      this.dataRef.set( dataHolder );

      fireTableStructureChanged();
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
      setRowSelectionAllowed( false );

      setSelectionMode( ListSelectionModel.SINGLE_SELECTION );

      setAutoCreateColumnsFromModel( true );
      setFillsViewportHeight( true );
      setIntercellSpacing( new Dimension( 2, 2 ) );
      setAutoCreateRowSorter( true );
      setShowGrid( false );

      setDefaultRenderer( DataAnnotation.class, new DataAnnotationCellRenderer() );
      setDefaultRenderer( Double.class, new TimeCellRenderer() );
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

        Object modelValue = model.getValueAt( aRow, aColumn );
        if ( modelValue instanceof DataAnnotation )
        {
          Object val = ( ( DataAnnotation )modelValue ).getProperties().get( DataAnnotation.KEY_COLOR );
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
   * Data holder for the {@link AnnotationContainer}.
   */
  private static class DataHolder
  {
    // VARIABLES

    final Object[][] data;
    final String[] columnNames;

    // CONSTRUCTORS

    /**
     * Creates a new {@link DataHolder} instance.
     */
    public DataHolder( final Object[][] aData, final String[] aColumnNames )
    {
      this.data = aData;
      this.columnNames = aColumnNames;
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

  private final AnnotationContainer container;

  private JTable table;
  private JButton exportButton;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationOverview} instance.
   */
  public AnnotationOverview( final SignalDiagramController aController )
  {
    super( aController );

    this.container = new AnnotationContainer( aController );
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
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Annotations" );

    this.table = new AnnotationJTable( this.container );

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
