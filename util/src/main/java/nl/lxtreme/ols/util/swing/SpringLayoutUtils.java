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
package nl.lxtreme.ols.util.swing;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * A 1.4 file that provides utility methods for creating form- or grid-style
 * layouts with SpringLayout. These utilities are used by several programs, such
 * as SpringBox and SpringCompactGrid.
 */
public final class SpringLayoutUtils
{
  // CONSTANTS

  public static final String SEPARATOR = "SEPARATOR";

  // VARIABLES

  private static Color selectionColor = null;

  // CONSTRUCTORS

  /**
   * Creates a new SpringLayoutUtils instance, never used.
   */
  private SpringLayoutUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Adds a labeled separator to the given container.
   * 
   * @param aContainer
   *          the container to add the separator + label to, cannot be
   *          <code>null</code>.
   * @param aText
   *          the (optional) text of the label to add, may be <code>null</code>.
   * @return the given container, never <code>null</code>.
   */
  public static final Container addSeparator( final Container aContainer, final String aText )
  {
    final Color color = getSeparatorForegroundColor();

    final JLabel label = new JLabel( aText );
    label.setAlignmentY( Component.BOTTOM_ALIGNMENT );
    // Take the selection color of a JList to make a "striking" difference
    // between separators and other components...
    label.setForeground( color );
    label.setFont( label.getFont().deriveFont( Font.BOLD ) );
    label.setHorizontalAlignment( SwingConstants.RIGHT );
    setSeparatorProperty( label );

    final JSeparator separator = new JSeparator();
    // Apparently, on Mac OSX, the height of a separator is slightly different
    // than on other OSs...
    separator.setAlignmentY( HostUtils.getHostInfo().isMacOS() ? 0.25f : Component.CENTER_ALIGNMENT );
    setSeparatorProperty( separator );

    aContainer.add( label );
    aContainer.add( separator );

    return aContainer;
  }

  /**
   * Adds a constant value to a given edge of a given constraints.
   * 
   * @param aConstraints
   *          the constraints to mutate;
   * @param aEdgeName
   *          the name of the edge to mutate;
   * @param aConstant
   *          the new value to set.
   */
  public static void addToConstraint( final SpringLayout.Constraints aConstraints, final String aEdgeName,
      final int aConstant )
  {
    aConstraints.setConstraint( aEdgeName,
        Spring.sum( aConstraints.getConstraint( aEdgeName ), Spring.constant( aConstant ) ) );

  }

  /**
   * Aligns the first <code>rows</code> * <code>cols</code> components of
   * <code>parent</code> in a grid. Each component in a column is as wide as the
   * maximum preferred width of the components in that column; height is
   * similarly determined for each row. The parent is made just big enough to
   * fit them all.
   * 
   * @param aContainer
   *          the container to layout. Must have a SpringLayout as layout
   *          manager;
   * @param aRows
   *          number of rows
   * @param aCols
   *          number of columns
   * @param aInitialX
   *          x location to start the grid at
   * @param aInitialY
   *          y location to start the grid at
   * @param aXpad
   *          x padding between cells
   * @param aYpad
   *          y padding between cells
   */
  public static void makeCompactGrid( final Container aContainer, final int aRows, final int aCols,
      final int aInitialX, final int aInitialY, final int aXpad, final int aYpad )
  {
    if ( !( aContainer.getLayout() instanceof SpringLayout ) )
    {
      throw new IllegalArgumentException( "Container should have SpringLayout as layout manager!" );
    }

    final SpringLayout layout = ( SpringLayout )aContainer.getLayout();

    // Align all cells in each column and make them the same width.
    Spring x = Spring.constant( aInitialX );
    for ( int c = 0; c < aCols; c++ )
    {
      Spring width = Spring.constant( 0 );
      for ( int r = 0; r < aRows; r++ )
      {
        width = Spring.max( width, getConstraintsForCell( aContainer, aCols, r, c ).getWidth() );
      }
      for ( int r = 0; r < aRows; r++ )
      {
        final SpringLayout.Constraints constraints = getConstraintsForCell( aContainer, aCols, r, c );
        constraints.setX( x );
        constraints.setWidth( width );
      }
      x = Spring.sum( x, Spring.sum( width, Spring.constant( aXpad ) ) );
    }

    // Align all cells in each row and make them the same height.
    Spring y = Spring.constant( aInitialY );
    for ( int r = 0; r < aRows; r++ )
    {
      Spring height = Spring.constant( 0 );
      for ( int c = 0; c < aCols; c++ )
      {
        height = Spring.max( height, getConstraintsForCell( aContainer, aCols, r, c ).getHeight() );
      }
      for ( int c = 0; c < aCols; c++ )
      {
        final SpringLayout.Constraints constraints = getConstraintsForCell( aContainer, aCols, r, c );
        constraints.setHeight( height );
        constraints.setY( y );
      }
      y = Spring.sum( y, Spring.sum( height, Spring.constant( aYpad ) ) );
    }

    // Set the parent's size.
    final SpringLayout.Constraints pCons = layout.getConstraints( aContainer );
    pCons.setConstraint( SpringLayout.SOUTH, y );
    pCons.setConstraint( SpringLayout.EAST, x );
  }

  /**
   * Makes a compact grid for use in "editors", in which a grid of two columns
   * is created.
   * 
   * @param aContainer
   *          the container to layout. Must have a SpringLayout as layout
   *          manager;
   * @param aInitialX
   *          the initial X-padding (left side);
   * @param aInitialY
   *          the initial Y-padding (top side).
   * @see #makeCompactGrid(Container, int, int, int, int, int, int)
   */
  public static void makeEditorGrid( final Container aContainer, final int aInitialX, final int aInitialY )
  {
    if ( !( aContainer.getLayout() instanceof SpringLayout ) )
    {
      throw new IllegalArgumentException( "Container should have SpringLayout as layout manager!" );
    }

    final int xPad = 6;
    final int yPad = 6;

    final int columns = 2;
    final int rows = ( aContainer.getComponentCount() / columns );

    final SpringLayout layout = ( SpringLayout )aContainer.getLayout();

    // Align all cells in each column and make them the same width.
    Spring x = Spring.constant( aInitialX );
    for ( int c = 0; c < columns; c++ )
    {
      Spring width = Spring.constant( 0 );
      for ( int r = 0; r < rows; r++ )
      {
        width = Spring.max( width, getConstraintsForCell( aContainer, columns, r, c ).getWidth() );
      }
      for ( int r = 0; r < rows; r++ )
      {
        final SpringLayout.Constraints constraints = getConstraintsForCell( aContainer, columns, r, c );
        constraints.setX( x );
        constraints.setWidth( width );
      }
      x = Spring.sum( x, Spring.sum( width, Spring.constant( xPad ) ) );
    }

    // Align all cells in each row and make them the same height.
    Spring y = Spring.constant( aInitialY );
    for ( int r = 0; r < rows; r++ )
    {
      Spring height = Spring.constant( 0 );
      for ( int c = 0; c < columns; c++ )
      {
        height = Spring.max( height, getConstraintsForCell( aContainer, columns, r, c ).getHeight() );
      }
      for ( int c = 0; c < columns; c++ )
      {
        final SpringLayout.Constraints constraints = getConstraintsForCell( aContainer, columns, r, c );
        final Component component = getCellComponent( aContainer, columns, r, c );

        constraints.setHeight( height );

        Spring actualY = y;

        // Keep vertical alignment in consideration (if needed)
        if ( isSeparatorComponent( component ) )
        {
          final float vAlignment = component.getAlignmentY();
          if ( ( vAlignment > 0.0 ) && ( vAlignment < 1.0 ) )
          {
            final int vOffset = ( int )( height.getValue() * component.getAlignmentY() );
            actualY = Spring.sum( y, Spring.constant( vOffset ) );
          }
        }

        constraints.setY( actualY );
      }
      y = Spring.sum( y, Spring.sum( height, Spring.constant( yPad ) ) );
    }

    // Set the parent's size.
    final SpringLayout.Constraints parentConstraints = layout.getConstraints( aContainer );
    parentConstraints.setConstraint( SpringLayout.SOUTH, y );
    parentConstraints.setConstraint( SpringLayout.EAST, x );
  }

  /**
   * Aligns the first <code>rows</code> * <code>cols</code> components of
   * <code>parent</code> in a grid. Each component is as big as the maximum
   * preferred width and height of the components. The parent is made just big
   * enough to fit them all.
   * 
   * @param aContainer
   *          the container to layout. Must have a SpringLayout as layout
   *          manager;
   * @param aRows
   *          number of rows
   * @param aCols
   *          number of columns
   * @param aInitialX
   *          x location to start the grid at
   * @param aInitialY
   *          y location to start the grid at
   * @param aXpad
   *          x padding between cells
   * @param aYpad
   *          y padding between cells
   */
  public static void makeGrid( final Container aContainer, final int aRows, final int aCols, final int aInitialX,
      final int aInitialY, final int aXpad, final int aYpad )
  {
    if ( !( aContainer.getLayout() instanceof SpringLayout ) )
    {
      throw new IllegalArgumentException( "Container should have SpringLayout as layout manager!" );
    }

    final SpringLayout layout = ( SpringLayout )aContainer.getLayout();

    final Spring xPadSpring = Spring.constant( aXpad );
    final Spring yPadSpring = Spring.constant( aYpad );
    final Spring initialXSpring = Spring.constant( aInitialX );
    final Spring initialYSpring = Spring.constant( aInitialY );
    final int max = aRows * aCols;

    // Calculate Springs that are the max of the width/height so that all
    // cells have the same size.
    Spring maxWidthSpring = layout.getConstraints( aContainer.getComponent( 0 ) ).getWidth();
    Spring maxHeightSpring = layout.getConstraints( aContainer.getComponent( 0 ) ).getWidth();
    for ( int i = 1; i < max; i++ )
    {
      final SpringLayout.Constraints cons = layout.getConstraints( aContainer.getComponent( i ) );

      maxWidthSpring = Spring.max( maxWidthSpring, cons.getWidth() );
      maxHeightSpring = Spring.max( maxHeightSpring, cons.getHeight() );
    }

    // Apply the new width/height Spring. This forces all the
    // components to have the same size.
    for ( int i = 0; i < max; i++ )
    {
      final SpringLayout.Constraints cons = layout.getConstraints( aContainer.getComponent( i ) );

      cons.setWidth( maxWidthSpring );
      cons.setHeight( maxHeightSpring );
    }

    // Then adjust the x/y constraints of all the cells so that they
    // are aligned in a grid.
    SpringLayout.Constraints lastCons = null;
    SpringLayout.Constraints lastRowCons = null;
    for ( int i = 0; i < max; i++ )
    {
      final SpringLayout.Constraints cons = layout.getConstraints( aContainer.getComponent( i ) );
      if ( ( i % aCols ) == 0 )
      { // start of new row
        lastRowCons = lastCons;
        cons.setX( initialXSpring );
      }
      else
      { // x position depends on previous component
        cons.setX( Spring.sum( lastCons.getConstraint( SpringLayout.EAST ), xPadSpring ) );
      }

      if ( ( i / aCols ) == 0 )
      { // first row
        cons.setY( initialYSpring );
      }
      else
      { // y position depends on previous row
        cons.setY( Spring.sum( lastRowCons.getConstraint( SpringLayout.SOUTH ), yPadSpring ) );
      }
      lastCons = cons;
    }

    // Set the parent's size.
    final SpringLayout.Constraints pCons = layout.getConstraints( aContainer );
    pCons.setConstraint( SpringLayout.SOUTH,
        Spring.sum( Spring.constant( aYpad ), lastCons.getConstraint( SpringLayout.SOUTH ) ) );
    pCons.setConstraint( SpringLayout.EAST,
        Spring.sum( Spring.constant( aXpad ), lastCons.getConstraint( SpringLayout.EAST ) ) );
  }

  /**
   * Marks the given component as "separator" component.
   * 
   * @param aComponent
   *          the component to mark as "separator", cannot be <code>null</code>.
   * @return the given component.
   */
  public static JComponent setSeparatorProperty( final JComponent aComponent )
  {
    aComponent.putClientProperty( SEPARATOR, Boolean.TRUE );
    return aComponent;
  }

  /**
   * Returns the actual Swing/AWT component for a denoted cell.
   * 
   * @param aContainer
   *          the container to get the component for. Must have a SpringLayout
   *          as layout manager;
   * @param aCols
   *          number of columns
   * @param aRow
   *          the row of the cell to return the constraints of;
   * @param aCol
   *          the column of the cell to return the constraints of.
   * @return the component, might be <code>null</code>.
   */
  private static Component getCellComponent( final Container aContainer, final int aCols, final int aRow, final int aCol )
  {
    return aContainer.getComponent( ( aRow * aCols ) + aCol );
  }

  /**
   * Returns the SpringLayout constraints for a denoted cell.
   * 
   * @param aContainer
   *          the container to get the cell constraints for. Must have a
   *          SpringLayout as layout manager;
   * @param aCols
   *          number of columns
   * @param aRow
   *          the row of the cell to return the constraints of;
   * @param aCol
   *          the column of the cell to return the constraints of.
   * @return the SpringLayout constraints, never <code>null</code>.
   */
  private static SpringLayout.Constraints getConstraintsForCell( final Container aContainer, final int aCols,
      final int aRow, final int aCol )
  {
    if ( !( aContainer.getLayout() instanceof SpringLayout ) )
    {
      throw new IllegalArgumentException( "Container should have SpringLayout as layout manager!" );
    }

    final SpringLayout layout = ( SpringLayout )aContainer.getLayout();
    final Component c = getCellComponent( aContainer, aCols, aRow, aCol );
    return layout.getConstraints( c );
  }

  /**
   * Determines a foreground color for the separator label.
   */
  private static Color getSeparatorForegroundColor()
  {
    if ( selectionColor == null )
    {
      if ( HostUtils.getHostInfo().isMacOS() )
      {
        selectionColor = UIManager.getColor( "List.selectionBackground" );
      }
      else
      {
        selectionColor = UIManager.getColor( "MenuItem.selectionBackground" );
      }
      if ( selectionColor == null )
      {
        selectionColor = Color.BLUE;
      }
    }
    return selectionColor;
  }

  /**
   * @param aComponent
   * @return
   */
  private static boolean isSeparatorComponent( final Component aComponent )
  {
    if ( !( aComponent instanceof JComponent ) )
    {
      return false;
    }

    final JComponent comp = ( JComponent )aComponent;
    final Object property = comp.getClientProperty( SEPARATOR );
    return Boolean.TRUE.equals( property );
  }
}
