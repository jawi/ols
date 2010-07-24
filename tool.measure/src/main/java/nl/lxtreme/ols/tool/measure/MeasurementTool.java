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
package nl.lxtreme.ols.tool.measure;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.*;

/**
 * @author jawi
 *
 */
public class MeasurementTool implements Tool
{
  // INNER TYPES

  /**
   * Updates the cursors when the dialog retrieves the focus again.
   */
  final class DialogListener extends WindowAdapter
  {
    // METHODS

    /**
     * @see java.awt.event.WindowAdapter#windowGainedFocus(java.awt.event.WindowEvent)
     */
    @Override
    public void windowGainedFocus( final WindowEvent aEvent )
    {
      aEvent.getWindow().repaint();
    }

    /**
     * @see java.awt.event.WindowAdapter#windowStateChanged(java.awt.event.WindowEvent)
     */
    @Override
    public void windowStateChanged( final WindowEvent aEvent )
    {
      aEvent.getWindow().repaint();
    }
  }

  /**
   * ItemListener used to update the frequency & distance labels when one of the
   * cursor comboboxes is modified.
   */
  static final class PointItemListener implements ItemListener
  {
    // VARIABLES

    private final CapturedData data;
    private final JComboBox cursorA;
    private final JComboBox cursorB;
    private final JLabel frequencyLabel;
    private final JLabel distanceLabel;

    // CONSTRUCTORS

    /**
     * @param aData
     * @param aFrequencyLabel
     * @param aDistanceLabel
     */
    public PointItemListener( final CapturedData aData, final JComboBox aCursorA, final JComboBox aCursorB,
        final JLabel aFrequencyLabel, final JLabel aDistanceLabel )
    {
      this.data = aData;
      this.cursorA = aCursorA;
      this.cursorB = aCursorB;
      this.frequencyLabel = aFrequencyLabel;
      this.distanceLabel = aDistanceLabel;
    }

    // METHODS

    /**
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    @Override
    public void itemStateChanged( final ItemEvent aEvent )
    {
      final int cursorAidx = this.cursorA.getSelectedIndex();
      final int curosrBidx = this.cursorB.getSelectedIndex();

      final long cursorApos = this.data.getCursorPosition( cursorAidx );
      final long cursorBpos = this.data.getCursorPosition( curosrBidx );

      String distanceText = "<???>";
      String frequencyText = "<???>";

      if ( ( cursorApos >= 0 ) && ( cursorBpos >= 0 ) && ( cursorApos != cursorBpos ) )
      {
        long distance = cursorApos - cursorBpos;

        distanceText = DisplayUtils.displayScaledTime( Math.abs( distance ), this.data.rate );

        final double frequency = Math.abs( ( double )this.data.rate / ( double )distance );
        frequencyText = DisplayUtils.displayFrequency( frequency );
      }

      this.distanceLabel.setText( distanceText );
      this.frequencyLabel.setText( frequencyText );
    }
  }

  // CONSTANTS

  private static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  private static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // VARIABLES

  private JDialog dialog;

  // CONSTRUCTORS

  /**
   * Creates a new MeasurementTool instance.
   */
  public MeasurementTool()
  {
    // NO-op
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.tools.Tool#getName()
   */
  @Override
  public String getName()
  {
    return "Measure ...";
  }

  /**
   * @see nl.lxtreme.ols.api.tools.Tool#process(java.awt.Frame,
   *      nl.lxtreme.ols.api.CapturedData, nl.lxtreme.ols.api.tools.ToolContext,
   *      nl.lxtreme.ols.api.tools.AnalysisCallback)
   */
  @Override
  public void process( final Frame aParentFrame, final CapturedData aData, final ToolContext aContext,
      final AnalysisCallback aCallback )
  {
    // check if dialog exists with different owner and dispose if so
    if ( ( this.dialog != null ) && ( this.dialog.getOwner() != aParentFrame ) )
    {
      this.dialog.dispose();
      this.dialog = null;
    }
    // if no valid dialog exists, create one
    if ( this.dialog == null )
    {
      SwingUtilities.invokeLater( new Runnable()
      {
        @Override
        public void run()
        {
          MeasurementTool.this.dialog = new JDialog( aParentFrame, "Measure" );
          MeasurementTool.this.dialog.addWindowListener( new DialogListener() );

          final JComponent contentPane = ( JComponent )MeasurementTool.this.dialog.getContentPane();
          contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );
          contentPane.add( createDialogContent( aData, aContext ) );
          MeasurementTool.this.dialog.pack();

          MeasurementTool.this.dialog.setVisible( true );
        }
      } );
    }
  }

  /**
   * Creates the dialog content for the measurement dialog.
   * <p>
   * Should be called from the EDT!
   * </p>
   * 
   * @return a dialog panel, never <code>null</code>.
   */
  protected Component createDialogContent( final CapturedData aData, final ToolContext aContext )
  {
    final JPanel result = new JPanel();
    result.add( createMeasurePane( aData ), BorderLayout.WEST );
    result.add( createCursorListPane( aData ), BorderLayout.EAST );
    return result;
  }

  /**
   * Creates the cursor listing pane.
   * 
   * @param aData
   *          the captured data to take the cursors from, cannot be
   *          <code>null</code>.
   * @return a panel containing the cursor listing, never <code>null</code>.
   */
  private Component createCursorListPane( final CapturedData aData )
  {
    final JPanel result = new JPanel( new GridLayout( 10, 2, 4, 4 ) );
    for ( int i = 0; i < 10; i++ )
    {
      result.add( new JLabel( String.format( "Channel %d", i + 1 ) ) );
      result.add( new CursorLabel( aData, i ) );
    }
    return result;
  }
  /**
   * @param aData
   * @return
   */
  private Component createMeasurePane(final CapturedData aData) {
    final String[] cursorNames = { "Cursor 1", "Cursor 2", "Cursor 3", "Cursor 4", "Cursor 5", "Cursor 6", "Cursor 7", "Cursor 8", "Cursor 9", "Cursor 10" };

    final JPanel result = new JPanel( new GridBagLayout() );

    final JLabel frequency = new JLabel( "" );
    final JLabel distance = new JLabel( "" );
    final JComboBox pointA = new JComboBox( cursorNames );
    final JComboBox pointB = new JComboBox( cursorNames );

    final ItemListener listener = new PointItemListener( aData, pointA, pointB, frequency, distance );

    pointA.addItemListener( listener );
    pointB.addItemListener( listener );

    result.add( new JLabel( "Cursor A:" ), //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    result.add( pointA, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    result.add( new JLabel( "Cursor B:" ), //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    result.add( pointB, //
        new GridBagConstraints( 1, 1, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    result.add( new JLabel( "Distance:" ), //
        new GridBagConstraints( 0, 2, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    result.add( distance, //
        new GridBagConstraints( 1, 2, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    result.add( new JLabel( "Frequency:" ), //
        new GridBagConstraints( 0, 3, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_LEADING,
            GridBagConstraints.HORIZONTAL, LABEL_INSETS, 0, 0 ) );
    result.add( frequency, //
        new GridBagConstraints( 1, 3, 1, 1, 1.0, 1.0, GridBagConstraints.BASELINE_TRAILING,
            GridBagConstraints.HORIZONTAL, COMP_INSETS, 0, 0 ) );

    result.add( new JLabel(), //
        new GridBagConstraints( 0, 4, 2, 1, 10.0, 10.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
            LABEL_INSETS, 0, 0 ) );

    return result;
  }
}
