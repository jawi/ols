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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.logicsniffer.ui;


import static java.awt.GridBagConstraints.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a simple UI for editing triggers.
 */
public class SimpleTriggerEditorPanel extends JPanel
{
  // INNER TYPES

  /**
   * Provides an action to update the numeric editor according to the binary
   * editor.
   */
  static class BinaryMaskToNumericAction implements ActionListener
  {
    // VARIABLES

    private final JTextField textField;
    private final JCheckBox[] checkboxes;

    // CONSTRUCTORS

    /**
     * Creates a new {@link BinaryMaskToNumericAction} instance.
     */
    public BinaryMaskToNumericAction( final JTextField aTextField, final JCheckBox... aCheckboxes )
    {
      this.textField = aTextField;
      this.checkboxes = aCheckboxes;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      long value = 0;
      for ( int i = this.checkboxes.length - 1; i >= 0; i-- )
      {
        value <<= 1;
        if ( this.checkboxes[i].isSelected() )
        {
          value |= 1;
        }
      }

      this.textField.setText( "0x" + Integer.toHexString( ( int )( value & MAX_INT_VALUE ) ) );
    }
  }

  /**
   * Provides an action to update the binary editor according to the numeric
   * editor.
   */
  static class NumericMaskToBinaryAction implements ActionListener
  {
    // VARIABLES

    private final JTextField textField;
    private final JCheckBox[] checkboxes;

    // CONSTRUCTORS

    /**
     * Creates a new {@link NumericMaskToBinaryAction} instance.
     */
    public NumericMaskToBinaryAction( final JTextField aTextField, final JCheckBox... aCheckboxes )
    {
      this.textField = aTextField;
      this.checkboxes = aCheckboxes;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      String text = getValueAsString();

      boolean invert = text.startsWith( "!" ) || text.startsWith( "~" );
      if ( invert )
      {
        text = text.substring( 1 );
      }

      try
      {
        long hexValue = Long.decode( text ).longValue();
        if ( ( hexValue < 0L ) || ( hexValue > MAX_INT_VALUE ) )
        {
          throw new NumberFormatException();
        }
        if ( invert )
        {
          hexValue = ~hexValue & 0xFFFFFFFFL;
        }

        for ( int i = 0; i < this.checkboxes.length; i++ )
        {
          boolean value = ( ( hexValue >>> i ) & 0x01 ) != 0;
          this.checkboxes[i].setSelected( value );
        }
      }
      catch ( NumberFormatException exception )
      {
        JOptionPane.showMessageDialog( null, "Illegal numeric value!\n"
            + "Please enter a valid hexadecimal, decimal or octal value..." );
      }
    }

    /**
     * @return the text field as string, never <code>null</code>.
     */
    private String getValueAsString()
    {
      String text = this.textField.getText();
      if ( text == null )
      {
        text = "";
      }
      else
      {
        text = text.trim();
      }
      return text;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final long MAX_INT_VALUE = 0xFFFFFFFFL;

  private static final String PARALLEL = "Parallel";
  private static final String SERIAL = "Serial";

  // VARIABLES

  private final int stage;
  private final int stageCount;
  private final int channelCount;

  private JCheckBox[] triggerMask;
  private JCheckBox[] triggerValue;
  private JComboBox armTriggerOn;
  private JLabel triggerDelayLabel;
  private JTextField triggerDelay;
  private JLabel triggerDelayUnit;
  private JComboBox triggerMode;
  private JLabel triggerChannelLabel;
  private JComboBox triggerChannel;
  private JRadioButton startTriggerAction;
  private JRadioButton increaseTriggerLevelAction;
  private JTextField triggerHexMask;
  private JTextField triggerHexValue;
  private JButton applyHexMaskButton;
  private JButton applyHexValueButton;

  private JLabel armTriggerOnLabel;

  private JLabel triggerModeLabel;

  private JLabel triggerMaskLabel;

  private JLabel triggerValueLabel;

  private JLabel triggerActionLabel;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SimpleTriggerEditorPanel} instance.
   */
  public SimpleTriggerEditorPanel( final int aStage, final int aStageCount, final int aChannelCount )
  {
    super( new GridBagLayout() );

    this.stage = aStage;
    this.stageCount = aStageCount;
    this.channelCount = aChannelCount;

    init();
    build();

    setEnabled( false );
  }

  // METHODS

  /**
   * @return a new {@link GridBagConstraints} instance.
   */
  private static GridBagConstraints createConstraints( final int x, final int y, final int w, final int h,
      final double wx, final double wy, final int fill, final int anchor )
  {
    final GridBagConstraints gbc = new GridBagConstraints();
    gbc.fill = fill;
    gbc.anchor = anchor;
    gbc.insets = new Insets( 2, 2, 2, 2 );
    gbc.gridx = x;
    gbc.gridy = y;
    gbc.gridwidth = w;
    gbc.gridheight = h;
    gbc.weightx = wx;
    gbc.weighty = wy;
    return gbc;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnabled( final boolean aEnabled )
  {
    super.setEnabled( aEnabled );

    for ( int i = 0; i < this.triggerMask.length; i++ )
    {
      this.triggerMask[i].setEnabled( aEnabled );
      this.triggerValue[i].setEnabled( aEnabled );
    }

    this.armTriggerOnLabel.setEnabled( aEnabled );
    this.armTriggerOn.setEnabled( aEnabled );

    this.triggerModeLabel.setEnabled( aEnabled );
    this.triggerMode.setEnabled( aEnabled );

    this.triggerChannelLabel.setEnabled( aEnabled );
    this.triggerChannel.setEnabled( aEnabled );

    this.triggerMaskLabel.setEnabled( aEnabled );
    this.triggerHexMask.setEnabled( aEnabled );
    this.applyHexMaskButton.setEnabled( aEnabled );

    this.triggerValueLabel.setEnabled( aEnabled );
    this.triggerHexValue.setEnabled( aEnabled );
    this.applyHexValueButton.setEnabled( aEnabled );

    this.triggerActionLabel.setEnabled( aEnabled );
    this.startTriggerAction.setEnabled( aEnabled );
    this.increaseTriggerLevelAction.setEnabled( aEnabled );

    this.triggerDelayLabel.setEnabled( aEnabled );
    this.triggerDelay.setEnabled( aEnabled );
    this.triggerDelayUnit.setEnabled( aEnabled );

    if ( aEnabled )
    {
      // Make it logically correct again...
      updateComponents();
    }
  }

  /**
   * Updates the states of all components to make it logically correct.
   */
  final void updateComponents()
  {
    boolean enabled;

    this.armTriggerOn.setEnabled( this.stageCount > 1 );

    if ( this.stage == ( this.stageCount - 1 ) )
    {
      // In case of a single stage, we've got only one option...
      this.startTriggerAction.setEnabled( false );
      this.increaseTriggerLevelAction.setEnabled( false );
    }

    enabled = SERIAL.equals( this.triggerMode.getSelectedItem() );

    this.triggerChannel.setEnabled( enabled );
    this.triggerChannelLabel.setEnabled( enabled );

    enabled = this.startTriggerAction.isSelected();

    this.triggerDelayLabel.setEnabled( enabled );
    this.triggerDelayUnit.setEnabled( enabled );
    this.triggerDelay.setEnabled( enabled );
  }

  /**
   * Builds this panel by placing all of its components.
   */
  private void build()
  {
    setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    ActionListener maskListener = new BinaryMaskToNumericAction( this.triggerHexMask, this.triggerMask );
    ActionListener valueListener = new BinaryMaskToNumericAction( this.triggerHexValue, this.triggerValue );

    ButtonGroup bg = new ButtonGroup();
    bg.add( this.startTriggerAction );
    bg.add( this.increaseTriggerLevelAction );

    if ( this.stage == ( this.stageCount - 1 ) )
    {
      // In case of a single stage, we've got only one option...
      this.startTriggerAction.setSelected( true );
      this.increaseTriggerLevelAction.setVisible( false );
    }
    else
    {
      // In case of multiple stages, by default go to the next stage...
      this.increaseTriggerLevelAction.setSelected( true );
    }

    // @formatter:off
    
    // Row 0
    int row = 0;
    
    add( this.armTriggerOnLabel, createConstraints( 0, row, 1, 1, 0.5, 0.0, NONE, BASELINE_TRAILING ) );
    add( this.armTriggerOn, createConstraints( 1, row, 2, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( new JLabel( "" ), createConstraints( 3, row, 3, 1, 10.0, 0.0, HORIZONTAL, BASELINE_LEADING ) );

    // Row 1
    row++;
    add( this.triggerModeLabel, createConstraints( 0, row, 1, 1, 0.5, 0.0, NONE, BASELINE_TRAILING ) );
    add( this.triggerMode, createConstraints( 1, row, 2, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( this.triggerChannelLabel, createConstraints( 3, row, 1, 1, 0.0, 0.0, NONE, BASELINE_TRAILING ) );
    add( this.triggerChannel, createConstraints( 4, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 4
    row++;
    add( createBinaryEditorHeaders( this.channelCount ), createConstraints( 1, row, 5, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 3
    row++;
    add( this.triggerMaskLabel, createConstraints( 0, row, 1, 1, 0.5, 0.0, VERTICAL, BASELINE_TRAILING ) );
    add( createBinaryEditor( this.triggerMask, maskListener ), createConstraints( 1, row, 5, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 4
    row++;
    add( this.triggerHexMask, createConstraints( 1, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( this.applyHexMaskButton, createConstraints( 2, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 5
    row++;
    add( this.triggerValueLabel, createConstraints( 0, row, 1, 1, 0.5, 0.0, VERTICAL, BASELINE_TRAILING ) );
    add( createBinaryEditor( this.triggerValue, valueListener ), createConstraints( 1, row, 5, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 6
    row++;
    add( this.triggerHexValue, createConstraints( 1, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( this.applyHexValueButton, createConstraints( 2, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 7
    row++;
    add( this.triggerActionLabel, createConstraints( 0, row, 1, 1, 0.5, 0.0, NONE, BASELINE_TRAILING ) );
    add( this.startTriggerAction, createConstraints( 1, row, 2, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( this.triggerDelayLabel, createConstraints( 3, row, 1, 1, 0.0, 0.0, NONE, BASELINE_TRAILING ) );
    add( this.triggerDelay, createConstraints( 4, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );
    add( this.triggerDelayUnit, createConstraints( 5, row, 1, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 8
    row++;
    add( this.increaseTriggerLevelAction, createConstraints( 1, row, 2, 1, 0.0, 0.0, NONE, BASELINE_LEADING ) );

    // Row 9 (spacer)
    row++;
    add( new JLabel( "" ), createConstraints( 0, row, 6, 1, 0.0, 10.0, VERTICAL, CENTER ) );
    // @formatter:on
  }

  /**
   * Creates a panel for editing a mask/value in binary style.
   * 
   * @param aCheckBoxes
   *          the array with checkboxes representing the mask/value.
   * @return a panel.
   */
  private JPanel createBinaryEditor( final JCheckBox[] aCheckBoxes, final ActionListener aActionListener )
  {
    final JPanel pane = new JPanel( new SpringLayout() );

    int length = aCheckBoxes.length;
    for ( int j = length; j > 0; j-- )
    {
      int idx = j - 1;

      final JCheckBox cb = new JCheckBox();
      cb.setBorder( BorderFactory.createEmptyBorder() );
      cb.setEnabled( false );
      cb.setToolTipText( "Channel " + idx );
      cb.addActionListener( aActionListener );

      aCheckBoxes[idx] = cb;
      pane.add( cb );
    }

    SpringLayoutUtils.makeCompactGrid( pane, 1, length, 1, 1, 1, 1 );

    return pane;
  }

  /**
   * @param aChannelCount
   * @return
   */
  private JPanel createBinaryEditorHeaders( final int aChannelCount )
  {
    JPanel pane = new JPanel( new SpringLayout() );

    JCheckBox cb = new JCheckBox();
    cb.setBorder( BorderFactory.createEmptyBorder() );

    Font font = getFont();
    font = font.deriveFont( font.getSize2D() * 0.8f );

    for ( int j = aChannelCount; j > 0; j-- )
    {
      String channel = "";
      if ( ( ( j % 8 ) == 0 ) || ( ( j % 8 ) == 1 ) || ( j == aChannelCount ) )
      {
        channel = String.format( "%d", Integer.valueOf( j - 1 ) );
      }

      JLabel label = new JLabel( channel );
      label.setEnabled( false );
      label.setFont( font );
      label.setHorizontalAlignment( SwingConstants.CENTER );
      label.setVerticalAlignment( SwingConstants.BOTTOM );
      label.setMinimumSize( cb.getMinimumSize() );
      label.setMaximumSize( cb.getMaximumSize() );
      label.setPreferredSize( cb.getPreferredSize() );

      pane.add( label );
    }

    SpringLayoutUtils.makeCompactGrid( pane, 1, aChannelCount, 1, 1, 1, 1 );

    return pane;
  }

  /**
   * Initializes this component by creating all of its components.
   */
  private void init()
  {
    String tooltip;
    ActionListener al = new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        updateComponents();
      }
    };

    String[] levels = new String[this.stageCount];
    for ( int i = 0; i < levels.length; i++ )
    {
      levels[i] = ( ( i == 0 ) ? "Immediately" : "On level " + i );
    }

    this.armTriggerOnLabel = createRightAlignedLabel( "Arm" );

    tooltip = "At what trigger level will this stage become active?";
    this.armTriggerOn = new JComboBox( levels );
    this.armTriggerOn.setSelectedIndex( this.stage );
    this.armTriggerOn.setToolTipText( tooltip );

    this.triggerChannelLabel = createRightAlignedLabel( "Channel" );

    this.triggerModeLabel = createRightAlignedLabel( "Mode" );

    tooltip = "Interpret trigger mask and value in parallel or serially?";
    this.triggerMode = new JComboBox( new String[] { PARALLEL, SERIAL } );
    this.triggerMode.setSelectedIndex( 0 );
    this.triggerMode.setToolTipText( tooltip );
    this.triggerMode.addActionListener( al );

    final String[] channels = new String[this.channelCount];
    for ( int i = 0; i < channels.length; i++ )
    {
      channels[i] = "" + i;
    }
    tooltip = "What channel should be used to interpret serially?";
    this.triggerChannel = new JComboBox( channels );
    this.triggerChannel.setSelectedIndex( 0 );
    this.triggerChannel.setToolTipText( tooltip );

    this.triggerMaskLabel = createRightAlignedLabel( "Mask" );

    this.triggerMask = new JCheckBox[this.channelCount];

    tooltip = "Set the trigger mask using numeric value; use '!' or '~' to negate value.";
    this.triggerHexMask = new JTextField( "0x0", 7 );
    this.triggerHexMask.setHorizontalAlignment( SwingConstants.TRAILING );
    this.triggerHexMask.setToolTipText( tooltip );

    this.applyHexMaskButton = new JButton( "Apply" );
    this.applyHexMaskButton.addActionListener( new NumericMaskToBinaryAction( this.triggerHexMask, this.triggerMask ) );

    this.triggerValueLabel = createRightAlignedLabel( "Value" );

    this.triggerValue = new JCheckBox[this.channelCount];

    this.triggerHexValue = new JTextField( "0x0", 7 );
    this.triggerHexValue.setHorizontalAlignment( SwingConstants.TRAILING );
    this.triggerHexValue.setToolTipText( "Set the trigger value using numeric value; use '!' or '~' to negate value." );

    this.applyHexValueButton = new JButton( "Apply" );
    this.applyHexValueButton
        .addActionListener( new NumericMaskToBinaryAction( this.triggerHexValue, this.triggerValue ) );

    this.triggerActionLabel = createRightAlignedLabel( "Action" );

    tooltip = "When trigger condition is met, start capturing data.";
    this.startTriggerAction = new JRadioButton( "Start Capture" );
    this.startTriggerAction.setToolTipText( tooltip );
    this.startTriggerAction.addActionListener( al );

    tooltip = "When trigger condition is met, increase the trigger level by one.";
    this.increaseTriggerLevelAction = new JRadioButton( "Increase trigger level" );
    this.increaseTriggerLevelAction.setToolTipText( tooltip );
    this.increaseTriggerLevelAction.addActionListener( al );

    this.triggerDelayLabel = createRightAlignedLabel( "Delay" );

    this.triggerDelayUnit = new JLabel( "samples" );

    tooltip = "Wait # samples after trigger condition is met before capturing data.";
    this.triggerDelay = new JTextField( "0", 3 );
    this.triggerDelay.setHorizontalAlignment( SwingConstants.TRAILING );
    this.triggerDelay.setToolTipText( tooltip );
  }
}
