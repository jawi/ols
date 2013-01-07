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


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;
import static org.sump.device.logicsniffer.ConfigDialogHelper.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.device.logicsniffer.profile.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.metatype.*;
import org.sump.device.logicsniffer.*;


/**
 * 
 */
public class TriggerSettingsPanel extends JPanel implements DeviceProfileChangedListener
{
  // INNER TYPES

  /**
   * Listens to the ratio slider and updates a label with the chosen ratio
   * accordingly.
   */
  static final class TriggerRatioChangeListener implements ChangeListener
  {
    static final int DEFAULT_RATIO = 50;

    private final JLabel label;

    /**
     * @param aListeningLabel
     */
    public TriggerRatioChangeListener( final JLabel aListeningLabel )
    {
      this.label = aListeningLabel;
      updateLabel( DEFAULT_RATIO, DEFAULT_RATIO );
    }

    /**
     * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
     */
    @Override
    public void stateChanged( final ChangeEvent aEvent )
    {
      final JSlider slider = ( JSlider )aEvent.getSource();

      final int before = slider.getValue();
      final int after = ( slider.getMaximum() - before );

      slider.setToolTipText( updateLabel( before, after ) );
    }

    /**
     * @param aBeforeRatio
     * @param aAfterRatio
     * @return
     */
    private String updateLabel( final int aBeforeRatio, final int aAfterRatio )
    {
      final String ratioText = String
          .format( "%d / %d", Integer.valueOf( aBeforeRatio ), Integer.valueOf( aAfterRatio ) );
      this.label.setText( ratioText );
      return ratioText;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final List<JComponent> components;

  private JCheckBox triggerEnable;
  private JLabel ratioLabel;
  private JSlider ratioSlider;
  private JComboBox triggerTypeSelect;
  private JTabbedPane triggerStageTabs;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TriggerSettingsPanel} instance.
   */
  public TriggerSettingsPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    super( new SpringLayout() );

    this.components = new ArrayList<JComponent>();

    initPanel( aOCD, aInitialValues );
    buildPanel();

    // disable all components...
    enableComponents( false );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void deviceProfileChanged( final DeviceProfile aProfile )
  {
    enableComponents( ( aProfile != null ) && aProfile.isTriggerSupported() );

    if ( aProfile != null )
    {
      // Recreate the trigger editor to match the number of stages...
      createSimpleTriggerEditor( aProfile.getTriggerStages(), aProfile.getChannelCount() );

      // Triggers supported at all?
      updateCheckBoxState( this.triggerEnable, aProfile.isTriggerSupported() );
      // Complex triggers supported?
      updateTriggerTypeComboBoxModel( this.triggerTypeSelect, aProfile );

      enableTriggerStages( this.triggerEnable.isSelected() );
    }
  }

  /**
   * @param aValue
   *          <code>true</code> to enable the trigger stages, <code>false</code>
   *          to disable them.
   */
  private void enableTriggerStages( final boolean aValue )
  {
    this.ratioLabel.setEnabled( aValue );
    this.ratioSlider.setEnabled( aValue );
    this.triggerTypeSelect.setEnabled( aValue );
    this.triggerStageTabs.setEnabled( aValue );

    enableTriggerStageTabs( aValue );
  }

  /**
   * @param aValue
   *          <code>true</code> to enable the tabs, <code>false</code> to
   *          disable them.
   */
  private void enableTriggerStageTabs( final boolean aValue )
  {
    for ( int i = this.triggerStageTabs.getTabCount() - 1; i >= 0; i-- )
    {
      Component tab = this.triggerStageTabs.getComponentAt( i );
      if ( tab != null )
      {
        tab.setEnabled( aValue );
      }
    }
  }

  /**
   * Builds this panel by placing all of its components on it.
   */
  private void buildPanel()
  {
    final JPanel generalPane = new JPanel( new SpringLayout() );
    generalPane.add( createRightAlignedLabel( "Trigger" ) );
    generalPane.add( this.triggerEnable );
    generalPane.add( new JLabel() );

    generalPane.add( createRightAlignedLabel( "Before/After ratio" ) );
    generalPane.add( this.ratioSlider );
    generalPane.add( this.ratioLabel );

    generalPane.add( createRightAlignedLabel( "Type" ) );
    generalPane.add( this.triggerTypeSelect );
    generalPane.add( new JLabel() );

    SpringLayoutUtils.makeCompactGrid( generalPane, 3, 3, 6, 6, 6, 6 );

    add( generalPane );
    add( this.triggerStageTabs );

    SpringLayoutUtils.makeCompactGrid( this, 2, 1, 6, 6, 6, 6 );
  }

  /**
   * @param aStageCount
   *          the number of trigger stages, >= 0;
   * @param aChannelCount
   *          the number of channels that can be captured, >= 0.
   */
  private void createSimpleTriggerEditor( final int aStageCount, final int aChannelCount )
  {
    // Remove all existing tabs prior to adding new ones...
    this.triggerStageTabs.removeAll();

    for ( int i = 0; i < aStageCount; i++ )
    {
      JPanel stagePane = new SimpleTriggerEditorPanel( i, aStageCount, aChannelCount );

      this.triggerStageTabs.addTab( String.format( "Stage %d", Integer.valueOf( i + 1 ) ), stagePane );
    }
  }

  /**
   * @param aValue
   *          <code>true</code> to enable the components, <code>false</code> to
   *          disable them.
   */
  private void enableComponents( final boolean aValue )
  {
    this.triggerEnable.setEnabled( aValue );

    enableTriggerStages( aValue );
  }

  /**
   * Initializes the components used by this panel.
   * 
   * @param aOCD
   * @param aInitialValues
   */
  private void initPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    this.triggerEnable = new JCheckBox( "Enabled" );
    this.triggerEnable.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox source = ( JCheckBox )aEvent.getSource();
        enableTriggerStages( source.isSelected() );
      }
    } );

    this.ratioLabel = new JLabel( "" );
    fixLabelWidth( this.ratioLabel, "100 / 100" );

    this.ratioSlider = new JSlider( SwingConstants.HORIZONTAL, 0, 100, 50 );
    this.ratioSlider.setMajorTickSpacing( 10 );
    this.ratioSlider.setMinorTickSpacing( 5 );
    this.ratioSlider.setPaintLabels( true );
    this.ratioSlider.setPaintTicks( true );
    this.ratioSlider.addChangeListener( new TriggerRatioChangeListener( this.ratioLabel ) );
    // Issue #82: set the minimum and preferred size to avoid having a
    // "squeezed" slider in the UI...
    final Dimension size = new Dimension( 350, 50 );
    this.ratioSlider.setMinimumSize( size );
    this.ratioSlider.setPreferredSize( size );

    this.triggerTypeSelect = new JComboBox();
    // this.triggerTypeSelect.addActionListener( fieldUpdater );

    this.triggerStageTabs = new JTabbedPane();
    createSimpleTriggerEditor( LogicSnifferConfigImpl.TRIGGER_STAGES, Ols.MAX_CHANNELS );
  }
}
