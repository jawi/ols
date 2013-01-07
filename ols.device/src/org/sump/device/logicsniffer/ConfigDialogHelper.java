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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package org.sump.device.logicsniffer;


import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.device.logicsniffer.profile.*;
import nl.lxtreme.ols.device.logicsniffer.profile.DeviceProfile.*;


/**
 * @author jawi
 */
public final class ConfigDialogHelper
{
  // METHODS

  /**
   * @param aComboBox
   * @return
   */
  public static String getComboBoxText( final JComboBox aComboBox )
  {
    if ( aComboBox == null )
    {
      return null;
    }
    Object value = aComboBox.getSelectedItem();
    if ( value instanceof String )
    {
      return ( String )value;
    }
    if ( value == null )
    {
      return null;
    }
    return String.valueOf( value );
  }

  /**
   * @param aComboBox
   * @return
   */
  public static Integer getNumericValue( final JComboBox aTextField )
  {
    if ( aTextField == null )
    {
      return null;
    }
    Object value = aTextField.getSelectedItem();
    if ( value == null )
    {
      return null;
    }
    if ( value instanceof Integer )
    {
      return ( Integer )value;
    }
    try
    {
      return Integer.decode( String.valueOf( value ) );
    }
    catch ( NumberFormatException exception )
    {
      return Integer.valueOf( -1 );
    }
  }

  /**
   * @param aComboBox
   * @return
   */
  public static Integer getNumericValue( final JTextField aTextField )
  {
    if ( aTextField == null )
    {
      return null;
    }
    String value = aTextField.getText();
    if ( value == null )
    {
      return null;
    }
    try
    {
      return Integer.decode( value );
    }
    catch ( NumberFormatException exception )
    {
      return Integer.valueOf( -1 );
    }
  }

  /**
   * @param <T>
   * @param aComboBox
   * @param aValues
   */
  public static void updateCaptureSpeedComboBoxModel( final JComboBox aComboBox, final DeviceProfile aProfile )
  {
    Vector<Integer> sampleRates = new Vector<Integer>( Arrays.asList( aProfile.getSampleRates() ) );
    if ( aProfile.isDoubleDataRateSupported() )
    {
      // assume the sample rates are ordered in reverse order, so adding it at
      // the beginning should be fine...
      sampleRates.add( 0, Integer.valueOf( 2 * aProfile.getClockspeed() ) );
    }
    updateComboBoxModel( aComboBox, sampleRates );
  }

  /**
   * @param aChannelGroups
   * @param aProfile
   */
  public static void updateChannelGroups( final JCheckBox[] aChannelGroups, final DeviceProfile aProfile )
  {
    assert aChannelGroups != null : "Channel groups cannot be null!";
    assert aChannelGroups.length == 4 : "There should be 4 channel groups!";

    final int channelGroups = aProfile.getChannelGroupCount();
    for ( int i = 0; i < aChannelGroups.length; i++ )
    {
      final boolean enabled = i < channelGroups;
      updateCheckBoxState( aChannelGroups[i], enabled );
    }
  }

  /**
   * Updates the enabled state of the given checkbox. If <em>not</em> enabled,
   * it will also deselect the given checkbox.
   */
  public static void updateCheckBoxState( final JCheckBox aCheckBox, final boolean aEnabled )
  {
    if ( !aEnabled )
    {
      aCheckBox.setSelected( false );
    }
    aCheckBox.setEnabled( aEnabled );
  }

  /**
   * @param <T>
   * @param aComboBox
   * @param aValues
   */
  public static <T> void updateComboBoxModel( final JComboBox aComboBox, final T... aValues )
  {
    aComboBox.setModel( new DefaultComboBoxModel( aValues ) );
  }

  /**
   * @param <T>
   * @param aComboBox
   * @param aValues
   */
  public static <T> void updateComboBoxModel( final JComboBox aComboBox, final Vector<T> aValues )
  {
    aComboBox.setModel( new DefaultComboBoxModel( aValues ) );
  }

  /**
   * @param aTriggerMasks
   * @param aTriggerValues
   * @param aProfile
   */
  public static void updateTriggerChannels( final JCheckBox[][] aTriggerMasks, final JCheckBox[][] aTriggerValues,
      final DeviceProfile aProfile )
  {
    assert aTriggerMasks != null : "Trigger masks cannot be null!";
    assert aTriggerValues != null : "Trigger values cannot be null!";
    assert aTriggerMasks.length == aTriggerValues.length : "Trigger values length should match Trigger masks length";

    final int maxStages = aProfile.getTriggerStages();
    final int maxChannels = aProfile.getChannelCount();

    for ( int i = 0, stageCount = aTriggerMasks.length; i < stageCount; i++ )
    {
      final boolean stageEnabled = i < maxStages;
      final JCheckBox[] triggerMasks = aTriggerMasks[i];
      final JCheckBox[] triggerValues = aTriggerValues[i];

      assert triggerMasks.length == triggerValues.length;

      for ( int j = 0; j < triggerMasks.length; j++ )
      {
        final boolean triggerEnabled = stageEnabled && ( j < maxChannels );
        updateCheckBoxState( triggerMasks[j], triggerEnabled );
        updateCheckBoxState( triggerValues[j], triggerEnabled );
      }
    }
  }

  /**
   * @param <T>
   * @param aComboBox
   * @param aValues
   */
  public static void updateTriggerTypeComboBoxModel( final JComboBox aComboBox, final DeviceProfile aProfile )
  {
    final boolean complexTriggersSupported = aProfile.isComplexTriggersSupported();
    final TriggerType[] values;
    if ( complexTriggersSupported )
    {
      values = TriggerType.values();
    }
    else
    {
      values = new TriggerType[] { TriggerType.SIMPLE };
    }
    updateComboBoxModel( aComboBox, values );
    if ( !complexTriggersSupported )
    {
      aComboBox.setSelectedItem( TriggerType.SIMPLE );
    }
    aComboBox.setEnabled( aProfile.isTriggerSupported() );
  }
}
