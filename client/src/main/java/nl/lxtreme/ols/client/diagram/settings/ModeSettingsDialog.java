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
package nl.lxtreme.ols.client.diagram.settings;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;


/**
 * Stores diagram "mode" settings and provides a dialog for changing them.
 *
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public class ModeSettingsDialog extends JDialog implements Configurable, Closeable
{
  // INNER TYPES

  /**
   * @author jawi
   */
  final class CheckboxListener implements ItemListener
  {
    private final int group;
    private final int mask;

    /**
     * @param aGroup
     * @param aMask
     */
    public CheckboxListener( final int aGroup, final int aMask )
    {
      this.group = aGroup;
      this.mask = aMask;
    }

    /**
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    @Override
    public void itemStateChanged( final ItemEvent aEvent )
    {
      final JCheckBox source = ( JCheckBox )aEvent.getSource();
      updateGroupState( this.group, this.mask, source.isSelected() );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Insets GROUP_INSETS = new Insets( 6, 4, 6, 6 );
  private static final Insets LABEL_INSETS = new Insets( 6, 2, 6, 6 );
  private static final Insets COMP_INSETS = new Insets( 6, 4, 6, 2 );

  // VARIABLES

  private final MutableDiagramSettings settings;
  private boolean result;

  // CONSTRUCTORS

  /**
   * Constructs diagram settings component.
   *
   * @param aParent
   * @param aSettings
   */
  public ModeSettingsDialog( final Window aParent, final DiagramSettings aSettings )
  {
    super( aParent, "Diagram Settings", ModalityType.DOCUMENT_MODAL );

    this.settings = new MutableDiagramSettings( aSettings );

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
   * Returns the (mutated) diagram settings.
   *
   * @return the diagram settings, never <code>null</code>.
   */
  public final DiagramSettings getDiagramSettings()
  {
    return this.settings;
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    // TODO Auto-generated method stub
  }

  /**
   * Display the settings dialog. If the user clicks ok, all changes are
   * reflected in the properties of this object. Otherwise changes are
   * discarded.
   *
   * @return <code>OK</code> when user accepted changes, <code>CANCEL</code>
   *         otherwise
   */
  public boolean showDialog()
  {
    setVisible( true );
    return ( this.result );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @param aGroup
   * @param aMask
   * @param aSelected
   */
  final void updateGroupState( final int aGroup, final int aMask, final boolean aSelected )
  {
    if ( aMask == DiagramSettings.DISPLAY_BYTE )
    {
      this.settings.setShowByte( aGroup, aSelected );
    }
    else if ( aMask == DiagramSettings.DISPLAY_CHANNELS )
    {
      this.settings.setShowChannels( aGroup, aSelected );
    }
    else if ( aMask == DiagramSettings.DISPLAY_SCOPE )
    {
      this.settings.setShowScope( aGroup, aSelected );
    }
  }

  /**
   * @return
   */
  private JPanel createDisplayModePane()
  {
    final JPanel modePane = new JPanel( new GridBagLayout() );
    modePane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    final JCheckBox[][] groupSettingBoxes = new JCheckBox[4][3];
    for ( int i = 0; i < 4; i++ )
    {
      groupSettingBoxes[i][0] = new JCheckBox();
      groupSettingBoxes[i][1] = new JCheckBox();
      groupSettingBoxes[i][2] = new JCheckBox();

      modePane.add( new JLabel( "Group " + i + ": " ), //
          new GridBagConstraints( 0, i, 1, 1, 0.0, 0.0, GridBagConstraints.BASELINE, GridBagConstraints.HORIZONTAL,
              GROUP_INSETS, 0, 0 ) );

      groupSettingBoxes[i][0].setSelected( this.settings.isShowChannels( i ) );
      groupSettingBoxes[i][0].addItemListener( new CheckboxListener( i, DiagramSettings.DISPLAY_CHANNELS ) );
      modePane.add( groupSettingBoxes[i][0], //
          new GridBagConstraints( 1, i, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE, GridBagConstraints.NONE,
              COMP_INSETS, 0, 0 ) );
      modePane.add( new JLabel( "Channels" ), //
          new GridBagConstraints( 2, i, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
              LABEL_INSETS, 0, 0 ) );

      groupSettingBoxes[i][1].setSelected( this.settings.isShowScope( i ) );
      groupSettingBoxes[i][1].addItemListener( new CheckboxListener( i, DiagramSettings.DISPLAY_SCOPE ) );
      modePane.add( groupSettingBoxes[i][1], //
          new GridBagConstraints( 3, i, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE, GridBagConstraints.NONE,
              COMP_INSETS, 0, 0 ) );
      modePane.add( new JLabel( "Scope" ), //
          new GridBagConstraints( 4, i, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
              LABEL_INSETS, 0, 0 ) );

      groupSettingBoxes[i][2].setSelected( this.settings.isShowByte( i ) );
      groupSettingBoxes[i][2].addItemListener( new CheckboxListener( i, DiagramSettings.DISPLAY_BYTE ) );
      modePane.add( groupSettingBoxes[i][2], //
          new GridBagConstraints( 5, i, 1, 1, 1.0, 0.0, GridBagConstraints.BASELINE, GridBagConstraints.NONE,
              COMP_INSETS, 0, 0 ) );
      modePane.add( new JLabel( "Byte Value" ), //
          new GridBagConstraints( 6, i, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
              LABEL_INSETS, 0, 0 ) );
    }
    return modePane;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    final JComponent modePane = createDisplayModePane();
    final JButton cancel = StandardActionFactory.createCloseButton();

    final JButton ok = new JButton( "Ok" );
    ok.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        ModeSettingsDialog.this.result = true;
        close();
      }
    } );

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( ok, cancel );

    SwingComponentUtils.setupDialogContentPane( this, modePane, buttonPane, ok );
  }
}
