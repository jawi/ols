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
package nl.lxtreme.ols.tool.state;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jawi
 */
public final class StateAnalysisDialog extends BaseAsyncToolDialog<CapturedData, StateAnalysisWorker>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JComboBox edgeSelect;
  private JComboBox channelSelect;
  private RestorableAction runAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public StateAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    initDialog();

    setLocationRelativeTo( getOwner() );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void readPreferences( final UserSettings aSettings )
  {
    this.edgeSelect.setSelectedIndex( aSettings.getInt( "edge", -1 ) );
    this.channelSelect.setSelectedIndex( aSettings.getInt( "channel", -1 ) );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#reset()
   */
  @Override
  public void reset()
  {
    this.runAction.restore();
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(nl.lxtreme.ols.api.UserSettings)
   */
  @Override
  public void writePreferences( final UserSettings aSettings )
  {
    aSettings.putInt( "channel", this.channelSelect.getSelectedIndex() );
    aSettings.putInt( "edge", this.edgeSelect.getSelectedIndex() );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#onToolWorkerStarted()
   */
  @Override
  protected void onToolWorkerStarted()
  {
    setVisible( false );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setControlsEnabled(boolean)
   */
  @Override
  protected void setControlsEnabled( final boolean aEnabled )
  {
    this.runAction.setEnabled( aEnabled );
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final StateAnalysisWorker aToolWorker )
  {
    aToolWorker.setNumber( this.channelSelect.getSelectedIndex() );

    if ( "Rising".equals( this.edgeSelect.getSelectedItem() ) )
    {
      aToolWorker.setLevel( 0 );
    }
    else
    {
      aToolWorker.setLevel( 1 );
    }
  }

  /**
   * @return
   */
  private JPanel createContentPane()
  {
    final JPanel pane = new JPanel( new GridLayout( 2, 2, 5, 5 ) );
    pane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 0 ) );

    final String[] channels = new String[32];
    for ( int i = 0; i < channels.length; i++ )
    {
      channels[i] = Integer.toString( i );
    }
    this.channelSelect = new JComboBox( channels );
    pane.add( new JLabel( "Clock Channel:" ) );
    pane.add( this.channelSelect );

    final String[] edges = { "Rising", "Falling" };

    this.edgeSelect = new JComboBox( edges );
    pane.add( new JLabel( "Clock Edge:" ) );
    pane.add( this.edgeSelect );
    return pane;
  }

  /**
   * Initializes this dialog.
   */
  private void initDialog()
  {
    setResizable( false );

    final JComponent pane = createContentPane();
    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton closeButton = createCloseButton();

    final JComponent buttons = SwingComponentUtils.createButtonPane( runAnalysisButton, closeButton );

    SwingComponentUtils.setupDialogContentPane( this, pane, buttons, runAnalysisButton );
  }
}

/* EOF */
