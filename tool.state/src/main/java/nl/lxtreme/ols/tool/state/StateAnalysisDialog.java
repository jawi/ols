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

import org.osgi.service.prefs.*;

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

  private final JComboBox edgeSelect;
  private final JComboBox channelSelect;
  private final RestorableAction runAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public StateAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );
    setResizable( false );

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    setContentPane( contentPane );
    contentPane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    final JPanel pane = new JPanel( new GridLayout( 2, 2, 5, 5 ) );

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

    add( pane, //
        new GridBagConstraints( 0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    /*
     * add buttons
     */
    final JButton runAnalysisButton = createRunAnalysisButton();
    this.runAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton closeButton = createCloseButton();

    final JPanel buttons = new JPanel();
    final BoxLayout layoutMgr = new BoxLayout( buttons, BoxLayout.LINE_AXIS );
    buttons.setLayout( layoutMgr );
    buttons.add( Box.createHorizontalGlue() );
    buttons.add( runAnalysisButton );
    buttons.add( Box.createHorizontalStrut( 16 ) );
    buttons.add( closeButton );

    add( buttons, //
        new GridBagConstraints( 0, 1, 1, 1, 1.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL,
            COMP_INSETS, 0, 0 ) );

    pack();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.Configurable#readPreferences(org.osgi.service.prefs.Preferences)
   */
  public void readPreferences( final Preferences aPrefs )
  {
    SwingComponentUtils.setSelectedIndex( this.edgeSelect, aPrefs.getInt( "edge", -1 ) );
    SwingComponentUtils.setSelectedIndex( this.channelSelect, aPrefs.getInt( "channel", -1 ) );
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
   * @see nl.lxtreme.ols.api.Configurable#writePreferences(org.osgi.service.prefs.Preferences)
   */
  public void writePreferences( final Preferences aPrefs )
  {
    aPrefs.putInt( "channel", this.channelSelect.getSelectedIndex() );
    aPrefs.putInt( "edge", this.edgeSelect.getSelectedIndex() );
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
}

/* EOF */
