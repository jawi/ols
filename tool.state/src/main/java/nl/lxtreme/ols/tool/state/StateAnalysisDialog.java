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
import java.util.*;

import javax.swing.*;

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

  public int channel;
  public int edge;
  private final JComboBox edgeSelect;
  private final JComboBox channelSelect;
  private final String[] edges;
  private final String[] channels;

  private final RunAnalysisAction runAction;
  private final CloseAction closeAction;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public StateAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    final Container pane = getContentPane();
    pane.setLayout( new GridLayout( 3, 2, 5, 5 ) );
    getRootPane().setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

    this.channels = new String[32];
    for ( int i = 0; i < this.channels.length; i++ )
    {
      this.channels[i] = Integer.toString( i );
    }
    this.channelSelect = new JComboBox( this.channels );
    pane.add( new JLabel( "Clock Channel:" ) );
    pane.add( this.channelSelect );

    final String[] tmp = { "Rising", "Falling" };

    this.edges = tmp;
    this.edgeSelect = new JComboBox( this.edges );
    pane.add( new JLabel( "Clock Edge:" ) );
    pane.add( this.edgeSelect );

    this.runAction = new RunAnalysisAction();
    final JButton convert = new JButton( this.runAction );
    pane.add( convert );

    this.closeAction = new CloseAction();
    final JButton cancel = new JButton( this.closeAction );
    pane.add( cancel );

    pack();
    setResizable( false );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void readProperties( final String aNamespace, final Properties properties )
  {
    SwingComponentUtils.setSelectedItem( this.edgeSelect, properties.getProperty( aNamespace + ".edge" ) );
    SwingComponentUtils.setSelectedItem( this.channelSelect, properties.getProperty( aNamespace + ".channel" ) );
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
   * @see nl.lxtreme.ols.api.Configurable#writeProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void writeProperties( final String aNamespace, final Properties properties )
  {
    properties.setProperty( aNamespace + ".channel", ( String )this.channelSelect.getSelectedItem() );
    properties.setProperty( aNamespace + ".edge", ( String )this.edgeSelect.getSelectedItem() );
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
