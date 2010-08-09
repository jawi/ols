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
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * @author jawi
 */
public final class StateAnalysisDialog extends BaseAsyncToolDialog<CapturedData, StateAnalysisWorker> implements
ActionListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public final static int CANCEL = 0;
  public final static int OK = 1;
  public final static int RISING = 0;
  public final static int FALLING = 1;

  // VARIABLES

  public int channel;
  public int edge;
  private final JComboBox edgeSelect;
  private final JComboBox channelSelect;
  private final String[] edges;
  private final String[] channels;
  private int result;

  // CONSTRUCTORS

  /**
   * @param aOwner
   * @param aName
   */
  public StateAnalysisDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );

    Container pane = getContentPane();
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

    String[] tmp = { "Rising", "Falling" };
    this.edges = tmp;
    this.edgeSelect = new JComboBox( this.edges );
    pane.add( new JLabel( "Clock Edge:" ) );
    pane.add( this.edgeSelect );

    JButton convert = new JButton( "Convert" );
    convert.addActionListener( this );
    pane.add( convert );

    JButton cancel = new JButton( "Cancel" );
    cancel.addActionListener( this );
    pane.add( cancel );

    pack();
    setResizable( false );
    this.result = CANCEL;
  }

  // METHODS

  public void actionPerformed( final ActionEvent e )
  {
    this.channel = Integer.parseInt( ( String )this.channelSelect.getSelectedItem() );

    if ( ( ( String )this.edgeSelect.getSelectedItem() ).equals( "Rising" ) )
    {
      this.edge = RISING;
    }
    else
    {
      this.edge = FALLING;
    }

    if ( e.getActionCommand().equals( "Convert" ) )
    {
      this.result = OK;
    }
    else
    {
      this.result = CANCEL;
    }

    setVisible( false );
  }

  /**
   * @see nl.lxtreme.ols.api.Configurable#readProperties(java.lang.String,
   *      java.util.Properties)
   */
  public void readProperties( final String aNamespace, final Properties properties )
  {
    SwingComponentUtils.setSelectedItem( this.edgeSelect, properties.getProperty( aNamespace + ".edge" ) );
    SwingComponentUtils.setSelectedItem( this.channelSelect, properties.getProperty( aNamespace + ".channel" ) );
  }

  @Override
  public void reset()
  {
    // NO-op
  }

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#showDialog(nl.lxtreme.ols.api.data.AnnotatedData)
   */
  @Override
  public boolean showDialog( final AnnotatedData aData )
  {
    setVisible( true );
    return ( this.result == StateAnalysisDialog.OK );
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
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setControlsEnabled(boolean)
   */
  @Override
  protected void setControlsEnabled( final boolean aEnabled )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#setupToolWorker(nl.lxtreme.ols.tool.base.BaseAsyncToolWorker)
   */
  @Override
  protected void setupToolWorker( final StateAnalysisWorker aToolWorker )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#storeToCsvFile(java.io.File,
   *      java.lang.Object)
   */
  @Override
  protected void storeToCsvFile( final File aSelectedFile, final CapturedData aAnalysisResult )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseAsyncToolDialog#storeToHtmlFile(java.io.File,
   *      java.lang.Object)
   */
  @Override
  protected void storeToHtmlFile( final File aSelectedFile, final CapturedData aAnalysisResult )
  {
    // TODO Auto-generated method stub
  }
}

/* EOF */
