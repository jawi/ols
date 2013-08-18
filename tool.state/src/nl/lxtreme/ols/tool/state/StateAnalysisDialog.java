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
package nl.lxtreme.ols.tool.state;


import java.awt.*;

import javax.swing.*;

import org.osgi.framework.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.tool.base.ToolUtils.RestorableAction;
import nl.lxtreme.ols.util.osgi.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * @author jawi
 */
public final class StateAnalysisDialog extends BaseToolDialog<AcquisitionResult>
{
  // INNER TYPES

  /**
   * Provides a combobox renderer for {@link Edge} values.
   */
  static final class EdgeItemRenderer extends EnumItemRenderer<Edge>
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    /**
     * @see nl.lxtreme.ols.client.diagram.settings.GeneralSettingsDialog.EnumItemRenderer#getDisplayValue(java.lang.Enum)
     */
    @Override
    protected String getDisplayValue( final Edge aValue )
    {
      String text = super.getDisplayValue( aValue );
      if ( Edge.FALLING.equals( aValue ) )
      {
        text = "Falling";
      }
      else if ( Edge.RISING.equals( aValue ) )
      {
        text = "Rising";
      }
      return text;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final WhiteboardHelper<AcquisitionDataListener> acquisitionDataListenerHelper;

  private JComboBox edgeSelect;
  private JComboBox channelSelect;
  private RestorableAction runAction;

  // CONSTRUCTORS

  /**
   * Creates a new StateAnalysisDialog instance.
   * 
   * @param aOwner
   *          the owner of this dialog;
   * @param aToolContext
   *          the tool context;
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aTool
   *          the {@link StateAnalyser} tool.
   */
  public StateAnalysisDialog( final Window aOwner, final ToolContext aToolContext, final BundleContext aContext,
      final StateAnalyser aTool )
  {
    super( aOwner, aToolContext, aContext, aTool );

    this.acquisitionDataListenerHelper = new WhiteboardHelper<AcquisitionDataListener>( aContext,
        AcquisitionDataListener.class );

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
    // Issue #114: avoid setting illegal values...
    setComboBoxIndex( this.channelSelect, aSettings, "channel" );

    this.edgeSelect.setSelectedIndex( aSettings.getInt( "edge", this.edgeSelect.getSelectedIndex() ) );
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
   * {@inheritDoc}
   */
  @Override
  protected void onBeforeCloseDialog()
  {
    this.acquisitionDataListenerHelper.close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onBeforeShowDialog()
  {
    this.acquisitionDataListenerHelper.open( true /* trackAllServices */);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolEnded( final AcquisitionResult aResult )
  {
    this.acquisitionDataListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionDataListener>()
    {
      @Override
      public void visit( final AcquisitionDataListener aService )
      {
        aService.acquisitionComplete( aResult );
      }
    } );

    // Close this dialog & dispose everything...
    close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onToolStarted()
  {
    // NO-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void prepareToolTask( final ToolTask<AcquisitionResult> aToolTask )
  {
    final StateAnalysisTask toolTask = ( StateAnalysisTask )aToolTask;

    toolTask.setNumber( this.channelSelect.getSelectedIndex() );

    if ( Edge.RISING == this.edgeSelect.getSelectedItem() )
    {
      toolTask.setLevel( 0 );
    }
    else
    {
      toolTask.setLevel( 1 );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void setControlsEnabled( final boolean aEnabled )
  {
    this.runAction.setEnabled( aEnabled );
  }

  /**
   * @return
   */
  private JPanel createContentPane()
  {
    final int channelCount = getData().getChannels();

    this.channelSelect = SwingComponentUtils.createChannelSelector( channelCount, 0 );

    this.edgeSelect = new JComboBox( new Edge[] { Edge.RISING, Edge.FALLING } );
    this.edgeSelect.setSelectedIndex( 0 );
    this.edgeSelect.setRenderer( new EdgeItemRenderer() );

    final JPanel pane = new JPanel( new GridLayout( 2, 2, 5, 5 ) );
    pane.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 0 ) );

    pane.add( new JLabel( "Clock Channel" ) );
    pane.add( this.channelSelect );

    pane.add( new JLabel( "Clock Edge" ) );
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
    final JButton runAnalysisButton = ToolUtils.createRunAnalysisButton( this );
    this.runAction = ( RestorableAction )runAnalysisButton.getAction();

    final JButton closeButton = ToolUtils.createCloseButton();

    final JComponent buttons = SwingComponentUtils.createButtonPane( runAnalysisButton, closeButton );

    SwingComponentUtils.setupWindowContentPane( this, pane, buttons, runAnalysisButton );
  }
}

/* EOF */
