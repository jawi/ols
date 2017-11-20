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
package nl.lxtreme.ols.tool.base;


import java.awt.*;
import java.awt.Dialog.ModalExclusionType;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;

import org.osgi.framework.*;


/**
 * Provides a base tool dialog.
 */
public abstract class BaseToolDialog<RESULT_TYPE> extends JFrame
    implements ToolDialog, TaskStatusListener, Configurable, Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** Provides insets (padding) that can be used for labels. */
  protected static final Insets LABEL_INSETS = new Insets( 4, 4, 4, 2 );
  /** Provides insets (padding) that can be used for components. */
  protected static final Insets COMP_INSETS = new Insets( 4, 2, 4, 4 );

  // VARIABLES

  private final ToolContext context;
  private final Tool<RESULT_TYPE> tool;
  private final BundleContext bundleContext;

  private final TaskExecutionServiceTracker taskExecutionService;
  private final AnnotationListenerServiceTracker annotationListener;
  private final ToolProgressListenerServiceTracker toolProgressListener;

  private ServiceRegistration<?> serviceReg;
  private volatile Future<RESULT_TYPE> toolFutureTask;
  private volatile ToolTask<RESULT_TYPE> toolTask;
  private volatile RESULT_TYPE lastResult;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BaseToolDialog} instance that is document modal.
   *
   * @param aOwner
   *          the owning window of this dialog;
   * @param aTitle
   *          the title of this dialog;
   * @param aContext
   *          the tool context to use in this dialog.
   */
  protected BaseToolDialog( final Window aOwner, final ToolContext aContext, final BundleContext aBundleContext,
      final Tool<RESULT_TYPE> aTool )
  {
    super( aTool.getName() );

    this.context = aContext;
    this.bundleContext = aBundleContext;
    this.tool = aTool;

    setModalExclusionType( ModalExclusionType.NO_EXCLUDE );

    this.taskExecutionService = new TaskExecutionServiceTracker( aBundleContext );
    this.annotationListener = new AnnotationListenerServiceTracker( aBundleContext );
    this.toolProgressListener = new ToolProgressListenerServiceTracker( aBundleContext );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public final void cancelTool() throws IllegalStateException
  {
    if ( this.toolFutureTask == null )
    {
      throw new IllegalStateException( "Tool is already cancelled!" );
    }

    this.toolFutureTask.cancel( true /* mayInterruptIfRunning */ );
    this.toolFutureTask = null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void close()
  {
    this.taskExecutionService.close();
    this.annotationListener.close();
    this.toolProgressListener.close();

    try
    {
      this.serviceReg.unregister();
      this.serviceReg = null;
    }
    catch ( IllegalStateException exception )
    {
      // Ignore; we're closing anyway...
    }

    onBeforeCloseDialog();

    setVisible( false );
    dispose();
  }

  /**
   * @return
   */
  public final ToolContext getContext()
  {
    return this.context;
  }

  /**
   * Returns the current value of lastResult.
   *
   * @return the lastResult
   */
  public final RESULT_TYPE getLastResult()
  {
    return this.lastResult;
  }

  /**
   * Returns the current value of tool.
   *
   * @return the tool
   */
  public final Tool<RESULT_TYPE> getTool()
  {
    return this.tool;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean invokeTool() throws IllegalStateException
  {
    if ( this.toolFutureTask != null )
    {
      throw new IllegalStateException( "Tool is already running!" );
    }

    boolean settingsValid = validateToolSettings();
    if ( settingsValid )
    {
      this.toolTask = this.tool.createToolTask( this.context, this.toolProgressListener, this.annotationListener );
      prepareToolTask( this.toolTask );

      this.toolFutureTask = this.taskExecutionService.execute( this.toolTask );
    }
    return settingsValid;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void showDialog()
  {
    this.serviceReg = this.bundleContext.registerService( TaskStatusListener.class.getName(), this, null );

    this.taskExecutionService.open();
    this.annotationListener.open();
    this.toolProgressListener.open();

    onBeforeShowDialog();

    setVisible( true );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "unchecked" )
  public final <RT> void taskEnded( final Task<RT> aTask, final RT aResult )
  {
    if ( this.toolTask == aTask )
    {
      this.lastResult = ( RESULT_TYPE )aResult;

      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );

          setControlsEnabled( true );

          onToolEnded( BaseToolDialog.this.lastResult );
        }
      } );

      this.toolFutureTask = null;
      this.toolTask = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final <RT> void taskFailed( final Task<RT> aTask, final Exception aException )
  {
    if ( this.toolTask == aTask )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );

          setControlsEnabled( true );

          onToolFailed( aException );
        }
      } );

      this.toolFutureTask = null;
      this.toolTask = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final <RT> void taskStarted( final Task<RT> aTask )
  {
    if ( this.toolTask == aTask )
    {
      SwingComponentUtils.invokeOnEDT( new Runnable()
      {
        @Override
        public void run()
        {
          setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );

          setControlsEnabled( false );

          onToolStarted();
        }
      } );
    }
  }

  /**
   * Returns the current value of bundleContext.
   *
   * @return the bundleContext
   */
  protected final BundleContext getBundleContext()
  {
    return this.bundleContext;
  }

  /**
   * Returns the acquisition result data.
   *
   * @return the acquisition data, never <code>null</code>.
   */
  protected final AcquisitionResult getData()
  {
    return this.context.getData();
  }

  /**
   * Called right before this dialog is made invisible.
   */
  protected void onBeforeCloseDialog()
  {
    // NO-op
  }

  /**
   * Called right before this dialog is made visible.
   */
  protected void onBeforeShowDialog()
  {
    // NO-op
  }

  /**
   * Called when the tool finished its job.
   * <p>
   * <b>THIS METHOD WILL BE INVOKED ON THE EVENT-DISPATCH THREAD (EDT)!</b>
   * </p>
   *
   * @param aResult
   *          the result of the tool, can be <code>null</code>.
   */
  protected abstract void onToolEnded( RESULT_TYPE aResult );

  /**
   * Called when the tool is failed.
   * <p>
   * By default, shows a error dialog with the details of the failure.
   * </p>
   * <p>
   * <b>THIS METHOD WILL BE INVOKED ON THE EVENT-DISPATCH THREAD (EDT)!</b>
   * </p>
   *
   * @param aException
   *          the exception with the failure, can be <code>null</code>.
   */
  protected void onToolFailed( final Exception aException )
  {
    ToolUtils.showErrorMessage( getOwner(), "Tool failed!\nDetails: " + aException.getMessage() );
  }

  /**
   * Called when the tool is just started to do its task.
   * <p>
   * <b>THIS METHOD WILL BE INVOKED ON THE EVENT-DISPATCH THREAD (EDT)!</b>
   * </p>
   */
  protected abstract void onToolStarted();

  /**
   * Allows additional preparations to be performed on the given
   * {@link ToolTask} instance, such as setting parameters and such.
   * <p>
   * This method will be called right before the tool task is to be executed.
   * </p>
   *
   * @param aToolTask
   *          the tool task to prepare, cannot be <code>null</code>.
   */
  protected void prepareToolTask( final ToolTask<RESULT_TYPE> aToolTask )
  {
    // NO-op
  }

  /**
   * Convenience method to set the combobox index to a "safe" value, based on
   * the given user settings.
   *
   * @param aComboBox
   *          the combobox to set the selected index for;
   * @param aSettings
   *          the user settings to take the selected index from;
   * @param aSettingName
   *          the name of the user setting to use.
   */
  protected final void setComboBoxIndex( final JComboBox aComboBox, final UserSettings aSettings,
      final String aSettingName )
  {
    ToolUtils.setComboBoxIndex( aComboBox, aSettings.getInt( aSettingName, -1 ) );
  }

  /**
   * set the controls of the dialog enabled/disabled
   *
   * @param aEnabled
   *          status of the controls
   */
  protected void setControlsEnabled( final boolean aEnabled )
  {
    // NO-op
  }

  /**
   * @param aValue
   *          the value to consider as an ASCII value.
   * @return the ASCII string representation of the given value, or <tt>""</tt>
   *         if the given value is not a printable ASCII character.
   */
  protected final String toASCII( final int aValue )
  {
    // If it is in the "printable" ASCII range, show it, otherwise not...
    if ( ( aValue > 0x1f ) && ( aValue < 0x7f ) )
    {
      return Character.toString( ( char )aValue );
    }

    return "";
  }

  /**
   * Called right before the tool is invoked to allow additional validation on
   * the tool settings.
   *
   * @return <code>true</code> if the tool settings are correct and the task can
   *         be started, <code>false</code> if the settings are incorrect and
   *         the task should not be started.
   */
  protected boolean validateToolSettings()
  {
    return true;
  }
}
