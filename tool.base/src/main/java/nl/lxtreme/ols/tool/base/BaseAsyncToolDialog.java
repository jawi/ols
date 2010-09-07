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
package nl.lxtreme.ols.tool.base;


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.SwingWorker.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.tool.base.BaseAsyncTool.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a base class for asynchronous tools wishing to show their results in
 * a dialog.
 */
public abstract class BaseAsyncToolDialog<RESULT_TYPE, WORKER extends BaseAsyncToolWorker<RESULT_TYPE>> extends
    BaseToolDialog implements AsyncToolDialog<RESULT_TYPE, WORKER>, Configurable
{
  // INNER TYPES

  /**
   * Provides an action that can be restored to a particular (initial) state.
   */
  public static interface RestorableAction extends Action
  {
    /**
     * Restores the state of the action to its initial state.
     */
    void restore();
  }

  /**
   * Tool worker property change lister used to determine the state of the tool
   * worker and report this back to methods of this dialog.
   */
  final class ToolWorkerPropertyChangeListener implements PropertyChangeListener
  {
    /**
     * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
     */
    @Override
    @SuppressWarnings( "unchecked" )
    public void propertyChange( final PropertyChangeEvent aEvent )
    {
      final String name = aEvent.getPropertyName();
      final Object value = aEvent.getNewValue();

      if ( BaseAsyncTool.PROPERTY_STATE.equals( name ) )
      {
        // State change...
        final StateValue state = ( StateValue )value;
        if ( StateValue.STARTED.equals( state ) )
        {
          // Set the "wait" cursor...
          setCursor( new Cursor( Cursor.WAIT_CURSOR ) );

          onToolWorkerStarted();
        }
        else if ( StateValue.DONE.equals( state ) )
        {
          final WORKER worker = ( WORKER )aEvent.getSource();

          try
          {
            final RESULT_TYPE analysisResults = worker.get();

            setAnalysisResult( analysisResults );

            onToolWorkerReady( analysisResults );
          }
          catch ( CancellationException exception )
          {
            LOG.log( Level.WARNING, "Cancellation exception! Message: {0}", exception.getMessage() );
            onToolWorkerCancelled();
          }
          catch ( ExecutionException exception )
          {
            // Make sure to handle IO-interrupted exceptions properly!
            HostUtils.handleInterruptedException( exception.getCause() );

            LOG.log( Level.WARNING, "Execution exception! Message: {0}", exception.getCause().getMessage() );
            exception.getCause().printStackTrace();

            onToolWorkerCancelled();
          }
          catch ( InterruptedException exception )
          {
            // Make sure to handle IO-interrupted exceptions properly!
            HostUtils.handleInterruptedException( exception );

            LOG.log( Level.WARNING, "Interrupted exception! Message: {0}", exception.getMessage() );
            onToolWorkerCancelled();
          }
          finally
          {
            // Restore the original cursor...
            setCursor( new Cursor( Cursor.DEFAULT_CURSOR ) );
          }
        }
      }
    }
  }

  /**
   * Provides an export action, that is capable of exporting the analysis
   * results to either CSV or HTML.
   */
  protected final class ExportAction extends AbstractAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * 
     */
    public ExportAction()
    {
      super( "Export" );
      putValue( SHORT_DESCRIPTION, "Exports the analysis results to file" );
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_E ) );
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final File selectedFile = SwingComponentUtils.showFileSaveDialog( getOwner(), StdFileFilter.CSV,
          StdFileFilter.HTML );

      final RESULT_TYPE analysisResult = getAnalysisResult();
      if ( selectedFile != null )
      {
        if ( LOG.isLoggable( Level.INFO ) )
        {
          LOG.info( "Writing analysis results to " + selectedFile.getPath() );
        }

        final String filename = selectedFile.getName();
        if ( filename.endsWith( ".htm" ) || filename.endsWith( ".html" ) )
        {
          storeToHtmlFile( selectedFile, analysisResult );
        }
        else
        {
          storeToCsvFile( selectedFile, analysisResult );
        }
      }
    }
  }

  /**
   * Provides an "run" action that is actually starting the (asynchronous) tool
   * worker.
   */
  protected final class RunAnalysisAction extends AbstractAction implements RestorableAction
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * 
     */
    public RunAnalysisAction()
    {
      super( "Analyze" );
      restore();
    }

    // METHODS

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final String name = ( String )getValue( NAME );

      if ( "Abort".equals( name ) )
      {
        cancelToolWorker();

        putValue( NAME, "Analyze" );
      }
      else
      {
        startToolWorker();

        putValue( NAME, "Abort" );
        putValue( SHORT_DESCRIPTION, "Aborts current analysis..." );
        putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_ESCAPE ) );
      }
    }

    /**
     * 
     */
    public void restore()
    {
      putValue( NAME, "Analyze" );
      putValue( SHORT_DESCRIPTION, "Run analysis" );
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_ENTER ) );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( BaseAsyncToolDialog.class.getName() );

  // VARIABLES

  private transient volatile ToolWorkerFactory<RESULT_TYPE, WORKER> toolWorkerFactory;
  private transient volatile RESULT_TYPE analysisResult;
  private transient volatile WORKER worker;

  // CONSTRUCTORS

  /**
   * Creates a new modal BaseAsyncToolDialog instance.
   * 
   * @param aOwner
   *          the owning window;
   * @param aName
   *          the title/name of this dialog.
   */
  protected BaseAsyncToolDialog( final Window aOwner, final String aName )
  {
    super( aOwner, aName );
  }

  /**
   * Creates a new BaseAsyncToolDialog instance.
   * 
   * @param aOwner
   *          the owning window;
   * @param aName
   *          the title/name of this dialog;
   * @param aModality
   *          the modality of this dialog.
   */
  protected BaseAsyncToolDialog( final Window aOwner, final String aName, final ModalityType aModality )
  {
    super( aOwner, aName, aModality );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.tool.base.ToolDialog#reset()
   */
  @Override
  public void reset()
  {
    // NO-op
  }

  /**
   * @see nl.lxtreme.ols.tool.base.AsyncToolDialog#setToolWorkerFactory(nl.lxtreme.ols.tool.base.BaseAsyncTool.ToolWorkerFactory)
   */
  @Override
  public synchronized void setToolWorkerFactory( final ToolWorkerFactory<RESULT_TYPE, WORKER> aToolWorkerFactory )
  {
    if ( this.toolWorkerFactory != null )
    {
      // Make sure any existing tool workers are cancelled!
      cancelToolWorker();
    }

    this.toolWorkerFactory = aToolWorkerFactory;
  }

  /**
   * Cancels all running tool workers.
   */
  final void cancelToolWorker()
  {
    synchronized ( this.toolWorkerFactory )
    {
      if ( this.worker != null )
      {
        this.worker.cancel( true /* mayInterruptIfRunning */);
        this.worker = null;
      }

      this.analysisResult = null;

      setControlsEnabled( true );
    }
  }

  /**
   * Sets the analysis results.
   * 
   * @param aResult
   *          the analysis result to set, can be <code>null</code>.
   */
  final void setAnalysisResult( final RESULT_TYPE aResult )
  {
    synchronized ( this.toolWorkerFactory )
    {
      this.analysisResult = aResult;
    }
  }

  /**
   * Starts the tool worker.
   */
  final void startToolWorker()
  {
    synchronized ( this.toolWorkerFactory )
    {
      this.worker = this.toolWorkerFactory.createToolWorker();

      setControlsEnabled( false );
      setupToolWorker( this.worker );

      this.worker.addPropertyChangeListener( new ToolWorkerPropertyChangeListener() );
      this.worker.execute();
    }
  }

  /**
   * Closes this dialog, cancels any running tool workers if they are still
   * running.
   */
  @Override
  protected final void close()
  {
    synchronized ( this.toolWorkerFactory )
    {
      cancelToolWorker();
      super.close();
    }
  }

  /**
   * @return
   */
  protected final JButton createExportButton()
  {
    final ExportAction action = new ExportAction();
    final JButton exportButton = new JButton( action );
    SwingComponentUtils.registerKeystroke( exportButton, action, "EXPORT" );
    return exportButton;
  }

  /**
   * @return
   */
  protected final JButton createRunAnalysisButton()
  {
    final RunAnalysisAction action = new RunAnalysisAction();
    final JButton runButton = new JButton( action );
    SwingComponentUtils.registerKeystroke( runButton, action, "RUN-ANALYSIS" );
    return runButton;
  }

  /**
   * Returns the analysis result, after the tool worker is finished with its
   * job.
   * 
   * @return an analysis result, can be <code>null</code>.
   */
  protected final RESULT_TYPE getAnalysisResult()
  {
    return this.analysisResult;
  }

  /**
   * Called upon canceling the tool worker.
   */
  protected void onToolWorkerCancelled()
  {
    close();
  }

  /**
   * Called right after the tool worker has indicated that it is finished its
   * tasks.
   * 
   * @param aAnalysisResult
   *          the analysis results of the tool worker, can be <code>null</code>.
   */
  protected void onToolWorkerReady( final RESULT_TYPE aAnalysisResult )
  {
    this.analysisResult = aAnalysisResult;

    setControlsEnabled( true );
  }

  /**
   * Called when the tool worker is started (in the background).
   */
  protected void onToolWorkerStarted()
  {
    // NO-op
  }

  /**
   * Called to enable/disable the various UI-controls.
   * 
   * @param aEnabled
   *          <code>true</code> to enable the controls, <code>false</code> to
   *          disable them.
   */
  protected void setControlsEnabled( final boolean aEnabled )
  {
    // NO-op
  }

  /**
   * Called right before the tool worker is started.
   * <p>
   * Can be used to parameterize the toolworker with UI-options.
   * </p>
   * 
   * @param aToolWorker
   *          the tool worker to setup, cannot be <code>null</code>.
   */
  protected void setupToolWorker( final WORKER aToolWorker )
  {
    // NO-op
  }

  /**
   * Called when the user has chosen to export the analysis results as CSV.
   * 
   * @param aSelectedFile
   *          the file to write the CSV-data to;
   * @param aAnalysisResult
   *          the analysis results to write to CSV.
   */
  protected void storeToCsvFile( final File aSelectedFile, final RESULT_TYPE aAnalysisResult )
  {
    // NO-op
  }

  /**
   * Called when the user has chosen to export the analysis results as HTML.
   * 
   * @param aSelectedFile
   *          the file to write the CSV-data to;
   * @param aAnalysisResult
   *          the analysis results to write to CSV.
   */
  protected void storeToHtmlFile( final File aSelectedFile, final RESULT_TYPE aAnalysisResult )
  {
    // NO-op
  }
}
