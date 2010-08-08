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
import java.beans.*;
import java.util.concurrent.*;

import javax.swing.SwingWorker.StateValue;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a base class for tools that want to do its processing in the
 * background, or outside the EDT.
 */
public abstract class BaseAsyncTool<RESULT_TYPE, WORKER extends BaseAsyncToolWorker<RESULT_TYPE>> extends BaseTool
{
  // CONSTANTS

  private static final String PROPERTY_PROGRESS = "progress";
  private static final String PROPERTY_STATE = "state";

  // VARIABLES

  private WORKER toolWorker;

  // CONSTRUCTORS

  /**
   * Creates a new BaseTool instance.
   * 
   * @param aName
   *          the name of the tool as it should appear in the main UI.
   */
  protected BaseAsyncTool( final String aName )
  {
    super( aName );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.tools.Tool#process(java.awt.Frame,
   *      nl.lxtreme.ols.api.data.AnnotatedData,
   *      nl.lxtreme.ols.api.tools.ToolContext,
   *      nl.lxtreme.ols.api.tools.AnalysisCallback)
   */
  @Override
  public void process( final Frame aParentFrame, final AnnotatedData aData, final ToolContext aContext,
      final AnalysisCallback aCallback )
  {
    setupTool( aParentFrame );

    this.toolWorker = createToolWorker( aData );

    this.toolWorker.addPropertyChangeListener( new PropertyChangeListener()
    {
      /**
       * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
       */
      @Override
      public void propertyChange( final PropertyChangeEvent aEvent )
      {
        final String name = aEvent.getPropertyName();
        final Object value = aEvent.getNewValue();

        if ( PROPERTY_PROGRESS.equals( name ) )
        {
          // Progress update...
          final Integer percentage = ( Integer )value;
          aCallback.updateProgress( percentage );
        }
        else if ( PROPERTY_STATE.equals( name ) )
        {
          // State change...
          final StateValue state = ( StateValue )value;
          if ( StateValue.DONE.equals( state ) )
          {
            final WORKER worker = getToolWorker();

            RESULT_TYPE analysisResults = null;
            String abortReason = null;

            try
            {
              analysisResults = worker.get();
            }
            catch ( CancellationException exception )
            {
              abortReason = "Cancelled by user.";
            }
            catch ( ExecutionException exception )
            {
              abortReason = exception.getCause().getMessage();
            }
            catch ( InterruptedException exception )
            {
              abortReason = exception.getMessage();
            }

            if ( worker.isCancelled() || ( abortReason != null ) )
            {
              if ( abortReason == null )
              {
                abortReason = "";
              }
              aCallback.analysisAborted( abortReason );
            }
            else
            {
              CapturedData newData = null;
              if ( analysisResults instanceof CapturedData )
              {
                newData = ( CapturedData )analysisResults;
              }

              aCallback.analysisComplete( newData );

              toolWorkerDone( analysisResults );
            }
          }
        }

        // Pass through our local event handling method...
        BaseAsyncTool.this.propertyChange( aEvent );
      }
    } );

    doProcess( aData, aContext );
  }

  /**
   * Factory method for creating a tool worker.
   * 
   * @return a new instance of the intended tool worker, never <code>null</code>
   *         .
   */
  protected abstract WORKER createToolWorker( final AnnotatedData aData );

  /**
   * Does the actual processing of data.
   * <p>
   * By default, this method does <tt>getToolWorker().execute()</tt> to start
   * the tool worker in the background. Override this method to do something
   * different, for example, to wrap the tool worker in a UI-dialog.
   * </p>
   * 
   * @param aData
   *          the captured data to process in this tool;
   * @param aContext
   *          the tool context to use during the processing.
   */
  @Override
  protected void doProcess( final AnnotatedData aData, final ToolContext aContext )
  {
    getToolWorker().execute();
  }

  /**
   * Returns the current tool worker.
   * 
   * @return the tool worker, never <code>null</code>.
   */
  protected final WORKER getToolWorker()
  {
    return this.toolWorker;
  }

  /**
   * Allows for custom property change events.
   * 
   * @param aEvent
   *          the property change event, never <code>null</code>.
   */
  protected void propertyChange( final PropertyChangeEvent aEvent )
  {
    // NO-op
  }

  /**
   * Called when the tool worker is done processing its data.
   * 
   * @param aAnalysisResult
   *          the analysis result of the tool worker, never <code>null</code>.
   */
  protected void toolWorkerDone( final RESULT_TYPE aAnalysisResult )
  {
    // NO-op
  }
}

/* EOF */
