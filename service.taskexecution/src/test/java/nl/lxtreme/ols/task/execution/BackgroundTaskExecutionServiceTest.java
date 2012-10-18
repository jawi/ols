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
package nl.lxtreme.ols.task.execution;


import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.api.tools.*;

import org.junit.*;
import org.mockito.*;


/**
 * 
 */
public class BackgroundTaskExecutionServiceTest
{
  // CONSTANTS

  /**
   * Magic constant to denote that the call() method should throw an
   * exception...
   */
  private static final int THROW_EXCEPTION = -1;
  private static final int INFINITE_BLOCKING_CALL = -2;
  private static final int INTERRUPTABLE_BLOCKING_CALL = -3;

  // VARIABLES

  private TaskStatusListener mockTaskStatusListener;

  private BackgroundTaskExecutionService service;

  // METHODS

  /**
   * @param aTime
   * @throws InterruptedException
   */
  private static void sleep( final int aTime ) throws InterruptedException
  {
    Thread.sleep( aTime );
  }

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception
  {
    this.mockTaskStatusListener = mock( TaskStatusListener.class );

    this.service = new BackgroundTaskExecutionService( this.mockTaskStatusListener );
  }

  /**
   * @throws Exception
   */
  @After
  public void tearDown() throws Exception
  {
    if ( this.service != null )
    {
      try
      {
        this.service.close();
      }
      catch ( IllegalStateException exception )
      {
        // Ignore...
      }
    }
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test( expected = CancellationException.class )
  public void testExecuteCancelOk() throws Exception
  {
    final Task<Object> task = createMockTask( 500 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockTaskStatusListener );

    Future<Object> future = this.service.execute( task );

    sleep( 100 ); // sleep long enough for the entire method to complete...

    future.cancel( true /* mayInterruptIfRunning */);

    sleep( 50 ); // sleep long enough to allow callbacks to be invoked...

    inOrder.verify( this.mockTaskStatusListener ).taskStarted( eq( task ) );
    inOrder.verify( this.mockTaskStatusListener ).taskFailed( eq( task ), Matchers.<Exception> any() );
    inOrder.verifyNoMoreInteractions();

    assertTrue( future.isCancelled() );
    assertTrue( future.isDone() );
    assertNull( future.get() ); // throws exception!
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test( expected = ExecutionException.class )
  public void testExecuteCloseCancelsRunningToolsOk() throws Exception
  {
    final Task<Object> task = createMockTask( 500 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockTaskStatusListener );

    Future<Object> future = this.service.execute( task );

    sleep( 10 ); // sleep long enough for the entire method to complete...

    this.service.close();

    sleep( 50 ); // sleep long enough to allow callbacks to be invoked...

    inOrder.verify( this.mockTaskStatusListener ).taskStarted( eq( task ) );
    inOrder.verify( this.mockTaskStatusListener ).taskFailed( eq( task ), Matchers.<Exception> any() );
    inOrder.verifyNoMoreInteractions();

    assertFalse( future.isCancelled() );
    assertTrue( future.isDone() );
    assertNull( future.get() ); // throws exception!
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test
  public void testExecuteCloseOk() throws Exception
  {
    final Task<Object> task = createMockTask( 100 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockTaskStatusListener );

    Future<Object> future = this.service.execute( task );

    sleep( 200 ); // sleep long enough for the entire method to complete...

    this.service.close();

    sleep( 50 ); // sleep long enough to allow callbacks to be invoked...

    inOrder.verify( this.mockTaskStatusListener ).taskStarted( eq( task ) );
    inOrder.verify( this.mockTaskStatusListener ).taskEnded( eq( task ), anyObject() );
    inOrder.verifyNoMoreInteractions();

    assertFalse( future.isCancelled() );
    assertTrue( future.isDone() );
    assertNull( future.get() );
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test
  public void testExecuteCompleteOk() throws Exception
  {
    final Task<Object> task = createMockTask( 100 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockTaskStatusListener );

    Future<Object> future = this.service.execute( task );

    sleep( 200 ); // sleep long enough for the entire method to complete...

    inOrder.verify( this.mockTaskStatusListener ).taskStarted( eq( task ) );
    inOrder.verify( this.mockTaskStatusListener ).taskEnded( eq( task ), anyObject() );
    inOrder.verifyNoMoreInteractions();

    assertFalse( future.isCancelled() );
    assertTrue( future.isDone() );
    assertNull( future.get() );
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test( expected = ExecutionException.class )
  public void testExecuteFail() throws Exception
  {
    final Task<Object> task = createMockTask( THROW_EXCEPTION );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockTaskStatusListener );

    Future<Object> future = this.service.execute( task );

    sleep( 10 );

    inOrder.verify( this.mockTaskStatusListener ).taskStarted( eq( task ) );
    inOrder.verify( this.mockTaskStatusListener ).taskFailed( eq( task ), Matchers.<Exception> any() );
    inOrder.verifyNoMoreInteractions();

    assertTrue( future.isDone() );
    assertNull( future.get() ); // throws exception!
  }

  /**
   * Test method for
   * {@link BackgroundTaskExecutionService#execute(Tool, ToolContext)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testExecuteNullToolArgumentFail() throws Exception
  {
    this.service.execute( null );
  }

  /**
   * Creates a mock tool instance with a given time-to-complete.
   * 
   * @param aTimeout
   *          the time to wait in the {@link Tool#invoke(ToolContext)} method,
   *          >= 0.
   * @return a tool instance, never <code>null</code>.
   */
  private Task<Object> createMockTask( final int aTimeout )
  {
    Task<Object> tool = new Task<Object>()
    {
      /**
       * {@inheritDoc}
       */
      @Override
      public Object call() throws Exception
      {
        if ( aTimeout > 0 )
        {
          sleep( aTimeout );
        }
        else if ( aTimeout == THROW_EXCEPTION )
        {
          // Use a specific exception we can test for...
          throw new EOFException( "Hello World!" );
        }
        else if ( aTimeout == INTERRUPTABLE_BLOCKING_CALL )
        {
          while ( !Thread.currentThread().isInterrupted() )
          {
            // Avoid a busy wait loop...
          }
        }
        else if ( aTimeout == INFINITE_BLOCKING_CALL )
        {
          while ( true )
          {
            // Avoid a busy wait loop...
            try
            {
              sleep( 10 );
            }
            catch ( Exception exception )
            {
              // Eat any exception...
            }
          }
        }
        return null;
      }
    };

    return tool;
  }

}
