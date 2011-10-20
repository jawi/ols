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
package nl.lxtreme.ols.acquisition;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.acquisition.AcquisitionResultStatus.*;
import nl.lxtreme.ols.api.devices.*;

import org.junit.*;
import org.mockito.*;
import org.mockito.invocation.*;
import org.mockito.stubbing.*;


/**
 * Test cases for {@link BackgroundDataAcquisitionService}.
 */
public class BackgroundDataAcquisitionServiceTest
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

  private AcquisitionListenerHelper mockAcquisitionListenerHelper;

  private BackgroundDataAcquisitionService service;

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
    this.mockAcquisitionListenerHelper = mock( AcquisitionListenerHelper.class );

    this.service = new BackgroundDataAcquisitionService( this.mockAcquisitionListenerHelper );
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
   * Tests that a successful acquisition yields the correct events for
   * interested listeners.
   */
  @Test
  public void testAcquireDataCompleteOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 100 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    this.service.acquireData( deviceController );

    sleep( 200 ); // sleep long enough for the entire method to complete...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionComplete( any( AcquisitionResult.class ) );
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    assertEquals( ResultStatus.NORMAL, captor.getValue().getStatus() );
    assertNull( captor.getValue().getMessage() );
  }

  /**
   * Tests that an unsuccesful acquisition yields the correct events for
   * interested listeners.
   */
  @Test
  public void testAcquireDataFail() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( THROW_EXCEPTION );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    assertEquals( ResultStatus.FAILED, captor.getValue().getStatus() );
    assertEquals( "Hello World!", captor.getValue().getMessage() );
  }

  /**
   * Tests that a successful acquisition yields the correct events for
   * interested listeners.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAcquireDataNullArgumentFail() throws Exception
  {
    this.service.acquireData( null );
  }

  /**
   * Tests that when the acquisition is still ongoing that we only sent the
   * expected events to the interested listeners.
   */
  @Test
  public void testAcquireDataOngoingOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 100 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verifyNoMoreInteractions();

    sleep( 100 ); // wait a little while in order to get the method fully
                  // ended...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionComplete( any( AcquisitionResult.class ) );
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( any( AcquisitionResultStatus.class ) );
    inOrder.verifyNoMoreInteractions();
  }

  /**
   * Tests that an ongoing acquisition can be cancelled and that this will cause
   * the correct series of events to be send to all interested listeners.
   */
  @Test
  public void testCancelOngoingAcquisitionOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 200 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.cancelAcquisition();

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    assertEquals( ResultStatus.ABORTED, captor.getValue().getStatus() );
    assertNotNull( captor.getValue().getMessage() );
  }

  /**
   * Tests that an ongoing (infinite(!) blocking) acquisition can be cancelled
   * and that this will cause the correct series of events to be send to all
   * interested listeners.
   */
  @Test
  public void testCancelOngoingBlockingAcquisitionFail() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( INFINITE_BLOCKING_CALL );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.cancelAcquisition();

    sleep( 500 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verifyNoMoreInteractions();
  }

  /**
   * Tests that an ongoing (blocking) acquisition can be cancelled and that this
   * will cause the correct series of events to be send to all interested
   * listeners.
   */
  @Test
  public void testCancelOngoingBlockingAcquisitionOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( INTERRUPTABLE_BLOCKING_CALL );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.cancelAcquisition();

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    assertEquals( ResultStatus.ABORTED, captor.getValue().getStatus() );
    assertNotNull( captor.getValue().getMessage() );
  }

  /**
   * Tests that an ongoing acquisition can be cancelled and that this will cause
   * the correct series of events to be send to all interested listeners.
   */
  @Test( expected = IllegalStateException.class )
  public void testCancelWithoutOngoingAcquisitionFail() throws Exception
  {
    this.service.cancelAcquisition();
  }

  /**
   * Tests that if we're closing the service while an blocking acquisition is
   * going on, it will be canceled.
   */
  @Test
  public void testCloseCancelsOngoingBlockingAcquisitionFail() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( INFINITE_BLOCKING_CALL );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );

    // Check our preconditions: we don't expect it to be terminated...
    assertFalse( this.service.executorService.isTerminated() );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.close();

    sleep( 500 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verifyNoMoreInteractions();

    // Make sure the executor service itself is shutdown properly!
    assertTrue( this.service.executorService.isShutdown() );
    assertFalse( this.service.executorService.isTerminated() );
  }

  /**
   * Tests that if we're closing the service while an blocking acquisition is
   * going on, it will be canceled.
   */
  @Test
  public void testCloseCancelsOngoingBlockingAcquisitionOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( INTERRUPTABLE_BLOCKING_CALL );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    // Check our preconditions: we don't expect it to be terminated...
    assertFalse( this.service.executorService.isTerminated() );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.close();

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    // Make sure the executor service itself is shutdown properly!
    assertTrue( this.service.executorService.isShutdown() );
    assertTrue( this.service.executorService.isTerminated() );
  }

  /**
   * Tests that if we're closing the service while an interruptable acquisition
   * is going on, it will be canceled.
   */
  @Test
  public void testCloseCancelsOngoingInterruptableAcquisitionOk() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 200 );

    // Test whether the callback methods are called in the correct order...
    InOrder inOrder = inOrder( this.mockAcquisitionListenerHelper );
    ArgumentCaptor<AcquisitionResultStatus> captor = ArgumentCaptor.forClass( AcquisitionResultStatus.class );

    // Check our preconditions: we don't expect it to be terminated...
    assertFalse( this.service.executorService.isTerminated() );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.close();

    sleep( 10 ); // wait a little while in order to get the method started...

    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionStarted();
    inOrder.verify( this.mockAcquisitionListenerHelper ).acquisitionEnded( captor.capture() );
    inOrder.verifyNoMoreInteractions();

    assertEquals( ResultStatus.ABORTED, captor.getValue().getStatus() );
    assertNotNull( captor.getValue().getMessage() );

    // Make sure the executor service itself is shutdown properly!
    assertTrue( this.service.executorService.isShutdown() );
    assertTrue( this.service.executorService.isTerminated() );
  }

  /**
   * Tests that when the acquisition is still ongoing that we cannot submit a
   * new acquisition.
   */
  @Test( expected = IllegalStateException.class )
  public void testDoubleAcquireDataFail() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 100 );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    this.service.acquireData( deviceController ); // should fail!
  }

  /**
   * Tests that while an acquisition is going on, the status of this can be
   * queried on the service.
   */
  @Test
  public void testIsAcquiring() throws Exception
  {
    final DeviceController deviceController = createMockDeviceController( 100 );

    this.service.acquireData( deviceController );

    sleep( 10 ); // wait a little while in order to get the method started...

    assertTrue( this.service.isAcquiring() );

    sleep( 100 ); // wait a little while in order to get the method fully
                  // ended...

    assertFalse( this.service.isAcquiring() );
  }

  /**
   * @throws Exception
   * @throws IOException
   */
  private DeviceController createMockDeviceController( final int aTimeout ) throws Exception
  {
    final Device mockDevice = mock( Device.class );
    final AcquisitionResult mockResult = mock( AcquisitionResult.class );

    when( mockDevice.call() ).thenAnswer( new Answer<AcquisitionResult>()
    {
      @Override
      public AcquisitionResult answer( final InvocationOnMock aInvocation ) throws Throwable
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
        return mockResult;
      }
    } );

    final DeviceController mockDeviceController = mock( DeviceController.class );
    when( mockDeviceController.createDevice( any( AcquisitionProgressListener.class ) ) ).thenReturn( mockDevice );

    return mockDeviceController;
  }
}
