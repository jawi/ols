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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.osgi;


import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.data.project.*;

import org.junit.*;
import org.osgi.framework.*;
import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
public class WindowStateListenerTest
{
  // VARIABLES

  private ProjectManager projectManager;
  private AWTEventListener windowStateListener;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp() throws Exception
  {
    BundleContext bundleContext = mock( BundleContext.class );
    ServiceReference mockedServiceRef = mock( ServiceReference.class );
    PreferencesService mockedPreferenceService = mock( PreferencesService.class );

    ClientProperties mockProperties = mock( ClientProperties.class );
    this.projectManager = new SimpleProjectManager( mockProperties );

    PreferenceServiceTracker preferenceServiceTracker = new PreferenceServiceTracker( bundleContext,
        this.projectManager );

    when( bundleContext.getService( mockedServiceRef ) ).thenReturn( mockedPreferenceService );

    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    preferenceServiceTracker.addingService( mockedServiceRef );

    final List<AWTEventListener> temp = new ArrayList<AWTEventListener>();
    temp.addAll( Arrays.asList( Toolkit.getDefaultToolkit().getAWTEventListeners() ) );
    temp.removeAll( Arrays.asList( awtEventListenersBefore ) );

    assertEquals( 1, temp.size() );

    this.windowStateListener = spy( temp.get( 0 ) );
  }

  /**
   * @throws Exception
   */
  @After
  public void tearDown() throws Exception
  {
    if ( this.windowStateListener != null )
    {
      Toolkit.getDefaultToolkit().removeAWTEventListener( this.windowStateListener );
    }
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testMultipleWindowClosedEventCausesPreferencesToBeWrittenMultipleTimes()
  {
    final Window window = mock( Window.class, withSettings().extraInterfaces( Configurable.class ) );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_CLOSED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_CLOSED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_CLOSED ) );

    verify( ( ( Configurable )window ), times( 1 ) ).writePreferences( any( UserSettings.class ) );
    verify( ( ( Configurable )window ), times( 1 ) ).readPreferences( any( UserSettings.class ) );
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testMultipleWindowOpenedEventCausesPreferencesToBeReadOnlyOnce()
  {
    final Window window = mock( Window.class, withSettings().extraInterfaces( Configurable.class ) );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );

    verify( ( ( Configurable )window ), times( 1 ) ).readPreferences( any( UserSettings.class ) );
    verify( ( ( Configurable )window ), times( 0 ) ).writePreferences( any( UserSettings.class ) );
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testReopenClosedWindowCausesPreferencesToBeReadAndWrittenTwice()
  {
    final Window window = mock( Window.class, withSettings().extraInterfaces( Configurable.class ) );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_CLOSED ) );
    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );

    verify( ( ( Configurable )window ), times( 2 ) ).readPreferences( any( UserSettings.class ) );
    verify( ( ( Configurable )window ), times( 1 ) ).writePreferences( any( UserSettings.class ) );
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testWindowClosedEventCausesPreferencesToBeWrittenOk()
  {
    final Window window = mock( Window.class, withSettings().extraInterfaces( Configurable.class ) );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_OPENED ) );

    verify( ( ( Configurable )window ) ).readPreferences( any( UserSettings.class ) );

    // Ok; start over with a new event...
    reset( this.windowStateListener, window );

    this.windowStateListener.eventDispatched( new WindowEvent( window, WindowEvent.WINDOW_CLOSED ) );

    verify( ( ( Configurable )window ) ).writePreferences( any( UserSettings.class ) );
    // read should never be called!
    verify( ( ( Configurable )window ), times( 0 ) ).readPreferences( any( UserSettings.class ) );
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testWindowOpenedEventCausesPreferencesToBeReadOk()
  {
    final Window window = mock( Window.class, withSettings().extraInterfaces( Configurable.class ) );

    final WindowEvent event = new WindowEvent( window, WindowEvent.WINDOW_OPENED );
    this.windowStateListener.eventDispatched( event );

    verify( ( ( Configurable )window ) ).readPreferences( any( UserSettings.class ) );
    // write should never be called!
    verify( ( ( Configurable )window ), times( 0 ) ).writePreferences( any( UserSettings.class ) );
  }
}
