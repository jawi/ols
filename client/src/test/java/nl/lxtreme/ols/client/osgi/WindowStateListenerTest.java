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


import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.awt.*;
import java.awt.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.osgi.UserSessionManager.WindowStateListener;

import org.junit.*;
import org.osgi.service.log.*;
import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
public class WindowStateListenerTest
{
  // VARIABLES

  private WindowStateListener windowStateListener;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp() throws Exception
  {
    PreferencesService mockedPreferenceService = mock( PreferencesService.class );
    
    Project mockedProject = mock( Project.class );
    when( mockedProject.getSettings( anyString() ) ).thenReturn( mock( UserSettings.class ) );
    ProjectManager mockedProjectManager = mock( ProjectManager.class );
    when( mockedProjectManager.getCurrentProject() ).thenReturn( mockedProject );

    this.windowStateListener = spy( new WindowStateListener() );
    this.windowStateListener.setPreferenceService( mockedPreferenceService );
    this.windowStateListener.setProjectManager( mockedProjectManager );
    this.windowStateListener.setLogger( mock( LogService.class ) );
  }

  /**
   * Test method for {@link WindowStateListener#eventDispatched(AWTEvent)}.
   */
  @Test
  public void testMultipleWindowClosedEventCausesPreferencesToBeWrittenOnlyOnce()
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
