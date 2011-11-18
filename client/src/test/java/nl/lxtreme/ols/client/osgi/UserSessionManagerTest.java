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
import static org.mockito.Mockito.*;

import java.awt.*;
import java.awt.event.*;

import nl.lxtreme.ols.client.data.project.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
public class UserSessionManagerTest
{
  // VARIABLES

  private UserSessionManager userSessionManager;

  private SimpleProjectManager projectManager;
  private PreferencesService mockedPreferenceService;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp() throws Exception
  {
    this.mockedPreferenceService = mock( PreferencesService.class );

    HostProperties mockProperties = mock( HostProperties.class );

    this.projectManager = new SimpleProjectManager();
    this.projectManager.setHostProperties( mockProperties );

    this.userSessionManager = new UserSessionManager();
    this.userSessionManager.setProjectManager( this.projectManager );
    this.userSessionManager.setPreferenceService( this.mockedPreferenceService );
  }

  /**
   * Tears down the test cases.
   */
  @After
  public void tearDown()
  {
    this.userSessionManager.stop();
  }

  /**
   * Tests that adding a preference service instance also installs an AWT window
   * listener.
   */
  @Test
  public void testAddingMultipleServicesInstallsSingleWindowListenerOk() throws Exception
  {
    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    this.userSessionManager.start();
    this.userSessionManager.start();
    this.userSessionManager.start();

    final AWTEventListener[] awtEventListenersAfter = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length + 1, awtEventListenersAfter.length );
  }

  /**
   * Tests that adding a preference service instance also installs an AWT window
   * listener.
   */
  @Test
  public void testAddingServiceInstallsWindowListenerOk() throws Exception
  {
    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    this.userSessionManager.start();

    final AWTEventListener[] awtEventListenersAfter = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length + 1, awtEventListenersAfter.length );
  }

  /**
   * Tests that after removing a previously added preference service instance
   * also removes the installed AWT window listener.
   */
  @Test
  public void testRemovingAddedServiceRemovesWindowListenerOk() throws Exception
  {
    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    this.userSessionManager.start();

    final AWTEventListener[] awtEventListenersAfterAdd = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length + 1, awtEventListenersAfterAdd.length );

    this.userSessionManager.stop();

    final AWTEventListener[] awtEventListenersAfterRemove = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length, awtEventListenersAfterRemove.length );
  }

  /**
   * Tests that adding a preference service instance also installs an AWT window
   * listener.
   */
  @Test
  public void testRemovingNotInstalledServiceLeavesWindowListenersAloneOk() throws Exception
  {
    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    this.userSessionManager.stop();

    final AWTEventListener[] awtEventListenersAfterRemove = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length, awtEventListenersAfterRemove.length );
  }
}
