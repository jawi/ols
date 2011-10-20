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

import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.data.project.*;

import org.junit.*;
import org.osgi.framework.*;
import org.osgi.service.prefs.*;


/**
 * @author jawi
 */
public class PreferenceServiceTrackerTest
{
  // VARIABLES

  private PreferenceServiceTracker preferenceServiceTracker;

  private BundleContext bundleContext;
  private ProjectManager projectManager;

  private ServiceReference mockedServiceRef;
  private PreferencesService mockedPreferenceService;

  // METHODS

  /**
   * Sets up the test cases.
   */
  @Before
  public void setUp() throws Exception
  {
    this.bundleContext = mock( BundleContext.class );
    this.mockedServiceRef = mock( ServiceReference.class );
    this.mockedPreferenceService = mock( PreferencesService.class );

    ClientProperties mockProperties = mock( ClientProperties.class );
    this.projectManager = new SimpleProjectManager( mockProperties );

    this.preferenceServiceTracker = new PreferenceServiceTracker( this.bundleContext, this.projectManager );

    when( this.bundleContext.getService( this.mockedServiceRef ) ).thenReturn( this.mockedPreferenceService );
  }

  /**
   * @throws Exception
   */
  @After
  public void tearDown() throws Exception
  {
    if ( this.preferenceServiceTracker != null )
    {
      this.preferenceServiceTracker.removedService( this.mockedServiceRef, this.mockedPreferenceService );
    }
  }

  /**
   * Tests that adding a preference service instance also installs an AWT window
   * listener.
   */
  @Test
  public void testAddingServiceInstallsWindowListenerOk() throws Exception
  {
    final AWTEventListener[] awtEventListenersBefore = Toolkit.getDefaultToolkit().getAWTEventListeners();

    final Object addedService = this.preferenceServiceTracker.addingService( this.mockedServiceRef );
    assertNotNull( addedService );

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

    final Object addedService = this.preferenceServiceTracker.addingService( this.mockedServiceRef );
    assertNotNull( addedService );

    final AWTEventListener[] awtEventListenersAfterAdd = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length + 1, awtEventListenersAfterAdd.length );

    this.preferenceServiceTracker.removedService( this.mockedServiceRef, this.mockedPreferenceService );

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

    this.preferenceServiceTracker.removedService( this.mockedServiceRef, this.mockedPreferenceService );

    final AWTEventListener[] awtEventListenersAfterRemove = Toolkit.getDefaultToolkit().getAWTEventListeners();
    assertEquals( awtEventListenersBefore.length, awtEventListenersAfterRemove.length );
  }
}
