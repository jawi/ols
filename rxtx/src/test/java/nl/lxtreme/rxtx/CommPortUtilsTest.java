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
package nl.lxtreme.rxtx;


import static org.junit.Assert.*;
import static org.junit.Assume.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.junit.internal.*;


/**
 * Tests for {@link CommPortUtils}.
 */
public class CommPortUtilsTest
{
  // METHODS

  /**
   * Tests {@link CommPortUtils#enumerateDevices(String)} on Linux platforms.
   */
  @Test
  public void testEnumerateDevicesLinux()
  {
    assumeTrue( HostUtils.isUnix() );

    String enumeratedDevices = CommPortUtils.enumerateDevices();
    assertNotNull( enumeratedDevices );
  }

  /**
   * Tests {@link CommPortUtils#enumerateDevices(String)} on Mac OS platforms.
   */
  @Test
  public void testEnumerateDevicesMacOS()
  {
    assumeTrue( HostUtils.isMacOS() );

    String enumeratedDevices = CommPortUtils.enumerateDevices();
    assertNotNull( enumeratedDevices );
  }

  /**
   * Tests {@link CommPortUtils#enumerateDevices(String)} on Windows platforms.
   */
  @Test
  public void testEnumerateDevicesWindows() throws AssumptionViolatedException
  {
    assumeTrue( HostUtils.isWindows() );

    try
    {
      CommPortUtils.enumerateDevices();
      fail();
    }
    catch ( UnsupportedOperationException exception )
    {
      assertTrue( true );
    }
  }
}
