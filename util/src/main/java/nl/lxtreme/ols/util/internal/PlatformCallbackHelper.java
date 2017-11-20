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
package nl.lxtreme.ols.util.internal;


import nl.lxtreme.ols.util.*;


/**
 * Service facade for {@link ApplicationCallback}.
 */
public class PlatformCallbackHelper
{
  // METHODS

  /**
   * Called by Felix DM when a {@link PlatformCallback} is available.
   *
   * @param aCallback
   *          the platform callback to use, cannot be <code>null</code>.
   */
  public void setPlatformCallback( final PlatformCallback aCallback )
  {
    HostInfo hostInfo = HostUtils.getHostInfo();
    if ( hostInfo.isMacOS() )
    {
      if ( hostInfo.getJavaVersion() > 8 )
      {
        OSXPlatformHook.installPlatformCallback_Java9( aCallback );
      }
      else
      {
        OSXPlatformHook.installPlatformCallback_Java8( aCallback );
      }
    }
  }

  public void removePlatformCallback( final PlatformCallback aCallback )
  {
    // Nop
  }
}
