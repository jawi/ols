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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.api;


import java.util.*;


/**
 * Represents the configuration of a {@link Device}.
 * <p>
 * Implementations of this interface are considered to represent a read-only
 * view of the configuration parameters used by a {@link Device} to acquire its
 * data.<br>
 * The data returned by implementations should be complete and syntactically
 * correct, meaning that a {@link Device} should be able to use them as-is
 * without having to parse or validate it prior to its use.
 * </p>
 */
public interface DeviceConfiguration
{
  // METHODS

  /**
   * Converts this configuration to a map representation of key-value pairs.
   * 
   * @return this configuration as serialized map.
   */
  Map<String, String> asMap();

}
