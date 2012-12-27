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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.device.impl;


import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.client.ui.editor.*;
import org.osgi.service.metatype.*;


/**
 * Provides a generic editor for a device configuration.
 */
final class DeviceConfigurationEditor extends BaseConfigurationEditor
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DeviceConfigurationEditor} instance.
   */
  public DeviceConfigurationEditor( final Window aParent, final String aTitle )
  {
    super( aParent, aTitle, ModalityType.APPLICATION_MODAL );
  }

  // METHODS

  /**
   * Factor method for creating a proper {@link DeviceConfigurationEditor}
   * instance.
   * 
   * @param aParent
   *          the parent window to use;
   * @param aOCD
   *          the object-class definition to use;
   * @param aSettings
   *          the (initial) settings to use.
   * @return a new {@link DeviceConfigurationEditor} instance, never
   *         <code>null</code>.
   */
  public static DeviceConfigurationEditor create( final Window aParent, final ObjectClassDefinition aOCD,
      final Map<Object, Object> aSettings )
  {
    DeviceConfigurationEditor result = new DeviceConfigurationEditor( aParent, aOCD.getName() );
    result.initDialog( aOCD, aSettings );
    return result;
  }
}
