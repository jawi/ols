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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.logicsniffer.ui;


import java.util.*;

import javax.swing.*;

import org.osgi.service.metatype.*;

import nl.lxtreme.ols.device.logicsniffer.profile.*;


/**
 * 
 */
public class TriggerSettingsPanel extends JPanel implements DeviceProfileChangedListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  /**
   * Creates a new {@link TriggerSettingsPanel} instance.
   * 
   * @param aOCD
   * @param aInitialValues
   */
  public TriggerSettingsPanel( final ObjectClassDefinition aOCD, final Map<Object, Object> aInitialValues )
  {
    // TODO Auto-generated constructor stub
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deviceProfileChanged( final DeviceProfile aProfile )
  {
    // TODO Auto-generated method stub

  }

}
