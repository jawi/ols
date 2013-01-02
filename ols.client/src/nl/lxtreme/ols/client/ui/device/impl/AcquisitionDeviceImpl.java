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
import java.io.*;

import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.client.ui.util.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.metatype.*;


/**
 * Provides an implementation of {@link AcquisitionDevice}.
 */
public class AcquisitionDeviceImpl extends DelegateServiceWrapper<Device> implements AcquisitionDevice
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionDeviceImpl} instance.
   */
  public AcquisitionDeviceImpl( final Device aDevice )
  {
    super( aDevice );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData acquireData( final DeviceProgressListener aProgressListener ) throws IOException,
      InterruptedException
  {
    return getDelegate().acquireData( getConfiguration(), aProgressListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    getDelegate().cancelAcquisition();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return getDelegate().getName();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSetup()
  {
    return !getConfiguration().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ConfigurationEditor createEditor( final Window aParent, final ObjectClassDefinition aOCD )
  {
    return DeviceConfigurationEditor.create( aParent, aOCD, getConfiguration().asMap() );
  }
}
