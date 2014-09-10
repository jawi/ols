/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (c) 2010-2014 The Amdatu Foundation
 */
package nl.lxtreme.ols.config.provision.impl;


import nl.lxtreme.ols.config.provision.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Provides the bundle activator.
 * <p>
 * This code is taken and modified from Amdatu-Configurator.
 * </p>
 */
public class Activator extends DependencyActivatorBase
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    // Nop
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    aManager.add( createComponent().setInterface( Configurator.class.getName(), null )
        .setImplementation( ConfiguratorImpl.class )
        .add( createServiceDependency().setService( ConfigurationAdmin.class ).setRequired( true ) )
        .add( createServiceDependency().setService( LogService.class ).setRequired( false ) ) );
  }
}
