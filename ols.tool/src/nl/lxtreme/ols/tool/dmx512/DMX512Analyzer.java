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
package nl.lxtreme.ols.tool.dmx512;


import java.awt.*;

import org.osgi.framework.*;

import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a DMX512 analyzer tools.
 */
public class DMX512Analyzer implements Tool<DMX512DataSet>
{
  // VARIABLES

  // Injected by DependencyManager...
  private volatile BundleContext context;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolTask<DMX512DataSet> createToolTask( final ToolContext aContext,
      final ToolProgressListener aProgressListener, final AnnotationListener aAnnotationListener )
  {
    return new DMX512AnalyzerTask( aContext, aProgressListener, aAnnotationListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolCategory getCategory()
  {
    return ToolCategory.DECODER;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "DMX512 analyser ...";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke( final Window aParent, final ToolContext aContext )
  {
    new DMX512AnalyzerDialog( aParent, aContext, this.context, this ).showDialog();
  }
}
