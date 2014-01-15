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

import nl.lxtreme.ols.tool.api.*;


/**
 * Provides a DMX512 analyzer tools.
 */
public class DMX512Analyzer implements Tool<DMX512DataSet>
{
  // CONSTANTS

  static final String NAME = "DMX512 analyser";

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolTask<DMX512DataSet> createToolTask( final ToolContext aContext,
      final ToolProgressListener aProgressListener )
  {
    return new DMX512AnalyzerTask( aContext, aProgressListener );
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
    return NAME.concat( " ..." );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke( Window aParent, ToolContext aContext )
  {
    new DMX512AnalyzerDialog( aParent, this, aContext ).showDialog();
  }
}
