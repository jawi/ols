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
package nl.lxtreme.osgi.metatype.bnd;


import java.util.*;

import aQute.bnd.service.*;
import aQute.lib.osgi.*;


/**
 * Provides a custom plugin for metatype generation based upon annotations.
 */
public class MetaTypePlugin implements AnalyzerPlugin
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean analyzeJar( final Analyzer aAnalyzer ) throws Exception
  {
    Map<String, Map<String, String>> map = aAnalyzer.parseHeader( aAnalyzer.getProperty( Constants.METATYPE ) );

    Jar jar = aAnalyzer.getJar();
    for ( String name : map.keySet() )
    {
      // Collection<Clazz> metatypes = aAnalyzer.getClasses( "",
      // QUERY.ANNOTATION.toString(), Meta.OCD.class.getName(),
      // QUERY.NAMED.toString(), name );
      // for ( Clazz c : metatypes )
      // {
      // jar.putResource( "OSGI-INF/metatype/" + c.getFQN() + ".xml", new
      // MetaTypeReader( c, aAnalyzer ) );
      // }
    }
    return false;
  }
}
