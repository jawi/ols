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
package nl.lxtreme.ols.tool.linedecoder.impl;


import java.awt.*;
import java.util.*;
import java.util.List;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.linedecoder.*;
import nl.lxtreme.ols.tool.linedecoder.impl.decoders.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.framework.*;


/**
 * Provides a line decoder tool, that is able to decode several line encodings,
 * such as Manchester encoding.
 */
public class LineDecoderTool implements Tool<AcquisitionResult>
{
  // VARIABLES

  private final List<LineDecoder> lineDecoders;
  // Injected by DependencyManager...
  private volatile BundleContext context;
  private volatile DependencyManager dependencyManager;

  // CONSTRUCTORS

  /**
   * Creates a new LineDecoderTool instance.
   */
  public LineDecoderTool()
  {
    this.lineDecoders = new ArrayList<LineDecoder>();

    this.lineDecoders.add( new ManchesterLineDecoder() );
    this.lineDecoders.add( new NonReturnToZeroDecoder() );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public LineDecoderTask createToolTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    return new LineDecoderTask( aContext, aProgressListener, aAnnotationListener );
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
   * @return all current line decoders as array, never <code>null</code>.
   */
  public final LineDecoder[] getLineDecoders()
  {
    LineDecoder[] decoders;
    synchronized ( this.lineDecoders )
    {
      decoders = this.lineDecoders.toArray( new LineDecoder[this.lineDecoders.size()] );
    }
    return decoders;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Line decoder ...";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke( final Window aParent, final ToolContext aContext )
  {
    new LineDecoderToolDialog( aParent, aContext, this.context, this ).showDialog();
  }

  /**
   * Called by the dependency manager when a new line decoder is registered.
   * 
   * @param aLineDecoder
   *          the line decoder to register, cannot be <code>null</code>.
   */
  protected void addLineDecoder( final LineDecoder aLineDecoder )
  {
    synchronized ( this.lineDecoders )
    {
      if ( !this.lineDecoders.contains( aLineDecoder ) )
      {
        this.lineDecoders.add( aLineDecoder );
      }
    }
  }

  /**
   * Called when this tool is destroyed by the client framework.
   */
  protected void destroy( final Component aComponent )
  {
    // No-op
  }

  /**
   * Called when this tool is initialized by the client framework.
   * 
   * @param aComponent
   *          the DM component to use, never <code>null</code>.
   */
  protected void init( final Component aComponent )
  {
    aComponent.add( this.dependencyManager.createServiceDependency() //
        .setService( LineDecoder.class ) //
        .setCallbacks( "addLineDecoder", "removeLineDecoder" ) //
        .setRequired( false ) //
        );
  }

  /**
   * Called by the dependency manager when a new linedecoder is unregistered.
   * 
   * @param aLineDecoder
   *          the line decoder to unregister, cannot be <code>null</code>.
   */
  protected void removeLineDecoder( final LineDecoder aLineDecoder )
  {
    synchronized ( this.lineDecoders )
    {
      this.lineDecoders.remove( aLineDecoder );
    }
  }
}
