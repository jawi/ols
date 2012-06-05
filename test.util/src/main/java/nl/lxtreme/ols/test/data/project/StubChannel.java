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
package nl.lxtreme.ols.test.data.project;


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;


/**
 * 
 */
public class StubChannel implements Channel
{
  // VARIABLES

  private final int index;
  private final int mask;

  private String label;
  private boolean enabled;

  // CONSTRUCTORS

  /**
   * Creates a new StubChannel instance.
   * 
   * @param aIndex
   */
  public StubChannel( final int aIndex )
  {
    this.index = aIndex;
    this.mask = ( int )( 1L << aIndex );

    this.enabled = true;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addAnnotation( final Annotation<?> aAnnotation )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Channel aChannel )
  {
    return this.index - aChannel.getIndex();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<Annotation<?>> getAnnotations()
  {
    return Collections.emptyList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getIndex()
  {
    return this.index;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel()
  {
    return this.label;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getMask()
  {
    return this.mask;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasName()
  {
    return ( this.label != null ) && !this.label.trim().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled()
  {
    return this.enabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnabled( final boolean aEnabled )
  {
    this.enabled = aEnabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setLabel( final String aName )
  {
    this.label = aName;
  }
}
