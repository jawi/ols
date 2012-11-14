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
package nl.lxtreme.ols.tool.api;


/**
 * Provides a custom exception class for tools.
 */
public class ToolException extends RuntimeException
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolException} instance.
   */
  public ToolException()
  {
    super();
  }

  /**
   * Creates a new {@link ToolException} instance.
   * 
   * @param aMessage
   *          the message describing the possible cause.
   */
  public ToolException( final String aMessage )
  {
    super( aMessage );
  }

  /**
   * Creates a new {@link ToolException} instance.
   * 
   * @param aMessage
   *          the message additionally describing the cause;
   * @param aCause
   *          the originating cause of this exception.
   */
  public ToolException( final String aMessage, final Throwable aCause )
  {
    super( aMessage, aCause );
  }

  /**
   * Creates a new {@link ToolException} instance.
   * 
   * @param aCause
   *          the originating cause of this exception.
   */
  public ToolException( final Throwable aCause )
  {
    super( aCause );
  }
}
