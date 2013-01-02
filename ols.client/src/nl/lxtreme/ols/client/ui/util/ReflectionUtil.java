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
package nl.lxtreme.ols.client.ui.util;


import java.lang.reflect.*;


/**
 * Provides some utility methods for using reflection.
 */
final class ReflectionUtil
{
  // METHODS

  public static void callMethod( final Object aInstance, final String aMethodName, final Object... aArguments )
  {
    Method[] methods = aInstance.getClass().getDeclaredMethods();
    for ( Method method : methods )
    {
      if ( aMethodName.equals( method.getName() ) )
      {
        try
        {
          method.setAccessible( true );
          method.invoke( aInstance, aArguments );
        }
        catch ( Exception exception )
        {
          // Ignore; continue to next match...
          exception.printStackTrace();
        }
      }
    }
  }

  /**
   * Creates a new instance for the given type using the default constructor.
   * 
   * @param aType
   *          the type to instantiate, cannot be <code>null</code>.
   * @return a new instance, or <code>null</code> if instantiation failed.
   */
  public static <TYPE> TYPE newInstance( final Class<TYPE> aType )
  {
    return newInstance( aType, new Class<?>[][] { {} }, new Object[][] { {} } );
  }

  /**
   * Creates a new instance for the given type with the given signature and
   * corresponding parameters.
   * 
   * @param aType
   *          the type to instantiate, cannot be <code>null</code>.
   * @param aSignature
   *          the constructor signature to use, cannot be <code>null</code>;
   * @param aParameters
   *          the constructor parameters to use, cannot be <code>null</code>.
   * @return a new instance, or <code>null</code> if instantiation failed.
   */
  public static <TYPE> TYPE newInstance( final Class<TYPE> aType, final Class<?>[] aSignature,
      final Object[] aParameters )
  {
    return newInstance( aType, new Class<?>[][] { aSignature }, new Object[][] { aParameters } );
  }

  /**
   * Creates a new instance for the given type with the given signatures and
   * corresponding parameters.
   * 
   * @param aType
   *          the type to instantiate, cannot be <code>null</code>.
   * @param aSignatures
   *          the constructor signatures to use, cannot be <code>null</code>;
   * @param aParameters
   *          the constructor parameters to use, cannot be <code>null</code>.
   * @return a new instance, or <code>null</code> if instantiation failed.
   */
  public static <TYPE> TYPE newInstance( final Class<TYPE> aType, final Class<?>[][] aSignatures,
      final Object[][] aParameters )
  {
    if ( aSignatures.length != aParameters.length )
    {
      throw new IllegalArgumentException( "Signatures and parameters do not match!" );
    }

    TYPE result = null;

    for ( int idx = 0; ( result == null ) && ( idx < aSignatures.length ); idx++ )
    {
      Class<?>[] signature = aSignatures[idx];

      try
      {
        Constructor<TYPE> constructor = aType.getDeclaredConstructor( signature );
        constructor.setAccessible( true );

        result = constructor.newInstance( aParameters[idx] );
      }
      catch ( Exception exception )
      {
        // Ignore; continue to next match...
        exception.printStackTrace();
      }
    }

    return result;
  }
}
