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
package nl.lxtreme.ols.client.action;


import java.io.*;


/**
 * 
 */
class FileExtensionUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new FileExtensionUtils instance.
   */
  private FileExtensionUtils()
  {
    // Nop
  }

  // METHODS

  /**
   * Sets the filename to end with the given file extension, if this is not
   * already the case.
   * 
   * @param aFile
   *          the file that should get the given file extension, cannot be
   *          <code>null</code>;
   * @param aFileExtension
   *          the new file extension to add to the given file, cannot be
   *          <code>null</code>.
   * @return a file with the given file extension, never <code>null</code>.
   */
  public static final File setFileExtension( final File aFile, final String aFileExtension )
  {
    // Take care of any given periods in the extension...
    String extension = aFileExtension.trim();
    if ( extension.startsWith( "." ) )
    {
      extension = extension.substring( 1 );
    }

    // If the filename already has the given file extension, than simply do
    // nothing...
    if ( extension.isEmpty() )
    {
      return aFile;
    }

    File directory = aFile;
    String filename = "";
    boolean endsWithExtension = aFile.getName().toLowerCase().endsWith( extension.toLowerCase() );

    if ( !aFile.isDirectory() || endsWithExtension )
    {
      filename = aFile.getName();
      directory = aFile.getParentFile();
    }

    if ( !endsWithExtension )
    {
      filename = filename.concat( "." ).concat( aFileExtension );
    }

    return new File( directory, filename );
  }

  /**
   * Returns the "presumed" filename extension (like '.jpg', '.zip') from a
   * given file.
   * 
   * @param aFile
   *          the file to return the extension for, cannot be <code>null</code>.
   * @return the file extension (always in lower case), never <code>null</code>
   *         but can be empty if the given file has <em>no</em> file extension.
   */
  public static final String stripFileExtension( final File aFile, final String... aExtensions )
  {
    return stripFileExtension( aFile.getName(), aExtensions );
  }

  /**
   * Returns the "presumed" filename extension (like '.jpg', '.zip') from a
   * given file.
   * 
   * @param aFilename
   *          the name of the file to strip the extension from, cannot be
   *          <code>null</code>;
   * @param aExtensions
   *          the (optional) extensions that should be stripped. If omitted,
   *          everything behind the last dot will be removed.
   * @return the file extension (always in lower case), never <code>null</code>
   *         but can be empty if the given file has <em>no</em> file extension.
   */
  public static final String stripFileExtension( final String aFilename, final String... aExtensions )
  {
    String result = "";

    int idx = aFilename.lastIndexOf( '.' );
    if ( ( idx >= 0 ) && ( idx < ( aFilename.length() - 1 ) ) )
    {
      result = aFilename.substring( 0, idx );

      boolean found = ( aExtensions == null ) || ( aExtensions.length == 0 );

      final String ext = aFilename.substring( idx + 1 ).toLowerCase();
      if ( ( aExtensions != null ) && ( aExtensions.length > 0 ) )
      {
        for ( String extension : aExtensions )
        {
          if ( ext.equalsIgnoreCase( extension ) )
          {
            found = true;
            break;
          }
        }
      }

      if ( !found )
      {
        result = aFilename;
      }
    }

    return result;
  }

}
