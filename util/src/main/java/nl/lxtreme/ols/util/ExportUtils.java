/**
 * 
 */
package nl.lxtreme.ols.util;


import java.io.*;
import java.util.logging.*;


/**
 * @author jawi
 */
public final class ExportUtils
{
  // INNER TYPES

  /**
   * @author jawi
   */
  public static class CsvExporter
  {
    // CONSTANTS

    private static final Logger LOG = Logger.getAnonymousLogger();

    // VARIABLES

    private final char delimiter;

    private BufferedWriter writer;
    private int headerCount = -1;

    // CONSTRUCTORS

    /**
     * @param aFile
     * @throws IOException
     */
    CsvExporter( final File aFile ) throws IOException
    {
      this( aFile, ',' );
    }

    /**
     * @param aFile
     * @param aDelimiter
     * @throws IOException
     */
    CsvExporter( final File aFile, final char aDelimiter ) throws IOException
    {
      this.writer = new BufferedWriter( new FileWriter( aFile ) );
      this.delimiter = aDelimiter;
    }

    // METHODS

    /**
     * Adds a new row with cell values to the CSV output.
     * 
     * @param aValues
     *          the cell values to write in this row.
     * @throws IOException
     *           in case of I/O problems.
     */
    public void addRow( final Object... aValues ) throws IOException
    {
      if ( aValues.length != this.headerCount )
      {
        LOG.warning( "Number of cells not equal to header count! Header count = " + this.headerCount
            + ", cell count = " + aValues.length );
      }

      // If set, use the header count as leading cell count...
      final int length = Math.max( Math.min( this.headerCount, aValues.length ), this.headerCount );

      for ( int i = 0; i < length; i++ )
      {
        final Object value = i < aValues.length ? aValues[i] : null;

        this.writer.append( quote( value ) );

        if ( i < length - 1 )
        {
          this.writer.append( this.delimiter );
        }
      }
      this.writer.newLine();
    }

    /**
     * Closes this exporter.
     * 
     * @throws IOException
     *           in case of I/O problems.
     */
    public void close() throws IOException
    {
      try
      {
        this.writer.flush();
      }
      finally
      {
        this.writer.close();
        this.writer = null;
      }
    }

    /**
     * Sets the headers of the CSV file.
     * 
     * @param aHeaders
     *          the array of header names.
     * @throws IOException
     *           in case of I/O problems.
     */
    public void setHeaders( final String... aHeaders ) throws IOException
    {
      this.headerCount = aHeaders.length;
      for ( int i = 0; i < aHeaders.length; i++ )
      {
        this.writer.append( quote( aHeaders[i] ) );
        if ( i < aHeaders.length - 1 )
        {
          this.writer.append( this.delimiter );
        }
      }
      this.writer.newLine();
    }

    /**
     * @param aValue
     * @return
     */
    private String quote( final Object aValue )
    {
      final String value;
      if ( aValue == null )
      {
        value = "";
      }
      else
      {
        if ( aValue instanceof Character )
        {
          if ( Character.isLetterOrDigit( ( ( Character )aValue ).charValue() ) )
          {
            value = String.valueOf( aValue );
          }
          else
          {
            value = "";
          }
        }
        else
        {
          value = String.valueOf( aValue );
        }
      }
      return "\"" + value + "\"";
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new ExportUtils instance, never used.
   */
  private ExportUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates a CSV exporter for the given file.
   * 
   * @param aFile
   *          the file to export to, cannot be <code>null</code>.
   * @return a CSV exporter, never <code>null</code>.
   * @throws IOException
   *           in case of I/O errors.
   */
  public static CsvExporter createCsvExporter( final File aFile ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }
    return new CsvExporter( aFile );
  }
}
