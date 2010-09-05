/**
 * 
 */
package nl.lxtreme.ols.tool.measure;


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jajans
 */
public class ClockFrequencyMeasureWorker extends BaseAsyncToolWorker<ClockFrequencyMeasureWorker.ClockStats>
{
  // INNER TYPES

  public static class ClockStats
  {
    private final int rising;
    private final int falling;
    private final double period;
    private final double frequency;

    /**
     * 
     */
    ClockStats( final int aRising, final int aFalling, final long aSampleRate )
    {
      this.rising = aRising;
      this.falling = aFalling;

      this.period = ( this.rising + this.falling );
      this.frequency = aSampleRate / ( double )( this.rising + this.falling );
    }

    /**
     * @return
     */
    public double getDutyCycle()
    {
      if ( this.period == 0.0 )
      {
        return 0.0;
      }
      return this.rising / this.period;
    }

    /**
     * @return
     */
    public String getDutyCycleDisplayText()
    {
      final double dutycycle = getDutyCycle();
      if ( dutycycle == 0.0 )
      {
        return "";
      }
      return String.format( "%.1f%%", ( getDutyCycle() * 100.0 ) );
    }

    /**
     * @return the frequency
     */
    public double getFrequency()
    {
      return this.frequency;
    }

    /**
     * @return
     */
    public String getFrequencyDisplayText()
    {
      if ( this.period == 0.0 )
      {
        return "";
      }
      return DisplayUtils.displayFrequency( this.frequency );
    }
  }

  public enum Edge
  {
    RISING, FALLING;
  }

  // VARIABLES

  private final int channelMask;
  private final Map<Edge, Map<Integer, Integer>> stats;
  private final long startTime;
  private final long endTime;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureWorker instance.
   * 
   * @param aData
   *          the data to work upon;
   * @param aChannel
   *          the channel to measure the clock on;
   * @param aStartIndex
   *          the starting index (cursor A);
   * @param aEndIndex
   *          the ending index (cursor B).
   */
  public ClockFrequencyMeasureWorker( final DataContainer aData, final int aChannel, final int aStartIndex,
      final int aEndIndex )
  {
    super( aData, null ); // XXX

    this.channelMask = ( 1 << aChannel );
    this.startTime = aData.getCursorPosition( aStartIndex );
    this.endTime = aData.getCursorPosition( aEndIndex );
    this.stats = new HashMap<Edge, Map<Integer, Integer>>( 2 );
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected ClockStats doInBackground() throws Exception
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    int start = 0;
    while ( ( start < timestamps.length ) && ( timestamps[start] < this.startTime ) )
    {
      start++;
    }
    int end = start;
    while ( ( end < timestamps.length ) && ( timestamps[end] < this.endTime ) )
    {
      end++;
    }

    int i = Math.max( 0, start - 1 );
    long lastTransition = 0;
    int lastBitValue = values[i] & this.channelMask;

    for ( ; i < end; i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        final Edge edge;
        if ( lastBitValue > bitValue )
        {
          // rising edge
          edge = Edge.RISING;
        }
        else
        {
          // falling edge
          edge = Edge.FALLING;
        }

        Map<Integer, Integer> edgeStats = this.stats.get( edge );
        if ( edgeStats == null )
        {
          edgeStats = new HashMap<Integer, Integer>();
          this.stats.put( edge, edgeStats );
        }

        final Integer length = Integer.valueOf( ( int )( timestamps[i] - lastTransition ) );

        Integer count = edgeStats.get( length );
        if ( count == null )
        {
          count = Integer.valueOf( 1 );
        }
        else
        {
          count = Integer.valueOf( count.intValue() + 1 );
        }
        edgeStats.put( length, count );

        lastTransition = timestamps[i];
      }

      lastBitValue = bitValue;
    }

    final int bestRising = getBest( Edge.RISING );
    final int bestFalling = getBest( Edge.FALLING );

    return new ClockStats( bestRising, bestFalling, getSampleRate() );
  }

  /**
   * @return
   */
  private int getBest( final Edge aEdge )
  {
    final Map<Integer, Integer> edgeStats = this.stats.get( aEdge );
    if ( edgeStats == null )
    {
      return 0;
    }

    Integer rank = 0;
    int result = 0;

    for ( Map.Entry<Integer, Integer> entry : edgeStats.entrySet() )
    {
      if ( entry.getValue() > rank )
      {
        rank = entry.getValue();
        result = entry.getKey();
      }
    }

    return result;
  }
}
