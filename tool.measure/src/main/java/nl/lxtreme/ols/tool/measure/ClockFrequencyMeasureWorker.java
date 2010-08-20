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
public class ClockFrequencyMeasureWorker extends BaseAsyncToolWorker<String>
{
  // VARIABLES

  private final int channelMask;
  /*
   * Store as linked list with 2 int sized arrays as elements. Each array
   * element stores at index 0 the bitlength and at index 1 the number of
   * occurrences.
   */
  private LinkedList<int[]> statData;

  // CONSTRUCTORS

  /**
   * @param aData
   * @param channel
   */
  public ClockFrequencyMeasureWorker( final AnnotatedData aData, final int channel )
  {
    super( aData );
    this.channelMask = ( 1 << channel );
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected String doInBackground() throws Exception
  {
    this.statData = new LinkedList<int[]>();

    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    int a, lastBitValue, c;
    int[] valuePair;
    long lastTransition = 0;
    lastBitValue = values[0] & this.channelMask;
    a = 0;

    for ( int i = 0; i < values.length; i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        a = ( int )( timestamps[i] - lastTransition );
        c = findValue( a );
        if ( c < 0 )
        {
          valuePair = new int[2];
          valuePair[0] = a; // bitlength
          valuePair[1] = 1; // count
          this.statData.add( valuePair );
        }
        else
        {
          this.statData.get( c )[1]++;
        }

        lastTransition = timestamps[i];
      }

      lastBitValue = values[i] & this.channelMask;
    }

    final double best = getBest();
    return DisplayUtils.displayFrequency( best / getSampleRate() );
  }

  /**
   * @param val
   * @return
   */
  private int findValue( final int val )
  {
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[0] == val )
      {
        return i;
      }
    }
    return -1;
  }

  /**
   * @return
   */
  private int getBest()
  {
    int rank = 0;
    int index = 0;
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[1] > rank )
      {
        rank = this.statData.get( i )[1];
        index = i;
      }
    }
    if ( this.statData.size() == 0 )
    {
      return 0;
    }
    return this.statData.get( index )[0];
  }
}
