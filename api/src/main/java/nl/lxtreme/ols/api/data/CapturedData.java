/**
 * 
 */
package nl.lxtreme.ols.api.data;


/**
 * @author jawi
 *
 */
public interface CapturedData
{

  /** indicates that rate or trigger position are not available */
  public final static int NOT_AVAILABLE = -1;

  /**
   * @return the absoluteLength
   */
  public abstract long getAbsoluteLength();

  /**
   * @return the channels
   */
  public abstract int getChannels();

  /**
   * return the data value at a specified absolute time offset
   * 
   * @param abs
   *          absolute time value
   * @return data value
   */
  public abstract int getDataAt( final long abs );

  /**
   * @return the enabledChannels
   */
  public abstract int getEnabledChannels();

  /**
   * calculate index number from absolute time
   * 
   * @param abs
   *          absolute time value
   * @return sample number before selected absolute time
   */
  public abstract int getSampleIndex( final long abs );

  /**
   * @return the rate
   */
  public abstract int getSampleRate();

  /**
   * @return the timestamps
   */
  public abstract long[] getTimestamps();

  /**
   * @return the triggerPosition
   */
  public abstract long getTriggerPosition();

  /**
   * @return the values
   */
  public abstract int[] getValues();

  /**
   * Returns wether or not the object contains timing data
   * 
   * @return <code>true</code> when timing data is available
   */
  public abstract boolean hasTimingData();

  /**
   * Returns wether or not the object contains trigger data
   * 
   * @return <code>true</code> when trigger data is available
   */
  public abstract boolean hasTriggerData();

}
