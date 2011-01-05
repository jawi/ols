/**
 * 
 */
package org.sump.device.logicsniffer;


/**
 * @author jawi
 */
public final class LogicSnifferConfig
{
  // INNER TYPES

  /**
   * Denotes the source where the sample clock comes from.
   */
  static enum ClockSource
  {
    INTERNAL, EXTERNAL_RISING, EXTERNAL_FALLING;
  }

  // CONSTANTS

  final static int CLOCK = 100000000; // device clock in Hz
  // number of trigger stages
  final static int TRIGGER_STAGES = 4;

  // mask for delay value
  private final static int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private final static int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private final static int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private final static int TRIGGER_SERIAL = 0x04000000;
  // trigger will start capture when fired
  final static int TRIGGER_CAPTURE = 0x08000000;

  private ClockSource clockSource;
  private boolean demux;
  private boolean filterEnabled;
  private boolean triggerEnabled;
  private boolean rleEnabled;
  private boolean altNumberSchemeEnabled;
  private boolean testModeEnabled;
  private final int triggerMask[];
  private final int triggerValue[];
  private final int triggerConfig[];
  private int enabledChannels;
  private final boolean enabledGroups[];
  private int divider;
  private int size;
  private double ratio;

  private String portName;
  private int baudrate;

  /**
   * 
   */
  public LogicSnifferConfig()
  {
    this.triggerMask = new int[4];
    this.triggerValue = new int[4];
    this.triggerConfig = new int[4];
    for ( int i = 0; i < TRIGGER_STAGES; i++ )
    {
      this.triggerMask[i] = 0;
      this.triggerValue[i] = 0;
      this.triggerConfig[i] = 0;
    }
    this.triggerEnabled = false;
    this.filterEnabled = false;
    this.demux = false;
    setClockSource( ClockSource.INTERNAL );
    this.divider = 0;
    this.ratio = 0.5;
    this.size = 512;
    this.enabledGroups = new boolean[4];
    setEnabledChannels( -1 ); // enable all channels
  }

  // METHODS

  /**
   * Returns the number of available channels in current configuration.
   * 
   * @return number of available channels
   */
  public int getAvailableChannelCount()
  {
    if ( this.demux && isInternalClock() )
    {
      return 16;
    }
    else
    {
      return 32;
    }
  }

  /**
   * @return the baudrate
   */
  public int getBaudrate()
  {
    return this.baudrate;
  }

  /**
   * Returns the current clock source.
   * 
   * @return the clock source currently used as defined by the CLOCK_ properties
   */
  public ClockSource getClockSource()
  {
    return this.clockSource;
  }

  /**
   * @return the divider
   */
  public int getDivider()
  {
    return this.divider;
  }

  /**
   * Returns the currently enabled channels.
   * 
   * @return bitmask with enabled channels represented as 1
   */
  public int getEnabledChannels()
  {
    return ( this.enabledChannels );
  }

  /**
   * Get the maximum sampling rate available.
   * 
   * @return maximum sampling rate
   */
  public int getMaximumRate()
  {
    return ( 2 * CLOCK );
  }

  /**
   * @return the portName
   */
  public String getPortName()
  {
    return this.portName;
  }

  /**
   * @return the ratio
   */
  public double getRatio()
  {
    return this.ratio;
  }

  /**
   * @return the size
   */
  public int getSize()
  {
    return this.size;
  }

  /**
   * @return the triggerConfig
   */
  public int getTriggerConfig( final int aStage )
  {
    return this.triggerConfig[aStage];
  }

  /**
   * Returns the current trigger mask.
   * 
   * @param stage
   *          trigger stage to read mask from
   * @return current trigger mask
   */
  public int getTriggerMask( final int stage )
  {
    return this.triggerMask[stage];
  }

  /**
   * Returns the number of available trigger stages.
   * 
   * @return number of available trigger stages
   */
  public int getTriggerStageCount()
  {
    return TRIGGER_STAGES;
  }

  /**
   * Returns the current trigger value.
   * 
   * @param stage
   *          trigger stage to read value from
   * @return current trigger value
   */
  public int getTriggerValue( final int stage )
  {
    return this.triggerValue[stage];
  }

  /**
   * Returns the current number scheme mask.
   * 
   * @return current number scheme mask
   */
  public boolean isAltNumberSchemeEnabled()
  {
    return ( this.altNumberSchemeEnabled );
  }

  /**
   * @return the demux
   */
  public boolean isDemuxEnabled()
  {
    return this.demux;
  }

  /**
   * @return
   */
  public boolean isExternalClock()
  {
    return ( this.clockSource == ClockSource.EXTERNAL_FALLING ) || ( this.clockSource == ClockSource.EXTERNAL_RISING );
  }

  /**
   * Returns wether or not the noise filter can be used in the current
   * configuration.
   * 
   * @return <code>true</code> when noise filter is available,
   *         <code>false</code> otherwise
   */
  public boolean isFilterAvailable()
  {
    return ( !this.demux && isInternalClock() );
  }

  /**
   * Returns wether or not the noise filter is enabled.
   * 
   * @return <code>true</code> when noise filter is enabled, <code>false</code>
   *         otherwise
   */
  public boolean isFilterEnabled()
  {
    return ( this.filterEnabled );
  }

  public boolean isGroupEnabled( final int aGroupNr )
  {
    return this.enabledGroups[aGroupNr];
  }

  /**
   * @return
   */
  public boolean isInternalClock()
  {
    return this.clockSource == ClockSource.INTERNAL;
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled,
   *         <code>false</code> otherwise
   */
  public boolean isRleEnabled()
  {
    return ( this.rleEnabled );
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled,
   *         <code>false</code> otherwise
   */
  public boolean isTestModeEnabled()
  {
    return ( this.testModeEnabled );
  }

  /**
   * Returns wether or not the trigger is enabled.
   * 
   * @return <code>true</code> when trigger is enabled, <code>false</code>
   *         otherwise
   */
  public boolean isTriggerEnabled()
  {
    return ( this.triggerEnabled );
  }

  /**
   * Sets the Number Scheme Mask
   * 
   * @param mask
   *          bit map defining number scheme.
   */
  public void setAltNumberSchemeEnabled( final boolean enable )
  {
    this.altNumberSchemeEnabled = enable;
  }

  /**
   * @param aBaudrate
   *          the baudrate to set
   */
  public void setBaudrate( final int aBaudrate )
  {
    this.baudrate = aBaudrate;
  }

  /**
   * Sets the clock source to use.
   * 
   * @param aSource
   *          can be any CLOCK_ property of this class
   */
  public void setClockSource( final ClockSource aSource )
  {
    this.clockSource = aSource;
  }

  /**
   * Set enabled channels.
   * 
   * @param mask
   *          bit map defining enabled channels
   */
  public void setEnabledChannels( final int mask )
  {
    this.enabledChannels = mask;
    // determine enabled groups
    for ( int i = 0; i < 4; i++ )
    {
      this.enabledGroups[i] = ( ( this.enabledChannels >> ( 8 * i ) ) & 0xff ) > 0;
    }
  }

  /**
   * Sets wheter or not to enable the noise filter.
   * 
   * @param enable
   *          <code>true</code> enables the noise filter, <code>false</code>
   *          disables it.
   */
  public void setFilterEnabled( final boolean enable )
  {
    this.filterEnabled = enable;
  }

  /**
   * Configures the given trigger stage in parallel mode. Currenty the trigger
   * has four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one
   * channel. The LSB represents channel 0, the MSB channel 31.
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the
   * trigger level is 0.
   * 
   * @param stage
   *          trigger stage to write mask und value to
   * @param mask
   *          bit map defining which channels to watch
   * @param value
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 = immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only triggel level will increase
   */
  public void setParallelTrigger( final int stage, final int mask, final int value, final int level, final int delay,
      final boolean startCapture )
  {
    if ( !this.demux )
    { // TODO: demux modification should be done on the fly in
      // run() and not with stored properties
      this.triggerMask[stage] = mask;
      this.triggerValue[stage] = value;
    }
    else
    {
      this.triggerMask[stage] = mask & 0xffff;
      this.triggerValue[stage] = value & 0xffff;
      this.triggerMask[stage] |= this.triggerMask[stage] << 16;
      this.triggerValue[stage] |= this.triggerValue[stage] << 16;
    }
    this.triggerConfig[stage] = 0;
    this.triggerConfig[stage] |= delay & TRIGGER_DELAYMASK;
    this.triggerConfig[stage] |= ( level << 16 ) & TRIGGER_LEVELMASK;
    if ( startCapture )
    {
      this.triggerConfig[stage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * @param aPortName
   *          the portName to set
   */
  public void setPortName( final String aPortName )
  {
    this.portName = aPortName;
  }

  /**
   * Set the sampling rate. All rates must be a divisor of 200.000.000. Other
   * rates will be adjusted to a matching divisor.
   * 
   * @param aRate
   *          sampling rate in Hz
   */
  public void setRate( final int aRate )
  {
    if ( aRate > CLOCK )
    {
      this.demux = true;
      this.divider = ( 2 * CLOCK / aRate ) - 1;
    }
    else
    {
      this.demux = false;
      this.divider = ( CLOCK / aRate ) - 1;
    }
  }

  /**
   * Sets the ratio for samples to read before and after started.
   * 
   * @param ratio
   *          value between 0 and 1; 0 means all before start, 1 all after
   */
  public void setRatio( final double ratio )
  {
    this.ratio = ratio;
  }

  /**
   * Sets wheter or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> enables the RLE, <code>false</code> disables it.
   */
  public void setRleEnabled( final boolean enable )
  {
    this.rleEnabled = enable;
  }

  /**
   * Configures the given trigger stage in serial mode. Currenty the trigger has
   * four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one sample.
   * The LSB represents the oldest sample not yet shifted out, the MSB the most
   * recent. (The trigger compares to a 32bit shift register that is shifted by
   * one for each sample.)
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the
   * trigger level is 0.
   * 
   * @param stage
   *          trigger stage to write mask und value to
   * @param channel
   *          channel to attach trigger to
   * @param mask
   *          bit map defining which channels to watch
   * @param value
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 = immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only triggel level will increase
   */
  public void setSerialTrigger( final int stage, final int channel, final int mask, final int value, final int level,
      final int delay, final boolean startCapture )
  {
    if ( !this.demux )
    { // TODO: demux modification should be done on the fly in
      // run() and not with stored properties
      this.triggerMask[stage] = mask;
      this.triggerValue[stage] = value;
    }
    else
    {
      this.triggerMask[stage] = mask & 0xffff;
      this.triggerValue[stage] = value & 0xffff;
      this.triggerMask[stage] |= this.triggerMask[stage] << 16;
      this.triggerValue[stage] |= this.triggerValue[stage] << 16;
    }
    this.triggerConfig[stage] = 0;
    this.triggerConfig[stage] |= delay & TRIGGER_DELAYMASK;
    this.triggerConfig[stage] |= ( level << 16 ) & TRIGGER_LEVELMASK;
    this.triggerConfig[stage] |= ( channel << 20 ) & TRIGGER_CHANNELMASK;
    this.triggerConfig[stage] |= TRIGGER_SERIAL;
    if ( startCapture )
    {
      this.triggerConfig[stage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * Sets the number of samples to obtain when started.
   * 
   * @param size
   *          number of samples, must be between 4 and 256*1024
   */
  public void setSize( final int size )
  {
    this.size = size;
  }

  /**
   * Sets wheter or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> enables the RLE, <code>false</code> disables it.
   */
  public void setTestModeEnabled( final boolean enable )
  {
    this.testModeEnabled = enable;
  }

  /**
   * Sets wheter or not to enable the trigger.
   * 
   * @param enable
   *          <code>true</code> enables the trigger, <code>false</code> disables
   *          it.
   */
  public void setTriggerEnabled( final boolean enable )
  {
    this.triggerEnabled = enable;
  }

}
