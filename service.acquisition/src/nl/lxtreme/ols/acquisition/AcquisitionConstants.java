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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.acquisition;


import nl.lxtreme.ols.common.*;


/**
 * Constants used by {@link AcquisitionService}.
 */
public interface AcquisitionConstants
{
  // CONSTANTS

  /** Topic prefix used for all acquisition-related events. */
  String TOPIC_ACQUISITION_BASE = OlsConstants.TOPIC_DATA_BASE.concat( "/acquisition" );
  /** Topic where "acquisition started" events are posted. */
  String TOPIC_ACQUISITION_STARTED = TOPIC_ACQUISITION_BASE.concat( "/STARTED" );
  /** Topic where "acquisition complete" events are posted. */
  String TOPIC_ACQUISITION_FINISHED = TOPIC_ACQUISITION_BASE.concat( "/FINISHED" );
  /** Topic where "acquisition complete" events are posted. */
  String TOPIC_ACQUISITION_PROGRESS = TOPIC_ACQUISITION_BASE.concat( "/PROGRESS" );

  /** The name of the device causing the {@link #TOPIC_ACQUISITION_STARTED} event. */
  String TAS_DEVICE_NAME = "deviceName";

  /** The name of the device causing the {@link #TOPIC_ACQUISITION_PROGRESS} event. */
  String TAP_DEVICE_NAME = TAS_DEVICE_NAME;
  /** The progress percentage (0..100) in a {@link #TOPIC_ACQUISITION_PROGRESS} event. */
  String TAP_PROGRESS = "progress";
  
  /** The name of the device causing the {@link #TOPIC_ACQUISITION_FINISHED} event. */
  String TAC_DEVICE_NAME = TAP_DEVICE_NAME;
  /** The acquired data in a {@link #TOPIC_ACQUISITION_FINISHED} event. */
  String TAC_DATA = "data";
  /** The (optional) exception in a {@link #TOPIC_ACQUISITION_FINISHED} event. */
  String TAC_EXCEPTION = "exception";
  /** The (optional) execution time (in ms) in a {@link #TOPIC_ACQUISITION_FINISHED} event. */
  String TAC_EXECUTION_TIME = "executionTime";

}
