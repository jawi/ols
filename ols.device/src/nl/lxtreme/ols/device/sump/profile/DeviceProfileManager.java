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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump.profile;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.device.sump.*;

import org.osgi.framework.*;
import org.osgi.service.cm.*;


/**
 * Provides a device profile manager,
 */
public class DeviceProfileManager implements ManagedServiceFactory
{
  // CONSTANTS

  /**
   * Order the candidates based on their match-logic: first the ones with
   * filters, followed by the ones without wildcards, and the ones with
   * wildcards lastly...
   */
  private final class ProfileFilterWildcardComparator implements Comparator<DeviceProfile>
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public int compare( DeviceProfile aDP1, DeviceProfile aDP2 )
    {
      String[] keys1 = aDP1.getDeviceMetadataKeys();
      String[] keys2 = aDP2.getDeviceMetadataKeys();

      boolean containsFilter1 = containsFilter( keys1 );
      boolean containsFilter2 = containsFilter( keys2 );

      if ( containsFilter1 )
      {
        if ( !containsFilter2 )
        {
          return -1;
        }
      }
      else
      {
        if ( containsFilter2 )
        {
          return 1;
        }
      }

      boolean containsWildcard1 = containsWildcard( keys1 );
      boolean containsWildcard2 = containsWildcard( keys2 );

      if ( !containsWildcard1 )
      {
        if ( containsWildcard2 )
        {
          return -1;
        }
      }
      else
      {
        if ( !containsWildcard2 )
        {
          return 1;
        }
      }

      // Ensure proper (fixed) order...
      return aDP1.getType().compareTo( aDP2.getType() );
    }

    private boolean containsFilter( String... aKeys )
    {
      for ( String key : aKeys )
      {
        if ( key.length() > 2 && key.startsWith( "(" ) && key.endsWith( ")" ) )
        {
          return true;
        }
      }
      return false;
    }

    private boolean containsWildcard( String... aKeys )
    {
      for ( String key : aKeys )
      {
        if ( "*".equals( key ) )
        {
          return true;
        }
      }
      return false;
    }
  }

  public static final String SERVICE_PID = "ols.profile";

  /** The default profile type to use. See {@link #getProfile(String)}. */
  private static final String DEFAULT_PROFILE_TYPE = "OLS";

  // VARIABLES

  private final ConcurrentMap<String, DeviceProfile> profiles;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfileManager instance.
   */
  public DeviceProfileManager()
  {
    this.profiles = new ConcurrentHashMap<String, DeviceProfile>();
  }

  // METHODS

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#deleted(java.lang.String)
   */
  @Override
  public void deleted( final String aPid )
  {
    this.profiles.remove( aPid );
  }

  /**
   * Tries to match a device profile to the given identifier. The given
   * identifier is for example, "BPv3", or "Logic Sniffer".
   * <p>
   * This method will use the {@link DeviceProfile#DEVICE_METADATA_KEYS} of each
   * known device profile to try to find a match with the given identifier.<br/>
   * Note if one of these metadata keys is the string "*", it will always match.
   * </p>
   * 
   * @param aIdentifier
   *          the identifier as string, cannot be <code>null</code>.
   * @return the device profile matching the given identifier, or
   *         <code>null</code> if none is found.
   * @throws InvalidSyntaxException
   *           in case an invalid filter was used.
   */
  public DeviceProfile findProfile( DeviceMetadata aMetadata ) throws InvalidSyntaxException
  {
    Collection<DeviceProfile> allProfiles = getProfiles();
    SortedSet<DeviceProfile> candidates = new TreeSet<DeviceProfile>( new ProfileFilterWildcardComparator() );

    boolean allowWildcardMatch = false;
    for ( int tries = 0; tries < 2; tries++ )
    {
      for ( DeviceProfile profile : allProfiles )
      {
        if ( matches( profile, aMetadata, allowWildcardMatch ) )
        {
          candidates.add( profile );
        }
      }
      // If we've already found something, we do not look any further...
      if ( !candidates.isEmpty() )
      {
        break;
      }
      // None of the device profiles matched exactly, so try with wildcards...
      allowWildcardMatch = true;
    }

    // Pick the highest one...
    return candidates.iterator().next();
  }

  /**
   * Returns the "default" profile, as convenience.
   * 
   * @return a default profile (the OpenBench LogicSniffer), or
   *         <code>null</code> if this profile does not exist.
   * @see #getProfile(String)
   */
  public DeviceProfile getDefaultProfile()
  {
    return getProfile( DEFAULT_PROFILE_TYPE );
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#getName()
   */
  @Override
  public String getName()
  {
    return "LogicSniffer device profile factory";
  }

  /**
   * Returns the device profile for the device type.
   * 
   * @param aType
   *          the device type to get the profile for, cannot be
   *          <code>null</code>.
   * @return the device profile, can be <code>null</code> if no such profile is
   *         found.
   */
  public DeviceProfile getProfile( final String aType )
  {
    final Collection<DeviceProfile> allProfiles = getProfiles();

    for ( DeviceProfile profile : allProfiles )
    {
      if ( aType.equals( profile.getType() ) )
      {
        return profile;
      }
    }
    return null;
  }

  /**
   * Returns the device profile for the device type.
   * 
   * @param aType
   *          the device type to get the profile for, cannot be
   *          <code>null</code>.
   * @return the device profile, can be <code>null</code> if no such profile is
   *         found.
   */
  public List<DeviceProfile> getProfiles()
  {
    List<DeviceProfile> result = new ArrayList<DeviceProfile>();
    result.addAll( this.profiles.values() );
    // Issue #123: sort device profiles alphabetically...
    Collections.sort( result );
    return result;
  }

  /**
   * Returns the number of available profile types.
   * 
   * @return a number profile types.
   */
  public int getSize()
  {
    return this.profiles.size();
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#updated(java.lang.String,
   *      java.util.Dictionary)
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final String aPid, final Dictionary aProperties ) throws ConfigurationException
  {
    try
    {
      DeviceProfile profile = new DeviceProfile();
      profile.setProperties( aProperties );

      this.profiles.put( aPid, profile );
    }
    catch ( IllegalArgumentException exception )
    {
      throw new ConfigurationException( null, "Invalid or missing configuration!", exception );
    }
  }

  /**
   * Tries to find the given identifier in the given set of metadata keys.
   * 
   * @param aIdentifier
   *          the identifier to search for;
   * @param aMetadataKeys
   *          the metadata keys to search in.
   * @return <code>true</code> if the given identifier is found,
   *         <code>false</code> otherwise.
   * @throws InvalidSyntaxException
   *           in case an invalid filter was found.
   */
  private boolean matches( final DeviceProfile aProfile, final DeviceMetadata aMetadata, final boolean aAllowWildcard )
      throws InvalidSyntaxException
  {
    for ( String metadataKey : aProfile.getDeviceMetadataKeys() )
    {
      // Determine whether we're dealing with an OSGi/LDAP-filter or not...
      if ( metadataKey.startsWith( "(" ) && metadataKey.endsWith( ")" ) && metadataKey.length() > 2 )
      {
        Filter filter = FrameworkUtil.createFilter( metadataKey );
        if ( filter.match( aMetadata.asDictionary() ) )
        {
          return true;
        }
      }
      else if ( aMetadata.getName().startsWith( metadataKey ) )
      {
        return true;
      }
      else if ( aAllowWildcard && "*".equals( metadataKey ) )
      {
        return true;
      }
    }
    return false;
  }
}
