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
package nl.lxtreme.osgi.metatype;


import static nl.lxtreme.osgi.metatype.Config.*;

import java.lang.annotation.*;


/**
 * The AttributeDefinition element in the MetaType specification.
 */
@Target( ElementType.METHOD )
@Retention( RetentionPolicy.RUNTIME )
public @interface AD
{
  // METHODS

  /**
   * The cardinality of the attribute. If not explicitly set it will be derived
   * from the attributes return type. Collections return Integer.MIN_VALUE and
   * arrays use Integer.MAX_VALUE. If a single string needs to be converted to a
   * Collection or array then the | will be used as a separator to split the
   * line.
   * 
   * @return the cardinality of the attribute, defaults to 0, indicating only a
   *         single value is returned by this attribute.
   */
  int cardinality() default 0;

  /**
   * The default value. This value must be converted to the return type of the
   * attribute. For multi valued returns use the | as separator.
   * 
   * @return the default value, defaults to no value.
   */
  String deflt() default NULL;

  /**
   * A description of the attribute. Default is empty.
   * 
   * @return The description of the attribute.
   */
  String description() default NULL;

  /**
   * The id of the attribute. By default the name of the method. The id is the
   * key used to access the properties. This is the reason the AD is a runtime
   * annotation so the runtime can find the proper key.
   * 
   * @return the id, defaults to a generated name.
   */
  String id() default NULL;

  /**
   * The maximum value. This string must be converted to the attribute type
   * before comparison takes place.
   * 
   * @return the max value, defaults to no maximum value.
   */
  String max() default NULL;

  /**
   * The minimum value. This string must be converted to the attribute type
   * before comparison takes place.
   * 
   * @return the min value, defaults to no minimum value.
   */
  String min() default NULL;

  /**
   * The name of the attribute. By default the un-camel cased version of the
   * method name.
   * 
   * @return the name, defaults to no name.
   */
  String name() default NULL;

  /**
   * Provide labels for options. These labels must match the values. If no
   * labels are set, the un-cameled version of the values are used (if they are
   * set of course).
   * 
   * @return the option labels, defaults to no option labels.
   */
  String[] optionLabels() default NULL;

  /**
   * The values of options. If not set and the return type is an enum class then
   * the values will be derived from this return type.
   * 
   * @return the option labels, defaults to no option values.
   */
  String[] optionValues() default NULL;

  /**
   * Indicates that this attribute is required. By default attributes are
   * required.
   * 
   * @return <code>true</code> (the default) if this attribute is required,
   *         <code>false</code> if it is optional.
   */
  boolean required() default true;

  /**
   * The type of the field. This must be one of the basic types in the metatype
   * specification. By default, the type is derived from the return type of the
   * method. This includes most collections and arrays. Unrecognized types are
   * defaulted to String.
   * 
   * @return the type to be used, defaults to {@link Type#String}.
   */
  Type type() default Type.String;
}
