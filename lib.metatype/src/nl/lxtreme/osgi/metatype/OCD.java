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
 * Maps to the OCD element in the Metatype specification. The only difference is
 * that it is possible to create a Designate element as well.
 */
@Target( ElementType.TYPE )
@Retention( RetentionPolicy.RUNTIME )
public @interface OCD
{
  // METHODS

  /**
   * A description for this ocd. The default is empty.
   * 
   * @return the description
   */
  String description() default NULL;

  /**
   * Defines if this is for a factory or not.
   */
  boolean factory() default false;

  /**
   * The id of the component. Default the name of the class in FQN notation but
   * with nested classes using the $ as separator (not .). The Felix webconsole
   * always uses this id as the PID and not the pid in the Designate element.
   * Reported as an error.
   * 
   * @return the id, defaults to the name of the class this annotation is
   *         present on.
   */
  String id() default NULL;

  /**
   * The localization prefix. The default localization prefix is the name of the
   * class with a $ separator for nested classes.
   * 
   * @return the localization prefix.
   */
  String localization() default NULL;

  /**
   * The name for this component. The default name is a the short class name
   * that us un-camel cased to make it more readable.
   * 
   * @return The name of this component.
   */
  String name() default NULL;
}
