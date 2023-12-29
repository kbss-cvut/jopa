/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Designates a class whose mapping information is applied to the entities that inherit from it.
 * <p>
 * A mapped superclass itself does not represent any ontological type.
 * A class designated with the MappedSuperclass annotation can be mapped in the same way as an entity except that the mappings
 * will apply only to its subclasses since no OWL class exists for the mapped superclass itself.
 * When applied to the subclasses the inherited mappings will apply in the context of the subclass ontological types.
 */
@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface MappedSuperclass {
}
