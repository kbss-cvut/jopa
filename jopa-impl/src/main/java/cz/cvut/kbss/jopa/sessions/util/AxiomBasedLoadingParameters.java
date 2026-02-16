/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

/**
 * Loading parameters for loading an entity from axioms.
 *
 * @param cls         Entity class
 * @param descriptor  Entity descriptor
 * @param bypassCache Whether to bypass cache
 * @param axioms      Axioms to load the entity from
 * @param <T>         Entity type
 */
public record AxiomBasedLoadingParameters<T>(Class<T> cls, Descriptor descriptor, boolean bypassCache,
                                             Collection<Axiom<?>> axioms) {
}
