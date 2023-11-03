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
package cz.cvut.kbss.jopa.ic.impl;

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraint;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;

abstract class AbstractDataDomainRangeConstraintImpl implements
    IntegrityConstraint {

    private final OWLClass o;
    private final OWLDataProperty p;

    public AbstractDataDomainRangeConstraintImpl(OWLDataProperty p, OWLClass o) {
        this.o = o;
        this.p = p;
    }

    public OWLClass getClazz() {
        return o;
    }

    public OWLDataProperty getProperty() {
        return p;
    }
}
