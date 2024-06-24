/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraintVisitor;
import cz.cvut.kbss.jopa.ic.api.ObjectRangeConstraint;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ObjectRangeConstraintImpl extends AbstractObjectDomainRangeConstraintImpl implements ObjectRangeConstraint {

    final OWLClass subj;

    public ObjectRangeConstraintImpl(OWLClass c, OWLObjectProperty p, OWLClass o) {
        super(p, o);
        this.subj = c;
    }

    @Override
    public OWLClass getRange() {
        return getClazz();
    }

    @Override
    public void accept(IntegrityConstraintVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public OWLClass getOWLClass() {
        return subj;
    }
}
