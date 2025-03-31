/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.IntegrityConstraintVisitor;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

class DataParticipationConstraintImpl extends
    AbstractParticipationConstraintImpl<OWLDataProperty, OWLDatatype>
    implements DataParticipationConstraint {

    public DataParticipationConstraintImpl(OWLClass subject,
                                           OWLDataProperty predicate, OWLDatatype object,
                                           int min, int max) {
        super(subject, predicate, object, min, max);
    }

    @Override
    public void accept(IntegrityConstraintVisitor v) {
        v.visit(this);
    }
}
