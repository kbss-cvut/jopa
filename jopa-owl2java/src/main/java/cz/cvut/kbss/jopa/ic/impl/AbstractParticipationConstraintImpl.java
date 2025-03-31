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

import cz.cvut.kbss.jopa.ic.api.ParticipationConstraint;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

abstract class AbstractParticipationConstraintImpl<P extends OWLProperty, O extends OWLObject>
    implements ParticipationConstraint<P, O> {

    private final OWLClass subject;

    private final P predicate;

    private final O object;

    private final int min;

    private final int max;

    public AbstractParticipationConstraintImpl(final OWLClass subject,
                                               final P predicate, final O object, final int min, final int max) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
        this.min = min;
        this.max = max;
    }

    @Override
    public O getObject() {
        return object;
    }

    @Override
    public P getPredicate() {
        return predicate;
    }

    @Override
    public OWLClass getSubject() {
        return subject;
    }

    @Override
    public int getMin() {
        return min;
    }

    @Override
    public int getMax() {
        return max;
    }
}
