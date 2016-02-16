/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

abstract class AbstractParticipationConstraintImpl<P extends OWLProperty<?, ?>, O extends OWLObject>
        implements ParticipationConstraint<P, O> {

    private OWLClass subject;

    private P predicate;

    private O object;

    private int min;

    private int max;

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
