/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.ic.impl;

import cz.cvut.kbss.jopa.ic.api.*;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class IntegrityConstraintFactoryImpl implements IntegrityConstraintFactory {

    public AtomicSubClassConstraint SubClassConstraint(final OWLClass sub, final OWLClass sup) {
        return new AtomicSubClassConstraintImpl(sub, sup);
    }

    @Override
    public DataParticipationConstraint MinDataParticipationConstraint(final OWLClass s, final OWLDataProperty p,
                                                                      final OWLDatatype o, final int card) {
        return new DataParticipationConstraintImpl(s, p, o, card, -1);
    }

    @Override
    public DataParticipationConstraint MaxDataParticipationConstraint(
            final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
            final int card) {
        return new DataParticipationConstraintImpl(s, p, o, 0, card);
    }

    @Override
    public DataParticipationConstraint DataParticipationConstraint(
            final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
            final int min, int max) {
        return new DataParticipationConstraintImpl(s, p, o, min, max);
    }

    @Override
    public ObjectParticipationConstraint MinObjectParticipationConstraint(
            final OWLClass s, final OWLObjectProperty p, final OWLClass o,
            final int card) {
        return new ObjectParticipationConstraintImpl(s, p, o, card, -1);
    }

    @Override
    public ObjectParticipationConstraint MaxObjectParticipationConstraint(
            final OWLClass s, final OWLObjectProperty p, final OWLClass o,
            final int card) {
        return new ObjectParticipationConstraintImpl(s, p, o, 0, card);
    }

    @Override
    public ObjectParticipationConstraint ObjectParticipationConstraint(
            final OWLClass s, final OWLObjectProperty p, final OWLClass o,
            final int min, int max) {
        return new ObjectParticipationConstraintImpl(s, p, o, min, max);
    }

    @Override
    public ObjectDomainConstraint ObjectPropertyDomainConstraint(
            final OWLObjectProperty p, final OWLClass s) {
        return new ObjectDomainConstraintImpl(p, s);
    }

    @Override
    public ObjectRangeConstraint ObjectPropertyRangeConstraint(
            final OWLClass subj, final OWLObjectProperty p, final OWLClass s) {
        return new ObjectRangeConstraintImpl(subj, p, s);
    }

    @Override
    public DataDomainConstraint DataPropertyDomainConstraint(
            final OWLDataProperty p, final OWLClass s) {
        return new DataDomainConstraintImpl(p, s);
    }

    @Override
    public DataRangeConstraint DataPropertyRangeConstraint(
            final OWLClass subj, final OWLDataProperty p, final OWLDatatype s) {
        return new DataRangeConstraintImpl(subj, p, s);
    }
}
