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
package cz.cvut.kbss.jopa.ic.api;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public interface IntegrityConstraintFactory {

    AtomicSubClassConstraint SubClassConstraint(
        final OWLClass sub, final OWLClass sup);

    DataParticipationConstraint MinDataParticipationConstraint(
        final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
        final int card);

    DataParticipationConstraint MaxDataParticipationConstraint(
        final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
        final int card);

    DataParticipationConstraint DataParticipationConstraint(
        final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
        final int min, int max);

    ObjectParticipationConstraint MinObjectParticipationConstraint(
        final OWLClass s, final OWLObjectProperty p, final OWLClass o,
        final int card);

    ObjectParticipationConstraint MaxObjectParticipationConstraint(
        final OWLClass s, final OWLObjectProperty p, final OWLClass o,
        final int card);

    ObjectParticipationConstraint ObjectParticipationConstraint(
        final OWLClass s, final OWLObjectProperty p, final OWLClass o,
        final int min, int max);

    ObjectDomainConstraint ObjectPropertyDomainConstraint(
        final OWLObjectProperty p, final OWLClass s);

    ObjectRangeConstraint ObjectPropertyRangeConstraint(
        final OWLClass subj, final OWLObjectProperty p, final OWLClass s);

    DataDomainConstraint DataPropertyDomainConstraint(
        final OWLDataProperty p, final OWLClass s);

    DataRangeConstraint DataPropertyRangeConstraint(
        final OWLClass subj, final OWLDataProperty p, final OWLDatatype s);
}
