/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.DataRangeConstraint;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;

public class ClassDataPropertyComputer extends ClassPropertyComputer<DataParticipationConstraint, OWLDatatype> {

    public ClassDataPropertyComputer(
            final OWLClass clazz,
            final OWLDataProperty prop,
            final IntegrityConstraintSet set,
            final OWLOntology ontology
    ) {
        boolean hasFiller = true;
        set.getClassDataIntegrityConstraints(clazz, prop).forEach(integrityConstraint -> {
            if (integrityConstraint instanceof DataParticipationConstraint) {
                constraints.add((DataParticipationConstraint) integrityConstraint);
            } else if (integrityConstraint instanceof DataRangeConstraint) {
                this.filler = ((DataRangeConstraint) integrityConstraint).getRange();
            }
        });

        if (filler == null) {
            hasFiller = false;
            this.filler = ontology.getOWLOntologyManager().getOWLDataFactory().getRDFPlainLiteral();
        }

        if (constraints.isEmpty() && !hasFiller) {
            this.card = Card.NO;
        } else {
            this.card = Card.MULTIPLE;
            for (final DataParticipationConstraint opc : constraints) {
                final OWLDatatype dt2 = opc.getObject();
                if ((getFiller().equals(dt2) ||
                        dt2.equals(OWLManager.getOWLDataFactory().getTopDatatype())) && opc.getMax() == 1) {
                    this.card = Card.ONE;
                    return;
                }
            }
        }
    }
}
