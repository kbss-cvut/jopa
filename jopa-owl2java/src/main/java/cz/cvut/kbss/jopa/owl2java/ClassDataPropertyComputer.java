/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.DataRangeConstraint;
import java.util.HashSet;
import java.util.Set;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;

public class ClassDataPropertyComputer {

    private Set<DataParticipationConstraint> constraints = new HashSet<>();
    private OWLDatatype filler;
    private Card card;

    public ClassDataPropertyComputer(
        final OWLClass clazz,
        final OWLDataProperty prop,
        final IntegrityConstraintSet set,
        final OWLOntology ontology
    ) {
        set.getClassDataIntegrityConstraints(clazz, prop).forEach(integrityConstraint -> {
            if (integrityConstraint instanceof DataParticipationConstraint) {
                this.constraints.add((DataParticipationConstraint) integrityConstraint);
            } else if (integrityConstraint instanceof DataRangeConstraint) {
                this.filler = ((DataRangeConstraint) integrityConstraint).getRange();
            }
        });

        if (filler == null) {
            filler = ontology.getOWLOntologyManager().getOWLDataFactory().getRDFPlainLiteral();
        }

        if (constraints.isEmpty()) {
            card = Card.NO;
        } else {
            card = Card.MULTIPLE;
            for (final DataParticipationConstraint opc : getParticipationConstraints()) {
                final OWLDatatype dt2 = opc.getObject();
                if (getFiller().equals(dt2)
                    || dt2.equals(OWLManager.getOWLDataFactory()
                    .getTopDatatype())) {
                    if (opc.getMax() == 1) {
                        card = Card.ONE;
                        return;
                    }
                }
            }
        }
    }

    public Card getCard() {
        return card;
    }

    public OWLDatatype getFiller() {
        return filler;
    }

    public Set<DataParticipationConstraint> getParticipationConstraints() {
        return constraints;
    }

}
