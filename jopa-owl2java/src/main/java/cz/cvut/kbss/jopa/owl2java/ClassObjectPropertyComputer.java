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

import cz.cvut.kbss.jopa.ic.api.ObjectParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.ObjectRangeConstraint;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class ClassObjectPropertyComputer {

    private final Collection<ObjectParticipationConstraint> constraints = new HashSet<>();
    private OWLClass filler;
    private Card card;

    public ClassObjectPropertyComputer(final OWLClass clazz,
                                       final OWLObjectProperty prop,
                                       final IntegrityConstraintSet set,
                                       final OWLOntology merged
    ) {
        boolean hasFiller = true;
        set.getClassObjectIntegrityConstraints(clazz, prop).forEach(ic -> {
            if (ic instanceof ObjectParticipationConstraint) {
                constraints.add((ObjectParticipationConstraint) ic);
            } else if (ic instanceof ObjectRangeConstraint) {
                filler = ((ObjectRangeConstraint) ic).getRange();
            }
        });

        if (filler == null) {
            hasFiller = false;
            filler = merged.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
        }

        if (constraints.isEmpty() && !hasFiller) {
            card = Card.NO;
        } else {
            final OWLDataFactory f = merged.getOWLOntologyManager().getOWLDataFactory();

            final OWLClass object = filler;
            final Set<OWLClassExpression> superClasses = EntitySearcher.getSuperClasses(object, merged).collect(
                    Collectors.toSet());

            if (superClasses.contains(f.getOWLClass(IRI.create(SequencesVocabulary.c_List)))) {
                this.filler = new ClassObjectPropertyComputer(object,
                        f.getOWLObjectProperty(IRI.create(SequencesVocabulary.p_element)), set, merged).getFiller();
                card = Card.LIST;
            } else if (superClasses.contains(f.getOWLClass(IRI.create(SequencesVocabulary.c_OWLSimpleList)))) {
                this.filler = new ClassObjectPropertyComputer(object,
                        f.getOWLObjectProperty(IRI.create(SequencesVocabulary.p_hasNext)), set, merged).getFiller();
                card = Card.SIMPLELIST; // TODO referenced
            } else {
                card = Card.MULTIPLE;
                for (ObjectParticipationConstraint opc : constraints) {
                    OWLClass dt2 = opc.getObject();
                    if ((filler.equals(dt2) || dt2.equals(OWLManager.getOWLDataFactory().getOWLThing())) &&
                            opc.getMax() == 1) {
                        card = Card.ONE;
                        break;
                    }
                }
            }
        }
    }

    public Card getCard() {
        return card;
    }

    public OWLClass getFiller() {
        return filler;
    }

    public Collection<ObjectParticipationConstraint> getParticipationConstraints() {
        return constraints;
    }
}