package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.ParticipationConstraint;
import org.semanticweb.owlapi.model.OWLLogicalEntity;

import java.util.HashSet;
import java.util.Set;

abstract class ClassPropertyComputer<T extends ParticipationConstraint, F extends OWLLogicalEntity> {

    final Set<T> constraints = new HashSet<>();
    F filler;
    Card card;

    Set<T> getParticipationConstraints() {
        return constraints;
    }

    F getFiller() {
        return filler;
    }

    Card getCard() {
        return card;
    }
}
