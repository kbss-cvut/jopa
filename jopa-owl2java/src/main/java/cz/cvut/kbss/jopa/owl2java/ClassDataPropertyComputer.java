package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.DataParticipationConstraint;
import java.util.Set;
import org.semanticweb.owlapi.model.OWLDatatype;

public interface ClassDataPropertyComputer {

    OWL2JavaTransformer.Card getCard();

    OWLDatatype getFiller();

    Set<DataParticipationConstraint> getParticipationConstraints();
}
