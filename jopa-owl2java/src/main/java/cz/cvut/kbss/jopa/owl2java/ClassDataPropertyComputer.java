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
        final IntegrityConstraintParser parser,
        final OWLOntology ontology
    ) {
        parser.getClassDataIntegrityConstraints(clazz, prop).forEach(integrityConstraint -> {
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
