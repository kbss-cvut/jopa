package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.ObjectParticipationConstraint;
import cz.cvut.kbss.jopa.ic.api.ObjectRangeConstraint;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import java.util.Collection;
import java.util.HashSet;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

public class ClassObjectPropertyComputer {

    private Collection<ObjectParticipationConstraint> constraints = new HashSet<>();
    private OWLClass filler;
    private Card card;

    public ClassObjectPropertyComputer(final OWLClass clazz,
                                       final OWLObjectProperty prop,
                                       final IntegrityConstraintSet set,
                                       final OWLOntology merged
    ) {
        set.getClassObjectIntegrityConstraints(clazz, prop).forEach(ic -> {
            if (ic instanceof ObjectParticipationConstraint) {
                constraints.add((ObjectParticipationConstraint) ic);
            } else if (ic instanceof ObjectRangeConstraint) {
                filler = ((ObjectRangeConstraint) ic).getRange();
            }
        });

        if (filler == null) {
            filler = merged.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
        }

        if (constraints.isEmpty()) {
            card = Card.NO;
        } else {
            final OWLDataFactory f = merged.getOWLOntologyManager().getOWLDataFactory();

            final OWLClass object = filler;

            if (object.getSuperClasses(merged).contains(
                f.getOWLClass(IRI.create(SequencesVocabulary.c_List)))) {
                this.filler = new ClassObjectPropertyComputer(object,
                    f.getOWLObjectProperty(IRI.create(SequencesVocabulary.p_element)),
                    set,
                    merged
                )
                    .getFiller();
                card = Card.LIST;
            } else if (filler.getSuperClasses(merged).contains(
                f.getOWLClass(IRI.create(SequencesVocabulary.c_OWLSimpleList)))) {
                this.filler = new ClassObjectPropertyComputer(object,
                    f.getOWLObjectProperty(IRI.create(SequencesVocabulary.p_hasNext)),
                    set,
                    merged
                )
                    .getFiller();
                card = Card.SIMPLELIST; // TODO referenced
            } else {
                card = Card.MULTIPLE;
                for (ObjectParticipationConstraint opc : constraints) {
                    OWLClass dt2 = opc.getObject();
                    if (filler.equals(dt2)
                        || dt2.equals(OWLManager.getOWLDataFactory()
                        .getOWLThing())) {
                        if (opc.getMax() == 1) {
                            card = Card.ONE;
                            break;
                        }
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