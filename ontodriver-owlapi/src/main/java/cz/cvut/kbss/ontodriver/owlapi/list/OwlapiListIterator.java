package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

import java.util.Collection;

abstract class OwlapiListIterator {

    abstract boolean hasNext();

    abstract Axiom<NamedResource> next();

    void checkMaxSuccessors(OWLObjectProperty property, Collection<? extends OWLIndividual> successors) {
        if (successors.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid number of successors. Expected only 1 value of property " + property + ", but got " +
                            successors.size());
        }
    }

    void checkIsNamed(OWLIndividual individual) {
        if (!individual.isNamed()) {
            throw new IllegalArgumentException("Expected OWLNamedIndividual, but got an anonymous one.");
        }
    }
}
