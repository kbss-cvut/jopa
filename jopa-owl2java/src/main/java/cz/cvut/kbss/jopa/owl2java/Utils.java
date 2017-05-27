package cz.cvut.kbss.jopa.owl2java;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataRange;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;

public class Utils {

    static OWLDatatype ensureDatatype(final OWLDataRange r)
        throws UnsupportedICException {
        if (!r.isDatatype()) {
            throw new UnsupportedICException("Data ranges not supported: " + r);
        }

        if (!r.asOWLDatatype().isBuiltIn()) {
            throw new UnsupportedICException(
                "Only built in datatypes are supported: " + r);
        }

        return r.asOWLDatatype();
    }

    static OWLClass ensureClass(final OWLClassExpression r) {
        if (!r.isAnonymous()) {
            return r.asOWLClass();
        }
        throw new UnsupportedICException("Only named classes are supported: " + r);
    }

    static OWLDataProperty ensureDataProperty(final OWLDataPropertyExpression e) {
        if (e.isAnonymous()) {
            throw new UnsupportedICException(
                "Data property expressions not supported: " + e);
        }

        return e.asOWLDataProperty();
    }

    static OWLObjectProperty ensureObjectProperty(
        final OWLObjectPropertyExpression e) throws UnsupportedICException {
        if (e.isAnonymous()) {
            throw new UnsupportedICException(
                "Object property expressions not supported: " + e);
        }

        return e.asOWLObjectProperty();
    }
}
