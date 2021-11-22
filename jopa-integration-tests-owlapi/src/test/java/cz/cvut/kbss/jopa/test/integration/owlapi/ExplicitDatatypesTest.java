package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.ExplicitDatatypesRunner;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

public class ExplicitDatatypesTest extends ExplicitDatatypesRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ExplicitDatatypesTest.class);

    public ExplicitDatatypesTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    @Override
    protected void verifyValueDatatype(URI identifier, String property, String expectedDatatype) {
        // OWL2Query does not support ASK with a FILTER, so we have to do the check using OWLAPI
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        ((OwlapiDataAccessor) dataAccessor).verifyValueDatatype(ontology, identifier, property, expectedDatatype);
    }
}
