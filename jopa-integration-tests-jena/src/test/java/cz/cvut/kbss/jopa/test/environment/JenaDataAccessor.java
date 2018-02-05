package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.Collection;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertTrue;

public class JenaDataAccessor {

    public void persistTestData(Collection<Triple> data, EntityManager em) {
        em.getTransaction().begin();
        final Dataset ds = em.unwrap(Dataset.class);
        final Model model = ds.getDefaultModel();
        for (Triple t : data) {
            final Resource subject = createResource(t.getSubject().toString());
            final Property property = createProperty(t.getProperty().toString());
            final RDFNode value;
            if (t.getValue() instanceof URI) {
                value = createResource(t.getValue().toString());
            } else {
                value = t.getValue() instanceof String ?
                        ResourceFactory.createLangLiteral(t.getValue().toString(), t.getLanguage()) :
                        ResourceFactory.createTypedLiteral(t.getValue());
            }
            model.add(subject, property, value);
        }
        em.getTransaction().commit();
    }

    public void verifyDataPresence(Collection<Triple> expected, EntityManager em) {
        final Dataset ds = em.unwrap(Dataset.class);
        final Model model = ds.getDefaultModel();
        for (Triple t : expected) {
            if (t.getValue() instanceof URI) {
                assertTrue(model.contains(createResource(t.getSubject().toString()),
                        createProperty(t.getProperty().toString()), createResource(t.getValue().toString())));
            } else {
                if (t.getLanguage() != null) {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()),
                            createLangLiteral(t.getValue().toString(), t.getLanguage())));
                } else {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()), createTypedLiteral(t.getValue())));
                }
            }
        }
    }
}
